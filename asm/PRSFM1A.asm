*          DATA SET PRSFM1A    AT LEVEL 179 AS OF 08/31/20                      
*PHASE T41C1AA                                                                  
*INCLUDE PPBROWSE                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*               T41C1A - ESTIMATE MAINT/LIST                                    
*                                                                               
* KWAN 07/17/20 ESTIMATE UPLOAD (SPEC-45879)                                    
*                                                                               
* KWAN 08/05/16 Set Prisma estimate per F0 profile                              
*                                                                               
* KWAN 09/03/15 Fix estimate bucket check on add                                
*                                                                               
* KWAN 09/25/14 Change iDesk label to Prisma                                    
*                                                                               
* KWAN 09/16/14 Allow idesk flag changes                                        
*                                                                               
* KWAN 08/24/11 COS2 factor fix (cleared on change)                             
*                                                                               
* KWAN 04/04/11 iDesk Estimate                                                  
*                                                                               
* KWAN 12/15/08 MAKING TEST EST LIVE, CLEAR PLANNED COST BUCKET                 
*                                                                               
* KWAN 01/25/07 TEST EST CHANGE BUG FIX INTRODUCED BY STEW EST                  
*                                                                               
* KWAN 12/06/06 ACTUALIZATION LIST AND REPORT                                   
*                                                                               
* KWAN 11/08/06 ESTIMATE ACTUALIZATION DATE                                     
*                                                                               
* KWAN 10/20/06 BILL ON PLANNED COST EFFECTIVE DATE                             
*                                                                               
* KWAN 06/07/06 REDEFINE STATUS FILTER (PREVIOUSLY KNOWN AS TEST)               
*                                                                               
* KWAN 06/01/06 ENABLE LIST FILTER FOR REPORT ACTION                            
*                                                                               
* BOBY 02/16/06 ADD STEWARDSHIP TO ESTIMATE STATUS TYPES                        
*                                                                               
* SMYE 12/14/05 CODE FOR CHANGED SCREEN STATUS FIELD PARAMETERS                 
*                                                                               
* KWAN 11/10/05 ZERO ESTIMATE AND MASTER CLIENT FIX                             
*                                                                               
* KWAN 08/24/05 TEST ESTIMATE FIX                                               
*                                                                               
* KWAN 08/11/05 RATE TYPE FIX                                                   
*                                                                               
* KWAN 06/06/05 BROWSE FUNCTION                                                 
*                                                                               
* KWAN 03/16/05 CK MAX I/O WHEN FILTERING LARGE AMT OF EST DATA                 
*                                                                               
* KWAN 02/10/05 NEED TO GENERATE AN AUTO P41 T/A REPORT                         
*                                                                               
* KWAN 11/17/04 BILLING REP (PESTBREP)                                          
*                                                                               
* KWAN 09/10/04 FIX FOR CHANGING STATUS FROM TEST TO LIVE                       
*                                                                               
* KWAN 06/21/04 BUG FIX FOR SOON REPORT                                         
*                                                                               
* KWAN 05/06/04 MORE FILTER KEYWORDS FOR LIST                                   
*                                                                               
* KWAN 01/09/04 APPLY FEILD CONTROL FEATURE FOR OPTION FIELDS                   
*                                                                               
* KWAN 02/26/03 CONVERT ESTIMATE FROM FIL TO SFM                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                     *         
*  CALLED FROM  GENCON VIA T41C00 (SFM CONTROLLER)                    *         
*                                                                     *         
*  COMMENTS     SUPPORTS ADD, CHG, DISP, SEL, LIST, REP               *         
*                                                                     *         
*  INPUTS       SCREEN T41CA3 (MAINTENANCE)                           *         
*               SCREEN T41CA4 (LIST)                                  *         
*                                                                     *         
*  OUTPUTS                                                            *         
*                                                                     *         
*  REGISTERS    R0 -- WORK                                            *         
*               R1 -- WORK                                            *         
*               R2 -- WORK                                            *         
*               R3 -- WORK                                            *         
*               R4 -- WORK                                            *         
*               R5 -- WORK                                            *         
*               R6 -- WORK                                            *         
*               R7 -- SECOND BASE REGISTER                            *         
*               R8 -- SPOOLD                                          *         
*               R9 -- SYSD                                            *         
*               RA -- TWA                                             *         
*               RB -- FIRST BASE                                      *         
*               RC -- GEND                                            *         
*               RD -- SYSTEM                                          *         
*               RE -- SYSTEM                                          *         
*               RF -- SYSTEM                                          *         
*                                                                     *         
*  I/O AREAS                                                          *         
*                                                                     *         
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         TITLE 'T41C1A - ESTIMATE MAINT/LIST'                                   
*                                                                               
T41C1A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,T41C1A,R7,RR=R3   NOTE USE OF R7 AS 2ND BASE REGISTER          
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
*                                                                               
         ST    R3,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         CLI   TWAOFFC,C'*'        DDS TERMINAL ?                               
         BE    *+14                                                             
         OC    TWASAGN,TWASAGN     NEW SECURITY?                                
         JZ    ACCERR                                                           
*                                                                               
         BRAS  RE,INITIALZ         INITIALIZE WORKING STORAGES                  
         CLI   MODE,NEWSCR         SCR JUST BEING LOADED?                       
         JE    EXIT                YES, DISPLAY INIT'D SCR                      
*                                                                               
* IF FOLLOWING COMPARE IS NOT MADE, PF12 WILL NOT DISPLAY FIRST                 
* SELECTED RECORD (I.E. PF12 IS USED INSTEAD OF NORMAL ENTER)                   
*                                                                               
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         MVI   PFAID,0             SET PFKEY SAME AS ENTER                      
*                                                                               
         CLI   ACTNUM,ACTSEL       SELECT? (CKING FOR PF KEYS)                  
         BNE   CKMODE                                                           
         TM    GENSTAT2,NEXTSEL                                                 
         JO    EXIT                                                             
         CLI   PFAID,12            PF 12 OR 24 FOR RETURN?                      
         BE    RTN                                                              
         CLI   PFAID,24                                                         
         BNE   STY                                                              
RTN      OI    GENSTAT2,NEXTSEL+RETEQSEL                                        
         MVI   PFAID,0                                                          
         J     EXIT                                                             
*                                                                               
STY      OI    GENSTAT2,RETEQSEL   SET TO RETURN THIS SELECTION                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMODE   CLI   ACTNUM,ACTDEL       ACTION DELETE?                               
         JE    RECACERR                                                         
         CLI   ACTNUM,ACTREST      ACTION RESTORE?                              
         JE    RECACERR                                                         
*                                                                               
         CLI   PFAID,0             PFKEY IS PRESSED?                            
         BE    *+12                NO                                           
         BRAS  RE,CKPFKEYS                                                      
         JNE   PFKEYERR            INVALID PFKEY IS PRESSED                     
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
*                                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BNE   *+8                                                              
         BRAS  RE,DK                                                            
*                                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,XRECADD        AFTER ADD                                    
         BE    PPTRS                                                            
         CLI   MODE,XRECPUT        AFTER PUT                                    
         BE    PPTRS                                                            
         CLI   MODE,PRINTREP       PRINT REPORT                                 
         BE    PR                                                               
*                                                                               
SETCCEQ  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SETCCNEQ LTR   RB,RB               NOT EQUAL                                    
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VK       DS    0H                  VALIDATE KEY ROUTINE                         
         LA    R2,ESTMEDH          POINT TO MEDIA FLD                           
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTMEDH                                                       
         GOTO1 VALIMED             MEDIA IS REQUIRED EVEN FOR LIST              
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK10                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK10                                                             
         MVC   ESTMEDN,MEDNM                                                    
         OI    ESTMEDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK10     XC    SVKEY,SVKEY                                                      
         LA    R6,SVKEY                                                         
         USING PESTKEY,R6                                                       
*                                                                               
         MVC   PESTKAGY,AGENCY                                                  
         MVC   PESTKMED,QMED                                                    
         MVI   PESTKRCD,X'07'      RECORD CODE FOR EST                          
*                                                                               
         XC    CLTLSFLT,CLTLSFLT   CLEAR LIST FILTER ON CLT                     
         LA    R2,ESTCLTH          POINT TO CLT FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTCLTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK20                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JNE   MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
         OC    TWAACCS(2),TWAACCS  ANY LIMIT ACCESS?                            
         BZ    VK20                NO                                           
         CLI   CONWHENH+5,0        NOW, SOON, OR OV?                            
         BE    VK20                NO                                           
*                                                                               
         J     CLTRQERR            SECURITY - CLIENT REQUIRED                   
*                                                                               
VK20     CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK24                                                             
*                                                                               
         GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               0,(QMED,C' CLT'),0,RR=RELO                                       
         DC    H'0'                                                             
*                                                                               
VK24     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALICLT                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK30                FOR REPORT, NO NEED TO VALICLT               
         CLI   5(R2),0             NO INPUT?                                    
         BE    VK50                                                             
         CLC   =C'ALL',8(R2)       ALL CLIENT?                                  
         BE    VK50                YES, LEAVE CLIENT FILTER NULLS               
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   CLTLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    CLTLSFLT,SPACES     SPACE PADDED                                 
         B     VK50                                                             
*                                                                               
VK30     CLC   =C'ALL',8(R2)                                                    
         JE    INVFDERR            CLT CODE "ALL" IS NOT ALLOWED                
         GOTO1 VALICLT                                                          
         CLI   SVCPROF+5,C'1'      MASTER CLIENT?                               
         BNE   VK36                                                             
         LHI   R2,284              CANNOT ADD EST FOR MASTER CLT                
         BRAS  RE,GET_ETXT                                                      
         LA    R2,ESTCLTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
VK36     MVC   PESTKCLT,QCLT                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK50                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK50                                                             
         MVC   ESTCLTN,CLTNM                                                    
         OI    ESTCLTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK50     XC    PRDLSFLT,PRDLSFLT   CLEAR LIST FILTER ON PRD                     
         LA    R2,ESTPRDH          POINT TO PRD FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTPRDH                                                       
         CLI   5(R2),0                                                          
         BNE   VK60                                                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JNE   MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
* PRD SECURITY CAN BE CKED HERE!                                                
*                                                                               
VK60     CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK64                                                             
*                                                                               
         LA    RF,QCLT                                                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    RF,CLTLSFLT                                                      
*                                                                               
         OC    0(3,RF),0(RF)       CLIENT CODE PRESENT?                         
         BNZ   VK62                                                             
         LA    R2,ESTCLTH          POINT TO PRD FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTCLTH                                                       
         J     MSSNGERR            CANNOT BROWSE PRD W/O CLT CODE               
*                                                                               
VK62     GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,0(RF)),(QMED,C' PRD'),0,RR=RELO                               
         DC    H'0'                                                             
*                                                                               
VK64     CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALIPRD                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK70                FOR REPORT, NO NEED TO VALIPRD               
         CLI   5(R2),0             NO INPUT?                                    
         BE    VK74                                                             
         CLC   =C'ALL',8(R2)       ALL PRD?                                     
         BE    VK74                YES, LEAVE PRD FILTER NULLS                  
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PRDLSFLT(0),8(R2)   GET CLIENT CODE                              
         OC    PRDLSFLT,SPACES     SPACE PADDED                                 
         B     VK74                                                             
*                                                                               
VK70     CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK72                                                             
         CLC   =C'ALL',8(R2)                                                    
         JE    INVFDERR            CANNOT ADD PRD CODE "ALL"                    
         ZIC   RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PESTKPRD(0),8(R2)   GET PRD CODE                                 
         OC    PESTKPRD,SPACES     SPACE PADDED                                 
         B     VK74                                                             
*                                                                               
VK72     GOTO1 VALIPRD                                                          
         MVC   PESTKPRD,QPRD                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK74                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK74                                                             
         MVC   ESTPRDN,PRDNM                                                    
         OI    ESTPRDNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
* Validate estimate number                                                      
*                                                                               
VK74     XC    ESTLSFLT,ESTLSFLT   CLEAR LIST FILTER ON EST                     
         LA    R2,ESTESTH          POINT TO EST FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTESTH                                                       
         CLI   5(R2),0                                                          
         BNE   VK74E                                                            
*                                                                               
         BRAS  RE,AUTOESTN         Auto assign estimate number                  
         JE    VK74E                                                            
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                YES                                          
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         JNE   MSSNGERR            FIELD IS REQUIRED                            
*                                                                               
VK74E    CLI   8(R2),C'='          BROWSE?                                      
         BNE   VK74H                                                            
*                                                                               
         LA    RF,QCLT                                                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    RF,CLTLSFLT                                                      
*                                                                               
         OC    0(3,RF),0(RF)       CLIENT CODE PRESENT?                         
         BNZ   VK74F                                                            
         LA    R2,ESTCLTH          POINT TO PRD FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTCLTH                                                       
         J     MSSNGERR            CANNOT BROWSE PRD W/O CLT CODE               
*                                                                               
VK74F    LA    RE,QPRD                                                          
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    RE,PRDLSFLT                                                      
*                                                                               
         OC    0(3,RE),0(RE)       PRD CODE PRESENT?                            
         BNZ   VK74G                                                            
         LA    R2,ESTPRDH          POINT TO PRD FLD                             
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   *+8                                                              
         LA    R2,LSTPRDH                                                       
         J     MSSNGERR            CANNOT BROWSE EST W/O PRD CODE               
*                                                                               
VK74G    GOTO1 =V(PPBROWSE),DMCB,ACOMFACS,SYSRD,(R2),                  +        
               (0,0(RF)),(QMED,C' EST'),0,RR=RELO                               
         DC    H'0'                                                             
*                                                                               
VK74H    CLI   5(R2),3             MAX 3 CHARS                                  
         JH    INVFDERR                                                         
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                FOR LIST, NO NEED TO VALIEST                 
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VK74J               FOR REPORT, NO NEED TO VALIEST               
         CLI   5(R2),0             NO INPUT?                                    
         BE    VK90                                                             
         TM    4(R2),X'08'         VALID NUMBERIC?                              
         JZ    NTNUMERR                                                         
         BRAS  RE,PACK                                                          
         MVC   ESTLSFLT,SAVE_R0+2  BINARY ESTIMATE                              
         B     VK90                                                             
*                                                                               
VK74J    TM    4(R2),X'08'         VALID NUMBERIC?                              
         JZ    NTNUMERR                                                         
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VK74N                                                            
         BRAS  RE,PACK                                                          
         OC    SAVE_R0,SAVE_R0     ZERO ESTIMATE?                               
         JZ    INVESTER                                                         
         MVC   ESTLSFLT,SAVE_R0+2  BINARY ESTIMATE                              
         MVC   PESTKEST,SAVE_R0+2                                               
         MVC   BEST,PESTKEST       FOR AUTOREQ                                  
         B     VK90                                                             
*                                                                               
VK74N    GOTO1 VALIEST                                                          
         MVC   PESTKEST,BEST                                                    
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    VK90                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BE    VK90                                                             
         MVC   ESTESTN,ESTNM                                                    
         OI    ESTESTNH+6,X'80'    NOT DISPLAYED FOR LIST AND REPORT            
*                                                                               
VK90     LA    R0,F_FLDS                                                        
         LHI   R1,F_FLDLNQ         LENGTH OF FILTER FLDS STORAGE                
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION LIST?                                 
         BE    *+12                                                             
         CLI   ACTNUM,ACTREP       ACTION REPORT?                               
         BNE   VKX                                                              
         LA    R2,LSTFLTH          POINT TO FITLER FLD ON SCR                   
         CLI   5(R2),0                                                          
         BE    VKX                                                              
*                                                                               
         BRAS  RE,CKFLTFLD         VALIDATE FLT FLDS                            
         JNE   TRAPERR2            ERROR MSG IS SET IN ROUTINE                  
*                                                                               
VKX      MVC   KEY,SVKEY                                                        
         MVC   AIO,AIO1            RECORD WILL BE READ INTO AIO1                
         B     EXIT                                                             
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
VR       DS    0H                  VALIDATE RECORD                              
**NO-OP  LA    R0,WKCLTFIN                                                      
         LA    R0,WKSVSTRT                                                      
         LHI   R1,WKSVLNQ          LENGTH OF SAVING WRK STORAGE                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE               SAME AS XCEF, ONLY SHORTER                   
*                                                                               
         CLI   ACTNUM,ACTSEL       SEL?                                         
         BNE   VR05                                                             
*                                                                               
* FOLLOWING CODE TO PREVENT USERS FROM DESTROYING RECORD(S) THEY                
* DO NOT HAVE ACCESS TO (VALICLT IN DK MODE IS NOT GOOD ENOUGH)                 
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         MVC   SVWORK+L'KEY(L'AIO),AIO                                          
         MVI   USEIONUM,2                                                       
         LA    R2,ESTCLTH                                                       
         GOTO1 VALICLT             NEED TO REVALIDATE CLT FOR SECURITY          
         MVI   USEIONUM,1                                                       
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,SVWORK+L'KEY    RESTORE AIO                                  
*                                                                               
VR05     L     R6,AIO              AIO HAS RECORD TO BE ADDED/CHANGED           
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         MVC   DUB(L'AIO),AIO                                                   
         XC    KEY+07(L'KEY-07),KEY+07                                          
         MVI   KEY+03,X'02'                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     CLT REC FOUND?                               
         BE    *+12                                                             
         LA    R2,ESTCLTH                                                       
         J     RECNFERR                                                         
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,DUB             RESTORE ORIGINAL AIO POINTER                 
         L     RE,AIO3                                                          
         USING PCLTREC,RE                                                       
         MVC   WKCLTFIN,PCLTFIN    SAVE CLT FINANCIAL STATUS                    
         DROP  RE                                                               
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         MVC   DUB(L'AIO),AIO                                                   
         XC    KEY+10(L'KEY-10),KEY+10                                          
         MVI   KEY+03,X'06'                                                     
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     PRD REC FOUND?                               
         BE    *+12                                                             
         LA    R2,ESTPRDH                                                       
         J     RECNFERR                                                         
*                                                                               
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,DUB             RESTORE ORIGINAL AIO POINTER                 
         L     RE,AIO3                                                          
         USING PPRDREC,RE                                                       
         MVC   WKPRDOAN,PPRDOAN    SAVE OTHER AGY NAME FROM PRD REC             
         DROP  RE                                                               
*                                                                               
* LINE ID AND ADDRESS ARE NEEDED FOR EST CUT BACK VALIDATIONS                   
*                                                                               
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,(2,0)                                                  
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   LINID,FALINE        LINE ID                                      
         MVC   LINADDR,FAADDR      LINE ADDRESS                                 
         DROP  R1                                                               
*                                                                               
         MVI   MASTEMNL,0          INIT MASTER TERMINAL SWITCH                  
*                                                                               
         LA    RF,MASTTAB          TABLE OF MASTER TERMINAL IDS                 
VR06D    CLI   0(RF),X'FF'         END OF TABLE?                                
         BE    VR06H                                                            
         CLC   LINID(8),0(RF)      FOUND IN TABLE?                              
         BE    *+12                                                             
         LA    RF,8(RF)            NEXT ENTRY IN TABLE                          
         B     VR06D                                                            
*                                                                               
         MVI   MASTEMNL,C'Y'       SET MASTER TERMINAL SWITCH                   
         B     VR06X                                                            
*                                                                               
* FOR DDS TERMINAL DDS & EST NAME=BRUCEPLATT, TREAT IT AS MAST TERMNL           
*                                                                               
VR06H    CLI   1(RA),C'*'          DDS TERMINAL?                                
         BNE   VR06X                                                            
         CLI   ESTENAMH+5,10       EXACTLY 10 CHARS?                            
         BNE   VR06X                                                            
         CLC   ESTENAM(10),=C'BRUCEPLATT'                                       
         BNE   VR06X                                                            
         MVI   MASTEMNL,C'Y'       SET MASTER TERMINAL SWITCH                   
*                                                                               
VR06X    DS    0H                  DONE WITH SETTING MASTER TEMNL SW            
*                                                                               
         XC    WKESTST,WKESTST     EST START DATE                               
         XC    WKESTEND,WKESTEND   EST END DATE                                 
         XC    WKESTNAM,WKESTNAM   EST NAME (LINE 1)                            
         XC    WKESTRTY,WKESTRTY   RATE TYPE                                    
         XC    WKESPROF,WKESPROF   ESTIMATE PROFILE                             
         XC    WKESSTAT,WKESSTAT   EST STATUS                                   
         XC    WKESTEST,WKESTEST   TEST EST INDICATOR                           
         XC    WKESBILL,WKESBILL   EST BILLIMG PROFILE                          
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR10                                                             
         MVI   ELCODE,X'07'                                                     
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
         USING PESTELEM,R6                                                      
         MVC   WKESTST,PESTST      SAVE EST START DATE                          
         MVC   WKESTEND,PESTEND    SAVE EST END DATE                            
         MVC   WKESTNAM,PESTNAME   SAVE EST NAME (LINE 1)                       
         MVC   WKESTRTY,PESTRTYP   SAVE RATE TYPE                               
         MVC   WKESPROF,PESTPROF   SAVE ESTIMATE PROFILE                        
         MVC   WKESSTAT,PESTSTAT   SAVE EST STATUS                              
         MVC   WKESTEST,PESTTEST   SAVE TEST EST INDICATOR                      
         MVC   WKESBILL,PESTBILP   SAVE EST BILLIMG PROFILE                     
*                                                                               
         MVC   SVESTELM,PESTELEM   Save entire main estimate elem               
*                                                                               
         DROP  R6                                                               
*                                                                               
         MVI   ELCODE,X'07'                                                     
         GOTO1 REMELEM             REMOVE X'07' ELEM (MAIN ELEM)                
*                                                                               
         TM    GENSTAT6,GES$UPLD   DDLINK general upload?                       
         JNZ   VR10                                                             
         L     R6,AIO              Point to estimate record                     
         MVI   ELCODE,PEORGECQ                                                  
         BRAS  RE,GETEL                                                         
         JNE   VR10                                                             
         OI    CHGRQFLD,NOCHRQFQ   Not allowed to change required flds          
*                                                                               
VR10     XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PESTELEM,R6                                                      
         MVI   0(R6),X'07'         MAIN ELEM CODE                               
         MVI   1(R6),PESTELLQ      ELEM LENGTH                                  
*                                                                               
         CLI   ACTNUM,ACTADD                                                    
         BE    *+10                                                             
         MVC   ELEM(PESTELLQ),SVESTELM                                          
*                                                                               
* INIT EST PROFILE TO C'0' IF ADDING, ELSE USE SAVED VALUES                     
*                                                                               
         MVI   PESTPROF,C'0'                                                    
         MVC   PESTPROF+1(L'PESTPROF-1),PESTPROF                                
         CLI   ACTNUM,ACTADD                                                    
         BE    VR20                                                             
         MVC   PESTPROF,WKESPROF                                                
         MVC   PESTBILP,WKESBILL   ESBILL IS HANDLED IN PRSFM1F                 
                                                                                
*                                                                               
* NOTE: MASTER TERMINALS CANNOT CHANGE EST NAME ANY MORE                        
*                                                                               
VR20     LA    R2,ESTENAMH         EST NAME (REQUIRED FLD)                      
         GOTO1 ANY                                                              
         XC    PESTNAME,PESTNAME                                                
         MVC   PESTNAME,ESTENAM                                                 
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR22                                                             
         CLC   WKESTNAM,PESTNAME   Estimate name is changed?                    
         JE    VR22                                                             
         TM    CHGRQFLD,NOCHRQFQ   Allowed to change required flds?             
         JNZ   NOCHGERR                                                         
         CLI   MASTEMNL,C'Y'       MASTER TERMINAL?                             
         BNE   VR22                                                             
         XC    PESTNAME,PESTNAME                                                
         MVC   PESTNAME,WKESTNAM   RESTORE TO ORIGINAL EST NAME                 
*                                                                               
VR22     XC    PESTNAM2,PESTNAM2   EST NAME LINE 2                              
         LA    R2,ESTNAM2H                                                      
         CLI   5(R2),0                                                          
         BE    VR24                                                             
         SR    RE,RE                                                            
         IC    RE,5(R2)            INPUT LENGTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PESTNAM2(0),ESTNAM2                                              
*                                                                               
VR24     LA    R2,ESTSTH           EST START DATE                               
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,ESTST),WORK                                       
         OC    DMCB(4),DMCB                                                     
         JZ    INVDTERR                                                         
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR24K                                                            
         CLC   WKESTST,WORK        Estimate start date changed?                 
         JE    *+12                                                             
         TM    CHGRQFLD,NOCHRQFQ   Allowed to change required flds?             
         JNZ   NOCHGERR                                                         
         CLI   MASTEMNL,C'Y'       MASTER TERMINAL?                             
         BE    VR24K               MASTER TERMINAL CAN ADVANCE START DT         
         CLC   WKESTST(6),WORK                                                  
         JL    STDTAERR            START DATE CANNOT BE ADVANCED                
VR24K    MVC   PESTST(6),WORK                                                   
*                                                                               
         LA    R2,ESTENDH          EST END DATE                                 
         GOTO1 ANY                                                              
         GOTO1 DATVAL,DMCB,(0,ESTEND),WORK                                      
         OC    DMCB(4),DMCB                                                     
         JZ    INVDTERR                                                         
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR26D                                                            
         CLC   WKESTEND,WORK       Estimate end date changed?                   
         JE    *+12                                                             
         TM    CHGRQFLD,NOCHRQFQ   Allowed to change required flds?             
         JNZ   NOCHGERR                                                         
         CLI   MASTEMNL,C'Y'       MASTER TERMINAL?                             
         BE    VR26D               MASTER TERMINAL CAN CUT BACK END DT          
         CLC   WKESTEND(6),WORK                                                 
         JH    ENDTCERR                                                         
VR26D    MVC   PESTEND(6),WORK                                                  
*                                                                               
         CLC   PESTST,PESTEND      START DATE IS AFTER END DATE?                
         JH    STENDERR                                                         
*                                                                               
* WHEN ADDING EST REC FOR MASTER TERMINALS,                                     
* END DATE CAN BE BEFORE START OF CURRENT YEAR                                  
*                                                                               
         CLI   MASTEMNL,C'Y'                                                    
         BE    VR26K                                                            
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VR26K                                                            
         CLI   F0PROF+3,C'Y'       Allow estimate adds for prior years?         
         BE    VR26K                                                            
         GOTO1 DATCON,DMCB,(5,0),(0,WORK)                                       
         CLC   PESTEND(2),WORK     YEAR EARLIER THAN CURRENT YEAR?              
         JL    ENDTINV                                                          
*                                                                               
VR26K    XC    DUB,DUB                                                          
         GOTO1 DATCON,DMCB,(0,PESTST),(3,DUB+1)                                 
         GOTO1 DATCON,DMCB,(0,PESTEND),(3,DUB+5)                                
         L     RE,DUB+0                                                         
         L     RF,DUB+4                                                         
         SR    RF,RE                                                            
         C     RF,=F'65535'        MORE THAN A YEAR? (65535=X'FFFF')            
         JH    YEARERR                                                          
*                                                                               
* ON ACTION ADD, FOLLOWING CLC WILL BE "NOT EQUAL"                              
* NEED TO CK FOR EST DATES FROM PRD ZZZ (DATES HAVE TO BE SAME)                 
*                                                                               
         CLC   PESTST,WKESTST      EST START DATE IS CHANGED?                   
         BNE   *+14                                                             
         CLC   PESTEND,WKESTEND    EST END DATE IS CHANGED?                     
         BE    VR28                                                             
*                                                                               
         CLC   KEY+07(03),=C'ZZZ'  PRD ZZZ?                                     
         BE    VR26M               EST TO BE ADDED IS PRD ZZZ                   
         MVC   SVWORK(L'KEY),KEY                                                
         MVC   KEY+07(03),=C'ZZZ'  CK IF EST HAS PRD ZZZ                        
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     PRD ZZZ FOUND FOR THIS EST?                  
         BE    *+14                                                             
         MVC   KEY,SVWORK          NOT FOUND, RESTORE KEY AND                   
         B     VR28                GO VALIDATE NEXT FLD                         
         MVC   DUB(L'AIO),AIO                                                   
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO3                                                          
         LA    RE,33(RE)                                                        
         CLI   0(RE),X'07'         PRD ZZZ EST REC'S FIRST ELEM FOUND?          
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R2,ESTSTH                                                        
         CLC   PESTST,PESTST-PESTELEM(RE)                                       
         JNE   ZZZ1ERR                                                          
         LA    R2,ESTENDH                                                       
         CLC   PESTEND,PESTEND-PESTELEM(RE)                                     
         JNE   ZZZ1ERR                                                          
         MVC   AIO,DUB             RESTORE AIO                                  
         B     VR28                ZZZ DATES AGREE, GO CK NEXT FLD              
*                                                                               
* WHEN ADDING OR CHANGING A ZZZ EST REC, START AND END DATES                    
* MUST AGREE WITH ALL OTHER NON ZZZ ESTIMATES OF SAME NUMBER/CODE               
*                                                                               
VR26M    MVC   SVWORK(L'KEY),KEY   SAVE ORIGINAL KEY                            
         MVC   DUB(L'AIO),AIO      SAVE ORIGINAL AIO                            
         XC    KEY+07(03),KEY+07   CLR PRD CODE                                 
         MVC   AIO,AIO3                                                         
         MVC   KEYSAVE(25),KEY                                                  
         GOTO1 HIGH                                                             
VR26P    CLC   KEY(07),KEYSAVE     SAME AGY/MED/RTYPE/CLT/PRD?                  
         BNE   VR26U                                                            
         CLC   KEY+07(03),=C'ZZZ'  PRD IS ZZZ?                                  
         BE    VR26U                                                            
         CLC   KEY+10(02),KEYSAVE+10                                            
         BNE   VR26T                                                            
         GOTO1 GETREC                                                           
         L     RE,AIO3                                                          
         LA    RE,33(RE)                                                        
         CLI   0(RE),X'07'         FIRST ELEM FOUND?                            
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BNE   VR26R               NO                                           
         LA    R2,ESTSTH                                                        
         CLC   PESTST,PESTST-PESTELEM(RE)                                       
         JNE   ZZZ2ERR                                                          
         LA    R2,ESTENDH                                                       
         CLC   PESTEND,PESTEND-PESTELEM(RE)                                     
         JNE   ZZZ2ERR                                                          
         B     VR26T                                                            
*                                                                               
* ON POL CHANGES UPDATE BRAND ESTS                                              
*                                                                               
VR26R    MVC   PESTST-PESTELEM(L'PESTST,RE),PESTST                              
         MVC   PESTEND-PESTELEM(L'PESTEND,RE),PESTEND                           
         GOTO1 PUTREC                                                           
*                                                                               
VR26T    GOTO1 SEQ                 NEXT EST RECORD                              
         B     VR26P                                                            
*                                                                               
VR26U    MVC   KEY,SVWORK          RESTORE KEY                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC              RESTORE SEQUENCE FOR GENCON                  
         MVC   AIO,DUB             RESTORE ORIGINAL AIO                         
*                                                                               
* WHEN ADVANCING OR CUTTING BACK EST DATES, NO OTHER FLDS ARE                   
* VALIDATED, EXCEPT FOR USER FLDS                                               
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD, ADDING POL EST?                  
         BNE   VR46                                                             
*                                                                               
VR28     CLI   SECEFLT,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR29                                                             
         CLI   SECEFLT,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR29                                                             
         XC    PESTGRPS,PESTGRPS   FILTERS                                      
         LA    R2,ESTFLTRH                                                      
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR28D                                                            
         CLI   5(R2),3             MORE THAN MAX OF 3 CHARS?                    
         JH    INVFDERR                                                         
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PESTGRPS(0),8(R2)                                                
*                                                                               
VR28D    OC    F0PROF,F0PROF       F0PROF REQUIRES EST FILTERS?                 
         BZ    VR29                                                             
         LA    RE,PESTGRPS         POINT TO INPUT (NULLS OR DATA)               
         LA    RF,3                FOR BCT (3 CHARS)                            
         LA    R3,F0PROF           POINT TO PROFILE                             
VR28K    CLI   0(R3),C'Y'          PROF SAYS EST FILTER IS REQUIRED?            
         BNE   VR28M                                                            
         CLI   0(RE),C' '          EST FILTER IS REQ'D, INPUT PRESENT?          
         JNH   MSSNGERR                                                         
VR28M    LA    RE,1(RE)            POINT TO NEXT CHAR IN INPUT                  
         LA    R3,1(R3)            POINT TO NEXT PROFILE VALUE                  
         BCT   RF,VR28K                                                         
*                                                                               
VR29     NI    PESTTEST,X'FF'-X'20'                                             
         LA    R2,ESTIDKEH                                                      
         CLI   5(R2),0             Prisma estimate?                             
         JE    VR29H                                                            
         CLI   8(R2),C'N'                                                       
         JE    VR29H                                                            
         CLI   8(R2),C'Y'                                                       
         JNE   INVFDERR                                                         
VR29E    OI    PESTTEST,X'20'      Set to Prisma estimate                       
         J     VR30                                                             
VR29H    CLI   ACTNUM,ACTADD       Action ADD?                                  
         JNE   VR30                                                             
         CLI   F0PROF+7,C'Y'       Prisma estimate per F0 profile?              
         JE    VR29E                                                            
*                                                                               
VR30     CLI   SECERSC,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR32                                                             
         CLI   SECERSC,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR32                                                             
         XC    PESTRSCH,PESTRSCH   RETAIL SCHEME                                
         LA    R2,ESTRSCHH                                                      
         CLI   5(R2),0                                                          
         BE    VR32                                                             
         CLI   5(R2),2                                                          
         JH    INVFDERR                                                         
         MVC   PESTRSCH,8(R2)                                                   
         B     VR32                                                             
*                                                                               
VR32     CLI   SECERTY,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR34                                                             
         CLI   SECERTY,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR34                                                             
         MVI   PESTRTYP,0          RATE TYPE                                    
         LA    R2,ESTRTYPH                                                      
         TM    1(R2),X'20'         FIELD IS READ ONLY?                          
         BZ    *+14                                                             
         MVC   PESTRTYP,WKESTRTY   REVERT BACK TO SAVED VALUE                   
         B     VR34                                                             
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR32D                                                            
         CLI   5(R2),1             MORE THAN 1 CHAR INPUT?                      
         JH    INVFDERR                                                         
         CLI   8(R2),C'C'                                                       
         JNE   INVFDERR                                                         
         MVC   PESTRTYP,8(R2)      ONLY ACCEPT 'C' FOR NOW                      
*                                                                               
VR32D    CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    VR34                                                             
         CLC   WKESTRTY,PESTRTYP   SAME RATE TYPE AS BEFORE?                    
         BE    VR34                                                             
*                                                                               
         CLI   PESTRTYP,C'C'       IS NEW TYPE C?                               
         BNE   VR32H                                                            
         LA    RF,WKESBILL         POINT TO SAVED BILLING PROFILE               
         USING BILPROF,RF                                                       
         CLI   BILCMSW,C'C'        C BILLING FORMULA?                           
         JNE   NOCHGERR            CANNOT CHANGE ERROR                          
         DROP  RF                                                               
*                                                                               
* FOR NONE C TYPE FORMULA, COMPARE EST FROM BUY REC (NO CHG IF SAME)            
*                                                                               
VR32H    MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+10(L'KEY-10),KEY+10                                          
         MVI   KEY+3,X'20'                                                      
VR32M    GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     SAME AGY/MED/RTYPE/CLT/PRD?                  
         BNE   VR32P                                                            
         CLC   KEY+19(02),SVWORK+10                                             
         JE    NOCHGERR                                                         
         MVI   KEY+19,X'FF'        SKIP TO NEXT EST                             
         MVI   KEY+20,X'FF'                                                     
         XC    KEY+21(4),KEY+21    CLEAR ACTIVE PRD AND LINE                    
         B     VR32M                                                            
*                                                                               
VR32P    MVC   KEY,SVWORK                                                       
         GOTO1 HIGH                RESTORE DATA MANAGER SEQUENCE                
*                                                                               
VR34     XC    PESTJOB,PESTJOB     AD CODE                                      
         LA    R2,ESTADH                                                        
         CLI   5(R2),0                                                          
         BE    VR36                                                             
         SR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   PESTJOB(0),8(R2)                                                 
         OC    PESTJOB,SPACES      MAKE SURE NO NULLS                           
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+10(L'KEY-10),KEY+10                                          
         MVI   KEY+03,X'15'                                                     
         MVC   KEY+10(L'PESTJOB),PESTJOB                                        
         GOTO1 HIGH                                                             
         CLC   KEY(16),KEYSAVE     AD CODE ON FILE?                             
         JNE   RECNFERR                                                         
         MVC   KEY,SVWORK          RESTORE KEY                                  
*                                                                               
VR36     XC    PESTCOM,PESTCOM     STANDARD COMMENTS (TWO SETS)                 
         XC    PESTCOM2,PESTCOM2                                                
         LA    R2,ESTCOMH                                                       
         CLI   5(R2),0             ANY INPUT?                                   
         BE    VR38                NO                                           
         MVC   WORK(20),SPACES                                                  
         LA    RE,8(R2)            POINT TO BEGINNING OF INPUT                  
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         LA    RF,8(RF,R2)         POINT TO END OF INPUT                        
         SR    R3,R3               INIT COUNTER                                 
*                                                                               
* CK FOR MORE THAN TWO COMMENT ENTRIES                                          
*                                                                               
VR36D    CLI   0(RE),C','          COMMENT SEPARATOR CHARACTER?                 
         BNE   *+8                 NO                                           
         AHI   R3,1                ADD TO COUNTER                               
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         CR    RE,RF               LAST CHARACTER?                              
         BL    VR36D                                                            
         CHI   R3,1                                                             
         JH    INVFDERR            TOO MANY COMMENT SEPARATOR CHARS             
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         LA    RE,8(R2)            POINT TO BEGINNING OF INPUT                  
         STCM  RE,15,FULL          BEGINNING OF 1ST COMMENT CODE                
*                                                                               
VR36F    SR    R3,R3               INIT COUNTER                                 
*                                                                               
VR36H    CLI   0(RE),C','          COMMENT SEPARATOR CHARACTER?                 
         BE    VR36K               YES                                          
         AHI   R3,1                ADD TO COUNTER                               
         LA    RE,1(RE)            BUMP TO NEXT POSITION                        
         CR    RE,RF               LAST CHARACTER FOUND ?                       
         BL    VR36H                                                            
*                                                                               
VR36K    MVC   WORK(6),SPACES                                                   
         LTR   R3,R3               ANY DATA FOUND?                              
         JZ    INVFDERR                                                         
         CHI   R3,6                MORE THAN MAX OF 6 CHARS?                    
         JH    INVFDERR                                                         
*                                                                               
         STCM  RE,15,DUB+0         SAVE CURRENT INPUT CHARS POINTER             
         STCM  RF,15,DUB+4         SAVE END OF FLD POINTER                      
*                                                                               
         ICM   RF,15,FULL          BEGINNING OF INPUT                           
         LA    RE,6                                                             
         SR    RE,R3                                                            
         LA    RE,WORK(RE)         TO RIGHT ALIGN                               
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       GET COMMENT CODE                             
*                                                                               
         XC    KEY+03(L'KEY-03),KEY+03                                          
         MVI   KEY+03,X'40'                                                     
         MVC   KEY+04(6),WORK                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(10),KEYSAVE     COMMENT CODE ON FILE?                        
         JNE   RECNFERR                                                         
*                                                                               
         CLI   PESTCOM,0           1ST COMMENT IS ALREADY VALIDATED?            
         BNE   VR36M               YES, FINISH WITH 2ND COMMENT THEN            
         MVC   PESTCOM,KEY+04                                                   
         ICM   RE,15,DUB+0         POINT TO COMMENT SEPARATOR CHAR              
         ICM   RF,15,DUB+4         POINT TO END OF FLD                          
         CLI   0(RE),C','          NEED TO CK FOR 2ND COMMENT CODE?             
         BNE   VR36P               NO - ONLY ONE COMMENT - DONE                 
         LA    RE,1(RE)            GO VALIDATE 2ND COMMENT CODE                 
         STCM  RE,15,FULL          BEGINNING OF 2ND COMMENT CODE                
         B     VR36F                                                            
*                                                                               
VR36M    MVC   PESTCOM2,KEY+04                                                  
VR36P    MVC   KEY,SVWORK          RESTORE KEY                                  
*                                                                               
VR38     XC    PESTREVN,PESTREVN   REVISION NUMBER                              
         LA    R2,ESTREVH                                                       
         CLI   5(R2),0             ANY INPUTS?                                  
         BE    VR40                                                             
         TM    4(R2),X'08'                                                      
         JZ    NTNUMERR            INPUT IS NOT NUMERIC                         
         BRAS  RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         UNPK  PESTREVN,DUB        NUMERIC CHARS W/ LEADING ZEROS               
*                                                                               
VR40     XC    ESTREPN,ESTREPN     SPECIAL REP NAME (PROTECTED)                 
         CLI   SECESPR,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR40U                                                            
         CLI   SECESPR,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR40U                                                            
         XC    PESTREP,PESTREP     SPECIAL REP                                  
         LA    R2,ESTREPH                                                       
         CLI   5(R2),0                                                          
         BE    VR40U                                                            
         LA    R3,ESTREPNH                                                      
         BRAS  RE,VR_CKREP                                                      
         L     RE,AIO3                                                          
         USING PREPREC,RE                                                       
         MVC   PESTREP,PREPKREP    GET VALIDATED REP CODE                       
         MVC   ESTREPN,PREPNAME                                                 
         DROP  RE                                                               
*                                                                               
VR40U    OI    ESTREPNH+6,X'80'                                                 
*                                                                               
         XC    PESTBREP,PESTBREP   BILLING REP                                  
         LA    R2,ESTBREPH                                                      
         XC    ESTBRPN,ESTBRPN     BILLING REP NAME (PROTECTED)                 
         CLI   5(R2),0                                                          
         BE    VR42U                                                            
         LA    R3,ESTBRPNH                                                      
         BRAS  RE,VR_CKREP                                                      
         L     RE,AIO3                                                          
         USING PREPREC,RE                                                       
         MVC   PESTBREP,PREPKREP   GET VALIDATED REP CODE                       
         MVC   ESTBRPN,PREPNAME                                                 
         DROP  RE                                                               
*                                                                               
VR42U    OI    ESTBRPNH+6,X'80'                                                 
*                                                                               
         XC    PESTZZZ,PESTZZZ     ALLOCATION LINE FOR ZZZ ESTS                 
         LA    R2,ESTALLOH                                                      
         CLI   5(R2),0                                                          
         BE    VR44                NO INPUTS FOUND, CK NEXT FLD                 
         CLC   KEY+07(03),=C'ZZZ'                                               
         JNE   INVFDERR                                                         
         MVC   PESTZZZ,ESTALLO                                                  
*                                                                               
*        VALIDATE ESTIMATE STATUS                                               
*                                                                               
VR44     CLI   SECESTA,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR44X                                                            
         CLI   SECESTA,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR44X                                                            
         MVI   PESTSTAT,0          INIT STATUS AND TEST INDICATOR               
         NI    PESTTEST,X'FF'-X'C0'                                             
         LA    R2,ESTSTATH         POINT TO INPUT                               
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,1,5(R2)          GET INPUT LENGTH                             
         BNZ   *+16                WE HAVE SOME DATA                            
         CLI   WKCLTFIN,C'Y'       FINANCIAL CLT?                               
         JE    INVFDERR            LIVE OR TEST MUST BE SPECIFIED               
         B     VR44M               NO ENTRY  WILL DEFAULT TO LIVE               
*                                                                               
*                                                                               
         LA    R1,8(R2)            POINT TO INPUT                               
*                                                                               
         CLI   ESTSTAT,C'1'        IF SOFT LOCKOUT?                             
         BE    *+8                                                              
         CLI   ESTSTAT,C'2'        OR PERMANENT LOCKOUT?                        
         BNE   VR44A                                                            
*                                                                               
         MVC   PESTSTAT,ESTSTAT       SAVE LOCKING TYPE                         
         LA    R1,1(R1)               BYPASS LOCKOUT STATUS                     
         SHI   RF,1                   DECREMENT ENTRY LENGTH                    
         BNZ   VR44A                     OKAY IF MORE DATA                      
*                                                                               
         CLI   WKCLTFIN,C'Y'       FINANCIAL CLT?                               
         JE    INVFDERR            LIVE OR TEST MUST BE SPECIFIED               
*                                                                               
         B     VR44M                                                            
*                                                                               
VR44A    DS    0H                                                               
*                                                                               
*        VALIDATE REMAINING STATUS                                              
*                                                                               
         CHI   RF,4                MAX 4 CHS SHOULD BE LEFT                     
         JH    INVFDERR                                                         
*                                                                               
         BCTR  RF,0                DECREMENT FOR EXECUTE                        
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'LIVE'    LIVE ESTIMATE                                
         BE    VR44M                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'TEST'    TEST ESTIMATE                                
         BNE   *+12                                                             
         OI    PESTTEST,X'80'      MAKE IT TEST                                 
         B     VR44M                                                            
*                                                                               
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R1),=C'STEW'    STEWARD ESTIMATE                             
         BNE   *+12                                                             
         OI    PESTTEST,X'80'+X'40'  MAKE IT STEWARD ESTIMATE                   
         B     VR44M                                                            
*                                                                               
         J     INVFDERR            INVALID STATUS                               
*                                                                               
VR44M    DS    0H                                                               
*                                                                               
         OC    WKPRDOAN,WKPRDOAN   IF OTHER AGY NAME PRESENT?                   
         BZ    *+12                                                             
         TM    PESTTEST,X'80'         MUST BE A TEST/STEW ESTIMATE              
         JNO   INVFDERR                ERROR - MUST BE TEST OR STEW             
*                                                                               
         CLI   WKESSTAT,C'2'       WAS IT PERMANENTLY LOCKED OUT?               
         BNE   VR44P               NO, THEN IT'S OK TO CHG STATUS               
         CLI   ESTSTAT,C'2'        YES - BUT STATUS NOT BEING CHANGED           
         BE    VR44P               OKAY                                         
*                                                                               
         CLI   MASTEMNL,C'Y'                                                    
         JNE   NOCHGERR            MASTER TMNLS ALLOWED TO CHG STATUS           
*                                                                               
VR44P    DS    0H                                                               
*                                                                               
         TM    PESTTEST,X'C0'      IF STEW ESTIMATE                             
         BNO   VR44P1                                                           
         TM    WKESTEST,X'C0'         MUST HAVE BEEN STEW ESTIMATE              
         JO    *+12                                                             
         CLI   ACTNUM,ACTADD          OR ACTION ADD                             
         BNE   INVFDERR                                                         
*                                                                               
         B     VR44PX              OKAY                                         
*                                                                               
VR44P1   DS    0H                                                               
*                                                                               
         TM    PESTTEST,X'80'      IF TEST ESTIMATE                             
         BNO   VR44P2                                                           
*                                                                               
         TM    WKESTEST,X'80'         MUST HAVE BEEN TEST ESTIMATE              
         BNO   *+16                                                             
         TM    WKESTEST,X'40'         AND NOT STEW ESTIMATE                     
         JO    INVFDERR                                                         
         B     VR44PX              OKAY                                         
*                                                                               
         CLI   ACTNUM,ACTADD          OR ACTION ADD                             
         JNE   INVFDERR                                                         
*                                                                               
         B     VR44PX              OKAY                                         
*                                                                               
VR44P2   DS    0H                  LIVE ESTIMATE                                
         B     VR44Q                                                            
*                                                                               
VR44PX   DS    0H                                                               
         B     VR44X               ALL DONE                                     
*                                                                               
*        LIVE ESTIMATE                                                          
*                                                                               
VR44Q    DS    0H                                                               
*                                                                               
         TM    WKESTEST,X'C0'      WAS THIS A STEW EST?                         
         JO    INVFDERR            YES, CANNOT MAKE LIVE                        
*                                                                               
         TM    WKESTEST,X'80'      WAS THIS A TEST EST?                         
         BO    VR44Q_20            YES, NEED TO CLEAR EST BUCKETS               
*                                                                               
         BRAS  RE,CKESTBUK                                                      
         BE    VR44X                                                            
         DC    H'0'                OUT OF SYNC WITH EST BUCKETS                 
*                                                                               
* WHEN MAKING A TEST EST LIVE, CLR BUCKET ELEMS                                 
*                                                                               
VR44Q_20 MVC   SVWORK(L'KEY),KEY                                                
         MVC   DUB(L'AIO),AIO                                                   
         MVC   AIO,AIO3                                                         
         MVI   KEY+3,X'09'         EST BUCKET REC CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     EST BUCKET REC FOUND?                        
         BNE   VR44R_44                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO3                                                          
         LA    RE,33(RE)           POINT TO 1ST ELEM OF EST BUCKET REC          
         USING BKELEM,RE                                                        
VR44R    CLI   0(RE),X'00'         END OF EST BUCKET REC?                       
         BE    VR44R_40                                                         
         CLI   0(RE),X'21'         TODAY?                                       
         BE    VR44R_10                                                         
         CLI   0(RE),X'22'         REGULAR?                                     
         BE    VR44R_10                                                         
         CLI   0(RE),X'23'         SPECIAL MONTHS?                              
         BE    VR44R_10                                                         
         CLI   0(RE),X'31'         TODAY WITH GST?                              
         BE    VR44R_10                                                         
         CLI   0(RE),X'41'         TODAY - PLANNED COST?                        
         BE    VR44R_10                                                         
         CLI   0(RE),X'42'         REGULAR - PLANNED COST?                      
         BE    VR44R_10                                                         
         CLI   0(RE),X'43'         SPECIAL MONTHS - PLANNED COST?               
         BE    VR44R_10                                                         
         B     VR44R_20                                                         
VR44R_10 ZAP   BKOGRS,=P'0'                                                     
         ZAP   BKONET,=P'0'                                                     
         ZAP   BKOCD,=P'0'                                                      
         NI    BKIND,X'FE'         TURN OFF TEST EST INDICATOR                  
         CLI   BKELEM+1,(BKPCD-BKELEM)                                          
         BL    VR44R_20                                                         
         ZAP   BKPGRS,=P'0'                                                     
         ZAP   BKPNET,=P'0'                                                     
         ZAP   BKPCD,=P'0'                                                      
*                                                                               
VR44R_20 SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0               POINT TO NEXT ELEM IN EST BUCKET REC         
         B     VR44R                                                            
         DROP  RE                                                               
*                                                                               
VR44R_40 GOTO1 PUTREC              UPDATE EST BUCKET REC                        
VR44R_44 MVC   KEY,SVWORK          RESTORE KEY                                  
         GOTO1 HIGH                                                             
         GOTO1 GETREC              RESTORE SEQUENCE FOR GENCON                  
         MVC   AIO,DUB             RESTORE ORIGINAL AIO                         
*                                                                               
VR44T    NI    PESTTEST,X'FF'-X'80'                                             
         B     VR46                                                             
*                                                                               
VR44X    DS    0H                                                               
*                                                                               
VR46     GOTO1 ADDELEM             MAIN ELEM IS VALIDATED AND ADDED             
         DROP  R6                  FOR PPRDELEM                                 
*                                                                               
* CKING FOR PRD USER FLDS, IN CASE SECURITY IS ENFORCED, ACTIVATE               
* FOLLOWING CODES (THIS IS ENFORCED AT PRD LEVEL)                               
*                                                                               
* * * *  L     RF,ATWA                                                          
* * * *  TM    12(RF),X'08'        AUTHORIZED?                                  
* * * *  BO    VR50                NO CHANGES TO USER FLDS                      
*                                                                               
         XC    SVPUSER1,SVPUSER1                                                
         XC    SVPUSER2,SVPUSER2                                                
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'                                                     
         BRAS  RE,GETEL                                                         
         BNE   VR47                                                             
         USING PESTUDEF,R6                                                      
         MVC   SVPUSER1,PEUSER1    SAVE FOR FIELD CONTROL                       
         MVC   SVPUSER2,PEUSER2    SAVE FOR FIELD CONTROL                       
         DROP  R6                                                               
*                                                                               
VR47     MVI   ELCODE,X'08'                                                     
         GOTO1 REMELEM             REMOVE X'08' ELEM (USER DEF FLDS)            
         LA    R6,ELEM             BUILT ELEM AND ADD IT TO PRD REC             
         XC    ELEM,ELEM                                                        
         USING PESTUDEF,R6                                                      
         MVI   ELEM+00,X'08'       ELEM CODE                                    
         MVI   ELEM+01,2+32+16     ELEM LENGTH                                  
*                                                                               
         MVC   PEUSER1,SVPUSER1                                                 
         CLI   SECEUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR48                                                             
         CLI   SECEUD1,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR48                                                             
         LA    R2,ESTDSC1H                                                      
         LA    R3,SVE1USER                                                      
         BAS   RE,VR_USERF                                                      
         MVC   PEUSER1,WORK        GET VALIDATED USER FLD 1 IF PRESENT          
*                                                                               
VR48     MVC   PEUSER2,SVPUSER2                                                 
         CLI   SECEUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR49                                                             
         CLI   SECEUD2,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR49                                                             
         LA    R2,ESTDSC2H                                                      
         LA    R3,SVE2USER                                                      
         BAS   RE,VR_USERF                                                      
         MVC   PEUSER2,WORK        GET VALIDATED USER FLD 2 IF PRESENT          
*                                                                               
* CK IF ELEM IS NOT EMPTY                                                       
*                                                                               
VR49     OC    ELEM+2(L'PEUSER1+L'PEUSER2),ELEM+2                               
         BZ    VR50                                                             
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR50     CLI   SECPCED,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR60                                                             
         CLI   SECPCED,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR60                                                             
         BRAS  RE,BPCEFFDT         BILL ON PLANNED COST EFFECTIVE DATE          
         JNE   TRAPERR2                                                         
*                                                                               
VR60     CLI   SECACTD,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR70                                                             
         CLI   SECACTD,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR70                                                             
         BRAS  RE,ACTULZDT         ESTIMATE ACTUALIZATION DATE                  
         JNE   TRAPERR2                                                         
*                                                                               
VR70     CLI   SECEPON,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    VR76                                                             
         CLI   SECEPON,C'Y'        READ ONLY (FIELD SECURITY)?                  
         BE    VR76                                                             
         MVI   ELCODE,PEPONECQ                                                  
         GOTO1 REMELEM             REMOVE PURCHASE ORDER ELEMENT                
         XC    ELEM,ELEM                                                        
         USING PESTPOND,R6                                                      
         LA    R6,ELEM                                                          
         MVI   PEPONELC,PEPONECQ                                                
         MVI   PEPONELN,PEPONELQ                                                
         MVC   PEPONPON,ESTPON     PO#                                          
         OC    PEPONPON,SPACES                                                  
         LA    R2,ESTPONH                                                       
         BRAS  RE,VALPROR          MAKE SURE PO IS ALPHANUMERIC                 
         JNE   INVFDERR                                                         
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
VR76     BRAS  RE,CKESTORG         CHECK FOR ESTIMATE ORIGIN                    
         JE    VR82                                                             
         LHI   R2,323              Undefined estimate source                    
         BRAS  RE,GET_ETXT                                                      
         LA    R2,ESTESTH                                                       
         J     TRAPERR2                                                         
*                                                                               
VR82     DS    0H                                                               
*                                                                               
VRX      J     DR                  RECORD VALIDATED, REDISPLAY IT               
*                                                                               
* TABLE OF MASTER TERMINALS                                                     
*                                                                               
MASTTAB  DS    0H                                                               
* * * *  DC    C'DDNYF11T'         DDS                                          
* * * *  DC    C'DDNY720T'                                                      
* * * *  DC    C'DX03901T'                                                      
* * * *  DC    C'DDNYD26T'         DDS                                          
         DC    C'DDNYD03T'                                                      
* * * *  DC    C'DDNY700T'                                                      
         DC    C'DDNYE00T'                                                      
         DC    C'DX06200T'         (WAS DDNY700T)                               
         DC    C'HDTO80CT'         HDTO                                         
         DC    C'HDTO800T'         HDTO (WAS HDTO830T)                          
         DC    C'DDL1136T'         DDS-LA                                       
         DC    C'DDL1137T'         DDS-LA                                       
         DC    C'DDL1138T'         DDS-LA                                       
         DC    X'FF'               END OF TABLE                                 
*                                                                               
* ROUTINE TO CK FOR USER DEFINITION FLDS                                        
*                                                                               
* INPUT:  R2 = A(INPUT FIELD)                                                   
*         R3 = USER DEFINITION FLD BLOCK FROM CLT RECORD                        
*                                                                               
* OUTPUT: WORK = DATA OR NULLS                                                  
*                                                                               
VR_USERF DS    0H                                                               
         USING UDEFD,R3                                                         
         ST    RE,FULL             SAVE RETURN ADDRESS                          
         XC    WORK,WORK                                                        
         OC    0(L'SVE1USER,R3),0(R3)                                           
         BZ    VR_USRX                                                          
         CLI   5(R2),0             ANY INPUT?                                   
         BNE   VR_USR10                                                         
         TM    UFLG1,X'80'         INPUT REQUIRED?                              
         BNO   VR_USRX                                                          
         J     MSSNGERR                                                         
*                                                                               
VR_USR10 CLC   5(1,R2),ULEN        INPUT LENGTH IS VALID?                       
         JH    LONGERR                                                          
         CLI   UTYPE,C' '          ACCEPT ANY INPUT?                            
         BNH   VR_USR60                                                         
         CLI   UTYPE,C'N'          NUMERIC?                                     
         BNE   VR_USR40                                                         
         TM    4(R2),X'08'         INPUT MUST BE NUMERIC                        
         BO    VR_USR60                                                         
         ZIC   R1,5(R2)            BUT ALLOW '- /'                              
         LA    R4,8(R2)                                                         
*                                                                               
VR_USR12 CLI   0(R4),C'0'                                                       
         BL    VR_USR14                                                         
         CLI   0(R4),C'9'                                                       
         BNH   VR_USR16                                                         
*                                                                               
VR_USR14 CLI   0(R4),C' '                                                       
         BE    VR_USR16                                                         
         CLI   0(R4),C'/'                                                       
         BE    VR_USR16                                                         
         CLI   0(R4),C'-'                                                       
         JNE   INVFDERR                                                         
*                                                                               
VR_USR16 LA    R4,1(R4)                                                         
         BCT   R1,VR_USR12                                                      
         B     VR_USR60                                                         
*                                                                               
VR_USR40 CLI   UTYPE,C'C'          ALPHA TYPE?                                  
         BNE   VR_USR50                                                         
         ZIC   R1,5(R2)            ALLOW ALL INPUT EXCEPT NUMBERS               
         LA    R4,8(R2)                                                         
*                                                                               
VR_USR44 CLI   0(R4),C'0'                                                       
         BL    VR_USR48                                                         
         CLI   0(R4),C'9'                                                       
         JNH   INVFDERR                                                         
*                                                                               
VR_USR48 LA    R4,1(R4)                                                         
         BCT   R1,VR_USR44                                                      
         B     VR_USR60                                                         
*                                                                               
VR_USR50 CLI   UTYPE,C'D'          DATE TYPE?                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATVAL,DMCB,(0,8(R2)),WKCDATE                                    
         OC    DMCB(4),DMCB                                                     
         JZ    INVDTERR                                                         
         L     R4,0(R1)            L'DATE                                       
         ZIC   R1,5(R2)            L'INPUT                                      
         SR    R1,R4                                                            
         JNZ   INVFDERR                                                         
*                                                                               
VR_USR60 ZIC   R1,5(R2)            GET INPUT LENGTH                             
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   WORK(0),8(R2)       MOVE INPUT INTO WORK                         
*                                                                               
VR_USRX  L     RE,FULL                                                          
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
* R2   = INPUT FLD                                                              
* R3   = REP NAME HEADER FLD                                                    
* AIO3 = RETURN REP RECORD IF VALID                                             
*                                                                               
VR_CKREP ST    RE,FULL              VALIDATE REP CODE                           
         CLI   5(R2),4                                                          
         JH    LONGERR             INPUT IS TOO LONG ERROR MSG                  
         TM    4(R2),X'08'                                                      
         JZ    NTNUMERR            INPUT IS NOT NUMERIC                         
         BRAS  RE,PACK                                                          
         OI    DUB+7,X'0F'                                                      
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+03(L'KEY-03),KEY+03                                          
         MVI   KEY+03,X'11'                                                     
         UNPK  KEY+04(04),DUB                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     REP RECORD FOUND?                            
         BE    *+12                FOUND                                        
         OI    6(R3),X'80'         DISPLAY CLR'D REP NAME                       
         J     RECNFERR                                                         
*                                                                               
         MVC   DUB(L'AIO),AIO                                                   
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         MVC   KEY,SVWORK          RESTORE KEY                                  
         MVC   AIO,DUB             RESTORE ORIGINAL AIO                         
         L     RE,FULL                                                          
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DR       DS    0H                  DISPLAY RECORD                               
         BRAS  RE,DISP_REC                                                      
*                                                                               
         LA    R2,CONACTH          NEED TO RESET FLD POINTER                    
         CLI   ACTNUM,ACTSEL       ACTION SEL?                                  
         JNE   EXIT                                                             
         XC    CONHEAD,CONHEAD                                                  
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         JE    EXIT                                                             
         CLI   MODE,VALREC         MODE IS VALREC?                              
         JE    EXIT                                                             
         CLI   ACTNUM,ACTCHA       ACTION IS CHANGE?                            
         JE    EXIT                                                             
         MVC   CONHEAD(L'SELMSG01),SELMSG01                                     
         OI    CONHEADH+6,X'80'                                                 
         J     TRAPERR2                                                         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR       DS    0H                  List records                                 
         BRAS  RE,CLRLSSCR         Clear list screen                            
         BRAS  RE,LIST_REC                                                      
*                                                                               
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BZ    LRX_10                                                           
         LA    R4,LSTTL4                                                        
         USING LS_TTL4,R4                                                       
         CLI   LTFSTART,0          TITLES DISPLAYED?                            
         BH    *+12                                                             
         LA    R2,WORK             POINT TO LISTING AREA (DUMMY USE)            
         BRAS  RE,LR_DFLTF         DISPLAY FLT FLDS (ONLY NEED TITLES)          
         OI    LSTTL4H+6,X'80'                                                  
         OI    LSTUL4H+6,X'80'                                                  
         DROP  R4                                                               
*                                                                               
LRX_10   TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BZ    EXIT                                                             
         LA    R2,LSTFLTH          PLACE CURSOR ON FILTER FLD                   
         B     TRAPERR2                                                         
*                                                                               
L_DLNQ   EQU   100                 DATA TITLE AND DATA ITSELF                   
L_DASHES DC    50C'-'                                                           
*                                                                               
* R2 POINTS TO DISPLAYING AREA, AIO2 HAS TABLE OF FLT VALUES                    
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PR       DS    0H                  PRINT RECORDS                                
         B     LR                                                               
*                                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPTRS    DS    0H                  REC IS JUST CHANGED                          
*                                                                               
         BRAS  RE,ADDBUCKT         ADD EMPTY ESTIMATE BUCKET REC                
*                                                                               
         BRAS  RE,PUTREQRC         TO REQUEST AUTO T/A REPORTS                  
         BE    PPTRS30                                                          
         LHI   R2,65               CANNOT GENERATE REQ FOR T/A REPORT           
         BRAS  RE,GET_ITXT                                                      
         LA    R2,CONACTH                                                       
         J     TRAPERR2            ERROR OR COMPLETION MSG IS SET               
*                                                                               
PPTRS30  BRAS  RE,CKUPLMSG         Check for upload message                     
*                                                                               
PPTRS_X  B     EXIT                                                             
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
TRAPERR  GOTO1 ERREX                                                            
*                                                                               
TRAPERR2 GOTO1 ERREX2                                                           
*                                                                               
MSSNGERR MVI   ERROR,001                                                        
         J     TRAPERR                                                          
*                                                                               
INVFDERR MVI   ERROR,002                                                        
         J     TRAPERR                                                          
*                                                                               
NTNUMERR MVI   ERROR,003           NOT VALID NUMERIC DATA                       
         J     TRAPERR                                                          
*                                                                               
INVDTERR MVI   ERROR,006           INVALID DATE EXPRESSION                      
         J     TRAPERR                                                          
*                                                                               
RECACERR MVI   ERROR,012           INVALID RECORD ACTION ERROR                  
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
RECNFERR MVI   ERROR,053           RECORD NOT FOUND                             
         J     TRAPERR                                                          
*                                                                               
INVCLERR MVI   ERROR,062           INVALID CLIENT                               
         J     TRAPERR                                                          
*                                                                               
INVESTER MVI   ERROR,064           INVALID ESTIMATE                             
         J     TRAPERR                                                          
*                                                                               
STENDERR MVI   ERROR,069           START DATE IS LATER THAN END DATE            
         J     TRAPERR                                                          
*                                                                               
LONGERR  MVI   ERROR,071           INPUT IS TOO LONG                            
         J     TRAPERR                                                          
*                                                                               
NOCHGERR MVI   ERROR,079           FLD CANNOT BE CHANGED                        
         J     TRAPERR                                                          
*                                                                               
CLTRQERR MVI   ERROR,085           SPECIFIC CLT REQUIRED (SECURITY)             
         J     TRAPERR                                                          
*                                                                               
ACCERR   MVI   ERROR,087           INVALID FOR SELECTED RECORD TYPE             
         J     TRAPERR                                                          
*                                                                               
PFKEYERR MVI   ERROR,088           INVALID PFKEY                                
         LA    R2,CONACTH          POINT TO ACTION FLD                          
         J     TRAPERR                                                          
*                                                                               
CLTACERR MVI   ERROR,089           CLT LIMIT ACCESS ERROR                       
         J     TRAPERR                                                          
*                                                                               
STDTAERR MVI   ERROR,091           START DATE CANNOT BE ADVANCED                
         J     TRAPERR                                                          
*                                                                               
ENDTCERR MVI   ERROR,098           END DATE CANNOT BE CUT BACK                  
         J     TRAPERR                                                          
*                                                                               
ENDTINV  MVI   ERROR,099           END DATE CANNOT BE BEFORE CURRENT YR         
         J     TRAPERR                                                          
*                                                                               
YEARERR  MVI   ERROR,111           EST PERIOD CANNOT EXCEED ONE YEAR            
         J     TRAPERR                                                          
*                                                                               
ZZZ1ERR  MVI   ERROR,112           EST DATES MUST AGREE W/ ZZZ EST'S            
         J     TRAPERR                                                          
*                                                                               
ZZZ2ERR  MVI   ERROR,113           ZZZ EST DATES MUST AGREE W/ BRAND'S          
         J     TRAPERR                                                          
*                                                                               
DUPEDERR MVI   ERROR,179           DUPLICATE ENTRIES ERROR MSG                  
         J     TRAPERR                                                          
*                                                                               
SELMSG01 DC    C'Record displayed - hit Pf12 to return or next sel'             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
AUTOESTN NTR1  BASE=*,LABEL=*      Auto assign estimate number                  
*                                                                               
         CLI   ACTNUM,ACTADD       Adding a new estimate record?                
         JNE   SETCCNEQ                                                         
         TM    GENSTAT6,GES$UPLD   DDLINK general upload?                       
         JZ    SETCCNEQ                                                         
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         LA    R6,KEY                                                           
         USING PESTKEY,R6                                                       
         MVI   PESTKRCD,X'07'      Set to read estimate records                 
         MVC   PESTKPRD,ESTPRD                                                  
         OC    PESTKPRD,SPACES     Space padded                                 
         LHI   R3,1                Start at estimate 1                          
*                                                                               
AUTOE#30 STCM  R3,3,PESTKEST                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(25),KEYSAVE     Estimate record found?                       
         JNE   AUTOE#50                                                         
         AHI   R3,1                                                             
         CHI   R3,999              Max allowed estimate number?                 
         JNH   AUTOE#30                                                         
         SR    R3,R3               Set estimate to 0 for error                  
*                                                                               
AUTOE#50 EDIT  (R3),ESTEST,0,ALIGN=RIGHT,ZERO=NOBLANK,FILL=0                    
*                                                                               
         OI    ESTESTH+4,X'08'     Set valid numeric                            
         MVI   ESTESTH+5,3         Set input length                             
         OI    ESTESTH+6,X'80'     Transmit field                               
*                                                                               
         MVC   KEY,SVWORK          Restore KEY                                  
*                                                                               
         J     SETCCEQ                                                          
         DROP  RB,R6                                                            
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKUPLMSG NTR1  BASE=*,LABEL=*      Check for upload message                     
*                                                                               
         TM    GENSTAT6,GES$UPLD   DDLINK general upload?                       
         JZ    EXIT                                                             
         MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD+00(03),ESTEST                                            
         CLI   ACTNUM,ACTADD                                                    
         JNE   *+10                                                             
         MVC   CONHEAD+04(20),=C'(estimate#) is added'                          
         CLI   ACTNUM,ACTCHA                                                    
         JNE   *+10                                                             
         MVC   CONHEAD+04(22),=C'(estimate#) is changed'                        
         OI    CONHEADH+6,X'80'                                                 
         OI    GENSTAT2,USMYOK                                                  
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKESTORG NTR1  BASE=*,LABEL=*      Check for estimate origin                    
*                                                                               
         TM    GENSTAT6,GES$UPLD   DDLINK general upload?                       
         JZ    SETCCEQ                                                          
         CLI   ACTNUM,ACTADD       Add?                                         
         JNE   SETCCEQ                                                          
         CLI   ESTEORG,PEORGPRQ    Estimate origin is Prisma?                   
         JE    CKEORG30                                                         
         CLI   ESTEORG,PEORGRAQ    Estimate origin is Radia?                    
         JE    CKEORG30                                                         
         J     SETCCNEQ                                                         
*                                                                               
CKEORG30 XC    ELEM,ELEM                                                        
         USING PEORGELM,R6                                                      
         LA    R6,ELEM                                                          
         MVI   PEORGELC,PEORGECQ                                                
         MVI   PEORGELN,PEORGLNQ                                                
         MVC   PEORGCOD,ESTEORG    Estimate origin code                         
         MVC   PEORGDAT,BTODAY                                                  
         BRAS  RE,GET_BPID                                                      
         MVC   PEORGPID,SV_B_PID                                                
         GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
         J     EXIT                                                             
         DROP  RB                                                               
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DK       NTR1  BASE=*,LABEL=*      DISPLAY KEY                                  
*                                                                               
         L     R6,AIO                                                           
         CLI   3(R6),X'07'         EST RECORD CODE?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTKEY,R6                                                       
         MVC   ESTMED,PESTKMED                                                  
         MVC   ESTCLT,PESTKCLT                                                  
         MVC   ESTPRD,PESTKPRD                                                  
         EDIT  (B2,PESTKEST),ESTEST,0,ALIGN=RIGHT,ZERO=NOBLANK,FILL=0           
         DROP  R6                                                               
*                                                                               
         MVI   USEIONUM,2          USE AIO2 FOR MED/CLT/PRD/EST RECS            
*                                                                               
         OI    ESTMEDH+6,X'80'     TRANSMIT MEDIA CODE                          
         XC    ESTMEDN,ESTMEDN                                                  
         OI    ESTMEDNH+6,X'80'    CLEARED MEDIA NAME                           
         MVI   ESTMEDH+5,1         INPUT LENGTH                                 
         LA    R2,ESTMEDH                                                       
         GOTO1 VALIMED                                                          
         MVC   ESTMEDN,MEDNM                                                    
         OI    ESTMEDNH+6,X'80'    TRANSMIT MEDIA NAME                          
*                                                                               
         OI    ESTCLTH+6,X'80'     TRANSMIT CLT CODE                            
         XC    ESTCLTN,ESTCLTN                                                  
         OI    ESTCLTNH+6,X'80'    CLEARED CLT NAME                             
         MVI   ESTCLTH+5,3         INPUT LENGTH                                 
         LA    R2,ESTCLTH                                                       
         GOTO1 VALICLT                                                          
         MVC   ESTCLTN,CLTNM                                                    
         OI    ESTCLTNH+6,X'80'    TRANSMIT CLT NAME                            
*                                                                               
         OI    ESTPRDH+6,X'80'     TRANSMIT PRD CODE                            
         XC    ESTPRDN,ESTPRDN                                                  
         OI    ESTPRDNH+6,X'80'    CLEARED PRD NAME                             
         MVI   ESTPRDH+5,3         INPUT LENGTH                                 
         LA    R2,ESTPRDH                                                       
         GOTO1 VALIPRD                                                          
         MVC   ESTPRDN,PRDNM                                                    
         OI    ESTPRDNH+6,X'80'    TRANSMIT PRD NAME                            
*                                                                               
         OI    ESTESTH+6,X'80'     TRANSMIT EST CODE                            
         XC    ESTESTN,ESTESTN                                                  
         OI    ESTESTNH+6,X'80'    CLEARED est NAME                             
         MVI   ESTESTH+5,3         INPUT LENGTH                                 
         LA    R2,ESTESTH                                                       
         GOTO1 VALIEST                                                          
         MVC   ESTESTN,ESTNM                                                    
         OI    ESTESTNH+6,X'80'    TRANSMIT EST NAME                            
*                                                                               
DKX      DS    0H                                                               
         MVC   AIO,AIO1            AIO1 HAS REC TO BE DISPLAYED                 
         MVI   USEIONUM,1          RESET TO AIO1                                
         J     EXIT                                                             
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PACK     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    SAVE_R0,SAVE_R0                                                  
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         ZAP   DUB,=P'0'                                                        
         IC    R1,5(R2)                                                         
         CHI   R1,0                                                             
         BNH   PACK_X              EXIT ON ZERO LENGTH                          
         TM    4(R2),X'08'                                                      
         BZ    PACK_X              OR NON NUMERIC                               
         BCTR  R1,R0                                                            
         EX    R1,VARPACK                                                       
         CVB   R0,DUB                                                           
         B     PACK_X                                                           
*                                                                               
VARPACK  PACK  DUB,8(0,R2)                                                      
*                                                                               
PACK_X   ST    R0,SAVE_R0                                                       
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BPCEFFDT NTR1 BASE=*,LABEL=*       BILL ON PLANNED COST EFFECTIVE DATE          
*                                                                               
* * * *  XC    WORK,WORK           GET B2B PROFILE                              
* * * *  XC    SVWORK,SVWORK                                                    
* * * *  MVC   WORK(4),=C'PB2B'                                                 
* * * *  NI    WORK,X'BF'          LOWER CASE                                   
* * * *  MVC   WORK+4(2),AGENCY                                                 
* * * *  MVC   WORK+6(1),QMED                                                   
* * * *  MVC   WORK+7(3),QCLT                                                   
* * * *  CLI   SVCPROF+30,C' '                                                  
* * * *  BNH   *+14                                                             
* * * *  MVI   WORK+10,C'*'                                                     
* * * *  MVC   WORK+11(1),SVCPROF+30                                            
* * * *  GOTO1 GETPROF,DMCB,WORK,SVWORK,DATAMGR                                 
*                                                                               
         XC    SVWORK,SVWORK                                                    
         LA    R2,ESTPCEDH                                                      
         MVI   ELCODE,PEBPCECQ                                                  
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   BPCED10                                                          
         ZIC   RE,1(R6)            SAVE OLD ELEM                                
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVWORK(0),0(R6)                                                  
         B     BPCED20                                                          
                                                                                
BPCED10  CLI   5(R2),0             ANY INPUTS?                                  
         BNH   BPCED_X                                                          
         B     BPCED30                                                          
*                                                                               
BPCED20  GOTO1 REMELEM             REMOVE BILL ON PC EFF. DATE ELEM             
         CLI   5(R2),0             ANY INPUTS?                                  
         BH    BPCED30                                                          
         XC    FULL,FULL           SET YEAR AND MONTH TO X'0000'                
         B     BPCED60                                                          
*                                                                               
BPCED30  GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(X'40',ELEM)                           
         CLI   DMCB+4,X'01'                                                     
         BE    BPCED_E4                                                         
         USING PERVALD,R6                                                       
         LA    R6,ELEM                                                          
         CLI   PVALBSTA+2,1        FIRST DAY OF MONTH?                          
         BH    BPCED_E5                                                         
         GOTO1 DATVAL,DMCB,(0,ESTST),WORK                                       
         GOTO1 DATCON,DMCB,(0,WORK),(3,DUB)                                     
         CLC   PVALBSTA(2),DUB                                                  
         BL    BPCED_E3                                                         
         GOTO1 DATVAL,DMCB,(0,ESTEND),WORK                                      
         GOTO1 DATCON,DMCB,(0,WORK),(3,DUB)                                     
         CLC   PVALBSTA(2),DUB                                                  
         BH    BPCED_E3                                                         
         MVC   FULL(3),PVALBSTA                                                 
*                                                                               
BPCED60  XC    ELEM,ELEM                                                        
         USING PESTBPCE,R6                                                      
         LA    R6,ELEM                                                          
         MVI   PEBPCELC,PEBPCECQ                                                
         MVI   PEBPCELN,PEBPCELQ                                                
         MVC   PEBPCEFF,FULL       BINARY YEAR & MONTH                          
         BRAS  RE,GET_BPID                                                      
         MVC   PEBPCPID,SV_B_PID                                                
         MVC   PEBPCCHG,BTODAY                                                  
*                                                                               
         OC    SVWORK,SVWORK       HAVE OLD ELEM?                               
         BZ    BPCED70                                                          
         CLC   PEBPCEFF,SVWORK+(PEBPCEFF-PESTBPCE)                              
         BNE   BPCED70                                                          
         MVC   ELEM,SVWORK         NOT A REAL CHANGE                            
*                                                                               
BPCED70  GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
BPCED_X  J     SETCCEQ                                                          
*                                                                               
BPCED_E3 LHI   R2,97               DATE IS OUTSIDE OF ESTIMATE RANGE            
         J     BPCED_EX                                                         
BPCED_E4 LHI   R2,193              INVALID DATE EXPRESSION                      
         J     BPCED_EX                                                         
BPCED_E5 LHI   R2,294              EFFECTIVE DATE FORMAT IS MMM/YY              
*                                                                               
BPCED_EX BRAS  RE,GET_ETXT                                                      
         LA    R2,ESTPCEDH         POINT TO BILL ON PLANNED COST FLD            
         J     SCCNEQ_R                                                         
*                                                                               
SCCEQ_R  CR    RB,RB               EQUAL                                        
         J     *+6                                                              
SCCNEQ_R LTR   RB,RB               NOT EQUAL                                    
X_R2     XIT1  REGS=(R2)           CURSOR POSITION                              
*                                                                               
GET_BPID LR    R0,RE                                                            
         XC    SV_B_PID,SV_B_PID   BINARY PID                                   
         L     RF,ACOMFACS                                                      
         L     RF,CGETFACT-COMFACSD(RF)                                         
         GOTOR (RF),DMCB,(2,0),0,0                                              
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   SVSECAGY,FATAGYSC   ALSO NEEDED TO GET CORRECT PID               
         TM    FATFLAG,X'08'       CHECK IF SECET CODE IS THERE                 
         JZ    *+10                                                             
         MVC   SV_B_PID,FAPASSWD   SAVE PASSWORD ID NUMBER                      
         LR    RE,R0                                                            
         BR    RE                                                               
         DROP  R1                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
D_BPCEDT NTR1  BASE=*,LABEL=*      DISPLAY BILL ON PC EFFECTIVE DATE            
*                                                                               
         XC    ESTPCED,ESTPCED                                                  
         XC    ESTPCAC,ESTPCAC                                                  
         CLI   SECPCED,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    D_BPC_X                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,PEBPCECQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   D_BPC_X                                                          
         USING PESTBPCE,R6                                                      
         OC    PEBPCEFF,PEBPCEFF   HAVE YEAR AND MONTH?                         
         BZ    D_BPC20                                                          
         MVC   DUB(L'PEBPCEFF),PEBPCEFF                                         
         MVI   DUB+L'PEBPCEFF,1                                                 
         GOTO1 DATCON,DMCB,(3,DUB),(6,ESTPCED)                                  
*                                                                               
D_BPC20  OC    PEBPCPID,PEBPCPID   HAVE PID?                                    
         BZ    D_BPC_X                                                          
         MVC   ESTPCAC(L'BPC_TXT1),BPC_TXT1                                     
         BRAS  RE,GET_BPID                                                      
         MVC   HALF,PEBPCPID                                                    
         BRAS  RE,GET_CPID                                                      
         OC    WORK,WORK                                                        
         BZ    D_BPC_X                                                          
         MVC   ESTPCAC+L'BPC_TXT1+1(L'SAPALPID),WORK                            
         OC    PEBPCCHG,PEBPCCHG                                                
         BZ    D_BPC_X                                                          
         LA    RF,ESTPCAC+(L'ESTPCAC-1)                                         
         BRAS  RE,LAST_CHR                                                      
         LR    R2,RF                                                            
         MVI   2(R2),C'('                                                       
         GOTO1 DATCON,DMCB,(3,PEBPCCHG),(10,3(R2))                              
         LA    RF,ESTPCAC+(L'ESTPCAC-1)                                         
         BRAS  RE,LAST_CHR                                                      
         MVI   1(RF),C')'                                                       
*                                                                               
D_BPC_X  OI    ESTPCEDH+6,X'80'                                                 
         OI    ESTPCACH+6,X'80'                                                 
         J     EXIT                                                             
*                                                                               
LAST_CHR CLI   0(RF),0                                                          
         JE    *+8                                                              
         CLI   0(RF),C' '                                                       
         JH    *+10                                                             
         BCTR  RF,0                                                             
         J     *-18                                                             
         BR    RE                                                               
*                                                                               
BPC_TXT1 DC    C'Last Chg:'                                                     
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GET_CPID NTR1  BASE=*,LABEL=*      GET CHAR PID                                 
*                                                                               
         XC    WORK,WORK           INIT REPLY BLOCK                             
         L     RF,AIO3                                                          
         USING CT0REC,RF           ESTABLISH KEY AS PERSON AUTH REC             
         XC    CT0KEY,CT0KEY       INIT KEY                                     
         MVI   CT0KTYP,CT0KTEQU    SET RECORD TYPE                              
         MVC   CT0KNUM,HALF        SET PID                                      
         MVC   CT0KAGY,SVSECAGY                                                 
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'CTFILE',(RF),(RF)                 
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO3                                                          
         LA    RE,CT0DATA          POINT TO FIRST ELEMENT                       
         SR    R0,R0                                                            
         USING SAPALEL,RE                                                       
G_CPID02 CLI   SAPALEL,0           CHECK FOR END OF RECORD                      
         BE    G_CPID_X                                                         
         CLI   SAPALEL,SAPALELQ    MATCH ON ELEMENT CODE?                       
         BE    *+14                                                             
         IC    R0,SAPALLN          GET ELEMENT LENGTH                           
         AR    RE,R0               BUMP TO NEXT ELEMENT                         
         B     G_CPID02            GO FIND NEXT ELEMENT                         
*                                                                               
         MVC   WORK(L'SAPALPID),SAPALPID                                        
*                                                                               
G_CPID_X J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,RE,RF                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ACTULZDT NTR1 BASE=*,LABEL=*       ESTIMATE ACTUALIZATION DATE                  
*                                                                               
         XC    SVWORK,SVWORK                                                    
         LA    R2,ESTACTDH                                                      
         MVI   ELCODE,PEACTECQ                                                  
         L     R6,AIO                                                           
         BRAS  RE,GETEL                                                         
         BNE   ACTZD10                                                          
         ZIC   RE,1(R6)            SAVE ELEM                                    
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   SVWORK(0),0(R6)                                                  
         B     ACTZD20                                                          
*                                                                               
ACTZD10  CLI   5(R2),0             ANY INPUTS?                                  
         BNH   ACTZD_X                                                          
         B     ACTZD30                                                          
*                                                                               
ACTZD20  GOTO1 REMELEM             REMOVE ACTUALIZE DATE ELEM                   
         CLI   5(R2),0             ANY INPUTS?                                  
         BH    ACTZD30                                                          
         XC    FULL,FULL           SET YEAR AND MONTH TO X'0000'                
         B     ACTZD60                                                          
*                                                                               
ACTZD30  GOTO1 PERVAL,DMCB,(5(R2),8(R2)),(X'40',ELEM)                           
         CLI   DMCB+4,X'01'                                                     
         BE    ACTZD_E4                                                         
         USING PERVALD,R6                                                       
         LA    R6,ELEM                                                          
         CLI   PVALBSTA+2,1        FIRST DAY OF MONTH?                          
         BH    ACTZD_E5                                                         
         GOTO1 DATVAL,DMCB,(0,ESTST),WORK                                       
         GOTO1 DATCON,DMCB,(0,WORK),(3,DUB)                                     
         CLC   PVALBSTA(2),DUB                                                  
         BL    ACTZD_E3                                                         
         GOTO1 DATVAL,DMCB,(0,ESTEND),WORK                                      
         GOTO1 DATCON,DMCB,(0,WORK),(3,DUB)                                     
         CLC   PVALBSTA(2),DUB                                                  
         BH    ACTZD_E3                                                         
         MVC   FULL(3),PVALBSTA                                                 
*                                                                               
ACTZD60  XC    ELEM,ELEM                                                        
         USING PESTACTD,R6                                                      
         LA    R6,ELEM                                                          
         MVI   PEACTELC,PEACTECQ                                                
         MVI   PEACTELN,PEACTELQ                                                
         MVC   PEACTDAT,FULL       BINARY YEAR & MONTH                          
         BRAS  RE,GET_BPID                                                      
         MVC   PEACTPID,SV_B_PID                                                
         MVC   PEACTCHG,BTODAY                                                  
*                                                                               
         OC    SVWORK,SVWORK       HAVE OLD ELEM?                               
         BZ    ACTZD70                                                          
         CLC   PEACTDAT,SVWORK+(PEACTDAT-PESTACTD)                              
         BNE   ACTZD70                                                          
         MVC   ELEM,SVWORK         NOT A REAL CHANGE                            
*                                                                               
ACTZD70  GOTO1 ADDELEM                                                          
         DROP  R6                                                               
*                                                                               
ACTZD_X  J     SETCCEQ                                                          
*                                                                               
ACTZD_E3 LHI   R2,97               DATE IS OUTSIDE OF ESTIMATE RANGE            
         J     ACTZD_EX                                                         
ACTZD_E4 LHI   R2,193              INVALID DATE EXPRESSION                      
         J     ACTZD_EX                                                         
ACTZD_E5 LHI   R2,294              EFFECTIVE DATE FORMAT IS MMM/YY              
*                                                                               
ACTZD_EX BRAS  RE,GET_ETXT                                                      
         LA    R2,ESTACTDH         POINT TO ACTUALIZE THRU FLD                  
         J     SCCNEQ_R                                                         
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
D_ACTZDT NTR1  BASE=*,LABEL=*      DISPLAY ACTUALIZE DATE                       
*                                                                               
         XC    ESTACTD,ESTACTD                                                  
         XC    ESTACTA,ESTACTA                                                  
         CLI   SECACTD,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    D_AZD_X                                                          
         L     R6,AIO                                                           
         MVI   ELCODE,PEACTECQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   D_AZD_X                                                          
         USING PESTACTD,R6                                                      
         OC    PEACTDAT,PEACTDAT   HAVE YEAR AND MONTH?                         
         BZ    D_AZD20                                                          
         MVC   DUB(L'PEACTDAT),PEACTDAT                                         
         MVI   DUB+L'PEACTDAT,1                                                 
         GOTO1 DATCON,DMCB,(3,DUB),(6,ESTACTD)                                  
*                                                                               
D_AZD20  OC    PEACTPID,PEACTPID   HAVE PID?                                    
         BZ    D_AZD_X                                                          
         MVC   ESTACTA(L'AZD_TXT1),AZD_TXT1                                     
         BRAS  RE,GET_BPID                                                      
         MVC   HALF,PEACTPID                                                    
         BRAS  RE,GET_CPID                                                      
         OC    WORK,WORK                                                        
         BZ    D_AZD_X                                                          
         MVC   ESTACTA+L'AZD_TXT1+1(L'SAPALPID),WORK                            
         OC    PEACTCHG,PEACTCHG                                                
         BZ    D_AZD_X                                                          
         LA    RF,ESTACTA+(L'ESTACTA-1)                                         
         BRAS  RE,LAST_CHR                                                      
         LR    R2,RF                                                            
         MVI   2(R2),C'('                                                       
         GOTO1 DATCON,DMCB,(3,PEACTCHG),(10,3(R2))                              
         LA    RF,ESTACTA+(L'ESTACTA-1)                                         
         BRAS  RE,LAST_CHR                                                      
         MVI   1(RF),C')'                                                       
*                                                                               
D_AZD_X  OI    ESTACTDH+6,X'80'                                                 
         OI    ESTACTAH+6,X'80'                                                 
         J     EXIT                                                             
*                                                                               
AZD_TXT1 DC    C'Last Chg:'                                                     
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
D_ESTORG NTR1  BASE=*,LABEL=*      Display estimate origin                      
*                                                                               
         XC    ESTESTO,ESTESTO                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,PEORGECQ                                                  
         BRAS  RE,GETEL                                                         
         JNE   D_ESO_X                                                          
         USING PEORGELM,R6                                                      
         CLI   PEORGCOD,PEORGPRQ   Prisma?                                      
         JNE   *+14       123457                                                
         MVC   ESTESTO,=C'*Prisma'                                              
         J     D_ESO_X                                                          
         CLI   PEORGCOD,PEORGRAQ   Radia?                                       
         JNE   *+14                                                             
         MVC   ESTESTO,=C'*Radia '                                              
         J     D_ESO_X                                                          
*                                                                               
D_ESO_X  OI    ESTESTOH+6,X'80'                                                 
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKESTBUK NTR1  BASE=*,LABEL=*      MAKE SURE EST BUCKETS ARE IN SYNC            
*                                                                               
         MVC   SVWORK(L'KEY),KEY                                                
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         JNE   ESTB_10                                                          
         MVC   SVWORK(L'SVKEY),SVKEY                                            
         MVC   KEY(L'SVKEY),SVKEY                                               
*                                                                               
ESTB_10  MVC   DUB(L'AIO),AIO                                                   
         MVC   AIO,AIO3                                                         
         MVI   KEY+3,X'09'         EST BUCKET REC CODE                          
         GOTO1 HIGH                                                             
         CLC   KEY(12),KEYSAVE     EST BUCKET REC FOUND?                        
         BNE   ESTB_X                                                           
         GOTO1 GETREC                                                           
         L     RE,AIO                                                           
         LA    RE,33(RE)           POINT TO 1ST ELEM OF EST BUCKET REC          
         USING BKELEM,RE                                                        
ESTB_20  CLI   0(RE),X'00'         END OF EST BUCKET REC?                       
         BE    ESTB_X                                                           
         CLI   0(RE),X'21'         TODAY?                                       
         BE    ESTB_30                                                          
         CLI   0(RE),X'22'         REGULAR?                                     
         BE    ESTB_30                                                          
         CLI   0(RE),X'23'         SPECIAL MONTHS?                              
         BE    ESTB_30                                                          
         CLI   0(RE),X'31'         TODAY WITH GST?                              
         BE    ESTB_30                                                          
         CLI   0(RE),X'41'         TODAY - PLANNED COST?                        
         BE    ESTB_30                                                          
         CLI   0(RE),X'42'         REGULAR - PLANNED COST?                      
         BE    ESTB_30                                                          
         CLI   0(RE),X'43'         SPECIAL MONTHS - PLANNED COST?               
         BE    ESTB_30                                                          
         B     ESTB_40                                                          
ESTB_30  TM    BKIND,X'01'         TEST ESTIMATE?                               
         JNZ   SETCCNEQ                                                         
ESTB_40  SR    R0,R0                                                            
         IC    R0,1(RE)                                                         
         AR    RE,R0               POINT TO NEXT ELEM IN EST BUCKET REC         
         B     ESTB_20                                                          
         DROP  RE                                                               
*                                                                               
ESTB_X   MVC   KEY,SVWORK          RESTORE KEY                                  
         GOTO1 HIGH                                                             
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         JE    ESTB_XX                                                          
         GOTO1 GETREC              RESTORE SEQUENCE FOR GENCON                  
ESTB_XX  MVC   AIO,DUB             RESTORE ORIGINAL AIO                         
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
DISP_REC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R6,AIO                                                           
         CLI   3(R6),X'07'         EST RECORD CODE?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTELEM,R6                                                      
         MVI   ELCODE,X'07'        FIRST EST ELEM CODE                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         XC    ESTENAM,ESTENAM     EST NAME LINE 1                              
         MVC   ESTENAM(L'PESTNAME),PESTNAME                                     
         OI    ESTENAMH+6,X'80'                                                 
*                                                                               
         XC    ESTNAM2,ESTNAM2     EST NAME LINE 2                              
         MVC   ESTNAM2(L'PESTNAM2),PESTNAM2                                     
         OI    ESTNAM2H+6,X'80'                                                 
*                                                                               
         XC    ESTST,ESTST         EST START DATE                               
         GOTO1 DATCON,DMCB,(0,PESTST),(5,ESTST)                                 
         OI    ESTSTH+6,X'80'                                                   
*                                                                               
         XC    ESTEND,ESTEND       EST END DATE                                 
         GOTO1 DATCON,DMCB,(0,PESTEND),(5,ESTEND)                               
         OI    ESTENDH+6,X'80'                                                  
*                                                                               
         XC    ESTFLTR,ESTFLTR     EST FILTER                                   
         CLI   SECEFLT,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    *+10                                                             
         MVC   ESTFLTR(L'PESTGRPS),PESTGRPS                                     
         OI    ESTFLTRH+6,X'80'                                                 
*                                                                               
         XC    ESTIDKE,ESTIDKE     iDesk Estimate                               
         TM    PESTTEST,X'20'                                                   
         JZ    *+8                                                              
         MVI   ESTIDKE,C'Y'                                                     
         OI    ESTIDKEH+6,X'80'                                                 
*                                                                               
         XC    ESTRSCH,ESTRSCH     RETAIL SCHEME                                
         CLI   SECERSC,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    *+10                                                             
         MVC   ESTRSCH(L'PESTRSCH),PESTRSCH                                     
         OI    ESTRSCHH+6,X'80'                                                 
*                                                                               
         XC    ESTRTYP,ESTRTYP     RATE TYPE                                    
         CLI   SECERTY,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    *+10                                                             
         MVC   ESTRTYP(L'PESTRTYP),PESTRTYP                                     
         OI    ESTRTYPH+6,X'80'                                                 
*                                                                               
         XC    ESTSTAT,ESTSTAT                                                  
*                                                                               
         CLI   SECESTA,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR30U                                                            
*                                                                               
         LA    R1,ESTSTAT          POINT TO OUTPUT                              
*                                                                               
         CLI   PESTSTAT,C'1'                                                    
         BE    *+8                                                              
         CLI   PESTSTAT,C'2'                                                    
         BE    *+12                                                             
         MVI   PESTSTAT,0          RESET STAT IF OTHER VALUES FOUND             
         B     DR30J                                                            
*                                                                               
         MVC   ESTSTAT(1),PESTSTAT DISPLAY LOCK OPTION                          
         LA    R1,1(R1)            BUMP TO NEXT OUTPUT AREA                     
*                                                                               
DR30J    DS    0H                                                               
*                                                                               
         TM    PESTTEST,X'40'      STEWARDSHIP ESTIMATE?                        
         BZ    DR30K               NO                                           
*                                                                               
         MVC   0(4,R1),=C'STEW'                                                 
*                                                                               
         B     DR30U                                                            
*                                                                               
DR30K    DS    0H                                                               
*                                                                               
         TM    PESTTEST,X'80'      TEST ESTIMATE?                               
         BZ    DR30M               NO                                           
*                                                                               
         MVC   0(4,R1),=C'TEST'                                                 
*                                                                               
         B     DR30U                                                            
*                                                                               
DR30M    MVC   0(4,R1),=C'LIVE'    DEFAULT TO LIVE                              
*                                                                               
DR30U    OI    ESTSTATH+6,X'80'    FORCE RE-TRANSMISSION                        
*                                                                               
         XC    ESTAD,ESTAD         AD NUMBER (JOB CODE)                         
         MVC   ESTAD(L'PESTJOB),PESTJOB                                         
         OI    ESTADH+6,X'80'                                                   
*                                                                               
         XC    ESTCOM,ESTCOM       STANDARD COMMENT(S)                          
         LA    RE,ESTCOM                                                        
         CLI   PESTCOM,0                                                        
         BNH   *+14                                                             
         MVC   0(6,RE),PESTCOM     FIRST COMMENT CODE                           
         LA    RE,6(RE)                                                         
         CLI   PESTCOM2,0          SECOND STANDARD COMMENT ?                    
         BNH   *+14                NO                                           
         MVI   0(RE),C','                                                       
         MVC   1(6,RE),PESTCOM2                                                 
DR32U    OI    ESTCOMH+6,X'80'                                                  
*                                                                               
         XC    ESTREV,ESTREV       REVISION NUMBER                              
         MVC   ESTREV(L'PESTREVN),PESTREVN                                      
         OI    ESTREVH+6,X'80'                                                  
*                                                                               
         XC    ESTREP,ESTREP       SPECIAL REP                                  
         XC    ESTREPN,ESTREPN     BILLING REP NAME (PROTECTED)                 
         CLI   SECESPR,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR36U                                                            
         OC    PESTREP,PESTREP     REP CODE PRESENT?                            
         BZ    DR36U                                                            
         LA    R2,PESTREP                                                       
         LA    R3,ESTREPNH                                                      
         BRAS  RE,DR_DREPN                                                      
         MVC   ESTREP(L'PESTREP),PESTREP                                        
DR36U    OI    ESTREPH+6,X'80'                                                  
         OI    ESTREPNH+6,X'80'                                                 
*                                                                               
         XC    ESTBREP,ESTBREP     BILLING REP                                  
         XC    ESTBRPN,ESTBRPN     BILLING REP NAME (PROTECTED)                 
         OC    PESTBREP,PESTBREP   REP CODE PRESENT?                            
         BZ    DR38U                                                            
         LA    R2,PESTBREP                                                      
         LA    R3,ESTBRPNH                                                      
         BRAS  RE,DR_DREPN                                                      
         MVC   ESTBREP(L'PESTBREP),PESTBREP                                     
DR38U    OI    ESTBREPH+6,X'80'                                                 
         OI    ESTBRPNH+6,X'80'                                                 
*                                                                               
         XC    ESTALLO,ESTALLO                                                  
         MVC   ESTALLO(L'PESTZZZ),PESTZZZ                                       
         OI    ESTALLOH+6,X'80'                                                 
*                                                                               
         DROP  R6                  USING PESTELEM (MAIN ELEM)                   
*                                                                               
         XC    ESTDSC1,ESTDSC1     USER DEFINITION FLDS                         
         XC    ESTDSC2,ESTDSC2                                                  
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DR40U                                                            
         USING PESTUDEF,R6                                                      
         CLI   SECEUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR40M                                                            
         OC    SVE1USER,SVE1USER                                                
         BZ    *+10                                                             
         MVC   ESTDSC1,PEUSER1                                                  
DR40M    CLI   SECEUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR40U                                                            
         OC    SVE2USER,SVE2USER                                                
         BZ    *+10                                                             
         MVC   ESTDSC2,PEUSER2                                                  
DR40U    OI    ESTDSC1H+6,X'80'                                                 
         OI    ESTDSC2H+6,X'80'                                                 
         DROP  R6                                                               
*                                                                               
         BRAS  RE,D_BPCEDT         DISPLAY BILL ON PC EFFECTIVE DATE            
         BRAS  RE,D_ACTZDT         DISPLAY ACTUALIZE DATE                       
         BRAS  RE,D_ESTORG         DISPLAY estimate origin                      
*                                                                               
         XC    ESTPON,ESTPON                                                    
         CLI   SECEPON,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    DR50                                                             
         L     R6,AIO                                                           
         MVI   ELCODE,PEPONECQ     PURCHASE ORDER NUMBER ELEMENT                
         BRAS  RE,GETEL                                                         
         BNE   DR50                                                             
         USING PESTPOND,R6                                                      
         MVC   ESTPON,PEPONPON     PURCHASE ORDER #                             
DR50     OI    ESTPONH+6,X'80'                                                  
         DROP  R6                                                               
*                                                                               
DRX      CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         BE    DRX_10              NO NEED TO RESTORE ON ADD                    
         MVC   DUB(L'AIO),AIO      AIO HAS UPDATED REC                          
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC              RESTORE SEQ FOR GENCON'S PUTREC              
         MVC   AIO,DUB             RESTORE ORIGINAL AIO                         
*                                                                               
DRX_10   J     EXIT                                                             
*                                                                               
* R2 = REP CODE, R3 = REP NAME HEADER FLD                                       
*                                                                               
DR_DREPN ST    RE,FULL             DISPLAY REP NAME                             
         MVC   SVWORK(L'KEY),KEY                                                
         XC    KEY+03(L'KEY-03),KEY+03                                          
         MVI   KEY+03,X'11'                                                     
         MVC   KEY+04(04),0(R2)                                                 
         GOTO1 HIGH                                                             
         CLC   KEYSAVE(25),KEY     REP CODE ON DIRECTORY?                       
         BE    *+14                                                             
         MVC   8(21,R3),=C'** REP NOT ON FILE **'                               
         B     DR_DREPX                                                         
         MVC   DUB(L'AIO),AIO      SAVE ORIGINAL AIO                            
         MVC   AIO,AIO3                                                         
         GOTO1 GETREC                                                           
         L     RE,AIO3                                                          
         USING PREPREC,RE                                                       
         MVC   8(L'PREPNAME,R3),PREPNAME                                        
         DROP  RE                                                               
         MVC   AIO,DUB             RESTORE AIO AND KEY                          
DR_DREPX MVC   KEY,SVWORK                                                       
         L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LR_DFLTF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    RE,LSTTL4                                                        
         USING LS_TTL4,RE                                                       
         LA    RE,LTFSTART         POINT TO TITLE START                         
         DROP  RE                                                               
         LA    RF,LSTUL4                                                        
         USING LS_TTL4,RF                                                       
         LA    RF,LTFSTART         POINT TO TITLE (UNDERLINE) START             
         DROP  RF                                                               
*                                                                               
         LR    R1,RE                                                            
         AHI   R1,LTFFLDQ-3        3 CHARS BEFORE END OF DISPLAYING FLD         
*                                                                               
         L     R3,AIO2             POINT TO TABLE OF LISTING DATA               
         SR    R4,R4               LOOP COUNTER                                 
*                                                                               
LR_DFF10 CHI   R4,F_SMAXQ          NUMBER OF SCANNER ENTRIES REACHED?           
         BNL   LR_DFFX                                                          
         OC    0(L_DLNQ,R3),0(R3)  ANYTHING TO BE LISTED?                       
         BNZ   LR_DFF20                                                         
         LA    R3,L_DLNQ(R3)       NEXT ENTRY IN LISTING DATA TABLE             
         AHI   R4,1                LOOP COUNTER UP BY ONE                       
         B     LR_DFF10                                                         
*                                                                               
LR_DFF20 SR    R5,R5                                                            
         IC    R5,0(R3)            LENGTH OF TITLE                              
         CHI   R5,0                                                             
         BH    *+6                                                              
         DC    H'0'                INCORRECT LENGTH!                            
*                                                                               
         AR    RE,R5                                                            
         CR    RE,R1               STILL HAVE ROOM TO DISPLAY?                  
         BNL   LR_DFF50                                                         
         SR    RE,R5                                                            
*                                                                               
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),1(R3)       TITLE                                        
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),L_DASHES    TITLE (UNDERLINED)                           
         AHI   R5,1+2              1 FROM EX AND 2 SPACES                       
         AR    RE,R5                                                            
         AR    RF,R5                                                            
         IC    R5,L_DLNQ/2(R3)     LENGTH OF LIST FILTERING DATA                
         CHI   R5,0                                                             
         BH    *+6                                                              
         DC    H'0'                INCORRECT LENGTH!                            
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),L_DLNQ/2+1(R3)                                           
         IC    R5,0(R3)            LENGTH OF TITLE                              
         AHI   R5,2                2 SPACES                                     
         AR    R2,R5               POINT TO NEXT LISTING DATA AREA              
         LA    R3,L_DLNQ(R3)       NEXT ENTRY IN LISTING DATA TABLE             
         AHI   R4,1                LOOP COUNTER UP BY ONE                       
         B     LR_DFF10                                                         
*                                                                               
LR_DFF50 SR    RE,R5                                                            
         MVC   0(03,RE),=C'...'    CANNOT DISPLAY ANYMORE DATA                  
         MVC   0(03,RF),=C'...'                                                 
         MVC   0(03,R2),=C'...'                                                 
*                                                                               
LR_DFFX  J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* BUMP I/O COUNTER AND CHECK FOR MAX I/O                                        
*                                                                               
* CC NOT EQUAL - MAX I/O REACHED                                                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKMAXIO  NTR1  BASE=*,LABEL=*                                                   
         TM    WHEN,X'20'          SOON?                                        
         JNZ   SETCCEQ                                                          
         TM    WHEN,X'10'          OV?                                          
         JNZ   SETCCEQ                                                          
*                                                                               
         LH    RF,IO_COUNT                                                      
         CHI   RF,300              CHECK EVERY 300 I/O'S                        
         BL    CKMXIO50                                                         
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         SR    R2,R2                                                            
         SR    R3,R3                                                            
         ICM   R3,3,FATMAXIO       MAX IO'S                                     
         MHI   R3,MXIOPCT          GET % OF MAX                                 
         D     R2,=F'100'                                                       
         CLM   R3,3,FATIOCNT       STILL LESS THAN MAX%?                        
         BH    *+12                                                             
         OI    CNTLSW,C_MAXIOQ                                                  
         J     SETCCNEQ            MAX I/O EXCEEDED                             
*                                                                               
         SR    RF,RF               RESET COUNT FOR ANOTHER 300                  
*                                                                               
CKMXIO50 AHI   RF,1                                                             
         STH   RF,IO_COUNT                                                      
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R1                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CLRLSSCR NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         SR    RE,RE                                                            
         IC    RE,NLISTS           NUMBER OF LIST LINES                         
         LA    RF,LSTSELH          POINT TO FIRST LINE                          
         SR    R1,R1                                                            
CLRLS20  XC    8(03,RF),8(RF)                                                   
         OI    6(RF),X'80'                                                      
         IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         XC    8(74,RF),8(RF)                                                   
         OI    6(RF),X'80'                                                      
         IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         BCT   RE,CLRLS20                                                       
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LIST_REC NTR1  BASE=*,LABEL=*      List record                                  
*                                                                               
         XC    SVCLTLSC,SVCLTLSC                                                
         XC    SVPRDLSC,SVPRDLSC                                                
         CLI   KEY+3,X'07'         STILL EST RECORD?                            
         BNE   LR10                IF NOT, NEED TO WRAP AROUND AGAIN            
         OC    KEY,KEY                                                          
         BNZ   LR20_HI             CONTINUE LISTING                             
LR10     MVC   KEY,SVKEY           KEY BUILT FROM VK                            
*                                                                               
LR20_HI  GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 HIGH                FIRST RECORD                                 
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         B     LR40                                                             
*                                                                               
LR30_SEQ GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 SEQ                 NEXT RECORD                                  
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
LR40     CLC   KEY(4),KEYSAVE      SAME AGY/MED/RECORD CODE?                    
         BNE   LRX                                                              
*                                                                               
         CLC   KEY+04(3),CLTLSFLT  CLT IS HIGHER THAN FILTER?                   
         BL    LR30_SEQ                                                         
         CLC   KEY+07(3),PRDLSFLT  PRD IS HIGHER THAN FILTER?                   
         BL    LR30_SEQ                                                         
         CLC   KEY+10(2),ESTLSFLT  EST IS HIGHER THAN FILTER?                   
         BL    LR30_SEQ                                                         
*                                                                               
         XC    LISTAR,LISTAR       PREPARING LIST LINE                          
         MVC   SVWORK(L'KEY),KEY   RESTORING SEQUENCE LATER                     
*                                                                               
* BY PASS RECS WITH LIMIT ACCESS (NOT TO LIST THEM)                             
*                                                                               
         MVI   ERROPT,C'Y'         RETURN TO APPL INSTEAD OF GENCON ERR         
         XC    WKTMPFLD,WKTMPFLD                                                
         MVI   WKTMPFLD+0,8+3      TOTAL LENGTH OF TEMP FLD                     
         MVI   WKTMPFLD+5,3        INPUT LENGTH OF TEMP FLD                     
         MVC   WKTMPFLD+8(3),KEY+4 CLT CODE TO BE VALIDATED FOR SEC             
         LA    R2,WKTMPFLD                                                      
         MVC   WKTMPAIO,AIO                                                     
         MVC   WKTMPION,USEIONUM                                                
         MVI   USEIONUM,3          VALICLT WILL USE AIO3                        
         XC    CLTNM,CLTNM         IT WILL BE SET IF CLT IS OK                  
         GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 VALICLT                                                          
*                                                                               
         OC    CLTNM,CLTNM         CLT CODE PASS SECURITY?                      
         BNZ   LR42                                                             
         MVC   KEY,SVWORK          RESTORE EST KEY                              
         SR    RE,RE                                                            
         ICM   RE,7,KEY+4          CLIENT CODE                                  
         AHI   RE,1                WILL BUMP TO NEXT CLIENT CODE                
         STCM  RE,7,KEY+4                                                       
         MVC   AIO,WKTMPAIO                                                     
         MVC   USEIONUM,WKTMPION                                                
         B     LR20_HI             TRY NEXT CLIENT                              
*                                                                               
LR42     MVC   KEY,SVWORK          RESTORE EST KEY                              
         MVC   AIO,WKTMPAIO                                                     
         MVC   USEIONUM,WKTMPION                                                
         GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         GOTO1 HIGH                RESTORE SEQUENCE FOR PRD READING             
*                                                                               
         GOTOR CKMAXIO             CK FOR MAX I/O                               
         BNE   LR94_LM                                                          
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        1ST EST ELEM CODE                            
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                IT HAS TO BE THERE!                          
         USING PESTELEM,R6                                                      
         EDIT  (B2,KEY+10),(3,LSESTC),0,ALIGN=RIGHT,ZERO=NOBLANK,FILL=0         
         MVI   LSESTC+L'LSPRDC,C'/'                                             
         MVC   LSEST,PESTNAME                                                   
*                                                                               
* FILTERING ON FLITER FLDS                                                      
*                                                                               
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BNZ   LR50                                                             
*                                                                               
* NO FITLER VALUE(S) FOUND, DISPLAY DEFAULT LIST DATA                           
*                                                                               
         CLI   PESTSTAT,C'1'       SOFT?                                        
         BNE   *+14                                                             
         MVC   LSESTAT,=C'SOFT'                                                 
         B     LR46                                                             
         CLI   PESTSTAT,C'2'       PERM LOCKOUT?                                
         BNE   LR46                                                             
         MVC   LSESTAT,=C'LOCK'                                                 
*                                                                               
LR46     GOTO1 DATCON,DMCB,(0,PESTST),(10,LSESTART)                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTEND),(10,LSEEND)                              
*                                                                               
         MVC   LSADNUM,PESTJOB                                                  
         B     LR90                                                             
*                                                                               
LR50     DS    0H                  CKING FOR INDIVIDUAL FILTER(S)               
*                                                                               
         L     R0,AIO2             TO BUILD LISTING DATA                        
         LHI   R1,F_SMAXQ*L_DLNQ   LENGTH OF LISTING DATA TABLE                 
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   F_ESTAT+L'F_ESTAT,0                                              
         BE    LR52                                                             
         SR    RE,RE                                                            
         IC    RE,F_ESTAT+L'F_ESTAT                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),04           LENGTH OF TITLE AND DASHES                   
         MVC   01(04,RF),=C'Stat'                                               
         MVI   L_DLNQ/2+00(RF),04  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_ESTAT,C' '        FLT KEYWORD ONLY?                            
         BE    LR51P                                                            
         CLI   F_ESTAT,C'*'        WILDCARD?                                    
         BNE   LR51H                                                            
         OC    PESTSTAT,PESTSTAT   ANYTHING IN EST STAT?                        
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR51P                                                            
LR51H    CLI   F_ESTAT,C'S'        SOFT?                                        
         BNE   LR51K                                                            
         CLI   PESTSTAT,C'1'       MATCH THAT OF REC?                           
         BNE   LR30_SEQ                                                         
         B     LR51P                                                            
LR51K    CLI   F_ESTAT,C'L'        PERM LOCKOUT?                                
         BNE   LR30_SEQ                                                         
         CLI   PESTSTAT,C'2'       MATCH THAT OF REC?                           
         BNE   LR30_SEQ                                                         
LR51P    SR    RE,RE                                                            
         IC    RE,F_ESTAT+L'F_ESTAT                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),04                                               
         CLI   PESTSTAT,C'1'       SOFT?                                        
         BNE   *+14                                                             
         MVC   L_DLNQ/2+01(04,RF),=C'SOFT'                                      
         B     LR52                                                             
         CLI   PESTSTAT,C'2'       PERM LOCKOUT?                                
         BNE   *+10                                                             
         MVC   L_DLNQ/2+01(04,RF),=C'LOCK'                                      
*                                                                               
LR52     CLI   F_ESTART+L'F_ESTART,0                                            
         BE    LR53                                                             
         SR    RE,RE                                                            
         IC    RE,F_ESTART+L'F_ESTART                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),08           LENGTH OF TITLE AND DASHES                   
         MVC   01(08,RF),=C'StrtDate'                                           
         MVI   L_DLNQ/2+00(RF),08  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_ESTART,C' '       FLT KEYWORD ONLY?                            
         BE    LR52P                                                            
         CLI   F_ESTART,C'*'       WILDCARD?                                    
         BNE   LR52H                                                            
         OC    PESTST,PESTST       ANYTHING IN EST START DATE?                  
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR52P                                                            
LR52H    CLC   F_ESTART,PESTST     MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR52P    GOTO1 DATCON,DMCB,(0,PESTST),(10,WORK)                                 
         SR    RE,RE                                                            
         IC    RE,F_ESTART+L'F_ESTART                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),08                                               
         MVC   L_DLNQ/2+01(08,RF),WORK                                          
*                                                                               
LR53     CLI   F_EEND+L'F_EEND,0                                                
         BE    LR54                                                             
         SR    RE,RE                                                            
         IC    RE,F_EEND+L'F_EEND                                               
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),08           LENGTH OF TITLE AND DASHES                   
         MVC   01(07,RF),=C'EndDate'                                            
         MVI   L_DLNQ/2+00(RF),08  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_EEND,C' '         FLT KEYWORD ONLY?                            
         BE    LR53P                                                            
         CLI   F_EEND,C'*'         WILDCARD?                                    
         BNE   LR53H                                                            
         OC    PESTEND,PESTEND     ANYTHING IN EST END DATE?                    
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR53P                                                            
LR53H    CLC   F_EEND,PESTEND      MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR53P    GOTO1 DATCON,DMCB,(0,PESTEND),(10,WORK)                                
         SR    RE,RE                                                            
         IC    RE,F_EEND+L'F_EEND                                               
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),08                                               
         MVC   L_DLNQ/2+01(08,RF),WORK                                          
*                                                                               
LR54     CLI   F_ADNUM+L'F_ADNUM,0                                              
         BE    LR55                                                             
         SR    RE,RE                                                            
         IC    RE,F_ADNUM+L'F_ADNUM                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),05           LENGTH OF TITLE AND DASHES                   
         MVC   01(05,RF),=C'AdNum'                                              
         MVI   L_DLNQ/2+00(RF),05  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_ADNUM,C' '        FLT KEYWORD ONLY?                            
         BE    LR54P                                                            
         CLI   F_ADNUM,C'*'        WILDCARD?                                    
         BNE   LR54H                                                            
         OC    PESTJOB,PESTJOB     ANYTHING IN AD NUMBER?                       
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR54P                                                            
LR54H    CLC   F_ADNUM,PESTJOB     MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR54P    SR    RE,RE                                                            
         IC    RE,F_ADNUM+L'F_ADNUM                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PESTJOB                                        
         MVC   L_DLNQ/2+01(L'PESTJOB,RF),PESTJOB                                
*                                                                               
LR55     CLI   F_FILTRS+L'F_FILTRS,0                                            
         BE    LR56                                                             
         OC    F_FILTRS,F_FILTRS                                                
         BZ    LR56                MUST BE DOING POSITIONAL FILTERING           
         SR    RE,RE                                                            
         IC    RE,F_FILTRS+L'F_FILTRS                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),07           LENGTH OF TITLE AND DASHES                   
         MVC   01(07,RF),=C'Filters'                                            
         MVI   L_DLNQ/2+00(RF),07  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_FILTRS,C' '       FLT KEYWORD ONLY?                            
         BE    LR55P                                                            
         CLI   F_FILTRS,C'*'       WILDCARD?                                    
         BNE   LR55H                                                            
         OC    PESTGRPS,PESTGRPS   ANYTHING IN EST FILTERS (GROUPS)?            
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR55P                                                            
LR55H    CLC   F_FILTRS,PESTGRPS   MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR55P    SR    RE,RE                                                            
         IC    RE,F_FILTRS+L'F_FILTRS                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PESTGRPS                                       
         MVC   L_DLNQ/2+01(L'PESTGRPS,RF),PESTGRPS                              
*                                                                               
LR56     CLI   F_RTYPE+L'F_RTYPE,0                                              
         BE    LR57                                                             
         SR    RE,RE                                                            
         IC    RE,F_RTYPE+L'F_RTYPE                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),05           LENGTH OF TITLE AND DASHES                   
         MVC   01(05,RF),=C'RType'                                              
         MVI   L_DLNQ/2+00(RF),05  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_RTYPE,C' '        FLT KEYWORD ONLY?                            
         BE    LR56P                                                            
         CLI   F_RTYPE,C'*'        WILDCARD?                                    
         BNE   LR56H                                                            
         OC    PESTRTYP,PESTRTYP   ANYTHING IN RATE TYPE?                       
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR56P                                                            
LR56H    CLC   F_RTYPE,PESTRTYP    MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR56P    SR    RE,RE                                                            
         IC    RE,F_RTYPE+L'F_RTYPE                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PESTRTYP                                       
         MVC   L_DLNQ/2+01(L'PESTRTYP,RF),PESTRTYP                              
*                                                                               
LR57     CLI   F_REVNUM+L'F_REVNUM,0                                            
         BE    LR58                                                             
         SR    RE,RE                                                            
         IC    RE,F_REVNUM+L'F_REVNUM                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),06           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'RevNum'                                             
         MVI   L_DLNQ/2+00(RF),06  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_REVNUM,C' '       FLT KEYWORD ONLY?                            
         BE    LR57P                                                            
         CLI   F_REVNUM,C'*'       WILDCARD?                                    
         BNE   LR57H                                                            
         OC    PESTREVN,PESTREVN   ANYTHING IN REVISION NUMBER?                 
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR57P                                                            
LR57H    CLC   F_REVNUM,PESTREVN   MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR57P    SR    RE,RE                                                            
         IC    RE,F_REVNUM+L'F_REVNUM                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PESTREVN                                       
         MVC   L_DLNQ/2+01(L'PESTREVN,RF),PESTREVN                              
*                                                                               
LR58     CLI   F_STAT+L'F_STAT,0                                                
         BE    LR59                                                             
         SR    RE,RE                                                            
         IC    RE,F_STAT+L'F_STAT                                               
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),06           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'Status'                                             
         MVI   L_DLNQ/2+00(RF),06  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_STAT_W,C'L'       WANT LIVE?                                   
         BE    LR58D                                                            
         OC    F_STAT,F_STAT       FLT KEYWORD ONLY?                            
         BZ    LR58P                                                            
         B     LR58H                                                            
LR58D    TM    PESTTEST,X'80'      TEST EST BIT IS ON?                          
         BNZ   LR30_SEQ            NOTHING, SKIP IT                             
LR58F    CLI   F_STAT,0            FILTERING ON LOCKOUT STATUS?                 
         BE    LR58P                                                            
         CLC   F_STAT(1),PESTSTAT                                               
         BNE   LR30_SEQ                                                         
         B     LR58P                                                            
LR58H    CLI   F_STAT+1,0          FILTERING ONLY ON LOCKOUT STATUS?            
         BE    LR58F                                                            
         MVC   BYTE,PESTTEST                                                    
         TM    F_STAT+1,X'80'      WANT TEST?                                   
         BZ    *+8                                                              
         NI    BYTE,X'80'          ONLY TEST EST BIT IS ON                      
         TM    F_STAT+1,X'40'      WANT STEWARDSHIP?                            
         BZ    *+8                                                              
         NI    BYTE,X'40'          ONLY STEW EST BIT IS ON                      
         CLC   F_STAT+1(1),BYTE    MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
         TM    F_STAT+1,X'80'      WANT TEST?                                   
         BZ    *+12                                                             
         TM    PESTTEST,X'40'      STEWARDSHIP EST?                             
         BNZ   LR30_SEQ                                                         
         B     LR58F               NOW CK FOR LOCKOUT STATUS FILTER             
LR58P    SR    RE,RE                                                            
         IC    RE,F_STAT+L'F_STAT                                               
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),05                                               
         MVC   L_DLNQ/2+02(04,RF),=C'LIVE'                                      
         TM    PESTTEST,X'80'      TEST EST BIT IS ON?                          
         BZ    *+14                                                             
         MVI   L_DLNQ/2+00(RF),05                                               
         MVC   L_DLNQ/2+02(04,RF),=C'TEST'                                      
         TM    PESTTEST,X'40'      STEW EST BIT IS ON?                          
         BZ    *+14                                                             
         MVI   L_DLNQ/2+00(RF),05                                               
         MVC   L_DLNQ/2+02(04,RF),=C'STEW'                                      
         MVC   L_DLNQ/2+01(01,RF),PESTSTAT                                      
*                                                                               
LR59     CLI   F_SFH+L'F_SFH,0                                                  
         BE    LR60                                                             
         SR    RE,RE                                                            
         IC    RE,F_SFH+L'F_SFH                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),03           LENGTH OF TITLE AND DASHES                   
         MVC   01(03,RF),=C'SFH'                                                
         MVI   L_DLNQ/2+00(RF),03  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_SFH_W,C'*'        WILDCARD?                                    
         BE    LR59D                                                            
         CLI   F_SFH,0             FLT KEYWORD ONLY?                            
         BE    LR59P                                                            
         B     LR59H                                                            
LR59D    TM    PESTTEST,X'01'      SFH BIT IS ON?                               
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
         B     LR59P                                                            
LR59H    MVC   BYTE,PESTTEST                                                    
         NI    BYTE,X'01'          ONLY SFH BIT IS ON                           
         CLC   F_SFH,BYTE          MATCH THAT OF FLT?                           
         BNE   LR30_SEQ            NO, SKIP IT                                  
LR59P    SR    RE,RE                                                            
         IC    RE,F_SFH+L'F_SFH                                                 
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),01                                               
         MVI   L_DLNQ/2+01(RF),C'N'                                             
         TM    PESTTEST,X'01'      SFH BIT IS ON?                               
         BZ    *+8                                                              
         MVI   L_DLNQ/2+01(RF),C'Y'                                             
*                                                                               
LR60     CLI   F_PORDER+L'F_PORDER,0                                            
         BE    LR61                                                             
         SR    RE,RE                                                            
         IC    RE,F_PORDER+L'F_PORDER                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),11           LENGTH OF TITLE AND DASHES                   
         MVC   01(11,RF),=C'PurchsOrder'                                        
         MVI   L_DLNQ/2+00(RF),11  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_PORDER,C' '       FLT KEYWORD ONLY?                            
         BE    LR60P                                                            
         CLI   F_PORDER,C'*'       WILDCARD?                                    
         BNE   LR61                                                             
         OC    PESTPURO,PESTPURO   ANYTHING IN PURCHASE ORDER?                  
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
LR60P    XC    ELEM,ELEM                                                        
         OC    PESTPURO,PESTPURO                                                
         BZ    LR60T                                                            
         EDIT  (B4,PESTPURO),(11,ELEM),2,ALIGN=RIGHT,ZERO=BLANK                 
LR60T    SR    RE,RE                                                            
         IC    RE,F_PORDER+L'F_PORDER                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),11                                               
         MVC   L_DLNQ/2+01(11,RF),ELEM                                          
*                                                                               
LR61     CLI   F_COST2+L'F_COST2,0                                              
         BE    LR62                                                             
         SR    RE,RE                                                            
         IC    RE,F_COST2+L'F_COST2                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),08           LENGTH OF TITLE AND DASHES                   
         MVC   01(05,RF),=C'Cost2'                                              
         MVI   L_DLNQ/2+00(RF),08  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_COST2,C' '        FLT KEYWORD ONLY?                            
         BE    LR61P                                                            
         CLI   F_COST2,C'*'        WILDCARD?                                    
         BNE   LR62                                                             
         OC    PESTCF,PESTCF       ANYTHING IN COST2 FACTOR?                    
         BZ    LR30_SEQ            NOTHING, SKIP IT                             
LR61P    XC    ELEM,ELEM                                                        
         OC    PESTCF,PESTCF       COST2 VALUE PRESENT?                         
         BZ    LR61T                                                            
         EDIT  (P5,PESTCF),(08,ELEM),6,ALIGN=RIGHT,FILL=0,ZERO=NOBLANK          
LR61T    SR    RE,RE                                                            
         IC    RE,F_COST2+L'F_COST2                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),08                                               
         MVC   L_DLNQ/2+01(11,RF),ELEM                                          
*                                                                               
LR62     CLI   F_IDESK+L'F_IDESK,0                                              
         BE    LR63                                                             
         SR    RE,RE                                                            
         IC    RE,F_IDESK+L'F_IDESK                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),05           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'Prisma'                                             
         MVI   L_DLNQ/2+00(RF),6   DEFAULT LENGTH OF FLT DATA                   
         CLI   F_IDK_W,C'*'        WILDCARD?                                    
         JE    LR62D                                                            
         CLI   F_IDESK,0           FLT KEYWORD ONLY?                            
         JE    LR62P                                                            
         J     LR62H                                                            
LR62D    TM    PESTTEST,X'20'      iDesk Estimate?                              
         JZ    LR30_SEQ            NOTHING, SKIP IT                             
         J     LR62P                                                            
LR62H    MVC   BYTE,PESTTEST                                                    
         NI    BYTE,X'20'                                                       
         CLC   F_IDESK,BYTE        MATCH THAT OF FLT?                           
         JNE   LR30_SEQ            NO, SKIP IT                                  
LR62P    SR    RE,RE                                                            
         IC    RE,F_IDESK+L'F_IDESK                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),01                                               
         MVI   L_DLNQ/2+01(RF),C' '                                             
         TM    PESTTEST,X'20'      iDesk Estimate?                              
         JZ    *+8                                                              
         MVI   L_DLNQ/2+01(RF),C'Y'                                             
*                                                                               
LR63     BRAS  RE,L_USRFLD         DO USER DEFINITION FLDS LIST                 
         BNE   LR30_SEQ            FILTER FAILED, SKIP THIS ONE                 
*                                                                               
         BRAS  RE,L_ESTREP         DO ESTIMATE SPECIAL REP FLD LIST             
         BNE   LR30_SEQ            FILTER FAILED, SKIP THIS ONE                 
*                                                                               
         BRAS  RE,L_RETSCH         DO ESTIMATE RETAIL BILLING SCHEME            
         BNE   LR30_SEQ            FILTER FAILED, SKIP THIS ONE                 
*                                                                               
         BRAS  RE,L_EBILLF         DO ESTIMATE BILL FORMULA                     
         BNE   LR30_SEQ            FILTER FAILED, SKIP THIS ONE                 
*                                                                               
         BRAS  RE,L_POSFLT         DO POSITIONAL FILTER SEARCH                  
         BNE   LR30_SEQ            FILTER FAILED, SKIP THIS ONE                 
*                                                                               
         BRAS  RE,L_ACTLZD         DO ACTUALIZATION DATE FILTER SEARCH          
         BNE   LR30_SEQ            FILTER FAILED, SKIP THIS ONE                 
*                                                                               
LR75     DS    0H                  FOR FUTURE FILTER FLDS                       
*                                                                               
         LA    R2,LSFSTART         POINT TO LISTING AREA                        
         BRAS  RE,LR_DFLTF         DISPLAY FLT FLDS                             
*                                                                               
LR90     CLC   SVCLTLSC,KEY+04     CLT CODE ALREADY DISPLAYED?                  
         BE    *+16                YES, NO NEED TO DISPLAY IT AGAIN             
         MVC   SVCLTLSC,KEY+04     SET CLT CODE ON LIST                         
         MVC   LSCLTC,KEY+04                                                    
*                                                                               
         CLC   SVPRDLSC,KEY+07     PRD CODE ALREADY DISPLAYED?                  
         BE    *+16                YES, NO NEED TO DISPLAY IT AGAIN             
         MVC   SVPRDLSC,KEY+07     SET PRD CODE ON LIST                         
         MVC   LSPRDC,KEY+07                                                    
*                                                                               
         CLI   LISTNUM,0           1ST LIST LINE?                               
         BNE   LR94_LM                                                          
         MVC   LSCLTC,KEY+04       1ST LIST LINE NEED TO HAVE CLT CODE          
         MVC   LSPRDC,KEY+07       1ST LIST LINE NEED TO HAVE PRD CODE          
*                                                                               
LR94_LM  GOTO1 LISTMON             DISPLAY LINE                                 
         DROP  R6                                                               
*                                                                               
         GOTOR SET_LSER            SET ERROR FOR LIST                           
*                                                                               
         CLI   MODE,PRINTREP       REPORTING USING LIST ACTION?                 
         BNE   LR98                                                             
         MVC   H5+00(07),=C'Filters'                                            
         MVC   H5+08(L'LSTFLT),LSTFLT                                           
         MVC   H7+05(L'LSTTL2),LSTTL2                                           
         MVC   H8+05(L'LSTUL2),LSTUL2                                           
         MVC   H7+05+L'LSTTL2+02(L'LSTTL3),LSTTL3                               
         MVC   H8+05+L'LSTUL2+02(L'LSTUL3),LSTUL3                               
         MVC   H7+05+L'LSTTL2+02+L'LSTTL3+02(L'LSTTL4),LSTTL4                   
         MVC   H8+05+L'LSTUL2+02+L'LSTUL3+02(L'LSTUL4),LSTUL4                   
         XC    P1,P1                                                            
         MVC   P1+05(L'LISTAR),LISTAR                                           
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
         TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BZ    LR30_SEQ                                                         
         XC    P1,P1                                                            
         MVC   P1(L'CONHEAD),CONHEAD                                            
         GOTO1 SPOOL,DMCB,SPOOLD   PRINT LINE                                   
*                                                                               
LR98     TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BNZ   LRX                                                              
*                                                                               
         B     LR30_SEQ            GET NEXT RECORD                              
*                                                                               
LRX      DS    0H                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* SET ERROR FOR LIST SCREEN, ERROR WILL BE RETURNED IN CONHEAD                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
SET_LSER NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         TM    CNTLSW,C_MAXIOQ     MAX I/O REACHED?                             
         BZ    SLSER50                                                          
         LHI   R2,64               TOO MANNY RECORDS                            
         BRAS  RE,GET_ITXT                                                      
         J     EXIT                                                             
*                                                                               
SLSER50  DS    0H                  FOR FUTURE ERRORS                            
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* PUT OUT USER DEFINITION FLDS ON LIST LINE IF FLT KEYWORDS ARE PRESENT         
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
L_USRFLD NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         BRAS  RE,FMTUDEFD         RESULT WILL BE IN ELEM                       
*                                                                               
         CLI   F_USER1+L'F_USER1,0                                              
         BE    L_UFS40                                                          
         SR    RE,RE                                                            
         IC    RE,F_USER1+L'F_USER1                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),32           LENGTH OF TITLE AND DASHES                   
         MVC   01(16,RF),=C'User Def Field 1'                                   
         MVI   L_DLNQ/2+00(RF),32  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         CLI   F_USER1,C' '        FLT KEYWORD ONLY?                            
         BE    L_UFS20                                                          
         CLI   F_USER1,C'*'        WILDCARD?                                    
         BNE   L_UFS40                                                          
         OC    ELEM(32),ELEM                                                    
         JZ    SETCCNEQ            NOTHING, SKIP THIS ONE                       
L_UFS20  SR    RE,RE                                                            
         IC    RE,F_USER1+L'F_USER1                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),32                                               
         MVC   L_DLNQ/2+01(32,RF),ELEM                                          
*                                                                               
L_UFS40  CLI   F_USER2+L'F_USER2,0                                              
         BE    L_UFS60                                                          
         SR    RE,RE                                                            
         IC    RE,F_USER2+L'F_USER2                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),16           LENGTH OF TITLE AND DASHES                   
         MVC   01(16,RF),=C'User Def Field 2'                                   
         MVI   L_DLNQ/2+00(RF),16  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         CLI   F_USER2,C' '        FLT KEYWORD ONLY?                            
         BE    L_UFS42                                                          
         CLI   F_USER2,C'*'        WILDCARD?                                    
         BNE   L_UFS60                                                          
         OC    ELEM+32(16),ELEM+32                                              
         JZ    SETCCNEQ            NOTHING, SKIP THIS ONE                       
L_UFS42  SR    RE,RE                                                            
         IC    RE,F_USER2+L'F_USER2                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),16                                               
         MVC   L_DLNQ/2+01(16,RF),ELEM+32                                       
*                                                                               
L_UFS60  DS    0H                  FOR FUTURE USES                              
*                                                                               
         J     SETCCEQ             EXIT WITH CC EQUAL                           
*                                                                               
FMTUDEFD DS    0H                                                               
         ST    RE,FULL                                                          
         XC    ELEM,ELEM                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,X'08'        EST USER DEF FLDS ELEM CODE                  
         BRAS  RE,GETEL                                                         
         JNE   F_UFSX                                                           
         MVC   ELEM(50-2),2(R6)    USER1 AND USER2                              
F_UFSX   L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
L_ESTREP NTR1  BASE=*,LABEL=*      LISTING ESTIMATE SPECIAL REP                 
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'         FIRST EST ELEM CODE                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTELEM,R6                                                      
*                                                                               
         CLI   F_ESTREP+L'F_ESTREP,0                                            
         JE    SETCCEQ                                                          
         SR    RE,RE                                                            
         IC    RE,F_ESTREP+L'F_ESTREP                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),04           LENGTH OF TITLE AND DASHES                   
         MVC   01(04,RF),=C'SRep'                                               
         MVI   L_DLNQ/2+00(RF),05  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_ESTREP,C' '       FLT KEYWORD ONLY?                            
         BE    L_REP30                                                          
         CLI   F_ESTREP,C'*'       WILDCARD?                                    
         BNE   L_REP20                                                          
         OC    PESTREP,PESTREP     ANYTHING IN SPECIAL REP?                     
         JZ    SETCCNEQ            NOTHING, SKIP IT                             
         B     L_REP30                                                          
L_REP20  CLC   F_ESTREP,PESTREP    MATCH THAT OF FLT?                           
         JNE   SETCCNEQ            NO, SKIP IT                                  
L_REP30  SR    RE,RE                                                            
         IC    RE,F_ESTREP+L'F_ESTREP                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PESTREP                                        
         MVC   L_DLNQ/2+01(L'PESTREP,RF),PESTREP                                
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
L_RETSCH NTR1  BASE=*,LABEL=*      LISTING RETAIL SCHEME                        
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'         FIRST EST ELEM CODE                         
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTELEM,R6                                                      
*                                                                               
         CLI   F_RETSCH+L'F_RETSCH,0                                            
         JE    SETCCEQ                                                          
         SR    RE,RE                                                            
         IC    RE,F_RETSCH+L'F_RETSCH                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),06           LENGTH OF TITLE AND DASHES                   
         MVC   01(06,RF),=C'RetSch'                                             
         MVI   L_DLNQ/2+00(RF),02  DEFAULT LENGTH OF FLT DATA                   
         CLI   F_RETSCH,C' '       FLT KEYWORD ONLY?                            
         BE    L_RSC30                                                          
         CLI   F_RETSCH,C'*'       WILDCARD?                                    
         BNE   L_RSC20                                                          
         OC    PESTRSCH,PESTRSCH   ANYTHING IN RETAIL BILLING SCHEME?           
         JZ    SETCCNEQ            NOTHING, SKIP IT                             
         B     L_RSC30                                                          
L_RSC20  CLC   F_RETSCH,PESTRSCH   MATCH THAT OF FLT?                           
         JNE   SETCCNEQ            NO, SKIP IT                                  
L_RSC30  SR    RE,RE                                                            
         IC    RE,F_RETSCH+L'F_RETSCH                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PESTRSCH                                       
         MVC   L_DLNQ/2+01(L'PESTRSCH,RF),PESTRSCH                              
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
L_EBILLF NTR1  BASE=*,LABEL=*      LISTING ESTIMATE BILL FORMULA                
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        FIRST EST ELEM CODE                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTELEM,R6                                                      
*                                                                               
         CLI   F_BILLF+L'F_BILLF,0                                              
         JE    SETCCEQ                                                          
         SR    RE,RE                                                            
         IC    RE,F_BILLF+L'F_BILLF                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),15           LENGTH OF TITLE AND DASHES                   
         MVC   01(12,RF),=C'Bill Formula'                                       
         MVI   L_DLNQ/2+00(RF),15  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         BRAS  RE,FMTBILLF         RESULT WILL BE IN ELEM                       
*                                                                               
         CLI   F_BILLF,C' '        FLT KEYWORD ONLY?                            
         BE    L_EBF10                                                          
         CLI   F_BILLF,C'*'        WILDCARD?                                    
         JNE   SETCCEQ                                                          
         CLC   ELEM,SPACES                                                      
         JE    SETCCNEQ            NOTHING, SKIP THIS ONE                       
L_EBF10  SR    RE,RE                                                            
         IC    RE,F_BILLF+L'F_BILLF                                             
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),15                                               
         MVC   L_DLNQ/2+01(15,RF),ELEM                                          
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
L_POSFLT NTR1  BASE=*,LABEL=*      POSITIONAL FILTER SEARCH                     
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'07'        FIRST EST ELEM CODE                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING PESTELEM,R6                                                      
*                                                                               
         CLI   F_FILTRS+L'F_FILTRS,0                                            
         JE    SETCCEQ                                                          
*                                                                               
         CLI   F_FPOS,0            NOT POSITIONAL FILTER SEARCH?                
         JE    SETCCEQ                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,F_FILTRS+L'F_FILTRS                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),07           LENGTH OF TITLE AND DASHES                   
         MVC   01(07,RF),=C'Filters'                                            
         MVI   L_DLNQ/2+00(RF),07  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         SR    RE,RE               POSITION MUST BE 1,2 OR 3                    
         IC    RE,F_FPOS                                                        
         CHI   RE,0                                                             
         BH    *+6                                                              
         DC    H'0'                                                             
         CHI   RE,3                                                             
         BNH   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BCTR  RE,0                                                             
         LA    RF,PESTGRPS                                                      
         AR    RF,RE               POINT TO POSITION TO BE FILTERED             
         CLI   F_FCHAR,C'-'        NEGATIVE FILTERING?                          
         BNE    L_POF20                                                         
         CLC   0(1,RF),F_FCHAR+1                                                
         JE    SETCCNEQ            FAILED SEARCH CRITERIA                       
         B     L_POF50                                                          
*                                                                               
L_POF20  CLC   0(1,RF),F_FCHAR                                                  
         JNE   SETCCNEQ            FAILED SEARCH CRITERIA                       
*                                                                               
L_POF50  SR    RE,RE                                                            
         IC    RE,F_FILTRS+L'F_FILTRS                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),L'PESTGRPS                                       
         MVC   L_DLNQ/2+01(15,RF),PESTGRPS                                      
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
L_ACTLZD NTR1  BASE=*,LABEL=*      ACTUALIZATION DATE SEARCH                    
*                                                                               
         CLI   F_ACTLZD+L'F_ACTLZD,0                                            
         JE    SETCCEQ                                                          
*                                                                               
         SR    RE,RE                                                            
         IC    RE,F_ACTLZD+L'F_ACTLZD                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   00(RF),07           LENGTH OF TITLE AND DASHES                   
         MVC   01(07,RF),=C'ActDate'                                            
         MVI   L_DLNQ/2+00(RF),07  DEFAULT LENGTH OF FLT DATA                   
*                                                                               
         L     R6,AIO                                                           
         USING PESTACTD,R6                                                      
         MVI   ELCODE,PEACTECQ     ESTIMATE ACTUALIZATION ELEMENT               
*                                                                               
         CLI   F_ACTLZD,C'*'       WILDCARD?                                    
         BNE   L_ACZ20                                                          
         OC    F_ACTLZD+1(L'F_ACTLZD-1),F_ACTLZD+1                              
         BNZ   L_ACZ20                                                          
         BRAS  RE,GETEL                                                         
         JNE   SETCCNEQ                                                         
         OC    PEACTDAT,PEACTDAT   HAVE ACTUALIZATION MONTH AND YEAR?           
         JZ    SETCCNEQ                                                         
         B     L_ACZ50                                                          
*                                                                               
L_ACZ20  BRAS  RE,GETEL                                                         
         BE    L_ACZ30                                                          
         OC    F_ACTLZD,F_ACTLZD   ACTUALIZATION COLUMN ONLY?                   
         JZ    SETCCEQ                                                          
         J     SETCCNEQ                                                         
*                                                                               
L_ACZ30  OC    F_ACTSTR,F_ACTSTR   ANYTHING IN START DATE FILTER?               
         BZ    *+14                                                             
         CLC   PEACTDAT,F_ACTSTR   ACTUALIZATION DATE > START DATE?             
         JL    SETCCNEQ                                                         
         OC    F_ACTEND,F_ACTEND   ANYTHING IN END DATE FILTER?                 
         BZ    *+14                                                             
         CLC   PEACTDAT,F_ACTEND   ACTUALIZATION DATE < START DATE?             
         JH    SETCCNEQ                                                         
*                                                                               
L_ACZ50  XC    DUB,DUB                                                          
         MVC   DUB(L'PEACTDAT),PEACTDAT                                         
         GOTO1 DATCON,DMCB,(3,DUB),(6,WORK)                                     
         SR    RE,RE                                                            
         IC    RE,F_ACTLZD+L'F_ACTLZD                                           
         MHI   RE,L_DLNQ           DISPLACEMENT TO ENTRY IN TABLE               
         L     RF,AIO2                                                          
         AR    RF,RE               POINT TO LISTING DATA ENTRY                  
         MVI   L_DLNQ/2+00(RF),6                                                
         MVC   L_DLNQ/2+01(15,RF),WORK                                          
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB,R6                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* FORMAT BILL FORMULA                                                           
* ELEM -  SPACES (IF NO FORMULA IS PRESENT)                                     
* ELEM -  CL15 FORMULA                                                          
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
FMTBILLF NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   ELEM,SPACES                                                      
*                                                                               
         L     R6,AIO              AIO STILL POINTS TO PRD REC                  
         MVI   ELCODE,X'07'        MAIN EST ELEM CODE                           
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                HAS TO BE THERE                              
         USING PESTELEM,R6                                                      
*                                                                               
         LA    R4,PESTBILP                                                      
         USING BILPROF,R4                                                       
         CLI   BILCMSW,C'C'        SEE IF COMMISSION ONLY                       
         BNH   *+10                                                             
         MVC   ELEM(1),BILCMSW                                                  
         LA    R3,ELEM+1                                                        
         LA    R5,BILBASA                                                       
         BAS   RE,FMTBAS                                                        
*                                                                               
         LA    R3,ELEM+10                                                       
         OC    BILADJ,BILADJ                                                    
         BNZ   FMTBF20                                                          
         CLI   BILBASA,0                                                        
         BE    FMTBF40                                                          
         MVC   ELEM+6(2),=C'+0'                                                 
         B     FMTBF40                                                          
*                                                                               
FMTBF20  LA    R3,ELEM+7                                                        
*                                                                               
         EDIT  (B3,BILADJ),(8,0(R3)),4,ALIGN=LEFT,DROP=2                        
         MVI   ELEM+6,C'+'                                                      
         TM    BILADJ,X'80'                                                     
         BZ    *+8                                                              
         MVI   ELEM+6,C'-'                                                      
         LA    R3,ELEM+14                                                       
         CLI   0(R3),C' '                                                       
         BH    FMTBF30                                                          
         BCT   R3,*-8                                                           
*                                                                               
FMTBF30  LA    R3,2(R3)                                                         
FMTBF40  LA    R5,BILBASB                                                       
         BAS   RE,FMTBAS                                                        
*                                                                               
         J     EXIT                                                             
*                                                                               
FMTBAS   ST    RE,FULL                                                          
         LA    RE,BASLST                                                        
         LA    RF,BASLSTN                                                       
FB2      CLC   0(1,R5),0(RE)                                                    
         BE    FB3                                                              
         LA    RE,5(RE)                                                         
         BCT   RF,FB2                                                           
         B     FBX                                                              
FB3      MVC   0(4,R3),1(RE)                                                    
FBX      L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
BASLST   DS    0C                                                               
         DC    X'01',C'G   '                                                    
         DC    X'02',C'N   '                                                    
         DC    X'05',C'G-CD'                                                    
         DC    X'06',C'N-CD'                                                    
         DC    X'08',C'AC  '                                                    
BASLSTN  EQU   (*-BASLST)/5        NUMBER IN LIST                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R6,R4                                                         
         EJECT                                                                  
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKFLTFLD NTR1  BASE=*,LABEL=*      CKING FOR FILTER KEYWORD(S)                  
*                                                                               
         LA    R3,F_SMAXQ          MAX NUM OF SCANNER ENTRIES                   
*                                                                               
         GOTO1 SCANNER,DMCB,(20,(R2)),((R3),AIO3)                               
         CLI   DMCB+4,0                                                         
         BE    CKFFD_E2            SCANNER RETURNED ERROR                       
         XC    HALF,HALF                                                        
         MVC   HALF+1(1),DMCB+4    NUMBER OF ENTRIES                            
         LA    R5,1                LOOP COUNTER                                 
*                                                                               
         L     R4,AIO3                                                          
         USING SCANBLKD,R4                                                      
*                                                                               
CKFFD20  CH    R5,HALF             ALL SCANNER FLDS PROCESSED?                  
         JH    SETCCEQ             YES, DONE (SET CC TO EQ)                     
*                                                                               
*KFFD30  CLC   =C'STAT',SC1STFLD                                                
*        BNE   CKFFD32                                                          
*        CLI   SECESTA,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
*        BE    CKFFD_E4                                                         
*        CLI   SC1STLEN,4                                                       
*        BNE   CKFFD_E2                                                         
*        CLI   SC2NDLEN,4          SOFT, LOCK?                                  
*        BH    CKFFD_E2                                                         
*        MVC   F_ESTAT,SC2NDFLD    EST STATUS                                   
*        CLI   F_ESTAT+L'F_ESTAT,0                                              
*        BNE   CKFFD_E1            ALREADY ENTERED                              
*        STC   R5,F_ESTAT+L'F_ESTAT                                             
*        B     CKFFD90                                                          
*                                                                               
CKFFD32  CLC   =C'START',SC1STFLD                                               
         BNE   CKFFD34                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0                                                       
         BH    *+14                                                             
         MVC   F_ESTART,SC2NDFLD   EST START DATE, FLT ONLY                     
         B     CKFFD32U                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_ESTART,C'*'                                                    
         B     CKFFD32U                                                         
         GOTO1 PERVAL,DMCB,(SC2NDLEN,SC2NDFLD),(X'40',ELEM)                     
         CLI   DMCB+4,X'01'                                                     
         JE    INVDTERR                                                         
         LA    RE,ELEM                                                          
         USING PERVALD,RE                                                       
         MVC   F_ESTART,PVALESTA   START DATE (YYMMDD)                          
         DROP  RE                                                               
CKFFD32U CLI   F_ESTART+L'F_ESTART,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_ESTART+L'F_ESTART                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD34  CLC   =C'END',SC1STFLD                                                 
         BNE   CKFFD36                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0                                                       
         BH    *+14                                                             
         MVC   F_EEND,SC2NDFLD     EST END DATE, FLT ONLY                       
         B     CKFFD34U                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_EEND,C'*'                                                      
         B     CKFFD34U                                                         
         GOTO1 PERVAL,DMCB,(SC2NDLEN,SC2NDFLD),(X'40',ELEM)                     
         CLI   DMCB+4,X'01'                                                     
         JE    INVDTERR                                                         
         LA    RE,ELEM                                                          
         USING PERVALD,RE                                                       
         MVC   F_EEND,PVALESTA     END DATE (YYMMDD)                            
         DROP  RE                                                               
CKFFD34U CLI   F_EEND+L'F_EEND,0                                                
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_EEND+L'F_EEND                                               
         B     CKFFD90                                                          
*                                                                               
CKFFD36  CLC   =C'ADNUM',SC1STFLD                                               
         BNE   CKFFD38                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_ADNUM                                               
         BH    CKFFD_E2                                                         
         MVC   F_ADNUM,SC2NDFLD    AD NUMBER (JOB CODE)                         
         CLI   F_ADNUM+L'F_ADNUM,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_ADNUM+L'F_ADNUM                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD38  CLC   =C'FILTER',SC1STFLD                                              
         BNE   CKFFD40                                                          
         CLI   SECEFLT,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,7          FILTER OR FILTERS ENTERED?                   
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_FILTRS                                              
         BH    CKFFD_E2                                                         
         MVC   F_FILTRS,SC2NDFLD   EST FILTERS (GROUPS)                         
         CLI   F_FILTRS+L'F_FILTRS,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_FILTRS+L'F_FILTRS                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD40  CLC   =C'RTYPE',SC1STFLD                                               
         BNE   CKFFD42                                                          
         CLI   SECERTY,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,5                                                       
         BH    CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_RTYPE                                               
         BH    CKFFD_E2                                                         
         MVC   F_RTYPE,SC2NDFLD    RATE TYPE                                    
         CLI   F_RTYPE+L'F_RTYPE,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_RTYPE+L'F_RTYPE                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD42  CLC   =C'REVNUM',SC1STFLD                                              
         BNE   CKFFD44                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_REVNUM                                              
         BH    CKFFD_E2                                                         
         MVC   F_REVNUM,SC2NDFLD   DEFAULT IS PADDED SPACES                     
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BE    CKFFD42M                                                         
         CLI   SC2NDLEN,0                                                       
         BE    CKFFD42M                                                         
         TM    SC2NDVAL,SCNUMQ     FLT VALUE IS NUMERIC?                        
         BZ    CKFFD_E2                                                         
         MVI   F_REVNUM,C'0'       INIT TO C'0'                                 
         MVC   F_REVNUM+1(L'F_REVNUM-1),F_REVNUM                                
         LA    R0,F_REVNUM+L'F_REVNUM                                           
         LA    RF,SC2NDFLD                                                      
         SR    R1,R1                                                            
         IC    R1,SC2NDLEN                                                      
         BRAS  RE,RT_ALIGN                                                      
CKFFD42M CLI   F_REVNUM+L'F_REVNUM,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_REVNUM+L'F_REVNUM                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD44  CLC   =C'STATUS',SC1STFLD                                              
         BNE   CKFFD46                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD44U            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,5                                                       
         BH    CKFFD_E2                                                         
         LA    RE,SC2NDFLD                                                      
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BE    CKFFD44U            SAME AS FILTER ONLY                          
         CLI   SC2NDFLD,C'1'       REGULAR LOCKOUT?                             
         BE    *+12                                                             
         CLI   SC2NDFLD,C'2'       PERMANENT LOCKOUT?                           
         BNE   *+14                                                             
         MVC   F_STAT(1),SC2NDFLD                                               
         LA    RE,1(RE)                                                         
         CLI   0(RE),C'L'          LIVE OR L?                                   
         BNE   *+12                                                             
         MVI   F_STAT_W,C'L'                                                    
         B     CKFFD44U                                                         
         CLI   0(RE),C'T'          TEST OR T?                                   
         BNE   *+12                                                             
         OI    F_STAT+1,X'80'      TEST EST BIT                                 
         B     CKFFD44U                                                         
         CLI   0(RE),C'S'          STEW OR S?                                   
         BNE   *+12                                                             
         OI    F_STAT+1,X'40'                                                   
         B     CKFFD44U                                                         
         CLI   0(RE),C' '                                                       
         BH    CKFFD_E2            INVALID FILTER VALUE ENTERED                 
CKFFD44U CLI   F_STAT+L'F_STAT,0                                                
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_STAT+L'F_STAT                                               
         B     CKFFD90                                                          
*                                                                               
CKFFD46  CLC   =C'SFH',SC1STFLD                                                 
         BNE   CKFFD47                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD46U            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,3          YES OR Y?                                    
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_SFH_W,C'*'                                                     
         B     CKFFD46U                                                         
         CLI   SC2NDFLD,C'Y'       YES OR Y?                                    
         BNE   *+8                                                              
         OI    F_SFH,X'01'         SFH BIT                                      
CKFFD46U CLI   F_SFH+L'F_SFH,0                                                  
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_SFH+L'F_SFH                                                 
         B     CKFFD90                                                          
*                                                                               
CKFFD47  CLC   =C'PRISMA',SC1STFLD                                              
         BNE   CKFFD48                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0          JUST FILTER?                                 
         BNH   CKFFD47U            NOTE: NULL(S), MEANS FLT ONLY                
         CLI   SC2NDLEN,3          YES OR Y?                                    
         BH    CKFFD_E2                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_IDK_W,C'*'                                                     
         B     CKFFD47U                                                         
         CLI   SC2NDFLD,C'Y'       YES OR Y?                                    
         BNE   *+8                                                              
         OI    F_IDESK,X'20'       iDesk Estimate                               
CKFFD47U CLI   F_IDESK+L'F_IDESK,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_IDESK+L'F_IDESK                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD48  CLC   =C'PORDER',SC1STFLD                                              
         BNE   CKFFD50                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_PORDER                                              
         BH    CKFFD_E2                                                         
         MVC   F_PORDER,SPACES     INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD48U                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_PORDER,SC2NDFLD   PURCHASE ORDER FILTER                        
CKFFD48U CLI   F_PORDER+L'F_PORDER,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_PORDER+L'F_PORDER                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD50  CLC   =C'COST2',SC1STFLD                                               
         BNE   CKFFD52                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_COST2                                               
         BH    CKFFD_E2                                                         
         MVC   F_COST2,SPACES      INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD50U                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_COST2,SC2NDFLD    COST2 FACTOR FILTER                          
CKFFD50U CLI   F_COST2+L'F_COST2,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_COST2+L'F_COST2                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD52  CLC   =C'USER1',SC1STFLD                                               
         BNE   CKFFD53                                                          
         CLI   SECEUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_USER1                                               
         BH    CKFFD_E2                                                         
         MVC   F_USER1,SPACES      INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD52K                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_USER1,SC2NDFLD    USER DEFINITION FLD 1 FILTER                 
CKFFD52K CLI   F_USER1+L'F_USER1,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_USER1+L'F_USER1                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD53  CLC   =C'USER2',SC1STFLD                                               
         BNE   CKFFD54                                                          
         CLI   SECEUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_USER2                                               
         BH    CKFFD_E2                                                         
         MVC   F_USER2,SPACES      INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD53K                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_USER2,SC2NDFLD    USER DEFINITION FLD 2 FILTER                 
CKFFD53K CLI   F_USER2+L'F_USER2,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_USER2+L'F_USER2                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD54  CLC   =C'REP',SC1STFLD                                                 
         BNE   CKFFD55                                                          
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_ESTREP                                              
         BH    CKFFD_E2                                                         
         MVC   F_ESTREP,SC2NDFLD   DEFAULT IS PADDED SPACES                     
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BE    CKFFD54M                                                         
         CLI   SC2NDLEN,0                                                       
         BE    CKFFD54M                                                         
         TM    SC2NDVAL,SCNUMQ     FLT VALUE IS NUMERIC?                        
         BZ    CKFFD_E2                                                         
         MVI   F_ESTREP,C'0'       INIT TO C'0'                                 
         MVC   F_ESTREP+1(L'F_ESTREP-1),F_ESTREP                                
         LA    R0,F_ESTREP+L'F_ESTREP                                           
         LA    RF,SC2NDFLD                                                      
         SR    R1,R1                                                            
         IC    R1,SC2NDLEN                                                      
         BRAS  RE,RT_ALIGN                                                      
CKFFD54M CLI   F_ESTREP+L'F_ESTREP,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_ESTREP+L'F_ESTREP                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD55  CLC   =C'RETSCH',SC1STFLD                                              
         BNE   CKFFD56                                                          
         CLI   SC1STLEN,6                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_RETSCH                                              
         BH    CKFFD_E2                                                         
         MVC   F_RETSCH,SC2NDFLD   SPECIAL REP CODE                             
         CLI   F_RETSCH+L'F_RETSCH,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_RETSCH+L'F_RETSCH                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD56  CLC   =C'BILLF',SC1STFLD                                               
         BNE   CKFFD57                                                          
         CLI   SC1STLEN,5                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,L'F_BILLF                                               
         BH    CKFFD_E2                                                         
         MVC   F_BILLF,SPACES      INIT TO SPACES (KEYWORD ONLY)                
         CLI   SC2NDLEN,0          NO FILTER INPUT?                             
         BE    CKFFD56K                                                         
         CLI   SC2NDFLD,C'*'       ONLY ALLOWING WILDCARD                       
         BNE   CKFFD_E2                                                         
         MVC   F_BILLF,SC2NDFLD    BILL FORMULA FILTER                          
CKFFD56K CLI   F_BILLF+L'F_BILLF,0                                              
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_BILLF+L'F_BILLF                                             
         B     CKFFD90                                                          
*                                                                               
CKFFD57  CLI   SC1STLEN,1          POSITIONAL FILTERING?                        
         BNE   CKFFD58                                                          
         CLI   SC2NDLEN,2                                                       
         BH    CKFFD_E2                                                         
         CLI   SC1STFLD,C'1'       POSITION MUST 1,2 OR 3                       
         BE    CKFFD57F                                                         
         CLI   SC1STFLD,C'2'                                                    
         BE    CKFFD57F                                                         
         CLI   SC1STFLD,C'3'                                                    
         BNE   CKFFD_E2                                                         
CKFFD57F MVC   F_FPOS,SC1STNUM+3   GET ITS BINARY NUMERIC VALUE                 
         CLI   SC2NDLEN,1                                                       
         BNE   CKFFD57K                                                         
         CLI   SC2NDFLD,C'-'       NEED MORE INFO                               
         BE    CKFFD_E2                                                         
         B     CKFFD57M                                                         
CKFFD57K CLI   SC2NDLEN,2                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDFLD,C'-'                                                    
         BNE   CKFFD_E2            MUST BE NEGATIVE FILTERING                   
         CLI   SC2NDFLD+1,C'-'                                                  
         BE    CKFFD_E2            CANNOT HAVE -- AS NEGATIVE FILTERING         
CKFFD57M MVC   F_FCHAR,SC2NDFLD                                                 
         CLI   F_FILTRS+L'F_FILTRS,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_FILTRS+L'F_FILTRS                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD58  CLC   =C'ACT',SC1STFLD                                                 
         BNE   CKFFD59                                                          
         CLI   SECACTD,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    CKFFD_E4                                                         
         CLI   SC1STLEN,3                                                       
         BNE   CKFFD_E2                                                         
         CLI   SC2NDLEN,0                                                       
         BH    *+14                                                             
         XC    F_ACTLZD,F_ACTLZD   ACTUALIZATION COLUMN                         
         B     CKFFD58U                                                         
         CLI   SC2NDFLD,C'*'       WILDCARD?                                    
         BNE   *+12                                                             
         MVI   F_ACTLZD,C'*'                                                    
         B     CKFFD58U                                                         
         GOTO1 PERVAL,DMCB,(SC2NDLEN,SC2NDFLD),(0,ELEM)                         
         CLI   DMCB+4,X'00'                                                     
         JNE   INVDTERR                                                         
         LA    RE,ELEM                                                          
         USING PERVALD,RE                                                       
         MVC   F_ACTSTR,PVALBSTA                                                
         MVC   F_ACTEND,PVALBEND                                                
         DROP  RE                                                               
CKFFD58U CLI   F_ACTLZD+L'F_ACTLZD,0                                            
         BNE   CKFFD_E1            ALREADY ENTERED                              
         STC   R5,F_ACTLZD+L'F_ACTLZD                                           
         B     CKFFD90                                                          
*                                                                               
CKFFD59  DS    0H                  FOR FUTURE FILTER KEYWORDS                   
*                                                                               
         B     CKFFD_E2            KEYWORD IS NOT DEFINED                       
*                                                                               
CKFFD90  LA    R4,SCBLKLQ(R4)      POINT TO NEXT SCANNER BLK                    
         AHI   R5,1                LOOP COUNTER UP BY ONE                       
         B     CKFFD20             CK FOR NEXT SCANNER BLK                      
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
CKFFD_E1 XC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         J     DUPEDERR                                                         
*                                                                               
CKFFD_E2 LHI   R2,INVFUSGE                                                      
         B     CKF_GTXT                                                         
*                                                                               
CKFFD_E3 XC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         J     LONGERR                                                          
*                                                                               
CKFFD_E4 LHI   R2,FLTKWNTA                                                      
         B     CKF_GTXT                                                         
*                                                                               
CKF_GTXT XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     R3,FULL                                                          
         GOTOR GETTXT,DMCB+12,(R2),0,(C'E',DMCB),0,0,(R3)                       
         OI    CONHEADH+6,X'80'                                                 
         J     SETCCNEQ                                                         
*                                                                               
* RF => POINTS TO INPUT TO BE MOVED TO RIGHT ALIGNING FLD                       
* R0 => POINTS TO END OF RIGHT ALIGNING FLD                                     
* R1 => LENGTH OF INPUT FLD                                                     
*                                                                               
RT_ALIGN DS    0H                                                               
         ST    RE,FULL                                                          
         LR    RE,R0                                                            
         CHI   R1,0                                                             
         BNH   RT_ALNX                                                          
         SR    RE,R1               POINT TO BEGINNING OF ALIGNMENT              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RE),0(RF)       RIGHT ALIGNED                                
RT_ALNX  L     RE,FULL                                                          
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB,R4                                                            
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
INITIALZ NTR1  BASE=*,LABEL=*      INITIALIZE WORKING STORAGES                  
*                                                                               
         MVI   CHGRQFLD,0          Init change required field switch            
*                                                                               
         CLI   TRANSSW,C'Y'        TRANSFERRED INTO PROGRAM?                    
         BNE   INITI50                                                          
         CLI   PFAID,0             PF KEY PRESSED?                              
         BE    INITI50             NO                                           
*                                                                               
         OC    KEY(25),KEY         HAVE KEY?                                    
         BZ    INITI50                                                          
         LA    R3,KEY                                                           
         CLI   3(R3),X'07'         EST RECORD CODE?                             
         BNE   INITI50                                                          
         USING PESTKEY,R3                                                       
         LA    R2,ESTMEDH          MEDIA FLD ON MAINT SCR                       
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTMEDH          POINT TO MEDIA FLD ON LIST SCR               
         MVC   8(1,R2),PESTKMED                                                 
         MVI   5(R2),1             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ESTCLTH          CLIENT FLD ON MAINT SCR                      
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTCLTH          POINT TO CLIENT FLD ON LIST SCR              
         MVC   8(3,R2),PESTKCLT                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ESTPRDH          PRD FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTPRDH          POINT TO PRD FLD ON LIST SCR                 
         MVC   8(3,R2),PESTKPRD                                                 
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
*                                                                               
         LA    R2,ESTESTH          EST FLD ON MAINT SCR                         
         CLI   ACTNUM,ACTLIST      LIST?                                        
         BNE   *+8                                                              
         LA    R2,LSTESTH          POINT TO EST FLD ON LIST SCR                 
         EDIT  (B2,PESTKEST),(3,8(R2)),0,ALIGN=RIGHT,                  +        
               ZERO=NOBLANK,FILL=0                                              
         MVI   5(R2),3             INPUT LENGTH                                 
         OI    6(R2),X'80'         TRANSMIT                                     
         DROP  R3                                                               
*                                                                               
INITI50  OI    GENSTAT4,NODELLST   NO DELETE ON LIST                            
         MVI   ACTELOPT,C'N'       NO ACTIVITY ELEM WILL BE ADDED               
         MVI   NLISTS,14           14 LINES ON LIST SCREEN                      
         XC    IO_COUNT,IO_COUNT   I/O COUNTER                                  
         MVI   CNTLSW,0            CONTROL SWITCH                               
*                                                                               
         XC    SECVALS(SECVALSL),SECVALS                                        
         L     RF,ATWA                                                          
         OC    4(2,RF),4(RF)       TEST AGENCY ON NEW SECURITY                  
         BNZ   *+14                                                             
         OC    6(2,RF),6(RF)       TEST ANY LIMIT ACCESS                        
         BZ    INITI52X                                                         
*                                                                               
         LA    R2,SECFLDS          R2=A(FIELD SECURITY DISPLACEMENTS)           
         LHI   R0,SECFLDSN         R0=N'SECURITY FIELDS                         
INITI52E GOTOR SECRET,DMCB,('SECPFLDP',ASECBLK),1(R2)                           
         BE    INITI52H                                                         
         LA    RF,C'Y'             C'Y'=READ ONLY                               
         BH    *+8                                                              
         LA    RF,C'N'             C'N'=NO ACCESS                               
         SR    RE,RE                                                            
         IC    RE,0(R2)            RE=DISPLACEMENT TO SECURITY VALUE            
         LA    RE,SECVALS(RE)                                                   
         STC   RF,0(RE)            YES - DISPLAY AND CHANGE                     
INITI52H AHI   R2,L'SECFLDS        BUMP TO NEXT DISPLACEMENT                    
         BCT   R0,INITI52E         DO FOR NUMBER OF SECURITY FIELDS             
INITI52X DS    0H                                                               
*                                                                               
         CLI   PHSCREEN,X'A4'      EST LIST SCR?                                
         BNE   INITI51                                                          
         XC    LSTTL4+20+4+2(LTFFLDQ),LSTTL4+20+4+2                             
         XC    LSTUL4+20+4+2(LTFFLDQ),LSTUL4+20+4+2                             
         LA    RE,F_FLDLNQ                                                      
         CHI   RE,255                                                           
         BNH   *+6                                                              
         DC    H'0'                TOTAL FILTER FLDS CANNOT EXCEED 255!         
         OC    F_FLDS(F_FLDLNQ),F_FLDS                                          
         BNZ   INITI50U                                                         
T        USING LS_TTL4,LSTTL4                                                   
         MVC   T.LTESTAT(04),=C'Stat'                                           
         MVC   T.LTESTART(08),=C'StrtDate'                                      
         MVC   T.LTEEND(07),=C'EndDate'                                         
         MVC   T.LTADNUM(05),=C'AdNum'                                          
         DROP  T                                                                
U        USING LS_TTL4,LSTUL4                                                   
         MVC   U.LTESTAT(04),=C'----'                                           
         MVC   U.LTESTART(08),=C'--------'                                      
         MVC   U.LTEEND(08),=C'--------'                                        
         MVC   U.LTADNUM(06),=C'------'                                         
         DROP  U                                                                
INITI50U OI    LSTTL4H+6,X'80'                                                  
         OI    LSTUL4H+6,X'80'                                                  
*                                                                               
INITI51  CLI   PHSCREEN,X'A3'      EST MAINT SCREEN?                            
         BNE   INITI52                                                          
         OI    ESTENAMH+6,X'81'    CHG TO MODIFIED FLD (GAIN CONTROL)           
         XC    ESTBOTL+PF12POSQ(L'PF12TXT),ESTBOTL+PF12POSQ                     
         OI    ESTBOTLH+6,X'80'                                                 
*                                                                               
INITI52  CLI   ACTNUM,ACTSEL       SELECT FROM LIST?                            
         BNE   INITI58                                                          
         CLI   PHSCREEN,X'A3'      EST MAINT SCREEN?                            
         BE    *+6                                                              
         DC    H'0'                WRONG SCREEN!                                
         MVC   ESTBOTL+PF12POSQ(L'PF12TXT),PF12TXT                              
         OI    ESTBOTLH+6,X'80'                                                 
*                                                                               
INITI58  LA    RE,HEDSPECS         SET UP HEADHOOK AND SPECS                    
         ST    RE,SPECS                                                         
         LA    RE,HOOK                                                          
         ST    RE,HEADHOOK                                                      
         MVC   H2+15(10),MEDNM                                                  
         MVC   H2+10(01),QMED                                                   
*                                                                               
         CLI   ACTNUM,ACTLIST      ACTION IS LIST?                              
         BE    INITIX                                                           
         CLI   ACTNUM,ACTREP       ACTION IS LIST?                              
         BE    INITIX                                                           
         CLI   PHSCREEN,X'A3'      EST MAINT SCREEN?                            
         BE    *+6                                                              
         DC    H'0'                WRONG SCREEN!                                
*                                                                               
* IF FLD CONTROL NOT PRESENT, STILL NEED TO CK FOR HEX CONTROL                  
*                                                                               
         OC    SECVALS(SECVALSL),SECVALS                                        
         BZ    INITI66X                                                         
*                                                                               
         LA    RF,SECEFLT                                                       
         LA    R2,ESTFLTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECERTY                                                       
         LA    R2,ESTRTTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECESPR                                                       
         LA    R2,ESTSRTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECESTA                                                       
         LA    R2,ESTSTTLH                                                      
         BRAS  RE,PRCFLD                                                        
         CLI   SECESTA,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BNE   *+10                                                             
         XC    ESTSTT2,ESTSTT2     CLEAR "INFO" FIELD FOLLOWING STATUS          
         OI    ESTSTT2H+6,X'80'    XMIT                                         
*                                                                               
         LA    RF,SECERSC                                                       
         LA    R2,ESTRSTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECPCED          BILL ON PC EFFECTIVE DATE                    
         LA    R2,ESTPCETH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECACTD          EST ACTUALIZE DATE                           
         LA    R2,ESTACTTH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECEPON          EST PURCHASE ORDER #                         
         LA    R2,ESTPOTLH                                                      
         BRAS  RE,PRCFLD                                                        
*                                                                               
         LA    RF,SECEUD1                                                       
         LA    R2,ESTDEF1H                                                      
         BRAS  RE,PRCFLD                                                        
         XC    ESTDEF1,ESTDEF1                                                  
         CLI   SECEUD1,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    *+10                                                             
         MVC   ESTDEF1,SVE1USER    NEED TO DISP USER DEF 1 TITLE                
         OI    ESTDEF1H+6,X'80'                                                 
*                                                                               
         LA    RF,SECEUD2                                                       
         LA    R2,ESTDEF2H                                                      
         BRAS  RE,PRCFLD                                                        
         XC    ESTDEF2,ESTDEF2                                                  
         CLI   SECEUD2,C'N'        NOT AUTHORIZED (FIELD SECURITY)?             
         BE    *+10                                                             
         MVC   ESTDEF2,SVE2USER    NEED TO DISP USER DEF 2 TITLE                
         OI    ESTDEF2H+6,X'80'                                                 
         B     INITI74             FCON APPLIED, NO NEED TO CK HEX              
INITI66X DS    0H                                                               
*                                                                               
* NOTE: THERE'S NO AUTHORIZATION CKING FOR EST, BUT IT DOES IN CLT              
*                                                                               
         XC    ESTDEF1,ESTDEF1                                                  
         OI    ESTDSC1H+1,X'20'    DEFAULT IS PROTECTED                         
* * * *  L     RF,ATWA                                                          
* * * *  TM    12(RF),X'08'        AUTHORIZED?                                  
* * * *  BO    *+8                                                              
         NI    ESTDSC1H+1,X'FF'-X'20'                                           
         CLI   MODE,VALKEY         VALIDATE RECORD KEY?                         
         JE    INITI68X                                                         
         OC    SVE1USER,SVE1USER   EST USER DEF 1 FLD PRESENT?                  
         BZ    *+14                                                             
         MVC   ESTDEF1,SVE1USER                                                 
         B     *+8                                                              
         OI    ESTDSC1H+1,X'20'    PROTECT, SINCE DEF FLD IS NOT THERE          
INITI68X OI    ESTDSC1H+6,X'80'                                                 
         OI    ESTDEF1H+6,X'80'                                                 
*                                                                               
         XC    ESTDEF2,ESTDEF2                                                  
         OI    ESTDSC2H+1,X'20'    DEFAULT IS PROTECTED                         
* * * *  L     RF,ATWA                                                          
* * * *  TM    12(RF),X'08'        AUTHORIZED?                                  
* * * *  BO    *+8                                                              
         NI    ESTDSC2H+1,X'FF'-X'20'                                           
         CLI   MODE,VALKEY         VALIDATE RECORD KEY?                         
         JE    INITI70X                                                         
         OC    SVE2USER,SVE2USER   EST USER DEF 1 FLD PRESENT?                  
         BZ    *+14                                                             
         MVC   ESTDEF2,SVE2USER                                                 
         B     *+8                                                              
         OI    ESTDSC2H+1,X'20'    PROTECT, SINCE DEF FLD IS NOT THERE          
INITI70X OI    ESTDSC2H+6,X'80'                                                 
         OI    ESTDEF2H+6,X'80'                                                 
*                                                                               
INITI74  DS    0H                                                               
*                                                                               
INITIX   J     EXIT                                                             
*                                                                               
HOOK     NTR1                      HEADLINE ROUTINES                            
*                                                                               
HOOKX    J     EXIT                                                             
*                                                                               
HEDSPECS SSPEC H1,01,REQUESTOR                                                  
         SSPEC H2,01,C'Media'                                                   
         SSPEC H1,56,C' Estimate Report'                                        
         SSPEC H2,56,C'-----------------'                                       
         SSPEC H1,99,AGYNAME                                                    
         SSPEC H2,99,AGYADD                                                     
         SSPEC H4,99,RUN                                                        
         SSPEC H5,99,REPORT                                                     
         SSPEC H6,99,C' '                                                       
         DC    H'0'                                                             
*                                                                               
PF12POSQ EQU   L'ESTBOTL-L'PF12TXT                                              
PF12TXT  DC    C'Pf12=Return/NextSel'                                           
*                                                                               
* RF POINTS FLD SECURITY VALUE                                                  
* R2 POINTS TITLE FLD TO BE PROCESSED                                           
*                                                                               
PRCFLD   DS    0H                  CLR TITLE & PROTECT FLDS                     
         SR    R1,R1                                                            
         IC    R1,0(R2)            TOTAL FLD LENGTH                             
         CLI   0(RF),0             FULL ACCESS?                                 
         BNE   PRCF20                                                           
         AR    R2,R1               TO TO INPUT FLD                              
         NI    1(R2),X'FF'-X'20'   UNPROTECT INPUT FLD                          
         B     PRCFLDX                                                          
PRCF20   CLI   0(RF),C'Y'          READ ONLY?                                   
         BE    PRCF30                                                           
         CLI   0(RF),C'N'          NO ACCESS?                                   
         BE    *+6                                                              
         DC    H'0'                INVALID SECURITY VALUE!                      
         SHI   R1,8+1              MINUS OVERHEAD AND ONE FOR EX                
         CHI   R1,0                                                             
         BNL   *+6                                                              
         DC    H'0'                BAD TITLE FLD LENGTH                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    8(0,R2),8(R2)                                                    
         OI    6(R2),X'80'         TRANSMIT CLEARED TITLE FLD                   
PRCF30   IC    R1,0(R2)                                                         
         AR    R2,R1               BUMP FROM TITLE TO INPUT FLD                 
         OI    1(R2),X'20'         PROTECT INPUT FLD                            
PRCFLDX  OI    6(R2),X'80'         TRANSMIT PROTECTED INPUT FLD                 
         BR    RE                                                               
*                                                                               
* SECURITY FIELDS TABLE - DISPLACEMENT (1 BYTE) AND FLD # (1 BYTE)              
*                                                                               
SECFLDS  DS    0XL2                ** DISPS. TO SECURITY VALUES **              
         DC    AL1(SECEFLT-SECVALS,008)                                         
         DC    AL1(SECERTY-SECVALS,009)                                         
         DC    AL1(SECESPR-SECVALS,010)                                         
         DC    AL1(SECESTA-SECVALS,011)                                         
         DC    AL1(SECERSC-SECVALS,012)                                         
         DC    AL1(SECEUD1-SECVALS,013)                                         
         DC    AL1(SECEUD2-SECVALS,014)                                         
         DC    AL1(SECPCED-SECVALS,017)                                         
         DC    AL1(SECACTD-SECVALS,018)                                         
         DC    AL1(SECEPON-SECVALS,021)                                         
*                                                                               
SECFLDSN EQU   (*-SECFLDS)/L'SECFLDS                                            
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GETREPLN NTR1  BASE=*,LABEL=*      R6 POINTS TO RECORD                          
*                                                                               
         LA    R5,P1                                                            
         USING REP_LINE,R5                                                      
*                                                                               
         EDIT  (B2,10(R6)),(3,R_EST),0,ALIGN=RIGHT,ZERO=NOBLANK,FILL=0          
*                                                                               
         USING PESTELEM,R6                                                      
         MVI   ELCODE,X'07'        EST ELEM ID                                  
         BRAS  RE,GETEL                                                         
         JE    *+6                                                              
         DC    H'0'                FIRST ELEM MUST BE THERE!                    
*                                                                               
         MVC   R_ESTNM,PESTNAME    EST NAME (LINE 2)                            
         MVC   R_ESTNM2,PESTNAM2   EST NAME (LINE 2)                            
*                                                                               
* EST START AND END DATES IN RECORD IS YYMMDD, MM/DD/YY WHEN PRINTED            
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTST),(10,R_ESTST)                              
*                                                                               
         GOTO1 DATCON,DMCB,(0,PESTEND),(10,R_ESTEND)                            
*                                                                               
         J     EXIT                                                             
*                                                                               
         LTORG                                                                  
         DROP  R6,R5,RB                                                         
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
CKPFKEYS NTR1  BASE=*,LABEL=*      CKING FOR PK KEYS                            
*                                                                               
         CLI   PFAID,2             PF2, ESTIMAT 2 (ES2) MAINT?                  
         BE    CKPFK10                                                          
         CLI   PFAID,3             PF3, CLT MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,4             PF4, PRD MAINT?                              
         BE    CKPFK10                                                          
         CLI   PFAID,5             PF5, CLT LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,6             PF6, PRD LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,7             PF6, EST LIST?                               
         BE    CKPFK10                                                          
         CLI   PFAID,8             PF8, EST BILL MAINT?                         
         BE    CKPFK10                                                          
         CLI   PFAID,9             PF9, EST COPY?                               
         BE    CKPFK10                                                          
*                                                                               
         CLI   PFAID,12            PF12, EXIT/RETURN?                           
         JE    SETCCEQ                                                          
         CLI   PFAID,24            PF24, EXIT/RETURN?                           
         JE    SETCCEQ                                                          
*                                                                               
         J     SETCCNEQ            VALID PFKEY IS NOT ENTERED                   
*                                                                               
CKPFK10  XC    WORK,WORK           ESTABLISH AS XCTL ELEMENT                    
         LA    R1,WORK                                                          
         USING GLVXFRSY,R1                                                      
*                                                                               
         MVC   GLVXFRSY,=C'PRI'    SET FROM SYSTEM                              
         MVC   GLVXFRPR,=C'SFM'    SET FROM PROGRAM                             
         MVC   GLVXTOSY,=C'PRI'    SET TO   SYSTEM                              
         MVC   GLVXTOPR,=C'SFM'    SET TO   PROGRAM                             
         OI    GLVXFLG1,GLV1RETN                                                
         OI    GLVXFLG1,GLV1RETG                                                
*                                                                               
* SEND XCTL ELM                                                                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',WORK,14,GLVXCTL                           
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR RECORD FLD                    
*                                                                               
         CLI   PFAID,2             REC IS ES2 (FOR MAINT)?                      
         BNE   CKPFK13                                                          
         MVC   DUB,=C'ESTIMAT2'                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK13  CLI   PFAID,3             REC IS CLT (FOR MAINT)?                      
         BNE   CKPFK14                                                          
CKPFK13H MVC   DUB,=C'CLIENT  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK14  CLI   PFAID,4             REC IS PRD (FOR MAINT)?                      
         BNE   CKPFK15                                                          
CKPFK14H MVC   DUB,=C'PRODUCT '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK15  CLI   PFAID,5             REC IS CLT (FOR LIST)?                       
         BNE   CKPFK16                                                          
         B     CKPFK13H                                                         
*                                                                               
CKPFK16  CLI   PFAID,6             REC IS PRD (FOR LIST)?                       
         BNE   CKPFK17                                                          
         B     CKPFK14H                                                         
*                                                                               
CKPFK17  CLI   PFAID,7             REC IS EST (FOR LIST)?                       
         BNE   CKPFK18                                                          
CKPF17H  MVC   DUB,=C'ESTIMATE'                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK18  CLI   PFAID,8             REC IS EST BILL (MAINT)?                     
         BNE   CKPFK19                                                          
         MVC   DUB,=C'ESBILL  '                                                 
         B     CKPFK25                                                          
*                                                                               
CKPFK19  CLI   PFAID,9             REC IS EST (FOR COPY)?                       
         BNE   CKPFK20                                                          
         B     CKPF17H                                                          
*                                                                               
CKPFK20  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
* SET RECORD FLD                                                                
*                                                                               
CKPFK25  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXREC                             
*                                                                               
         MVC   DUB,SPACES          PREPARE IT FOR ACTION FLD                    
*                                                                               
         CLI   PFAID,2             ES2 MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,3             CLT MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,4             PRD MAINT?                                   
         BE    CKPFK28H                                                         
         CLI   PFAID,8             EST BILL MAINT?                              
         BE    CKPFK28H                                                         
*                                                                               
         B     CKPFK30             CK OTHER PFKEYS FOR LIST                     
*                                                                               
CKPFK28H MVC   DUB,=C'CHANGE  '                                                 
         CLI   ACTNUM,ACTCHA       CHANGE ACTION?                               
         BE    CKPFK40                                                          
         CLI   THISLSEL,C'C'       SEL CODE IS CHANGE ON LIST?                  
         BE    CKPFK40                                                          
         CLI   MODE,VALREC         MODE IS VALREC?                              
         BE    CKPFK40                                                          
         CLI   MODE,RECPUT         MODE IS PUTREC? (STILL CHG)                  
         BE    CKPFK40                                                          
         CLI   MODE,XRECPUT        MODE IS XPUTREC?                             
         BE    CKPFK40                                                          
         MVC   DUB,=C'DISPLAY '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK30  CLI   PFAID,5             CLT LIST?                                    
         BNE   CKPFK31                                                          
CKPFK30H MVC   DUB,=C'LIST    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK31  CLI   PFAID,6             PRD LIST?                                    
         BNE   CKPFK32                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK32  CLI   PFAID,7             EST LIST?                                    
         BNE   CKPFK33                                                          
         B     CKPFK30H                                                         
*                                                                               
CKPFK33  CLI   PFAID,9             EST COPY?                                    
         BNE   CKPFK34                                                          
         MVC   DUB,=C'COPY    '                                                 
         B     CKPFK40                                                          
*                                                                               
CKPFK34  DS    0H                  FOR FUTURE PFKEYS                            
*                                                                               
         J     SETCCNEQ            NO OTHER PFKEY DEFINED YET                   
*                                                                               
* SET ACTION FLD                                                                
*                                                                               
CKPFK40  GOTO1 VGLOBBER,DMCB,=C'PUTD',DUB,8,GLVXACT                             
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ESTMEDH,,GLVPRKEY   KEY                   
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ESTMEDH,,GLVPRMD    MEDIA                 
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ESTCLTH,,GLVPRCLT   CLIENT                
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ESTPRDH,,GLVPRPRD   PRODUCT               
         GOTO1 VGLOBBER,DMCB,=C'PUTF',ESTESTH,,GLVPREST   ESTIMATE              
*                                                                               
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                  FROM CKPFKEYS                                
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PUTREQRC NTR1  BASE=*,LABEL=*      PUT A REQUEST CARD FOR T/A REPORT            
*                                                                               
         XC    QCTL,QCTL                                                        
         MVC   QAREA,SPACES                                                     
         MVC   QAREA+00(2),=C'41'                                               
         MVC   QAREA+02(2),AGENCY                                               
         MVC   QAREA+04(1),QMED                                                 
         MVC   QAREA+05(3),QCLT                                                 
         MVC   QAREA+11(3),QPRD                                                 
*                                                                               
         SR    RE,RE                                                            
         ICM   RE,3,BEST                                                        
         CVD   RE,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  QAREA+20(3),DUB                                                  
*                                                                               
         MVC   QAREA+68(7),=C'AUTOREQ'                                          
*                                                                               
         MVI   QCTL+10,41                                                       
         MVI   QCTL+14,106                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'PREQUEST',QCTL,QCTL                    
*                                                                               
         TM    DMCB+8,X'FF'                                                     
         JZ    SETCCEQ                                                          
         J     SETCCNEQ                                                         
*                                                                               
GET_ITXT LR    R0,RE                                                            
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     RF,FULL                                                          
         XC    CONHEAD,CONHEAD                                                  
         GOTOR GETTXT,DMCB+12,(R2),0,(C'I',DMCB),0,0,(RF)                       
         OI    CONHEADH+6,X'80'                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET_ETXT LR    R0,RE                                                            
         XC    FULL,FULL                                                        
         MVI   FULL+1,25           SYSTEM                                       
         L     RF,FULL                                                          
         XC    CONHEAD,CONHEAD                                                  
         GOTOR GETTXT,DMCB+12,(R2),0,(C'E',DMCB),0,0,(RF)                       
         OI    CONHEADH+6,X'80'                                                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
ADDBUCKT NTR1  BASE=*,LABEL=*      ADD EMPTY ESTIMATE BUCKET REC                
*                                                                               
         CLI   ACTNUM,ACTADD       ACTION ADD?                                  
         JNE   SETCCNEQ                                                         
         CLC   =C'ZZZ',ESTPRD      ZZZ PRD?                                     
         JE    SETCCNEQ                                                         
*                                                                               
         L     RE,AIO                                                           
         LHI   RF,4096                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(25),0(RE)                                                    
         MVI   KEY+3,X'09'         ESTIMATE BUCKET REC CODE                     
         XCEFL                                                                  
         L     RE,AIO                                                           
         MVC   0(25,RE),KEY                                                     
         MVI   25+1(RE),33         EMPTY RECORD                                 
         GOTO1 ADDREC                                                           
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
***********************************************************************         
*        VALPROR - VALIDATE PURCHASE ORDER                            *         
***********************************************************************         
         USING PESTPOND,R6                                                      
VALPROR  NTR1  BASE=*,LABEL=*                                                   
         LA    R3,PEPONPON                                                      
         LA    R1,L'PEPONPON                                                    
VPROR10  CLI   0(R3),X'40'    SPACES,                                           
         BE    VPROR20                                                          
         CLI   0(R3),C'A'     ALPHA,                                            
         JL    SETCCNEQ                                                         
         CLI   0(R3),C'Z'                                                       
         BNH   VPROR20                                                          
         CLI   0(R3),C'0'     AND NUMERIC ARE OK                                
         JL    SETCCNEQ                                                         
         CLI   0(R3),C'9'                                                       
         JH    SETCCNEQ                                                         
VPROR20  AHI   R3,1                                                             
         BCT   R1,VPROR10                                                       
         J     SETCCEQ                                                          
*                                                                               
         LTORG                                                                  
         DROP  R6                                                               
         EJECT                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE DDBIGBOX                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMFFD                                                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA3D          EST MAINT SCREEN                             
         EJECT                                                                  
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE PRSFMA4D          EST LIST SCREEN                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE PRSFMWORKD                                                     
         ORG   SYSSPARE            WORKING AREA                                 
*                                                                               
CLTLSFLT DS    CL3                 LIST FILTER ON CLT CODE                      
PRDLSFLT DS    CL3                 LIST FILTER ON PRD CODE                      
ESTLSFLT DS    XL2                 LIST FILTER ON EST CODE (BINARY)             
SVWORK   DS    XL64                GENERAL WORKING STORAGE                      
SVCLTLSC DS    CL3                 SAVE CLT CODE ON LIST                        
SVPRDLSC DS    CL3                 SAVE PRD CODE ON LIST                        
MASTEMNL DS    CL1                 MASTER TERMINAL SWITCH                       
WKCDATE  DS    CL6                 WORKING CHARACTER DATE (MMDDYY)              
*                                                                               
CNTLSW   DS    XL1                 CONTROL SWTICH                               
C_MAXIOQ EQU   X'08'               MAX I/O IS REACHED                           
*                                                                               
IO_COUNT DS    H                   I/O COUNTER                                  
MXIOPCT  EQU   30                  % OF MAX IO ALLOWED                          
*                                                                               
SVSECAGY DS    XL2                                                              
SV_B_PID DS    XL2                                                              
*                                                                               
CHGRQFLD DS    X                   Change required field switch                 
NOCHRQFQ EQU   X'80'               Not allowed to change required flds          
*                                                                               
SAVERE   DS    F                   FOR SAVING RETURN ADDRESSES                  
SAVE_R0  DS    F                                                                
QCTL     DS    CL26                                                             
QAREA    DS    CL80                                                             
*                                                                               
WKTMPFLD DS    XL11                HDR IS 8 AND 3 INPUT CHARS                   
WKTMPKEY DS    XL(L'KEY)                                                        
WKTMPAIO DS    XL(L'AIO)                                                        
WKTMPION DS    XL(L'USEIONUM)                                                   
*                                                                               
WKSVSTRT EQU   *                                                                
PESTELLQ EQU   L'PESTELEM                                                       
SVESTELM DS    XL(PESTELLQ)        Save main estimate elem                      
WKCLTFIN DS    XL(L'PCLTFIN)       CLT FINANCIAL BYTE                           
WKESTNAM DS    CL(L'PESTNAME)      EST NAME (LINE 1)                            
WKESTST  DS    CL(L'PESTST)        EST START DATE (YYMMDD)                      
WKESTEND DS    CL(L'PESTEND)       EST END DATE   (YYMMDD)                      
WKESTRTY DS    CL(L'PESTRTYP)      EST RATE TYPE                                
WKESPROF DS    CL(L'PESTPROF)      EST PROFILE VALUES                           
WKESSTAT DS    CL(L'PESTSTAT)      EST STATUS                                   
WKESTEST DS    CL(L'PESTTEST)      EST TEST STAUS                               
WKESBILL DS    CL(L'PESTBILP)      EST BILLING PROFILE                          
WKSVLNQ  EQU   *-WKSVSTRT                                                       
*                                                                               
LINID    DS    CL(L'FALINE)        LINE ID                                      
LINADDR  DS    CL(L'FAADDR)        TERMINAL ADDRESS                             
*                                                                               
WKPRDOAN DS    CL(L'PPRDOAN)       PRD'S OTHER AGY NAME                         
*                                                                               
* 1ST FLD IS FILTER THEN FOLLOWED BY 1 BYTE POSITION COUNTER                    
*                                                                               
F_FLDS   DS    0X                  FILTER FLDS START                            
*                                                                               
F_ESTAT  DS    XL(L'PESTSTAT)      EST STATUS (SOFT OR PERM LOCKOUT)            
         DS    X                                                                
F_ESTART DS    XL(L'PESTST)        EST START DATE (YYMMDD)                      
         DS    X                                                                
F_EEND   DS    XL(L'PESTEND)       EST END DATE (YYMMDD)                        
         DS    X                                                                
F_ADNUM  DS    XL(L'PESTJOB)       JOB (AD) NUMBER                              
         DS    X                                                                
F_FILTRS DS    XL(L'PESTGRPS)      FILTERS                                      
         DS    X                                                                
F_FPOS   DS    X                   FILTER POSITION (1,2 OR 3)                   
F_FCHAR  DS    XL2                 FILTER CHAR     (X OR -X)                    
F_RTYPE  DS    XL(L'PESTRTYP)      RATE TYPE                                    
         DS    X                                                                
F_REVNUM DS    XL(L'PESTREVN)      REVISION NUMBER                              
         DS    X                                                                
F_STAT   DS    CL2                 STATUS - 1/2 AND L/T/S                       
         DS    X                                                                
F_STAT_W DS    C                   LIVE EST FILTER (WILDCARD N/A)               
F_SFH    DS    XL(L'PESTTEST)      EST SFH                                      
         DS    X                                                                
F_SFH_W  DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
F_IDESK  DS    XL(L'PESTTEST)      iDesk Estimate filter                        
         DS    X                                                                
F_IDK_W  DS    C                   WILDCARD CHAR (AVOID BITS CONFLICT)          
F_PORDER DS    X                   PURCHASE ORDER                               
         DS    X                                                                
F_COST2  DS    X                   COST2 FACTOR                                 
         DS    X                                                                
F_USER1  DS    X                   USER DEF 1 (WILDCARD ONLY)                   
         DS    X                                                                
F_USER2  DS    X                   USER DEF 2 (WILDCARD ONLY)                   
         DS    X                                                                
F_ESTREP DS    XL(L'PESTREP)       SPECIAL REP                                  
         DS    X                                                                
F_RETSCH DS    XL(L'PESTRSCH)      RETAIL BILLING SCHEME                        
         DS    X                                                                
F_BILLF  DS    X                   BILL FORMULA (WILDCARD ONLY)                 
         DS    X                                                                
F_ACTLZD DS    0XL6                ACTUALIZATION DATE RANGE                     
F_ACTSTR DS    XL3                 START DATE                                   
F_ACTEND DS    XL3                 END DATE                                     
         DS    X                                                                
F_FLDLNQ EQU   *-F_FLDS                                                         
*                                                                               
* SECURITY VALUES ARE TRANSLATED AS FOLLOW:                                     
* C'Y'   READ ONLY (FIELD WILL BE PROTECTED)                                    
* C'N'   NO ACCESS (FIELD WILL BE HIDDEN AND PROTECTED)                         
* X'00'  WRITE                                                                  
*                                                                               
SECVALS  DS    0X                  ** FIELD SECURITY VALUES **                  
*                                                                               
SECEFLT  DS    C                   EST FILTER                                   
SECERTY  DS    C                   EST RATE TYPE                                
SECESPR  DS    C                   EST SPECIAL REP                              
SECESTA  DS    C                   EST STATUS                                   
SECERSC  DS    C                   EST RETAIL SCHEME                            
SECEUD1  DS    C                   EST USER DEFINITION FLD 1                    
SECEUD2  DS    C                   EST USER DEFINITION FLD 2                    
SECPCED  DS    C                   BILL ON PC EFFECTIVE DATE                    
SECACTD  DS    C                   EST ACTUALIZE DATE                           
SECEPON  DS    C                   EST PURCHASE ORDER NUMBER                    
*                                                                               
SECVALSL EQU   *-SECVALS           MAX IS 255                                   
*                                                                               
SVPUSER1 DS    CL(L'PEUSER1)       SAVED USER DESCRIPTION 1                     
SVPUSER2 DS    CL(L'PEUSER2)       SAVED USER DESCRIPTION 2                     
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
*                                                                               
       ++INCLUDE PCLTREC           CLIENT RECORD                                
         EJECT                                                                  
*                                                                               
       ++INCLUDE PPRDREC           PRODUCT RECORD                               
         EJECT                                                                  
*                                                                               
       ++INCLUDE PESTREC           ESTIMATE RECORD                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE PREPREC           REP RECORD                                   
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDBKELEM          EST BUCKET ELEMS                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE PBILPROF          BILLING PROFILE                              
         EJECT                                                                  
*                                                                               
       ++INCLUDE FLDIND            FLD INDICATOR EQUATES                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE FAFACTS           MASTER SYS INFO BLOCK                        
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE CTGENFILE         DSECT FOR CONTROL FILE RECORDS               
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLVXCTLD        GLOBBER TRANSFER CONTROLS                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDGLOBEQUS        DSECT FOR GLOBBER                            
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDSCANBLKD        DSECT FOR SCANNER                            
F_SMAXQ  EQU   10                  MAX NUM OF FILTER SCANNER ENTRIES            
         EJECT                                                                  
*                                                                               
       ++INCLUDE DDPERVALD         DSECT FOR PERVAL                             
         EJECT                                                                  
*                                                                               
       ++INCLUDE FASECRETD                                                      
         EJECT                                                                  
*                                                                               
       ++INCLUDE SEACSFILE         SECURITY SYSTEM RECORD                       
         EJECT                                                                  
*                                                                               
         PRINT ON                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LSCLTC   DS    CL(L'PCLTKCLT)      CLT CODE                                     
         DS    CL2                                                              
LSPRDC   DS    CL(L'PPRDKPRD)      PRD CODE                                     
         DS    CL2                                                              
LSESTC   DS    CL3                 EST CODE                                     
         DS    CL1                 C'/'                                         
LSEST    DS    CL(L'PESTNAME)      ENTIRE EST NAME                              
         DS    CL2                                                              
*                                                                               
LSFSTART DS    0X                  FILTER DATA START HERE                       
*                                                                               
LSESTAT  DS    CL4                 EST STATUS                                   
         DS    CL2                                                              
LSESTART DS    CL8                 EST START DATE (MM/DD/YY)                    
         DS    CL2                                                              
LSEEND   DS    CL8                 EST END DATE (MM/DD/YY)                      
         DS    CL2                                                              
LSADNUM  DS    CL(L'PESTJOB)       EST JOB (AD) NUMBER                          
         DS    CL2                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
REP_LINE DSECT                     REPORT DSECT FOR PRINTING LINES              
*                                                                               
R_EST    DS    CL3                 EST CODE                                     
         DS    XL2                                                              
R_ESTNM  DS    CL(L'PESTNAME)      EST NAME (LINE 1)                            
         DS    XL2                                                              
R_ESTNM2 DS    CL(L'PESTNAM2)      EST NAME (LINE 2)                            
         DS    XL2                                                              
*                                                                               
R_ESTST  DS    CL8                 EST START DATE (MM/DD/YY)                    
         DS    XL2                                                              
*                                                                               
R_ESTEND DS    CL8                 EST END   DATE (MM/DD/YY)                    
         DS    XL2                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
UDEFD    DSECT                     USER DEFINITION FLDS                         
UDEF     DS    CL20                                                             
UTYPE    DS    CL1                                                              
ULEN     DS    XL1                                                              
UFLG1    DS    XL1                                                              
UFLG2    DS    XL1                                                              
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
LS_TTL4  DSECT                     DEFAULT LIST TITLE LINES                     
*                                                                               
LTESTC   DS    CL3                 EST CODE                                     
         DS    C                   /                                            
LTEST    DS    CL(L'PESTNAME)      EST NAME                                     
         DS    CL2                                                              
*                                                                               
LTFSTRTQ EQU   *-LS_TTL4+5+5+5     "SEL  CLT  PRD  " (15 CHARS)                 
LTFSTART DS    0X                  FILTER TITLE START HERE                      
*                                                                               
LTESTAT  DS    CL4                 STAT (SOFT, LOCK)                            
         DS    CL2                                                              
LTESTART DS    CL8                 STRTDATE (MM/DD/YY)                          
         DS    CL2                                                              
LTEEND   DS    CL8                 ENDDATE (MM/DD/YY)                           
         DS    CL2                                                              
LTADNUM  DS    CL(L'PESTJOB)       ADNUM                                        
         DS    CL2                                                              
*                                                                               
LTFFLDQ  EQU   80-1-LTFSTRTQ       LENGTH OF FLT DISPLAY AREA                   
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'179PRSFM1A   08/31/20'                                      
         END                                                                    
