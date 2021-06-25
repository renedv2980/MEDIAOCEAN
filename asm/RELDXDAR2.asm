*          DATA SET RELDXDAR2  AT LEVEL 066 AS OF 05/01/18                      
*          DATA SET RELDXDAR2  AT LEVEL 001 AS OF 04/17/18                      
*******20180412:048738: NEW MEMBER ADDED BY HWON FOR PROJ# SPEC-22165           
*******        :048738: Spot SPANKER for TMNYBLD to TMNY                        
*PHASE REEXTDAR                                                                 
*INCLUDE HEXOUT                                                                 
*INCLUDE RECUP                                                                  
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
         TITLE 'REEXTDAR - SUPPORT SPANK ORDERS FROM TMNY TO TMNYBLD'           
***********************************************************************         
* IN ADDITION TO SPANK IT STEP THAT MOVES RECORDS FROM TMNYBLD TO TMNY          
* THIS EXTERN WILL ADDITIONALLY MASSAGE DARE ORDERS AS FOLLOWS:                 
* - TMNYBLD IS CHANGED TO TMNY                                                  
* - LUNABLD IS CHANGED TO LUNA                                                  
* - POWER CODE IS CHANGED FROM QT TO TM                                         
* - AGENCY MEDIA CODE IS CHANGED FROM 11 TO 41                                  
***********************************************************************         
*                                                                               
* PARAMETER LIST                                                                
*                                                                               
* P1=A(RECORD)  PASS FIRST BYTE X'00'= INITIALISE                               
*                               X'01'= RECORD IN CORE                           
*                               X'FF'= END OF FILE                              
*               RETURN VALUE    X'00'= KEEP RECORD                              
*                               X'FF'= PURGE RECORD                             
*                               X'FF'/C'EOJ'=PURGE & CAUSE EOJ                  
* P2=A(TAPEOUT) PASS FIRST BYTE X'80'= TAPE INPUT                               
*                               X'40'= TAPE OUTPUT                              
*                               X'20'= RECORD IS I/S FILE RECORD                
* P3=A(PARAM CARD)                                                              
* P4=A(FILE DEFN)                                                               
* P5=A(PRINTER)                                                                 
* P6=A(CPRINT)                                                                  
*                                                                               
         PRINT NOGEN                                                            
DMLDEXT  CSECT                                                                  
         NMOD1 WORKX-WORKD,DMLDEXT,RR=R5                                        
         USING WORKD,RC                                                         
         EJECT                                                                  
*                                                                               
* CONTROL FLOW LOGIC                                                            
*                                                                               
DMXCTL   ST    R1,APARM            SAVE PARAMETERS                              
         MVC   PLIST,0(R1)                                                      
*                                                                               
         L     R2,VLDDEFN                                                       
         USING LDDEFND,R2                                                       
*                                                                               
         L     RA,VCPRINT          SET UP FOR PRINTING                          
         USING DPRINT,RA                                                        
*                                                                               
         L     RE,=V(PRINTBL)                                                   
         AR    RE,R5                                                            
         ST    RE,PRNTBL                                                        
*                                                                               
         SPACE 2                                                                
         CLI   PLIST,X'00'                                                      
         BE    DMXINIT             INITIALISE                                   
         CLI   PLIST,X'01'                                                      
         BE    DMXREC              PROCESS                                      
         CLI   PLIST,X'FF'                                                      
         BE    DMXEOF              END-OF-FILE                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXKEEP  L     R1,APARM                                                         
         MVI   0(R1),0                                                          
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPURGE L     R1,APARM            PURGE RECORD EXIT                            
         MVI   0(R1),X'FF'                                                      
         B     DMXIT                                                            
         SPACE 2                                                                
DMXPGEOF L     R1,APARM            PURGE AND CAUSE INPUT EOF EXIT               
         MVI   0(R1),X'FF'                                                      
         MVC   1(3,R1),=C'EOF'                                                  
         B     DMXIT                                                            
         SPACE 2                                                                
DMXIT    XMOD1 1                                                                
         SPACE 2                                                                
EXIT     XIT1                                                                   
*                                                                               
* INITIALIZE LOGIC                                                              
*                                                                               
DMXINIT  DS    0H                                                               
         XC    COUNT,COUNT                                                      
         MVC   P(08),=C'STARTED!'                                               
         GOTO1 VPRINTER                                                         
*                                                                               
         B     DMXIT                                                            
*                                                                               
* PROCESS RECORD LOGIC                                                          
*                                                                               
DMXREC   DS    0H                                                               
         L     R6,AREC             PROCESS DARE RECORDS                         
         ST    R6,MYAREC                                                        
         USING RDARREC,R6          HEADER RECORDS                               
*                                                                               
         L     R6,AREC                                                          
         CLI   RDARKTYP,X'41'       DARE RECORD?                                
         BE    DMXR05                                                           
         CLI   RDARKTYP,X'51'                                                   
         BNE   DMXKEEP             NO - TRY NEXT ONE                            
                                                                                
DMXR05   CLI   RDARKRT,X'10'       AGENCY HEADER?                               
         BNE   DMXKEEP                                                          
*                                                                               
         GOTO1 =V(PRNTBL),DMCB,=C'BEFORE',(R6),C'DUMP',200,=C'2D'               
         GOTO1 VPRINTER                                                         
*                                                                               
         NI    MISCFLG1,X'FF'-MF1RCCHG                                          
         L     R6,AREC             PROCESS DARE RECORDS                         
         USING RDARELEM,R6                                                      
         MVI   ELCODE,X'01'                                                     
         BRAS  RE,GETEL                                                         
         BNE   DMXKEEP                                                          
*                                                                               
         MVC   P(10),RDARSNDR                                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         CLC   RDARSNDR,=CL10'LUNABLD'   ID OF SENDER                           
         BNE   DMXR010                                                          
         MVC   RDARSNDR,=CL10'LUNA'      CHANGE LUNABLD TO LUNA                 
         OI    MISCFLG1,MF1RCCHG                                                
DMXR010  CLC   RDARSNDR,=CL10'TMNYBLD'                                          
         BNE   DMXR015                                                          
         MVC   RDARSNDR,=CL10'TMNY'      CHANGE TMNYBLD TO TMNY                 
         OI    MISCFLG1,MF1RCCHG                                                
*                                                                               
DMXR015  CLC   RDARRCVR,=CL10'LUNABLD'   ID OF RECEIVER                         
         BNE   DMXR020                                                          
         MVC   RDARRCVR,=CL10'LUNA'      CHANGE LUNABLD TO LUNA                 
         OI    MISCFLG1,MF1RCCHG                                                
DMXR020  CLC   RDARRCVR,=CL10'TMNYBLD'                                          
         BNE   DMXR030                                                          
         MVC   RDARRCVR,=CL10'TMNY'      CHANGE TMNYBLD TO TMNY                 
         OI    MISCFLG1,MF1RCCHG                                                
*                                                                               
DMXR030  LA    R3,RDARRTS                RETURN TO SENDER INFO                  
         USING RTN2SNDR,R3                                                      
         MVC   P(16),RDARRTS                                                    
         GOTO1 VPRINTER                                                         
         CLC   RTNPWRCD,=C'QT'           POWER CODE                             
         BNE   DMXR040                                                          
         MVC   RTNPWRCD,=C'TM'           CHANGE QT TO TM                        
         OI    MISCFLG1,MF1RCCHG                                                
*                                                                               
DMXR040  LA    R3,RDARRTS                RETURN TO SENDER INFO                  
         GOTO1 =V(HEXOUT),DMCB,RTNAGYMD,P,2                                     
         GOTO1 VPRINTER                                                         
         CLC   RTNAGYMD,=C'11'           AGENCY/MEDIA                           
         BNE   DMXR050                                                          
         MVC   RTNAGYMD,=C'41'           CHANGE 11 TO 41                        
         OI    MISCFLG1,MF1RCCHG                                                
         DROP  R3                                                               
*                                                                               
DMXR050  TM    MISCFLG1,MF1RCCHG                                                
         BZ    DMXKEEP                                                          
         L     RF,COUNT                                                         
         LA    RF,1(RF)                                                         
         ST    RF,COUNT                                                         
         MVC   P(20),=C'** RECORD UPDATED **'                                   
         GOTO1 VPRINTER                                                         
*                                                                               
         L     R6,AREC                                                          
         GOTO1 =V(PRNTBL),DMCB,=C'AFTER',(R6),C'DUMP',200,=C'2D'                
         GOTO1 VPRINTER                                                         
*                                                                               
* CODE TO PRINT IT OUT FOR DEBUG PURPOSES                                       
         BRAS  RE,PRTORDDT                                                      
***      GOTO1 PRNTREC,DMCB                                                     
* CODE TO PRINT IT OUT FOR DEBUG PURPOSES                                       
*                                                                               
         B     DMXKEEP                                                          
*                                                                               
DMXEOF   DS    0H                                                               
         MVC   P(27),=C'NUMBER OF RECORDS UPDATED: '                            
         EDIT  COUNT,(8,P+28),ALIGN=LEFT                                        
         GOTO1 VPRINTER                                                         
         B     DMXIT                                                            
***********************************************************************         
* PRINT ORDER DETAILS                                                           
***********************************************************************         
         USING PLINED,P                                                         
PRTORDDT NTR1                                                                   
         L     R3,AREC                                                          
         USING RDARKEY,R3          X'41' AND X'51' HEADER RECORDS               
         MVC   PREP,RDARKREP                                                    
         MVC   PSTA,RDARKSTA                                                    
         MVC   PAGY,RDARKAGY                                                    
         MVC   PAGYOFF,RDARKAOF                                                 
         GOTO1 =V(HEXOUT),DMCB,RDARKORD,PDARE,4  DARE ORDER NUMBER              
         DROP  R3                                                               
                                                                                
         GOTO1 VPRINTER                                                         
         B     EXIT                                                             
***********************************************************************         
* PRINT THE RECORD                                                              
***********************************************************************         
PRNTREC  NTR1                                                                   
         L     R3,MYAREC                                                        
         LA    R4,100                                                           
         B     PREC020                                                          
*                                                                               
PREC010  AR    R3,R4                                                            
         CLI   0(R3),0                                                          
         BE    PRECX                                                            
         ZIC   R4,1(R3)                                                         
PREC020  GOTO1 =V(HEXOUT),DMCB,(R3),P,(R4)                                      
         GOTO1 VPRINTER                                                         
         B     PREC010                                                          
*                                                                               
PRECX    B     EXIT                                                             
*                                                                               
         GETEL R6,34,ELCODE                                                     
*                                                                               
YES      CR    RB,RB                                                            
         J     XIT                                                              
NO       LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
         DC    CL8'COUNTERS'                                                    
BYTE     DS    X                                                                
COUNT2   DS    F                   # OF RECORDS PROCESSED                       
COUNT    DS    F                   # OF BUYS FOR THAT RECORD                    
SUBTOTAL DS    F                   # OF BUYS FOR THAT AGY/CLT/STA               
TOTAL    DS    F                   TOTAL # OF BUYS                              
CURRSYS  DS    C                   CURRENT SYSTEM CHARACTER                     
ASPT_TAB DS    A                                                                
PRNTBL   DS    A                                                                
*                                                                               
RECLEN1  DS    H                   INITIAL RECORD LENGTH                        
SUMRECLN DS    PL16                                                             
*                                                                               
DATADISP DS    H                                                                
ELCODE   DS    XL1                                                              
*                                                                               
*                                                                               
         DC    CL8'BINPARAM'                                                    
         DS    0D                                                               
BINPARAM DC    A(0)                A(RECORD TO BE FOUND OR ADDED)               
         DC    A(0)                A(TABLE)                                     
BINPCNT  DC    F'0'                NUMBER OF RECS IN TABLE                      
         DS    XL1                                                              
         DS    XL1                                                              
         DC    H'42'               LENGTH OF REC IN TABLE                       
         DC    AL1(0),AL3(9)       DISPLACEMENT OF KEY                          
BINPMAX  DC    A(2000000/L'REC)                                                 
*                                                                               
         DC    CL3'REC'                                                         
REC      DS    XL42                USED TO BUILD WORKER FILE                    
RECKEY   DS    XL13                                                             
SAVEKEY  DS    XL10                CHECK IF SAME RULES APPLY AS LAST -          
*                                  - BUY RECORD                                 
         DC    CL7'GETMAIN'                                                     
         DS    0F                                                               
GETSIZE  DC    XL4'001E8480'       GET 2,000,000 BYTES                          
ATABLE   DS    F                                                                
*                                                                               
         DC    CL9'ADDRESSES'                                                   
ADATCON  DS    A                                                                
ADATVAL  DS    A                                                                
         DS    0F                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         SPACE 2                                                                
WORKD    DSECT                                                                  
DUB      DS    D                                                                
FULL     DS    F                                                                
FULL2    DS    F                                                                
WORK     DS    CL64                                                             
DMCB     DS    6F                                                               
APARM    DS    A                                                                
MYAREC   DS    A                                                                
*                                                                               
PLIST    DS    0CL24                                                            
AREC     DS    A                                                                
VTAPEOUT DS    A                                                                
APARAMC  DS    A                                                                
VLDDEFN  DS    A                                                                
VPRINTER DS    A                                                                
VCPRINT  DS    A                                                                
*                                                                               
ELEM     DS    CL256                                                            
*                                                                               
MISCFLG1 DS    X                                                                
MF1RCCHG EQU   X'80'                                                            
                                                                                
WORKX    EQU   *                                                                
         EJECT                                                                  
*DMLDDEFN                                                                       
       ++INCLUDE DMLDDEFN                                                       
         EJECT                                                                  
*DDDPRINT                                                                       
       ++INCLUDE DDDPRINT                                                       
         EJECT                                                                  
*SPGENDRORD                                                                     
       ++INCLUDE SPGENDRORD                                                     
         EJECT                                                                  
*SPDARDARED                                                                     
       ++INCLUDE SPDARDARED                                                     
         EJECT                                                                  
*REGENDAR                                                                       
       ++INCLUDE REGENDAR                                                       
         EJECT                                                                  
*DDCOREQUS                                                                      
       ++INCLUDE DDCOREQUS                                                      
         EJECT                                                                  
*DDCOMFACS                                                                      
       ++INCLUDE DDCOMFACS                                                      
         EJECT                                                                  
*SPSTAPACKD                                                                     
       ++INCLUDE SPSTAPACKD                                                     
         EJECT                                                                  
*DMDTFPH                                                                        
       ++INCLUDE DMDTFPH                                                        
         EJECT                                                                  
*SPGENBUY                                                                       
BUYRECD   DSECT                                                                 
       ++INCLUDE SPGENBUY                                                       
         EJECT                                                                  
RECOUTD  DSECT                     USED TO BUILD WORKER FILE                    
ROAGYMED DS    XL1                 AGENCY/MEDIA                                 
ROCLT    DS    XL2                 CLIENT                                       
ROSTA    DS    XL3                 STATION                                      
ROEST    DS    XL1                 ESTIMATE                                     
ROPRD    DS    XL1                 PRODUCT                                      
ROPRD2   DS    XL1                 PRODUCT 2                                    
ROLINES  DS    XL(L'REC-9)         REMAINING SPACE FOR LINE NUMS                
*                                                                               
PLINED   DSECT                                                                  
PREP     DS    CL2                 REP CODE                                     
         DS    CL2                                                              
PSTA     DS    CL6                 STATION CODE                                 
         DS    CL2                                                              
PAGY     DS    CL3                 AGENCY CODE                                  
         DS    CL3                                                              
PAGYOFF  DS    CL2                 AGENCY OFFICE                                
         DS    CL3                                                              
PDARE    DS    CL8                 ORDER                                        
         DS    CL3                                                              
PLINEX   EQU   *-PLINED                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'066RELDXDAR2 05/01/18'                                      
         END                                                                    
