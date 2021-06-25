*          DATA SET CTREQ03S   AT LEVEL 006 AS OF 05/01/02                      
*PHASE TA0403A                                                                  
         TITLE 'CTREQ03 - REQUEST - VALIDATE DATA FIELDS - PART 1'              
         PRINT NOGEN                                                            
TA0403   CSECT                                                                  
         NMOD1 040,TA0403,RR=R9                                                 
*                                                                               
         ST    R9,RELO                                                          
         B     *+8                                                              
RELO     DC    F'0'                                                             
*                                                                               
         L     R9,0(R1)                                                         
         USING REQTEMP,R9          R9=A(W/S)                                    
         L     R3,ASAVE                                                         
         USING REQSAVE,R3          R3=A(TWA)                                    
         SPACE 2                                                                
         L     R1,FLDHADR          R1=A(FLD HDR TO BE VALIDATED)                
         SR    RF,RF                                                            
         IC    RF,ROUTNUM          RF=ROUTINE NUM REQUIRED                      
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO             RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF                                                            
         SPACE 2                                                                
EXIT     XMOD1 1                                                                
         EJECT                                                                  
IDNAVAL  NTR1                      ID NAME - 04=JCL,08=LIB                      
         GOTO1 AINITV              10=F                                         
         CLI   FIND,1                                                           
         BL    IDNAVO              ID NAME MISSING                              
         BH    IDNAVO              ID NAME=ALL                                  
         CLI   IFLDH+5,10                                                       
         BH    IDNAVE                                                           
         MVC   RIDNAME,IFLD                                                     
         XC    KEY,KEY                                                          
         MVC   KEY+14(10),IFLD                                                  
*                                                                               
IDNAV2   MVI   KEY,C'J'            TRY JCL RECORD                               
         GOTO1 ARFIL                                                            
         CLI   FERN,X'FE'                                                       
         BL    IDNAVX              DISK ERROR                                   
         BE    IDNAV4              NOT FOUND                                    
         OI    FIND,X'04'          ID NAME=JCL BOOK                             
         B     IDNAVX                                                           
*                                                                               
IDNAV4   MVI   KEY,C'L'            TRY LIB REC                                  
         GOTO1 ARFIL                                                            
         CLI   FERN,X'FE'                                                       
         BL    IDNAVX              DISK ERROR                                   
         BE    IDNAV5              NOT FOUND                                    
         OI    FIND,X'08'          ID NAME=LIB BOOK                             
         B     IDNAVX                                                           
*                                                                               
IDNAV5   MVI   KEY,C'F'                                                         
         GOTO1 ARFIL                                                            
         CLI   FERN,X'FE'                                                       
         BL    IDNAVX              DISK ERROR                                   
         BE    IDNAV6              NOT FOUND                                    
         OI    FIND,X'10'                                                       
         B     IDNAVX                                                           
*                                                                               
IDNAV6   MVI   FERN,13             RECORD NOT ON FILE                           
         B     IDNAVX                                                           
*                                                                               
IDNAVE   MVI   FERN,2              INV ID NAME                                  
IDNAVO   EQU   *                                                                
IDNAVX   XIT1                                                                   
         EJECT                                                                  
FTR1VAL  NTR1                      FILTER#N - 04=X                              
         LA    RA,RFTR1                                                         
         B     FTRVAL                                                           
FTR2VAL  NTR1                                                                   
         LA    RA,RFTR2                                                         
         B     FTRVAL                                                           
FTR3VAL  NTR1                                                                   
         LA    RA,RFTR3                                                         
         B     FTRVAL                                                           
*                                                                               
FTRVAL   GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    FTRVO               FILTER MISSING                               
         CLI   IFLDH+5,1                                                        
         BH    FTRVE                                                            
         MVC   0(1,RA),IFLD                                                     
         OI    FIND,X'04'          FILTER=X                                     
         B     FTRVO                                                            
*                                                                               
FTRVE    MVI   FERN,2              FILTER INVALID                               
FTRVO    EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
STRDVAL  NTR1                      START DATE - 04=YYMMDD 08=YYMM               
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    STRDVO              START DATE MISSING                           
         BH    STRDVE                                                           
         CLI   IFLDH+5,9                                                        
         BH    STRDVE                                                           
*                                                                               
         GOTO1 DATVAL,PLIST,(0,IFLD),RSTRD                                      
         OC    PLIST(4),PLIST                                                   
         BE    STRDV1                                                           
         OI    FIND,X'04'          START DATE = YYMMDD                          
         B     STRDVO                                                           
*                                                                               
STRDV1   GOTO1 (RF),(R1),(2,IFLD),RSTRD                                         
         OC    PLIST(4),PLIST                                                   
         BE    STRDVE                                                           
         MVC   RSTRD+4(2),=C'  '                                                
         OI    FIND,X'08'          START DATE = YYMM                            
         B     STRDVO                                                           
*                                                                               
STRDVE   MVI   FERN,29             START DATE INVALID                           
STRDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
ENDDVAL  NTR1                      END DATE - 04=YYMMDD 08=YYMM                 
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    ENDDVO              END DATE MISSING                             
         BH    ENDDVE                                                           
         CLI   IFLDH+5,9                                                        
         BH    ENDDVE                                                           
*                                                                               
         GOTO1 DATVAL,PLIST,(0,IFLD),RENDD                                      
         OC    PLIST(4),PLIST                                                   
         BE    ENDDV1                                                           
         OI    FIND,X'04'          END DATE = YYMMDD                            
         B     ENDDV2                                                           
*                                                                               
ENDDV1   GOTO1 (RF),(R1),(2,IFLD),RENDD                                         
         OC    PLIST(4),PLIST                                                   
         BE    ENDDVE                                                           
         MVC   RENDD+4(2),=C'  '                                                
         OI    FIND,X'08'          END DATE = YYMM                              
*                                                                               
ENDDV2   CLI   RSTRD,C' '                                                       
         BE    ENDDVO                                                           
         CLC   RSTRD,RENDD         CHECK START LE END                           
         BNH   ENDDVO                                                           
         MVI   FERN,30             END DATE INCOMPATIBLE                        
         B     ENDDVE+4                                                         
*                                                                               
ENDDVE   MVI   FERN,29             END DATE INVALID                             
         NI    FIND,1                                                           
ENDDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
OPT1VAL  NTR1                      OPTION#N - 04=X                              
         LA    R7,OPT1VTBL                                                      
         LA    RA,ROPT1                                                         
         B     OPTNVAL                                                          
OPT2VAL  NTR1                                                                   
         LA    R7,OPT2VTBL                                                      
         LA    RA,ROPT2                                                         
         B     OPTNVAL                                                          
OPT3VAL  NTR1                                                                   
         LA    R7,OPT3VTBL                                                      
         LA    RA,ROPT3                                                         
         B     OPTNVAL                                                          
OPT4VAL  NTR1                                                                   
         LA    R7,OPT4VTBL                                                      
         LA    RA,ROPT4                                                         
         B     OPTNVAL                                                          
OPT5VAL  NTR1                                                                   
         LA    R7,OPT5VTBL                                                      
         LA    RA,ROPT5                                                         
         B     OPTNVAL                                                          
OPT6VAL  NTR1                                                                   
         LA    R7,OPT6VTBL                                                      
         LA    RA,ROPT6                                                         
         B     OPTNVAL                                                          
*                                                                               
OPTNVAL  GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    OPTNVO              OPTION MISSING                               
         CLI   IFLDH+5,1                                                        
         BH    OPTNVE                                                           
         SR    R8,R8                                                            
OPTNV1   IC    R8,0(R7)            FIND REQ NUM ENTRY                           
         LTR   R8,R8                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,R7),REQNUM                                                   
         BE    OPTNV2                                                           
         AR    R7,R8                                                            
         B     OPTNV1                                                           
OPTNV2   BCTR  R8,0                                                             
         BCTR  R8,0                                                             
OPTNV3   CLI   2(R7),255           ANY VALUE                                    
         BE    OPTNV4                                                           
         CLC   2(1,R7),IFLD        CHECK LIST OF VALUES FOR REQ NUM             
         BE    OPTNV4                                                           
         LA    R7,1(R7)                                                         
         BCT   R8,OPTNV3                                                        
         B     OPTNVE                                                           
OPTNV4   MVC   0(1,RA),IFLD                                                     
         OI    FIND,X'04'          OPTION = X                                   
         B     OPTNVO                                                           
OPTNVE   MVI   FERN,2              OPTION INVALID                               
OPTNVO   EQU   *                                                                
         XIT1                                                                   
*                                                                               
*        OPTION TABLE ENTRY  XL1=LENGTH , XL1=REQNUM , XLN=VALUES               
*                                                                               
OPT1VTBL DS    0C                                                               
         DC    AL1(03,60),C'Y'                                                  
         DC    AL1(05,61),C'JLF'                                                
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT1VTX  DC    AL1(0)                                                           
*                                                                               
OPT2VTBL DS    0C                                                               
         DC    AL1(03,61),C'Y'                                                  
         DC    AL1(03,60),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT2VTX  DC    AL1(0)                                                           
*                                                                               
OPT3VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT3VTX  DC    AL1(0)                                                           
*                                                                               
OPT4VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT4VTX  DC    AL1(0)                                                           
*                                                                               
OPT5VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT5VTX  DC    AL1(0)                                                           
*                                                                               
OPT6VTBL DS    0C                                                               
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
         DC    AL1(03,00),X'FF'                                                 
OPT6VTX  DC    AL1(0)                                                           
         EJECT                                                                  
AGIDVAL  NTR1                      AGENCY ID - 04=XX                            
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    AGIDVO              AGID MISSING                                 
         BH    AGIDVO              AGID=ALL                                     
         CLI   IFLDH+5,2                                                        
         BNE   AGIDVE                                                           
         MVC   ROBJECT(2),IFLD                                                  
         OI    FIND,X'04'          AGID=XX                                      
         B     AGIDVO                                                           
*                                                                               
AGIDVE   MVI   FERN,2              AGID INVALID                                 
AGIDVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
SYIDVAL  NTR1                      SYSTEM ID - 04=NNN                           
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    AGIDVO              SYID MISSING                                 
         BH    AGIDVO              SYID=ALL                                     
         CLI   IFLDH+5,3                                                        
         BL    SYIDVE                                                           
         BH    SYIDV2                                                           
         TM    IFLDH+4,X'08'                                                    
         BZ    SYIDV2                                                           
         MVC   ROBJECT(3),IFLD                                                  
         OI    FIND,X'04'          SYID=NNN                                     
         B     SYIDVO                                                           
SYIDV2   LA    R7,SYIDVTBL                                                      
*                                                                               
SYIDV4   CLI   0(R7),0             SEARCH SYSTEM NAME TABLE                     
         BE    SYIDVE                                                           
         CLC   0(3,R7),IFLD                                                     
         BE    SYIDV6                                                           
         LA    R7,L'SYIDVTBL(R7)                                                
         B     SYIDV4                                                           
*                                                                               
SYIDV6   MVC   ROBJECT(3),7(R7)                                                 
         OI    FIND,X'04'          SYID=XXX....                                 
         B     SYIDVO                                                           
*                                                                               
SYIDVE   MVI   FERN,2              SYID INVALID                                 
SYIDVO   EQU   *                                                                
         XIT1                                                                   
*                                                                               
SYIDVTBL DS    0CL10                                                            
         DC    C'SYSTEM ',C'000'                                                
         DC    C'ACCOUNT',C'006'                                                
         DC    C'CONTROL',C'010'                                                
         DC    C'GAMES  ',C'011'                                                
         DC    C'CPP    ',C'012'                                                
         DC    C'MEDIA  ',C'004'                                                
         DC    C'SPOT   ',C'002'                                                
         DC    C'SPT    ',C'002'                                                
         DC    C'PRINT  ',C'004'                                                
         DC    C'PRT    ',C'004'                                                
         DC    C'REP    ',C'008'                                                
SYIDVTBX DC    X'00'                                                            
         DS    0H                                                               
         EJECT                                                                  
LINEVAL  NTR1                      LINE/ADDR - 04=XXX...                        
         GOTO1 AINITV                                                           
         CLI   FIND,1                                                           
         BL    LINEVO              LINE MISSING                                 
         BH    LINEVO              LINE=ALL                                     
*                                                                               
LINEV2   GOTO1 TRMVAL,DMCB,(R4)    TRY FOR TERMINAL ID                          
         L     RA,DMCB+4                                                        
         LTR   RA,RA               RA=A(UTL ENTRY)                              
         BZ    LINEV4                                                           
         GETLA (RA),ROBJECT,ADDR=ALPHA                                          
         OI    FIND,X'04'                                                       
         B     LINEVO                                                           
*                                                                               
LINEV4   CLI   IFLDH+5,4           TRY FOR LINE ID                              
         BH    LINEVE                                                           
         CLI   IFLDH+5,3                                                        
         BL    LINEVE                                                           
         TM    IFLDH+4,X'08'                                                    
         BO    LINEVE                                                           
         MVC   ROBJECT(4),IFLD                                                  
         OI    FIND,X'04'                                                       
         B     LINEVO                                                           
*                                                                               
LINEVE   MVI   FERN,2              LINE INVALID                                 
LINEVO   EQU   *                                                                
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*        THIS TABLE CONTAINS THE ADDRESSES OF THE VALIDATION ROUTINES           
*        CONTAINED IN THIS PHASE. INDEXED BY ROUTNUM.                           
*                                                                               
ROUTADRT DC    A(0)                00 - N/D                                     
         DC    A(IDNAVAL)          01 - ID NAME                                 
         DC    A(FTR1VAL)          02 - FILTER#1                                
         DC    A(FTR2VAL)          03 - FILTER#2                                
         DC    A(FTR3VAL)          04 - FILTER#3                                
         DC    A(STRDVAL)          05 - START DATE                              
         DC    A(ENDDVAL)          06 - END DATE                                
         DC    A(0)                07 - N/D                                     
         DC    A(0)                08 - N/D                                     
         DC    A(0)                09 - N/D                                     
         DC    A(OPT1VAL)          10 - OPTION#1                                
         DC    A(OPT2VAL)          11 - OPTION#2                                
         DC    A(OPT3VAL)          12 - OPTION#3                                
         DC    A(OPT4VAL)          13 - OPTION#4                                
         DC    A(OPT5VAL)          14 - OPTION#5                                
         DC    A(OPT6VAL)          15 - OPTION#6                                
         DC    A(AGIDVAL)          16 - AGENCY ID                               
         DC    A(SYIDVAL)          17 - SYSTEM ID                               
         DC    A(LINEVAL)          18 - LINE/ADDR                               
         EJECT                                                                  
*CTREQSAVE                                                                      
       ++INCLUDE CTREQSAVE                                                      
*CTREQTEMP                                                                      
       ++INCLUDE CTREQTEMP                                                      
       ++INCLUDE DDFLDIND                                                       
         SPACE 2                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006CTREQ03S  05/01/02'                                      
         END                                                                    
