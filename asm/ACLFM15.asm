*          DATA SET ACLFM15    AT LEVEL 068 AS OF 05/01/02                      
*PHASE T60315A,+0                                                               
*INCLUDE SCANNER                                                                
*INCLUDE UNSCAN                                                                 
*INCLUDE ADDAY                                                                  
*INCLUDE AMTVAL                                                                 
*INCLUDE PERVAL                                                                 
         TITLE 'PROGRAM TO BUILD/DISPLAY SALARY ELEMENTS'                       
T60315   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 50,*LFM15**,R9,RR=R5                                             
         LR    R8,RC                                                            
         USING PED,R8                                                           
         L     RA,0(R1)                                                         
         USING T603FFD,RA                                                       
         L     RC,4(R1)                                                         
         USING LOGWORKD,RC                                                      
         ST    R5,PRELOC                                                        
         L     R5,=V(SCANNER)                                                   
         A     R5,PRELOC                                                        
         ST    R5,SCANNER                                                       
         L     R5,=V(UNSCAN)                                                    
         A     R5,PRELOC                                                        
         ST    R5,UNSCAN                                                        
         L     R5,=V(ADDAY)                                                     
         A     R5,PRELOC                                                        
         ST    R5,ADDAY                                                         
         L     R5,=V(AMTVAL)                                                    
         A     R5,PRELOC                                                        
         ST    R5,AMTVAL                                                        
         L     R5,=V(PERVAL)                                                    
         A     R5,PRELOC                                                        
         ST    R5,PERVAL                                                        
         ZAP   MAXAMT,=PL8'9900000000'   MAX IS 99,000,000.00                   
         EJECT                                                                  
*                     *************************                                 
***********************   MODE  =  BUILDKEY   ************************          
*                     *************************                                 
         MVC   LOGHEAD,SPACES                                                   
         OI    LOGHEADH+6,X'80'                                                 
         CLI   MODE,BUILDKEY                                                    
         BNE   DSPR                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'1R'                                                  
         GOTO1 READ                                                             
         MVI   ELCODE,X'16'                                                     
         LA    R4,IO                                                            
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO LEDGER ELEMENT                            
         USING ACHEIRD,R4                                                       
         ZIC   R3,ACHRLEN                                                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   SAVLEDG(0),ACHREL                                                
         SPACE 1                                                                
         LA    R2,LOGDPTH          VALIDATE OFFICE / DEPT                       
         GOTO1 ANY                                                              
         ZIC   R3,LOGDPTH+5                                                     
         LA    R4,SAVLEDG                                                       
         USING ACHEIRD,R4                                                       
         ZIC   R5,ACHRLEVA                                                      
         CLI   ACHRLEVD,0                                                       
         BE    BLDKY10                                                          
         ZIC   R5,ACHRLEVB         IF 4 LEVEL A +B =OFFICE/DEPT                 
BLDKY10  STC   R5,SUBDISP           SAVE DISP SUB-DEPT                          
         CR    R3,R5                                                            
         BH    INVERR                                                           
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),LOGDPT                                                  
         MVC   LOGDPTN,SPACES                                                   
         OI    LOGDPTNH+6,X'80'                                                 
         SPACE 1                                                                
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
*        LR    R7,RA               *** LET BASE HANDLE SECURITY ***             
*        USING TWAD,R7                                                          
*        CLI   TWAACCS,C'*'                                                     
*        BNE   BLDKY20                                                          
*        CLC   TWAACCS+1(1),KEY+3                                               
*        BE    BLDKY20                                                          
*        MVI   ERROR,55            SECURITY LOCKOUT                             
*        B     XIT                                                              
*        DROP  R7                                                               
BLDKY20  TM    LOGDPTH+4,X'20'                                                  
         BO    BLDKY30                                                          
         OI    LOGDPTH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         SPACE  1                                                               
BLDKY30  LA    R2,LOGSBDH          VALIDATE SUB-DEPT                            
         CLI   LOGACT,C'N'                                                      
         BNE   BLDKY31       ONLY LOW LEVEL(STAFF) CAN BE ADDED(NEW)            
         GOTO1 ANY                                                              
BLDKY31  ZIC   R3,LOGSBDH+5                                                     
         LA    R4,SAVLEDG                                                       
         USING ACHEIRD,R4                                                       
         ZIC   R6,ACHRLEVB        IF 4 LEVEL LEDGER SUB-DEPT                    
         CLI   ACHRLEVD,0           IS AT LEVEL 3                               
         BE    *+8                                                              
         IC    R6,ACHRLEVC                                                      
         STC   R6,STFDISP                                                       
         ZIC   R5,SUBDISP                                                       
         SR    R6,R5               MAX. LENGTH OF SUB DEPT                      
         CR    R3,R6                                                            
         BH    INVERR                                                           
         LTR   R3,R3                                                            
         BZ    BLDKY33             SUB-DEPT IS NOT REQUIRED                     
         LA    R4,KEY+3                                                         
         AR    R4,R5               R4 TO SUB-DEPT FIELD IN KEY                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LOGSBD                                                   
BLDKY33  MVC   LOGSBDN,SPACES                                                   
         OI    LOGSBDNH+6,X'80'                                                 
         CLI   LOGSBDH+5,0                                                      
         BE    BLDKY35             IF NO SUB-DEPT DON'T READ                    
         GOTO1 READ                                                             
         GOTO1 NAMOUT                                                           
BLDKY35  TM    LOGSBDH+4,X'20'                                                  
         BO    BLDKY40                                                          
         OI    LOGSBDH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         SPACE 1                                                                
BLDKY40  LA    R2,LOGSTFH          VALIDATE STAFF                               
         CLI   LOGACT,C'N'                                                      
         BNE   BLDKY41                                                          
         GOTO1 ANY                 IF NEW MUST HAVE STAFF                       
BLDKY41  ZIC   R3,LOGSTFH+5                                                     
         LA    R4,SAVLEDG                                                       
         USING ACHEIRD,R4                                                       
         ZIC   R6,ACHRLEVC        IF 4 LEVEL LEDGER STAFF                       
         CLI   ACHRLEVD,0           IS AT LEVEL 4                               
         BE    *+8                                                              
         IC    R6,ACHRLEVD                                                      
         ZIC   R5,STFDISP                                                       
         SR    R6,R5               MAX. LENGTH OF STAFF                         
         CR    R3,R6                                                            
         BH    INVERR                                                           
         LTR   R3,R3                                                            
         BZ    BLDKY43             STAFF IS NOT REQUIRED                        
         LA    R2,LOGSBDH          IF STAFF MUST HAVE SUB-DEPT                  
         GOTO1 ANY                                                              
         LA    R2,LOGSTFH                                                       
         LA    R4,KEY+3                                                         
         AR    R4,R5               R4 TO SUB-DEPT FIELD IN KEY                  
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),LOGSTF                                                   
BLDKY43  TM    LOGSTFH+4,X'20'                                                  
         BO    BLDKY50                                                          
         CLI   LOGSTFH+5,0                                                      
         BE    BLDKY45                                                          
BLDKY45  OI    LOGSTFH+4,X'20'                                                  
         MVI   ANYKEY,C'Y'                                                      
         SPACE 1                                                                
BLDKY50  DC    0H'0'                                                            
         CLI   ANYKEY,C'Y'                                                      
         BNE   BLDKY52                                                          
         CLI   LOGSTFH+5,0                                                      
         BNE   BLDKY52                                                          
         MVC   LOGNAME,LOGSBDN    IF NO STAFF NAME IS SUB-DEPT                  
         OI    LOGNAMEH+6,X'80'                                                 
         MVC   LOGSBDN,SPACES                                                   
         OI    LOGSBDNH+6,X'80'                                                 
         CLI   LOGSBDH+5,0                                                      
         BNE   BLDKY52                                                          
         MVC   LOGNAME,LOGDPTN                                                  
         MVC   LOGDPTN,SPACES                                                   
         OI    LOGDPTNH+6,X'80'                                                 
*                                                                               
BLDKY52  MVC   START,=2X'FF'                                                    
         XC    ENDDATE,ENDDATE                                                  
         LA    R2,LOGSTRTH         NO STRT DATE - STRT AT 1ST 52 EL             
         TM    LOGSTRTH+4,X'20'                                                 
         BO    *+12                                                             
         MVI   ANYKEY,C'Y'                                                      
         OI    LOGSTRTH+4,X'20'                                                 
         CLI   5(R2),0                                                          
         BE    BLDKY59                                                          
*                                                                               
         XC    BLOCK(100),BLOCK                                                 
         LA    R2,LOGSTRTH                                                      
         GOTO1 SCANNER,DMCB,(0,(R2)),(2,BLOCK)                                  
         GOTO1 VALDATE,DMCB,BLOCK+12                                            
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                 INVALID DATE                                 
         MVC   START,FULL          SAVE START DATE                              
*                                                                               
         CLC   BLOCK+44(8),SPACES                                               
         BNH   BLDKY59                                                          
         GOTO1 VALDATE,DMCB,BLOCK+44                                            
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         MVC   ENDDATE,FULL        SAVE END DATE                                
*                                                                               
BLDKY59  LA    R2,LOGSTFH                                                       
         CLI   ANYKEY,C'Y'                                                      
         BNE   BLDKY60                                                          
         XC    ELBLOCK,ELBLOCK     IF KEY CHANGE CLEAR SAVED ELEMENTS           
         MVI   ELBKLEN,0                                                        
BLDKY60  B     XIT                                                              
         EJECT                                                                  
*                     ************************                                  
***********************   MODE = DSPLYREC   **************************          
*                     ************************                                  
DSPR     DS    0H                                                               
         CLI   MODE,DSPLYREC                                                    
         BNE   BLDR                                                             
         LA    R4,IO                                                            
         GOTO1 DISPLAY,DMCB,(R4)                                                
         LA    R2,LOGDPTH                                                       
         CLI   LOGACT,C'A'                                                      
         BNE   XIT                                                              
         LA    R2,LOGLINEH                                                      
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
*              ROUTINE TO DISPLAY A RECORD                            *         
***********************************************************************         
*                                                                               
DISPLAY  NTR1                                                                   
         L     R4,0(R1)                                                         
         ST    R4,AREC                                                          
         TWAXC LOGHIREH                                                         
         L     R4,AREC                                                          
         MVI   ELCODE,X'56'       EMPLOYEE HISTORY EL                           
         BAS   RE,GETEL                                                         
         BNE   DIS06                                                            
         USING ACEMPD,R4                                                        
         GOTO1 DATCON,DMCB,(1,ACEMPHIR),(8,LOGHIRE)   YMD PACKED                
*                                                     TO MMMDD/YY               
DIS04    OC    ACEMPTRM,ACEMPTRM   DO WE HAVE TERM DATE                         
         BZ    DIS06               NO                                           
         GOTO1 DATCON,DMCB,(1,ACEMPTRM),(8,LOGTERM)   YMD PACKED                
*                                                     TO MMMDD/YY               
DIS06    L     R4,AREC                                                          
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   DIS08                                                            
         USING ACNAMED,R4                                                       
         ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   LOGNAME(0),ACNMNAME                                              
         OI    LOGNAMEH+4,X'20'                                                 
*                                                                               
DIS08    MVC   BLOCK(200),SPACES                                                
         LA    R2,BLOCK            BUILD BLOCK OF PROFILES FOR UNSCAN           
         XR    R6,R6                                                            
         L     R4,AREC                                                          
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   DIS14                                                            
         USING ACSTATD,R4                                                       
         MVC   0(24,R2),SPACES                                                  
         TM    ACSTSTAT,X'20'      = A/C IS LOCKED OUT                          
         BZ    DIS11                                                            
         MVC   0(6,R2),=C'LOCKED'                                               
         MVI   10(R2),C'Y'                                                      
         LA    R2,24(R2)                                                        
         LA    R6,1(R6)                                                         
*                                                                               
DIS11    MVC   0(24,R2),SPACES                                                  
         TM    ACSTSTAT,X'04'      = PERSON IS A/C EXEC (COSTING)               
         BZ    DIS12                                                            
         MVC   0(4,R2),=C'EXEC'                                                 
         MVI   10(R2),C'Y'                                                      
         LA    R6,1(R6)                                                         
         LA    R2,24(R2)                                                        
*                                                                               
DIS12    MVC   0(24,R2),SPACES                                                  
         CLI   ACSTCOST,C' '                                                    
         BE    DIS13                                                            
         CLI   ACSTCOST,0                                                       
         BE    DIS13                                                            
         MVC   0(8,R2),=C'ANALYSIS'                                             
         MVC   10(1,R2),ACSTCOST                                                
         LA    R6,1(R6)                                                         
         LA    R2,24(R2)                                                        
*                                                                               
DIS13    MVC   0(24,R2),SPACES                                                  
         TM    ACSTSTX,X'08'                                                    
         BZ    DIS14                                                            
         MVC   0(3,R2),=C'IND'                                                  
         MVI   10(R2),C'Y'                                                      
         LA    R2,24(R2)                                                        
         LA    R6,1(R6)                                                         
*                                                                               
DIS14    L     R4,AREC                                                          
         MVI   ELCODE,X'DB'                                                     
         BAS   RE,GETEL                                                         
         B     *+8                                                              
DIS14A   BAS   RE,NEXTEL                                                        
         BNE   DIS19                                                            
         USING FFTELD,R4                                                        
         CLI   FFTTYPE,FFTTFEER    MAKE SURE WE HAVE THE RIGHT TYPE             
         BNE   DIS14A                                                           
         MVC   0(24,R2),SPACES                                                  
         MVC   0(2,R2),=C'FR'                                                   
         XC    WORK,WORK                                                        
         OC    FFTSTDT,FFTSTDT     OPEN START DATE?                             
         BNZ   DIS15                                                            
         MVC   WORK(2),FFTENDT     END DATE                                     
         MVI   WORK+2,X'01'        FORCE IN A DAY FOR DATCON                    
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+10)                                 
         MVI   WORK+9,C'-'         FR=-MMM/YY                                   
         MVC   10(7,R2),WORK+9                                                  
         LA    R2,24(R2)                                                        
         LA    R6,1(R6)                                                         
         B     DIS14A                                                           
*                                                                               
DIS15    CLC   FFTENDT,=X'FFFFFF'  OPEN END DATE?                               
         BNE   DIS17                                                            
         MVC   WORK(2),FFTSTDT     START DATE                                   
         MVI   WORK+2,X'01'        FORCE IN A DAY FOR DATCON                    
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+10)                                 
         MVI   WORK+16,C'-'        FR=MMM/YY-                                   
         MVC   10(7,R2),WORK+10                                                 
         LA    R2,24(R2)                                                        
         LA    R6,1(R6)                                                         
         B     DIS14A                                                           
*                                                                               
DIS17    MVC   WORK(2),FFTSTDT     START DATE                                   
         MVI   WORK+2,X'01'        FORCE IN A DAY FOR DATCON                    
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+10)                                 
         MVC   10(6,R2),WORK+10                                                 
         MVI   16(R2),C'-'                                                      
         XC    WORK,WORK                                                        
         MVC   WORK(2),FFTENDT     END DATE                                     
         MVI   WORK+2,X'01'        FORCE IN A DAY FOR DATCON                    
         GOTO1 DATCON,DMCB,(1,WORK),(6,WORK+10)                                 
         MVC   17(6,R2),WORK+10                                                 
         LA    R2,24(R2)                                                        
         LA    R6,1(R6)                                                         
         B     DIS14A                                                           
*                                                                               
DIS19    LTR   R6,R6                                                            
         BZ    DIS20                                                            
         GOTO1 UNSCAN,DMCB,((R6),BLOCK),(14,LOGPROFH)                           
         MVC   SVPROFS(L'LOGPROF),LOGPROF SAVE THE PROFILE LINE                 
         LA    R1,SVPROFS                                                       
         SR    RF,RF                                                            
DIS19A   CLI   0(R1),C' '          CHECK FOR END OF PROFILES                    
         BE    *+16                                                             
         LA    RF,1(RF)            KEEP COUNT OF LENGTH OF PROFILES             
         LA    R1,1(R1)                                                         
         B     DIS19A                                                           
         STC   RF,SVPROFSH+5       FORCE LENGTH INTO FAKE HEADER FIELD          
*                                                                               
DIS20    DS    0H                                                               
         XC    ELBLOCK,ELBLOCK                                                  
         MVI   ELBKLEN,0           LENGTH OF BLOCK                              
         MVI   NUMLIN,0                                                         
         LA    R7,LOGLINEH                                                      
         USING LINED,R7                                                         
         L     R4,AREC                                                          
         MVI   ELCODE,X'52'                                                     
         BAS   RE,GETEL                                                         
         BNE   DISXIT                                                           
*                                                                               
         USING ACSALRYD,R4                                                      
DIS22    DS    0H                                                               
         OC    ENDDATE,ENDDATE     IF BOTH START/END DATES INPUT                
         BZ    DIS22A              THEN ONLY SHOW ITEMS THAT MATCH              
         CLC   ACSALBEG,START      DATE RANGE EXACTLY                           
         BNE   DIS23                                                            
         CLC   ACSALEND,ENDDATE                                                 
         BNE   DIS23                                                            
         B     DIS30                                                            
*                                                                               
DIS22A   CLC   ACSALBEG,START                                                   
         BNH   DIS30                                                            
*                                                                               
DIS23    BAS   RE,NEXTEL                                                        
         BNE   DISXIT              NO ELEMENTS THAT QUALIFY                     
         B     DIS22                                                            
*                                                                               
DIS30    DS    0H                  BEGIN DISPLAY OF NEXT TEN ELEMENTS           
         ZIC   R3,ELBKLEN                                                       
         LA    R2,ELBLOCK(R3)      FIND NEXT AREA IN ELBLOCK                    
         ZIC   R1,ACSALEN                                                       
         SH    R1,=H'1'                                                         
         EXMVC R1,0(R2),ACSALEL     SAVE ELEMENT IN ELBLOCK                     
         LA    R3,1(R1,R3)                                                      
         STC   R3,ELBKLEN          UPDATE LENGTH OF BLOCK                       
*                                                                               
         LA    R6,TYPETBLE                                                      
         MVI   LINTYPE,C'S'                                                     
DIS32    CLI   0(R6),X'FF'                                                      
         BE    DIS34                                                            
         CLC   0(1,R6),ACSALTYP                                                 
         BE    DIS33                                                            
         LA    R6,5(R6)                                                         
         B     DIS32                                                            
         SPACE 1                                                                
DIS33    MVC   LINTYPE(4),1(R6)                                                 
DIS34    MVC   FULL(2),ACSALBEG                                                 
         MVI   FULL+2,1                                                         
         GOTO1 DATCON,DMCB,(1,FULL),(9,LINDATE)  YMD(P) TO MMM/YY               
         CLC   ACSALEND,=2X'00'                                                 
         BE    DIS40                                                            
         MVI   LINDATE+6,C','                                                   
         MVC   FULL(2),ACSALEND                                                 
         GOTO1 DATCON,DMCB,(1,FULL),(9,LINDATE+7)                               
         SPACE 1                                                                
DIS40    DC    0H'0'                                                            
         LA    R6,BASISTBL                                                      
         MVC   LINBASE(1),ACSALBAS                                              
DIS42    CLI   0(R6),X'FF'                                                      
         BE    DIS46                                                            
         CLC   ACSALBAS,0(R6)                                                   
         BE    DIS45                                                            
         LA    R6,3(R6)                                                         
         B     DIS42                                                            
         SPACE 1                                                                
DIS45    MVC   LINBASE(3),0(R6)                                                 
         TM    ACSALSTA,X'10'      IS IT 5 DECIMAL PLACE PERCENTAGE             
         BZ    DIS46               BRANCH IF NOT 5 DECIMAL PLACES               
         EDIT  ACSALARY,(13,LINRATE),5,ALIGN=LEFT,DROP=3                        
         B     DIS48               GO PUT IN PERCENT SIGN                       
         SPACE 1                                                                
DIS46    EDIT  ACSALARY,(13,LINRATE),2,COMMAS=YES,FLOAT=-,ALIGN=LEFT            
         TM    ACSALSTA,X'20'      IS IT 2 DECIMAL PERCENTAGE                   
         BNO   DIS50               NO --IT'S A DOLLAR AMOUNT                    
         SPACE 1                                                                
DIS48    LA    RE,LINRATE          ADDR OF RATE IN RE                           
DIS48A   CLI   0(RE),C' '          LOOK FOR THE FIRST BLANK                     
         BE    DIS48C              GO PUT IN PERCENT SIGN                       
         LA    RE,1(RE)            SET RE TO NEXT DIGIT OF RATE                 
         BCT   R0,DIS48A           LOOP FOR BLANK LOOKUP                        
         SPACE 1                                                                
DIS48C   MVI   0(RE),X'6C'         PERCENT SIGN                                 
         SPACE 1                                                                
DIS50    LA    R3,4                                                             
         LA    R2,LINTYPEH                                                      
DIS51    OI    4(R2),X'20'         TURN ON VALIDATED BITS                       
DIS55    ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,DIS51                                                         
         LA    R7,LINLEN(R7)       NEXT LINE                                    
         ZIC   R1,NUMLIN                                                        
         AH    R1,=H'1'                                                         
         STC   R1,NUMLIN                                                        
         CH    R1,=H'10'           MAX OF TEN LINES                             
         BL    DIS23                                                            
DISXIT   B     XIT                                                              
         EJECT                                                                  
*                **************************                                     
******************    MODE = BUILDREC     ****************************          
*                **************************                                     
BLDR     DS    0H                                                               
         CLC   KEY+1(2),=C'1R'                                                  
         BNE   BLDR10                                                           
         CLI   LOGACT,C'N'                                                      
         BNE   BLDR10                                                           
         CLI   LOGSTFH+5,0                                                      
         BE    BLDR10                                                           
         BAS   RE,CHKOVER                                                       
         CLI   ERROR,X'FF'                                                      
         BE    BLDR10                                                           
         BAS   RE,CHKPRSN                                                       
         LA    R2,LOGSTFH                                                       
         CLI   ERROR,X'FE'                                                      
         BE    XIT                                                              
*                                                                               
BLDR10   LA    R4,IO                                                            
         ST    R4,AREC                                                          
         MVI   ELCODE,X'52'        BUILD NEW BLOCK OF ELEMENTS                  
         MVI   NWBKLEN,0                                                        
         XC    NWBLOCK,NWBLOCK                                                  
         MVI   NUMLIN,0                                                         
         CLI   LOGACT,C'N'                                                      
         BE    BLDR21                                                           
         BAS   RE,GETEL                                                         
         BNE   BLDR20                                                           
         USING ACSALRYD,R4                                                      
BLDR11   OC    ENDDATE,ENDDATE     IF BOTH START/END DATES INPUT                
         BZ    BLDR12              THEN ONLY SHOW ITEMS THAT MATCH              
         CLC   ACSALBEG,START      DATE RANGE EXACTLY                           
         BNE   BLDR13                                                           
         CLC   ACSALEND,ENDDATE                                                 
         BNE   BLDR13                                                           
         B     BLDR15                                                           
*                                                                               
BLDR12   CLC   ACSALBEG,START                                                   
         BNH   BLDR15                                                           
BLDR13   BAS   RE,NEXTEL                                                        
         BNE   BLDR20              NO ELEMENTS THAT QUALIFY                     
         B     BLDR11                                                           
*                                                                               
BLDR15   DS    0H                  BEGIN DISPLAY OF NEXT TEN ELEMENTS           
         ZIC   R3,NWBKLEN                                                       
         LA    R2,NWBLOCK(R3)      FIND NEXT AREA IN NWBLOCK                    
         ZIC   R1,ACSALEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),ACSALEL     SAVE ELEMENT IN NWBLOCK                      
         LA    R3,1(R1,R3)                                                      
         STC   R3,NWBKLEN          UPDATE LENGTH OF BLOCK                       
         ZIC   R5,NUMLIN                                                        
         AH    R5,=H'1'                                                         
         STC   R5,NUMLIN                                                        
         CH    R5,=H'10'                                                        
         BL    BLDR13                                                           
         SPACE 1                                                                
BLDR20   CLC   ELBKLEN,NWBKLEN                                                  
         BNE   RECHANGE          RECORD HAS CHANGED SINCE LAST DISPLAY          
         ZIC   R1,ELBKLEN                                                       
         LTR   R1,R1                                                            
         BZ    BLDR21              NO PREVIOUS ELEMENTS                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ELBKLEN(0),NWBKLEN                                               
         BNE   RECHANGE                                                         
         SPACE 1                                                                
         USING ACKEYD,R4                                                        
BLDR21   CLI   LOGACT,C'N'                                                      
         BNE   BLDR23                                                           
         LA    R4,IO2                                                           
         MVC   15(27,R4),SPACES                                                 
         MVC   ACKEYACC,KEY                                                     
         MVC   ACLENGTH,DATADISP                                                
         GOTO1 STATIN                                                           
         GOTO1 BALIN               BAL AND STATUS ONLY ON NEW                   
         B     BLDR24                                                           
         SPACE                                                                  
BLDR23   LA    R2,IO2              ON AMENDS MOVE RECORD TO IO2                 
         LA    R3,IOLENQ                                                        
         LA    R4,IO                                                            
         XR    R5,R5                                                            
         ICM   R5,3,IO+42          ACLENGTH                                     
         MVCL  R2,R4                                                            
         SPACE 1                                                                
BLDR24   XC    ELEMENT,ELEMENT                                                  
         LA    R4,IO                                                            
         MVI   ELCODE,X'56'        CHECK FOR EXISTING X56                       
         BAS   RE,GETEL                                                         
         BNE   BLDR24A                                                          
         MVC   ELEMENT(ACEMPLNQ),0(R4)     SAVE OFF OLD EL                      
         USING ACEMPD,R4                                                        
         LA    R4,ELEMENT                                                       
         XC    ACEMPHIR,ACEMPHIR    BUT CLEAR HIRE AND TERM DATES               
         XC    ACEMPTRM,ACEMPTRM                                                
         B     *+10                                                             
BLDR24A  XC    ELEMENT,ELEMENT     CLEAR ELEMENT WORK AREA                      
         LA    R4,ELEMENT                                                       
         LA    R2,LOGHIREH                                                      
         CLI   5(R2),0             INPUT TO HIRE DATE                           
         BNE   BLDR25              YES- CONTINUE                                
         CLI   LOGTERMH+5,X'0'     INPUT TO TERM DATE                           
         BNE   HIREERR             YES - ERROR HIRE DATE NEEDED                 
         B     BLDR25A             SKIP X'56' EL STUFF                          
         USING ACEMPD,R4                                                        
BLDR25   MVI   ACEMPEL,X'56'       ELEMENT NUMBER                               
         MVI   ACEMPLEN,ACEMPLNQ   LENGTH                                       
         LA    R2,LOGHIREH         SET R2 TO HIRE DATE                          
         GOTO1 DATVAL,DMCB,(0,LOGHIRE),WORK   VALIDATE FOR M/D/Y                
         CLI   DMCB+3,0                       IF ZERO,ITS INVALID               
         BE    INVDTE                         DATE ERROR MESSAGE                
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACEMPHIR)      YMD PACKED                
*                                                     TO MMMDD/YY               
         LA    R2,LOGTERMH         SET R2 TO TERM DATE                          
         CLI   5(R2),0             INPUT TO TERM DATE                           
         BE    BLDR25A             NO-GO PUT OUT THE ELEMENT                    
         GOTO1 DATVAL,DMCB,(0,LOGTERM),WORK   VALIDATE FOR M/D/Y                
         CLI   DMCB+3,0                       IF ZERO,ITS INVALID               
         BE    INVDTE                         DATE ERROR MESSAGE                
         GOTO1 DATCON,DMCB,(0,WORK),(1,ACEMPTRM)      YMD PACKED                
*                                                     TO MMMDD/YY               
         CLC   ACEMPTRM,ACEMPHIR    COMPARE HIRE TO TERM DATE                   
         BL    STENDERR             ERROR- TERM DATE HIGH THAN HIRE             
BLDR25A  DS    0H                                                               
         GOTO1 REMANEL,DMCB,(X'53',0)       DELETE X'53' IF EXIST               
         GOTO1 REMANEL,DMCB,(X'56',0)       DELETE X'56' IF EXIST               
*        OC    ACEMPHIR,ACEMPHIR            SURE THERE'S A NEW ONE              
*        BZ    BLDR26                                                           
         OC    ACEMPHIR(ACEMPLNQ-2),ACEMPHIR  SURE THERE'S A NEW ONE            
         BZ    BLDR26                                                           
         GOTO1 ADDANEL                      ADD NEW X'56'                       
BLDR26   LA    R2,LOGNAMEH                                                      
         GOTO1 ANY                                                              
         CLI   LOGACT,C'A'         ACTION AMEND, SAVE NAME                      
         BNE   BLDR26_0                                                         
         GOTO1 CHKNAM,DMCB,(C'B',IO),NAMESAVE                                   
         LA    RF,NAMESAVE-LOCALS  SAVE DISP TO SAVED NAME                      
         STCM  RF,3,DSAVNAM        FOR SEARCH                                   
*                                                                               
BLDR26_0 GOTO1 NAMIN                                                            
*                                                                               
**T                                                                             
         TM    4(R2),X'20'                  HAS NAME CHANGED                    
         BO    BLDR26A                                                          
         BAS   RE,CHKOVER                                                       
         CLI   ERROR,X'FF'                                                      
         BE    BLDR26A                                                          
         BAS   RE,CHKPRSN                                                       
         CLI   ERROR,X'FE'                                                      
         BE    XIT                                                              
**T                                                                             
BLDR26A  CLI   ACCEMU,C'Y'                  IS IT THE NEW FILE                  
         BNE   BLDR27                                                           
         CLI   LOGACT,C'A'                  MUST BE AMEND                       
         BNE   BLDR27                                                           
         TM    4(R2),X'20'                  HAS NAME CHANGED                    
         BO    BLDR27                                                           
         GOTO1 CHKNAM,DMCB,(C'A',IO2),NAMESAVE                                  
BLDR27   DS    0H                                                               
*                                                                               
         LA    RE,BLOCK                                                         
         LA    RF,X'12C'           CLEAR 300 BYTES OF BLOCK                     
         XCEF                                                                   
         USING SCANBLKD,R3                                                      
         LA    R3,BLOCK                                                         
         LA    R5,4                MAX NUMBER OF PROFILES                       
         GOTO1 REMANEL,DMCB,(X'DB',0)       DELETE X'DB' IF EXIST               
         LA    R2,LOGPROFH                                                      
         CLI   5(R2),0                                                          
         BE    BLDR48                                                           
*                                                                               
         TM    4(R2),X'20'                  HAS NAME CHANGED                    
         BO    BLDR28                                                           
         BAS   RE,CHKOVER                                                       
         CLI   ERROR,X'FF'                                                      
         BE    BLDR28                                                           
         BAS   RE,CHKPRSN                                                       
         CLI   ERROR,X'FE'         ON NEW COST?                                 
         BNE   BLDR28                                                           
*                                                                               
* IF ON NEW COST MAKE SURE USER DOES NOT ADD/CHA/DELETE ANY OTHER               
* PROFILES EXCEPT FOR 'FR' PROFILE.                                             
*                                                                               
         GOTO1 SCANNER,DMCB,(14,(R2)),(4,(R3)) SCAN PROFILE LINE                
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R5,DMCB+4           R5=NUMBER OF PROFILES                        
         LA    R6,SVPROFSH                                                      
         GOTO1 SCANNER,DMCB,(14,(R6)),(4,BLOCK+160)                             
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         LA    R6,BLOCK+160                                                     
*                                                                               
* R6 POINTS TO SAVED PROFILE LINE. R3 POINTS TO SCREEN PROFILE LINE             
*                                                                               
BLDR27A  ZIC   R1,0(R3)            LENGTH OF 1ST HALF OF FIELD                  
         BCTR  R1,0                                                             
         EXCLC R1,12(R3),=C'FR'     FR= PROFILE                                 
         BE    BLDR27B             SKIP FR PROFILE                              
         EXCLC R1,12(R3),12(R6)    SAME PROFILE?                                
         BNE   INVERR                                                           
         ZIC   R1,1(R3)            LENGTH OF 2ND HALF OF FIELD                  
         BCTR  R1,0                                                             
         EXCLC R1,22(R3),22(R6)    SAME PROFILE?                                
         BNE   INVERR                                                           
BLDR27B  LA    R3,36(R3)                                                        
         LA    R6,36(R6)                                                        
         BCT   R5,BLDR27A                                                       
*                                                                               
BLDR28   LA    R5,4                MAX NUMBER OF PROFILES                       
         LA    R2,LOGPROFH                                                      
         LA    RE,BLOCK                                                         
         LA    RF,X'12C'           CLEAR 300 BYTES OF BLOCK                     
         XCEF                                                                   
         LA    R3,BLOCK                                                         
         GOTO1 SCANNER,DMCB,(14,(R2)),((R5),(R3))   SCAN PROFILE LINE           
         CLI   4(R1),0                                                          
         BE    INVERR                                                           
         ZIC   R5,DMCB+4                                                        
BLDR30   LA    R4,IO2                                                           
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                MUST HAVE STATUS ELEMENT                     
         SPACE                                                                  
         USING ACSTATD,R4                                                       
BLDR33   CLC   12(6,R3),=C'LOCKED'                                              
         BNE   BLDR35                                                           
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'DF'                                                   
         B     BLDR46                                                           
         SPACE 1                                                                
         CLI   22(R3),C'Y'                                                      
         BNE   INVERR                                                           
         OI    ACSTSTAT,X'20'                                                   
         B     BLDR46                                                           
         SPACE                                                                  
BLDR35   CLC   12(4,R3),=C'EXEC'                                                
         BNE   BLDR36                                                           
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTAT,X'FB'                                                   
         B     BLDR46                                                           
         SPACE                                                                  
         CLI   22(R3),C'Y'                                                      
         BNE   INVERR                                                           
         OI    ACSTSTAT,X'04'                                                   
         B     BLDR46                                                           
         SPACE 1                                                                
BLDR36   CLC   12(8,R3),=C'ANALYSIS'                                            
         BNE   BLDR38                                                           
         MVI   ACSTCOST,C' '                                                    
         CLC   22(2,R3),=C'NO'                                                  
         BE    BLDR46                                                           
         CLI   22(R3),C'('                                                      
         BE    BLDR36A                                                          
         CLI   22(R3),C')'                                                      
         BE    BLDR36A                                                          
         CLI   22(R3),C'A'                                                      
         BL    INVERR                                                           
         CLI   22(R3),C'9'                                                      
         BH    INVERR                                                           
BLDR36A  MVC   ACSTCOST,22(R3)                                                  
         B     BLDR46                                                           
         SPACE 1                                                                
BLDR38   CLC   12(3,R3),=C'IND'                                                 
         BNE   BLDR39                                                           
         CLI   22(R3),C'N'                                                      
         BNE   *+12                                                             
         NI    ACSTSTX,X'F7'                                                    
         B     BLDR46                                                           
         CLI   22(R3),C'Y'                                                      
         BNE   INVERR                                                           
         OI    ACSTSTX,X'08'                                                    
         B     BLDR46                                                           
*                                                                               
BLDR39   CLC   12(2,R3),=C'FR'                                                  
         BNE   INVERR                                                           
         XC    PVALBLK,PVALBLK                                                  
         XC    ELEMENT,ELEMENT                                                  
         XC    BYTE,BYTE                                                        
         OI    BYTE,X'20'                                                       
         GOTO1 PERVAL,DMCB,(SC2NDLEN,SC2NDFLD),(BYTE,PVALBLK)                   
         LA    R6,PVALBLK                                                       
         USING PERVALD,R6                                                       
         CLI   DMCB+4,PVRCINV1     DATE 1 INVALID?                              
         BE    INVERR                                                           
         TM    PVALASSM,PVALASM    ASSUMED START DATE?(FR=-MMM/YY)              
         BO    BLDR40                                                           
         TM    PVALASSM,PVALAEM    ASSUMED END DATE?(FR=MMM/YY-)                
         BO    BLDR42                                                           
         CLI   DMCB+4,PVRCINV2     DATE 2 INVALID?                              
         BE    INVERR                                                           
         CLC   PVALPSTA,PVALPEND   MAKE SURE END DATE IS HIGHER                 
         BNL   INVERR                                                           
         MVC   STDATE,PVALPSTA                                                  
         MVC   ENDATE,PVALPEND                                                  
         B     BLDR44                                                           
*                                                                               
* FR=-MMM/YY                                                                    
*                                                                               
BLDR40   XC    STDATE,STDATE       STORE BINARY ZEROS FOR START DATE            
         MVC   ENDATE,PVALPEND     MOVE IN PACKED ENDATE YYMM                   
         B     BLDR44                                                           
*                                                                               
* FR=MMM/YY-                                                                    
*                                                                               
BLDR42   MVC   STDATE,PVALPSTA     SAVE THE START DATE                          
         MVC   ENDATE,=X'FFFFFF'   MOVE FF'S INTO THE END DATE                  
*                                                                               
         USING FFTELD,R4                                                        
BLDR44   LA    R4,ELEMENT          BUILD ELEMENT                                
         XC    ELEMENT,ELEMENT                                                  
         MVI   FFTEL,FFTELQ        X'DB' ELEMENT                                
         MVI   FFTTYPE,FFTTFEER    TYPE=FEE REPORT                              
         MVI   FFTSEQ,0                                                         
         MVC   FFTSTDT,STDATE                                                   
         MVC   FFTENDT,ENDATE                                                   
         LA    RE,FFTLN1Q          ELEMENT OVERHEAD                             
         LA    RF,L'FFTDLEN        ACTUAL LENGTH OF TEXT                        
         AR    RE,RF                                                            
         LA    RF,8                2 FOR START DATE+2 FOR END DATE+             
         STC   RF,FFTDLEN                                                       
         AR    RE,RF               3 FOR CLIENT+1 FOR STATUS                    
         STC   RE,FFTLN                                                         
         GOTO1 ADDANEL                                                          
*                                                                               
         SPACE 1                                                                
BLDR46   LA    R3,36(R3)                                                        
         BCT   R5,BLDR33                                                        
         SPACE 1                                                                
BLDR48   DS    0H                                                               
         TM    BIT,NEWCOST         AGY ON NEW COST THEN DON'T BOTHER            
         BZ    *+12                                                             
         MVI   ERROR,X'FF'         CLEAR ERROR MESSAGE                          
         B     BLDR80              VALIDATING SINCE THEY CAN'T CHANGE           
*                                                                               
         MVI   NWBKLEN,0                                                        
         XC    NWBLOCK,NWBLOCK                                                  
         XC    ACTLIST,ACTLIST                                                  
         LA    R6,NWBLOCK                                                       
         LA    R5,ACTLIST                                                       
         LA    R7,LOGLINEH                                                      
         USING LINED,R7                                                         
         SPACE 1                                                                
BLDR50   CLI   LINACTH,9                                                        
         BE    BLDR60              END OF SCREEN - TAB FILED                    
         MVI   0(R5),C' '          ACTION FILED                                 
         LA    R2,LINACTH                                                       
         ZIC   R3,LINACTH+5                                                     
         LTR   R3,R3                                                            
         BZ    BLDR54                                                           
**T                                                                             
         LA    R2,LINACTH                                                       
         BAS   RE,CHKOVER                                                       
         CLI   ERROR,X'FF'                                                      
         BE    BLDR51                                                           
         BAS   RE,CHKPRSN                                                       
         CLI   ERROR,X'FE'                                                      
         BE    XIT                                                              
**T                                                                             
BLDR51   BCTR  R3,0                                                             
         LA    R4,ACTABLE                                                       
BLDR52   EX    R3,*+8              MATCH ACTION TO TABLE                        
         B     *+10                                                             
         CLC   LINACT(0),0(R4)                                                  
         BE    BLDR53                                                           
         LA    R4,3(R4)                                                         
         CLI   0(R4),X'FF'                                                      
         BE    INVERR                                                           
         B     BLDR52                                                           
BLDR53   MVC   0(1,R5),0(R4)       ACTION TO ACTLIST                            
BLDR54   CLI   0(R5),C' '                                                       
         BNE   BLDR57              AN ACTION HAS BEEN INPUT                     
         LA    R3,4                                                             
         LA    R2,LINACTH                                                       
         LA    RE,LINTYPEH                                                      
BLDR55   CLI   5(RE),0                                                          
         BE    BLDR56                                                           
         TM    4(RE),X'20'                                                      
         BO    BLDR56              DATA NOT CHANGED                             
         GOTO1 ANY                 DATA CHANGED MISSING ACTION                  
BLDR56   ZIC   R1,0(RE)                                                         
         AR    RE,R1                                                            
         BCT   R3,BLDR55                                                        
BLDR57   GOTO1 VALDATA                                                          
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                 ERROR IN VALIDATE - EXIT                     
         ZIC   R3,ELEMENT+1                                                     
         LTR   R3,R3                                                            
         BZ    BLDR60              NO INPUT - END OF EDIT                       
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),ELEMENT     MOVE ELEMENT TO NWBLOCK                      
         LA    R6,1(R3,R6)         NEXT AREA IN NWBLOCK                         
         LA    R5,1(R5)            NEXT BYTE IN ACTLIST                         
         LA    R7,LINLEN(R7)       NEXT LINE ON SCREEN                          
         ZIC   R3,NWBKLEN                                                       
         AH    R3,=H'1'                                                         
         STC   R3,NWBKLEN          SAVE NUMBER OF ELEMENTS                      
         B     BLDR50                                                           
         SPACE 1                                                                
BLDR60   DC    0H'0'                                                            
* DELETE ITEMS INPUT THIS TIME AS DELETE                                        
         GOTO1 RIDEL,DMCB,(C'D',NWBLOCK)                                        
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
* DELETE OLD ITEMS THAT WERE CHANGED                                            
         GOTO1 RIDEL,DMCB,(C'C',ELBLOCK)                                        
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         GOTO1 REMANEL,DMCB,(X'FF',0)                                           
         SPACE 1                                                                
* NOW ADD ALL NEW AND CHANGED ITEMS                                             
         LA    R5,NWBLOCK                                                       
         LA    R3,ACTLIST                                                       
BLDR65   CLI   0(R5),0             END OF BLOCK                                 
         BE    BLDR70                                                           
         XC    ELEMENT,ELEMENT                                                  
         ZIC   R2,1(R5)                                                         
         BCTR  R2,0                                                             
         EX    R2,*+8                                                           
         B     *+10                                                             
         MVC   ELEMENT(0),0(R5)                                                 
         CLI   0(R3),C'A'                                                       
         BE    *+12                                                             
         CLI   0(R3),C'C'                                                       
         BNE   BLDR67                                                           
         GOTO1 ADDANEL                                                          
BLDR67   LA    R3,1(R3)            NEXT ACTION                                  
         LA    R5,1(R2,R5)         NEXT ELEMENT IN NWBLOCK                      
         B     BLDR65                                                           
BLDR70   DC    0H'0'                                                            
         GOTO1 SORT,DMCB,IO2                                                    
         L     R4,FIRST52                                                       
         USING ACSALRYD,R4                                                      
BLDR71   CLI   0(R4),X'52'                                                      
         BNE   BLDR80              RECORD AMENDED GET OUT                       
         LR    R2,R4                                                            
         MVC   WORK(2),ACSALBEG                                                 
BLDR73   MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(0,WORK+3)                                  
         L     R6,=F'-1'                                                        
         GOTO1 ADDAY,DMCB,(1,WORK+3),(0,WORK+9),(R6)                            
         GOTO1 DATCON,DMCB,(0,WORK+9),(1,WORK)                                  
BLDR75   ZIC   R1,1(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),X'52'                                                      
         BNE   BLDR79                                                           
         CLC   ACSALTYP,ACSALTYP-ACSALRYD(R2)                                   
         BNE   BLDR75                                                           
         CLI   ACSALBAS,C'Y'                                                    
         BE    BLDR79                                                           
         DROP  R4                                                               
         USING ACSALRYD,R2                                                      
         OC    ACSALEND,ACSALEND                                                
         BZ    BLDR77              NO END DATE                                  
         CLC   ACSALEND,WORK       BEGIN LESS ONE OF LAST                       
         BNH   BLDR79                                                           
         B     DATERROR                                                         
BLDR77   CLC   ACSALBEG,WORK                                                    
         BH    DATERROR                                                         
         MVC   ACSALEND,WORK       END THIS ONE                                 
         MVC   WORK(2),ACSALBEG                                                 
BLDR79   ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     BLDR71                                                           
BLDR80   GOTO1 DISPLAY,DMCB,IO2    DISPLAY CHANGED RECORD                       
         LA    R2,LOGDPTH                                                       
         B     XIT                 ALL DONE                                     
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
*                                                                               
********  ROUTINE TO VALIDATE ELEMENT FIELDS AND BUILD ELEMENT  *****           
         SPACE                                                                  
VALDATA  NTR1                                                                   
         XC    ELEMENT,ELEMENT                                                  
         USING LINED,R7                                                         
         LA    R2,LINACTH                                                       
         LA    R3,5                                                             
VALD05   CLI   5(R2),0                                                          
         BNE   VALD08                                                           
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,VALD05                                                        
         B     XIT                 NO INPUT GET OUT                             
         DROP  R2                                                               
VALD08   LA    R4,ELEMENT                                                       
         USING ACSALRYD,R4                                                      
         LA    R2,LINTYPEH         *VALIDATE TYPE*                              
         MVI   ACSALTYP,X'50'      DEFAULT IS SALARY                            
         ZIC   R3,LINTYPEH+5                                                    
         LTR   R3,R3                                                            
         BZ    VALD20                                                           
         BCTR  R3,0                                                             
         LA    R5,TYPETBLE                                                      
VALD10   EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   1(0,R5),LINTYPE     MATCH TYPE TABLE TO SCREEN                   
         BE    VALD15                                                           
         LA    R5,5(R5)            NEXT TABLE ENTRY                             
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         B     VALD10                                                           
         SPACE                                                                  
VALD15   MVC   ACSALTYP,0(R5)      TYPE TO ELEMENT                              
         SPACE 1                                                                
VALD20   DC    0H'0'                                                            
         LA    R2,LINDATEH         *VALIDATE DATE*                              
         CLI   5(R2),0                                                          
         BE    MISERR                                                           
         GOTO1 VALDATE,DMCB,LINDATE                                             
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                 BAD DATE                                     
         MVC   ACSALBEG,FULL       START DATE                                   
         CLC   BYTE,LINDATEH+5                                                  
         BE    VALD30              NO END DATE                                  
         ZIC   R3,BYTE               LENGTH OF FIRST DATE                       
         LA    R5,LINDATE(R3)                                                   
         CLI   0(R5),C','                                                       
         BNE   INVERR                                                           
         LA    R5,1(R5)                                                         
         GOTO1 VALDATE,DMCB,(R5)    VALIDATE END DATE                           
         CLI   ERROR,X'FF'                                                      
         BNE   INVERR                                                           
         MVC   ACSALEND,FULL                                                    
         CLC   ACSALEND,ACSALBEG   END MUST BE GT BEGINNING DATE                
         BL    INVERR                                                           
         SPACE 1                                                                
VALD30   DC    0H'0'                                                            
         LA    R2,LINBASEH                                                      
         CLI   5(R2),0                                                          
         BE    MISERR                                                           
         ZIC   R3,LINBASEH+5                                                    
         BCTR  R3,0                                                             
         LA    R5,BASISTBL                                                      
VALD32   EX    R3,*+8              MATCH ACTION TO TABLE                        
         B     *+10                                                             
         CLC   LINBASE(0),0(R5)                                                 
         BE    VALD33                                                           
         LA    R5,3(R5)                                                         
         CLI   0(R5),X'FF'                                                      
         BE    INVERR                                                           
         B     VALD32                                                           
VALD33   DS    0H                                                               
         MVC   ACSALBAS,0(R5)      BASIS TO ELEMENTT                            
         CLI   ACSALTYP,X'08'                                                   
         BNE   VALD34                                                           
         CLI   ACSALBAS,C'A'                                                    
         BE    VALD40                                                           
         B     BASISERR                                                         
VALD34   CLI   ACSALTYP,X'07'                                                   
         BNE   VALD40                                                           
         CLI   ACSALBAS,C'H'                                                    
         BE    VALD40                                                           
         B     BASISERR                                                         
         SPACE 1                                                                
VALD40   LA    R2,LINRATEH                                                      
         CLI   5(R2),0             IS FIELD BLANK                               
         BE    MISERR              YES -- MISSING INPUT ERROR                   
         ZIC   R3,5(R2)            LENGTH OF RATE FIELD USED IN R3              
         BCTR  R3,0                REDUCE LENGTH BY 1                           
         LA    R5,LINRATE(R3)      POINT R5 PAST LAST DIGIT OF RATE             
         CLI   0(R5),X'6C'         IS THERE A PERCENT SIGN                      
         BE    VALD42                                                           
         LA    R3,1(R3)            INCREMENT R3 BY 1                            
         B     VALD49                                                           
VALD42   CH    R3,=H'3'            IS LENGTH OF RATE LESS THAN 3                
         BL    VALD48                                                           
         LR    R0,R3               LENGTH OF RATE INTO R0                       
         LA    R1,8(R2)            ADDR OF RATE INTO R1                         
         SPACE 1                                                                
VALD43   CLI   0(R1),C'.'          LOOK FOR DECIMAL PLACE                       
         BE    VALD44                                                           
         LA    R1,1(R1)            BUMP UP TO NEXT DIGIT IN RATE                
         BCT   R0,VALD43                                                        
         B     VALD48              NO DEC PT -- 2 IMPLIED                       
         SPACE 1                                                                
VALD44   LR    R1,R3               DECIMAL FOUND DELETE TRAILING 0              
         BCTR  R1,0                REDUCE BY 1                                  
         LA    R1,8(R1,R2)         POINT R1 PAST LAST DIGIT OF RATE             
         SPACE 1                                                                
VALD46   CLI   0(R1),C'0'          TRAILING ZEROS                               
         BNE   VALD48                                                           
         MVI   0(R1),C' '          REPLACE ZERO WITH BLANK                      
         BCTR  R3,0                FIX LENGTH FOR CASHVAL                       
         B     VALD46              GO LOOK FOR NEXT ZERO                        
         SPACE 1                                                                
VALD48   MVI   ACSALSTA,X'20'      TURN ON PERCENT BIT (2 DEC PLACE)            
VALD49   GOTO1 AMTVAL,DMCB,LINRATE,(R3),MAXAMT  *VALIDATE RATE*                 
         CLI   DMCB,X'FF'          FF=INVALID                                   
         BNE   VALD52                                                           
         SPACE 1                                                                
         CLI   0(R5),X'6C'         IS THERE A PERCENT SIGN                      
         BNE   VALD50                                                           
         MVI   ACSALSTA,X'10'      TURN ON PERCENT BIT (5 DEC PLACES)           
         GOTO1 AMTVAL,DMCB,(5,LINRATE),(R3),MAXAMT                              
         CLI   DMCB,X'FF'          FF=INVALID                                   
         BNE   VALD52                                                           
         SPACE 1                                                                
VALD50   B     INVERR              ***ERROR***                                  
         SPACE 1                                                                
VALD52   L     R5,4(R1)            A(RATE TO R5)                                
         LA    R5,0(R5)            RATE INTO DUB                                
         ZAP   DUB,0(8,R5)                                                      
         CLI   ACSALSTA,X'20'      IS IT A 2 DEC PCT                            
         BNE   VALD52A                                                          
         CP    DUB,=P'9900'        IS IT HIGHER THAN 99 PCT                     
         BH    VALD53              YES ERROR                                    
         B     VALD54                                                           
         SPACE 1                                                                
VALD52A  CLI   ACSALSTA,X'10'      IS IT A 5 DEC PCT                            
         BNE   VALD54              NO - ITS A DOLLAR AMOUNT OK                  
         CP    DUB,=P'9900000'     IS IT HIGHER THAN 99 PCT                     
         BNH   VALD54              NO - ITS VALID CONTINUE                      
         SPACE 1                                                                
VALD53   LA    R2,LINRATEH         SET CURSOR TO RATE                           
         MVC   LOGHEAD(21),=C'**ERROR - OVER 99% **'                            
         MVI   ERROR,X'FE'         I SET ERROR MESSAGE                          
         B     XIT                                                              
         SPACE                                                                  
VALD54   MVC   ACSALARY,DUB+2      MOVE INTO 52 EL                              
         CLI   ACSALBAS,C'Y'       IS IT YTD                                    
         BNE   VALDXIT                                                          
         OC    ACSALEND,ACSALEND                                                
         BNZ   VALDXIT                                                          
         LA    R2,LINDATEH                                                      
         B     NEEDEND              NEED END DATE ON YTD                        
VALDXIT  MVC   ACSALEL(2),=X'5211'                                              
         B     XIT                                                              
         EJECT                                                                  
***********************  ROUTINE TO DELETE A SALARY ELEMENT  *******            
         SPACE 1                                                                
*              P1  BYTE 0        C=CHANGED ITEMS  , D=DELETED ITEMS             
*                  BYTE 1-3      A(BLOCK OF ELEMENTS)                           
         USING ACSALRYD,R4                                                      
RIDEL    NTR1                                                                   
         L     R5,0(R1)            A(BLOCK OF ELEMENTS)                         
         MVC   BYTE,0(R1)          ACTION                                       
         LA    R2,LOGLINEH                                                      
         LA    R3,ACTLIST                                                       
RIDEL10  CLI   0(R5),0             END OF BLOCK                                 
         BE    XIT                                                              
         CLC   BYTE,0(R3)          MATCH ACTION                                 
         BNE   RIDELNXT                                                         
         LA    R4,IO2                                                           
         MVI   ELCODE,X'52'                                                     
         BAS   RE,GETEL                                                         
RIDEL15  BNE   NOTFND              ITEM NOT FOUND                               
         ZIC   R1,ACSALEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R5),ACSALEL     BLOCK VS. ELEMENT                            
         BE    RIDEL20                                                          
         BAS   RE,NEXTEL                                                        
         B     RIDEL15                                                          
         SPACE 1                                                                
RIDEL20  MVI   ACSALEL,X'FF'       DELETE ELEMENT                               
RIDELNXT ZIC   R1,1(R5)            NEXT ELEMENT IN BLOCK                        
         AR    R5,R1                                                            
         LA    R3,1(R3)            NEXT ACTION                                  
         LA    R2,LINLEN(R2)                                                    
         B     RIDEL10                                                          
********  MODULE TO SORT ELEMENTS AND VALIDATE START/END DATES  *******         
         SPACE 1                                                                
SORT     NTR1                                                                   
         L     R4,0(R1)            A(RECORD)                                    
         MVI   ELCODE,X'52'                                                     
         SR    R5,R5                                                            
         BAS   RE,GETEL                                                         
         BNE   XIT                                                              
         ST    R4,FIRST52                                                       
SORT20   AH    R5,=H'1'                                                         
         BAS   RE,NEXTEL                                                        
         BNE   SORT30                                                           
         B     SORT20                                                           
         SPACE 1                                                                
SORT30   CH    R5,=H'2'                                                         
         BL    XIT                                                              
         GOTO1 CALLOV,DMCB,0,X'D900A12'   =A(XSORT)                             
         L     RF,DMCB                                                          
         L     R6,FIRST52          START SORT HERE                              
         GOTO1 (RF),DMCB,(C'N',(R6)),(R5),17,5,8                                
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO VALIDATE DATES FOR MMM/YY                             
VALDATE  NTR1                                                                   
         L     R5,0(R1)                                                         
         GOTO1 DATVAL,DMCB,(0,0(R5)),WORK  VALIDATE FOR M/D/Y                   
         CLI   DMCB+3,0            VALID FOR M/D/Y=INVALID FOR PGM              
         BE    VALDT2                                                           
         B     INVDTE                                                           
         SPACE                                                                  
VALDT2   GOTO1 DATVAL,DMCB,(2,0(R5)),WORK  VALIDATE FOR M/Y                     
         CLI   DMCB+3,0                                                         
         BNE   VALDT3                                                           
         B     INVDTE                                                           
         SPACE                                                                  
VALDT3   MVC   BYTE,DMCB+3                                                      
         GOTO1 DATCON,DMCB,(0,WORK),(1,FULL)  YYMMDD TO YMD(P)                  
         B     XIT                                                              
         EJECT                                                                  
*              R4 AND R2 POINT TO ELEMENTS IN QUESTION                          
DATERROR DC    0H'0'                                                            
         GOTO1 FNDIT,DMCB,(R4),(R2)                                             
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         GOTO1 FNDIT,DMCB,(R2),(R4)                                             
         CLI   ERROR,X'FF'                                                      
         BNE   XIT                                                              
         LA    R2,LOGLINEH                                                      
         B     CONFLCT                                                          
         EJECT                                                                  
*              FIND CONFLICTING ITEM IN NEW BLOCK                               
FNDIT    NTR1                                                                   
         LM    R4,R5,0(R1)         OVERLAPPING ELEMENTS                         
         LA    R6,NWBLOCK                                                       
         LA    R3,ACTLIST                                                       
         LA    R7,LOGLINEH                                                      
FNDIT10  CLI   0(R6),0                                                          
         BE    XIT                 END OF BLOCK - NO MATCH                      
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R6),0(R4)       MATCH ITEM TO NWBLOCK                        
         BNE   FNDNXT                                                           
         CLI   0(R3),C'D'          IGNORE DELETED                               
         BE    FNDNXT                                                           
         LA    R2,LINDATEH         CURSOR TO DATE FIELD ON THIS LINE            
         LR    R4,R5                                                            
         USING ACSALRYD,R4                                                      
         MVC   WORK(2),ACSALBEG                                                 
         MVI   WORK+2,1                                                         
         GOTO1 DATCON,DMCB,(1,WORK),(6,LOGHEAD+30)                              
         OC    ACSALEND,ACSALEND                                                
         BZ    CONFLCT                                                          
         MVI   LOGHEAD+36,C'-'                                                  
         MVC   WORK(2),ACSALEND                                                 
         GOTO1 DATCON,DMCB,(1,WORK),(6,LOGHEAD+37)                              
         B     CONFLCT                                                          
FNDNXT   LA    R6,1(R1,R6)         NEXT ITEM IN NWBLOCK                         
         LA    R3,1(R3)            NEXT ACTION                                  
         LA    R7,LINLEN(R7)       NEXT LINE ON SCREEN                          
         B     FNDIT10                                                          
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CHECK LOW LEVEL OF 1R                                                  
*-------------------------------------------------------------------*           
CHKPRSN  NTR1                                                                   
         CLI   LOGSTFH+5,0                                                      
         BE    XIT                                                              
         NI    BIT,X'FF'-NEWCOST                                                
         MVC   SAVEKEY,KEY                                                      
         MVC   KEY,SPACES                                                       
         MVI   KEY,X'0F'                                                        
         MVC   KEY+1(1),COMPANY                                                 
         GOTO1 HIGH                                                             
         CLI   KEY,X'0F'                                                        
         BNE   CHKP10                                                           
         CLC   KEY+1(1),COMPANY                                                 
         BNE   CHKP10                                                           
         OI    BIT,NEWCOST         AGY ON NEWCOST                               
         MVC   LOGHEAD(39),=C'**ERR MUST ADD RECORDS AT PERSON SCREEN'          
         CLI   LOGACT,C'N'                                                      
         BE    CHKP05                                                           
         MVC   KEY,SAVEKEY                                                      
         GOTO1 READ                                                             
         MVC   LOGHEAD(40),=C'**ERR MUST MAKE CHANGES AT PERSON SCREEN'         
CHKP05   MVI   LOGHEADH+6,X'80'                                                 
         MVI   ERROR,X'FE'                                                      
         B     XIT                                                              
*                                                                               
CHKP10   DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         CLI   LOGACT,C'N'                                                      
         BE    XIT                                                              
         GOTO1 READ                                                             
         B     XIT                                                              
         EJECT                                                                  
*-------------------------------------------------------------------*           
*        CHECK IF OVERHEAD ACCOUNT                                              
*-------------------------------------------------------------------*           
CHKOVER  NTR1                                                                   
         MVI   ERROR,0                                                          
         LA    R4,SAVLEDG              CHECK IF AMENDING/ADDING LOW             
         USING ACHEIRD,R4              LEVEL ACCOUNT                            
**T      LA    R2,LOGDPTH                                                       
*                                                                               
         ZIC   R5,ACHRLEVA             LENGTH FOR COMPARE                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   LOGDPT(0),=12CL1'9'                                              
         BNE   *+8                                                              
         MVI   ERROR,X'FF'             INDICATE OVERHEAD ACCOUNT                
*                                                                               
         ZIC   R6,ACHRLEVA             DISPLACEMENT INTO ACCT FOR COMP          
         ZIC   R5,ACHRLEVB             LENGTH FOR COMPARE                       
         SR    R5,R6                   LENGTH FOR COMPARE                       
         LA    R3,LOGDPT                                                        
         AR    R3,R6                   BUMP DISPLACEMENT                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   0(0,R3),=12CL1'9'                                                
         BNE   *+8                                                              
         MVI   ERROR,X'FF'             INDICATE OVERHEAD ACCOUNT                
*                                                                               
         DS    0H                                                               
         ZIC   R5,ACHRLEVC             LENGTH FOR COMPARE                       
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         CLC   LOGSBD(0),=12CL1'9'                                              
         BNE   *+8                                                              
         MVI   ERROR,X'FF'             INDICATE OVERHEAD ACCOUNT                
*                                                                               
         CLC   LOGSTF(3),=12CL1'9'     MUST BE LEVEL D ACCOUNT                  
         BNE   *+8                                                              
         MVI   ERROR,X'FF'                                                      
         B     XIT                                                              
*                                                                               
         EJECT                                                                  
*------------------------------------------------------------------*            
*        GSTTAB  TABLE OF VALID GSTCODES                                        
*------------------------------------------------------------------*            
GSTTAB   DS    0C                                                               
         DC    C'S'                                                             
         DC    C'X'                                                             
         DC    C'Z'                                                             
*                                                                               
MISERR   MVI   ERROR,MISSING                                                    
         B     XIT                                                              
*                                                                               
INVERR   MVI   ERROR,INVALID                                                    
         B     XIT                                                              
*                                                                               
INVDTE   MVI   ERROR,DATERR                                                     
         B     XIT                                                              
*                                                                               
RECHANGE GOTO1 DISPLAY,DMCB,IO                                                  
         LA    R3,MSG1L                                                         
         LA    R2,LOGLINEH                                                      
         B     ERRMSG                                                           
*                                                                               
CONFLCT  LA    R3,MSG2L                                                         
         B     ERRMSG                                                           
*                                                                               
NOTFND   LA    R3,MSG3L                                                         
         B     ERRMSG                                                           
*                                                                               
NEEDEND  LA    R3,MSG4L                                                         
         B     ERRMSG                                                           
*                                                                               
HIREERR  LA    R3,MSG5L                                                         
         B     ERRMSG                                                           
*                                                                               
STENDERR LA    R3,MSG6L                                                         
         B     ERRMSG                                                           
*                                                                               
BASISERR LA    R3,MSG7L                                                         
         B     ERRMSG                                                           
*                                                                               
ONERANGE LA    R3,MSG8L                                                         
         B     ERRMSG                                                           
*                                                                               
NOCROSS  LA    R3,MSG9L                                                         
         B     ERRMSG                                                           
*                                                                               
**********  ROUTINE TO OUTPUT ERROR MESSAGES  ************************          
         SPACE                                                                  
ERRMSG   DS    0H                  R3=A(MSG)                                    
         MVI   ERROR,X'FE'                                                      
         XR    R4,R4                                                            
         IC    R4,0(R3)                                                         
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     XIT                                                              
         MVC   LOGHEAD(0),1(R3)                                                 
XIT      XIT1  REGS=(R2)                                                        
*                                                                               
         GETEL R4,DATADISP,ELCODE                                               
         EJECT                                                                  
WS       DS    0D                                                               
*****                  TABLE OF ERROR MESSAGES                                  
MSG1L    DC    AL1(L'MSG1)                                                      
MSG1     DC    C'RECORD DISPLAYED * RE-ENTER CHANGES'                           
MSG2L    DC    AL1(L'MSG2)                                                      
MSG2     DC    C'*** ERROR - CONFLICTING DATES'                                 
MSG3L    DC    AL1(L'MSG3)                                                      
MSG3     DC    C'*** ERROR - ITEM NOT FOUND CHECK INPUT'                        
MSG4L    DC    AL1(L'MSG4)                                                      
MSG4     DC    C'*** ERROR - END DATE REQUIRED FOR YTD'                         
MSG5L    DC    AL1(L'MSG5)                                                      
MSG5     DC    C'*** ERROR - HIRE DATE REQUIRED IF TERM DATE INPUT'             
MSG6L    DC    AL1(L'MSG6)                                                      
MSG6     DC    C'*** ERROR - TERM DATE BEFORE HIRE DATE'                        
MSG7L    DC    AL1(L'MSG7)                                                      
MSG7     DC    C'*** ERROR - INVALID BASIS FOR THIS TYPE'                       
MSG8L    DC    AL1(L'MSG8)                                                      
MSG8     DC    C'*** ERROR-ONLY ONE DATE RANGE PER FISCAL YEAR ALLOWED'         
MSG9L    DC    AL1(L'MSG9)                                                      
MSG9     DC    C'*** ERROR - CANNOT CROSS OVER FISCAL YEARS'                    
         DS    0H                                                               
TYPETBLE DC    X'50',CL4'SAL '                                                  
         DC    X'40',CL4'OT  '                                                  
         DC    X'30',CL4'TEMP'                                                  
         DC    X'20',CL4'BON '                                                  
         DC    X'15',CL4'PEN '                                                  
         DC    X'10',CL4'BEN '                                                  
         DC    X'09',CL4'ADMN'                                                  
         DC    X'08',CL4'BUD '                                                  
         DC    X'07',CL4'RTE '                                                  
         DC    X'FF'                                                            
         SPACE 1                                                                
BASISTBL DC    C'MON'                                                           
         DC    C'QTR'                                                           
         DC    C'YTD'                                                           
         DC    C'AN '                                                           
         DC    C'HR '                                                           
         DC    X'FF'                                                            
         SPACE 1                                                                
ACTABLE  DC    CL3'ADD'                                                         
         DC    CL3'CHA'                                                         
         DC    CL3'DEL'                                                         
         DC    X'FF'                                                            
         SPACE 1                                                                
*                                                                               
         DS    0H                                                               
PATCH    DC    64X'00'                                                          
         EJECT                                                                  
*                                                                               
*                                                                               
PED      DSECT                                                                  
SCANNER  DS    V                                                                
UNSCAN   DS    V                                                                
ADDAY    DS    V                                                                
AMTVAL   DS    V                                                                
PERVAL   DS    V                                                                
PRELOC   DS    F                                                                
*                                                                               
         EJECT                                                                  
LINED    DSECT                     DSECT FOR SALARY ELEMENT LINE                
LINACTH  DS    CL8                                                              
LINACT   DS    CL3                                                              
LINTYPEH DS    CL8                                                              
LINTYPE  DS    CL4                                                              
LINDATEH DS    CL8                                                              
LINDATE  DS    CL13                                                             
LINBASEH DS    CL8                                                              
LINBASE  DS    CL3                                                              
LINRATEH DS    CL8                                                              
LINRATE  DS    CL13                                                             
LINLEN   EQU   *-LINACTH                                                        
         SPACE 1                                                                
MONTABD  DSECT                                                                  
MONCHAR  DS    CL3                 CHARACTER MONTH                              
MONFISC  DS    XL1                 CORRESPONDING FISCAL MONTH                   
MONLNQ   EQU   *-MONTABD                                                        
         SPACE 2                                                                
         EJECT                                                                  
       ++INCLUDE ACLFMWORK                                                      
       ++INCLUDE ACLFMFFD                                                       
         ORG   LOGTABH                                                          
       ++INCLUDE ACLFMEAD                                                       
ELBKLEN  DS    CL1                                                              
ELBLOCK  DS    CL180                                                            
FIRST52  DS    F                   A(FIRST SALARY ELEMENT)                      
         SPACE 1                                                                
SAVLEDG  DS    CL100                                                            
ELCODE   DS    CL1                                                              
SUBDISP  DS    CL1                                                              
STFDISP  DS    CL1                                                              
AREC     DS    A                                                                
NUMLIN   DS    CL1                                                              
NWBKLEN  DS    CL1                                                              
NWBLOCK  DS    CL180                                                            
ACTLIST  DS    CL10                                                             
START    DS    CL2                                                              
ENDDATE  DS    CL2                                                              
BYTE     DS    CL1                                                              
MAXAMT   DS    PL8                                                              
STDATE   DS    XL2                                                              
ENDATE   DS    XL2                                                              
SAVEKEY  DS    CL42                                                             
PVALBLK  DS    CL100               PERVAL OUTPUT BLOCK                          
SVPROFSH DS    CL8                 HEADER FOR SCANNER                           
SVPROFS  DS    CL54                SAVED ACCOUNT PROFILE LINE                   
BIT      DS    XL1                 STATUS BYTE                                  
NEWCOST  EQU   X'80'               AGY ON NEWCOST                               
*                                                                               
*                                                                               
*                                                                               
*ACLFMEQU                                                                       
*DDFLDIND                                                                       
*FATWA                                                                          
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACLFMEQU                                                       
       ++INCLUDE DDFLDIND                                                       
       ++INCLUDE FATWA                                                          
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENFILE                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE DDPERVALD                                                      
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'068ACLFM15   05/01/02'                                      
         END                                                                    
