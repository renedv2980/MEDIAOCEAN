*          DATA SET ACSCR1C    AT LEVEL 125 AS OF 09/02/15                      
*PHASE T60C1CA,+0                                                               
***********************************************************************         
*                                                                     *         
* THIS VERSION MATCHES THE UK VERSION LEVEL 124 AS OF 12/16/10        *         
*                                                                     *         
***********************************************************************         
*                                                                               
         TITLE 'Extract Header/Trailer entries'                                 
T60C1C   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,0C1C,RA,R8,CLEAR=YES,RR=RE                                     
*                                                                               
         USING TWAD,R5                                                          
         USING WORKD,R7                                                         
         USING LWSD,RC                                                          
         L     RC,APALOCAL         4K AREA                                      
*                                                                               
         ST    RE,APRELO                                                        
         ST    RB,APBASE1                                                       
         ST    RA,APBASE2                                                       
         ST    RD,APWORKA                                                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         MVC   FVXTRA,SPACES                                                    
         EJECT ,                                                                
*                                                                               
         USING RESRECD,R2                                                       
INIT40   LA    R2,IOKEY                                                         
         SR    RF,RF                                                            
         IC    RF,APMODE                                                        
         SLL   RF,2                                                             
         B     *(RF)                                                            
*                                                                               
         B     VALKEY                                                           
         B     VALREC                                                           
         B     DISKEY                                                           
         B     DISREC                                                           
         B     EXIT                DELETE                                       
         B     EXIT                RESTORE                                      
         B     EXIT                VALSEL                                       
         B     EXIT                GETSEL                                       
         B     EXIT                DISSEL                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                LSTSCR                                       
         B     EXIT                VALREQ                                       
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                                                             
         B     EXIT                COPY                                         
         B     EXIT                                                             
         B     EXIT                                                             
*                                                                               
EXIT     CLI   APPFKEY,PFKNEXT                                                  
         BE    EXIT97                                                           
         CLI   APMODE,APMVALK                                                   
         BE    EXIT95                                                           
         CLI   APMODE,APMDISK                                                   
         BE    EXIT95                                                           
         TM    TWASWPST,TWASWAP    SHOULD WE SWAP?                              
         BZ    EXIT95              NO                                           
*                                                                               
         CLI   APPFKEY,PFKHLP                                                   
         BNE   *+10                                                             
         XC    ACURDEF,ACURDEF     SET TO BEGINING OF HELP                      
         XC    APCURSOR,APCURSOR   DON'T SET CURSOR ON WRONG SCREEN             
         MVI   APPFKEY,0                                                        
         MVI   APMODE,APMSWP                                                    
         MVC   APPARM(1),TWASWPRE          SWAP TO NEW RECORD                   
         MVC   APPARM+1(1),TWASWPAC        SWAP TO NEW ACTION                   
*                                                                               
EXIT95   OI    TWALSCTL,TWALSHLD                                                
*                                                                               
EXIT97   OI    TWALSCTL,TWALSRTN                                                
*                                                                               
EXIT99   CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    XIT                                                              
         CLI   APACTN,ACTCHA                                                    
         BNE   XIT                                                              
*                                                                               
XIT      XIT1                                                                   
         EJECT ,                                                                
***********************************************************************         
*  VALKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING KEYWRD,R6                                                        
VALKEY   L     R6,=A(VALIDKYW)     RESET TO NO                                  
         A     R6,APRELO                                                        
VALKEY02 CLI   KEYESC,EOT          End of table                                 
         BE    VALKEY04                                                         
         MVC   KEYWORD,KEYESC                                                   
         GOTO1 VDICTAT,APPARM,C'SU  ',KEYWORD,0                                 
         LA    R6,KEYWLNQ(,R6)                                                  
         B     VALKEY02                                                         
         DROP  R6                                                               
*                                                                               
VALKEY04 MVI   NEWKEY,NO           RESET TO NO                                  
         MVI   SEQNUM,1                                                         
         MVC   RESKEY,SPACES                                                    
         MVI   RESKTYP,RESKTYPQ    X'2D'                                        
         MVI   RESKSUB,RESKSUBQ    X'02'                                        
         MVC   RESKCPY,CUABIN      COMPANY CODE                                 
*                                                                               
         TM    TWAMODE,TWAMLSM                                                  
         BO    VALKEY05                                                         
         TM    EXTCODEH+4,FVITHIS  NEW FORMAT INPUT?                            
         BO    VALKEY05                                                         
         CLC   SAVFORM,EXTCODE                                                  
         BE    VALKEY05                                                         
         MVI   CURRCOLN,0                                                       
         MVC   EXTCODE,SAVFORM                                                  
         OI    EXTCODEH+6,FVOXMT                                                
*                                                                               
VALKEY05 GOTO1 AFVAL,EXTCODEH                                                   
         MVC   RESKFORM,FVIFLD                                                  
         BE    VALKEY15                                                         
         TM    EXTCODEH+4,FVITHIS  ANY INPUT?                                   
         BZ    *+10                                                             
         MVC   SAVFORM,SPACES                                                   
         CLC   SAVFORM,SPACES                                                   
         BNH   IVALEKEY            ENTER KEY                                    
         MVC   EXTCODE,SAVFORM                                                  
         OI    EXTCODEH+6,FVOXMT   TRANSMIT                                     
         MVC   RESKFORM,SAVFORM                                                 
*                                                                               
VALKEY15 MVC   SAVFORM,RESKFORM                                                 
         TM    TWAMODE,TWAMLSM                                                  
         BO    VALKEY18                                                         
         CLC   SAVFORM,FVIFLD                                                   
         BE    *+10                                                             
         XC    SAVRECK,SAVRECK                                                  
*                                                                               
VALKEY18 MVC   APRECKEY(L'RESKEY),RESKEY                                        
         LA    R1,IORD+IOACCFIL+IO1                                             
         CLI   APACTN,ACTDIS                                                    
         BE    *+8                                                              
         LA    R1,IOLOCK(,R1)      READ FOR UPDATE                              
         GOTO1 AIO                                                              
         BL    VALKEY99            IO ERROR                                     
         BNE   VALKEY99                                                         
         L     R2,AIOAREA1                                                      
         GOTO1 GETTYPE,(R2)                                                     
         OI    SCRTYPH+6,FVOXMT                                                 
         MVC   SCRTYP(L'APREPCDE),APREPCDE                                      
         MVI   APINDS,APIOKDIS+APIOKCHA                                         
*                                                                               
VALKEY91 CLC   APRECKEY(L'RESKEY),SAVRECK                                       
         BE    VALKEY98                                                         
         OI    INFLAG1,INFDISR                                                  
*                                                                               
VALKEY98 MVC   FVMSGNO,=AL2(FVFOK)                                              
*                                                                               
VALKEY99 B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  VALREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
         USING RFLELD,R9                                                        
VALREC   L     R2,AIOAREA1                                                      
         GOTO1 AFVAL,EXTNMEH                                                    
         BNE   VREC005                        Name has not been input           
         GOTO1 ADDNAME,APPARM,(R2),EXTNMEH    Get format name                   
         BNE   VREC999                        On error exit                     
***********************************************************************         
*        Header / Total trailer / Trialer                             *         
***********************************************************************         
                                                                                
VREC005  MVC   RESKEY,APRECKEY                                                  
         MVI   APELCODE,XTRELQ     X'C6' element                                
         GOTO1 DELEL,(R2)                                                       
*                                                                               
         GOTO1 SCR2ELM,APPARM,EXTHDR1H,('XTRHDR',1)                             
         BH    VREC999                                                          
         BL    VREC020                                                          
         GOTO1 ADDEL,(R2)                                                       
         BNE   VREC999                                                          
*                                                                               
VREC020  MVC   RESKEY,APRECKEY                                                  
         GOTO1 SCR2ELM,APPARM,EXTHDR2H,('XTRHDR',2)                             
         BH    VREC999                                                          
         BL    VREC030                                                          
         GOTO1 ADDEL,(R2)                                                       
         BNE   VREC999                                                          
*                                                                               
VREC030  MVC   RESKEY,APRECKEY                                                  
         GOTO1 SCR2ELM,APPARM,EXTHDR3H,('XTRHDR',3)                             
         BH    VREC999                                                          
         BL    VREC040                                                          
         GOTO1 ADDEL,(R2)                                                       
         BNE   VREC999                                                          
*                                                                               
VREC040  GOTO1 AFVAL,EXTNUMH                                                    
         BE    VREC120                                                          
         CLC   EXTTT1,SPACES                                                    
         BH    IVALIPUT            Need row or column value                     
         B     VREC150             Nothing expected                             
*                                                                               
VREC120  LA    R3,XTRRTOT          Row total type                               
         MVI   APBYTE,RRWELQ       X'C2' row element code                       
         CLC   APROWCHR,FVIFLD     Row                                          
         BE    VREC122                                                          
         LA    R3,XTRCTOT          Column total type                            
         MVI   APBYTE,RCLELQ       X'C3' col element code                       
         CLC   APCOLCHR,FVIFLD     Column                                       
         BNE   IVALIPUT            Need to specify                              
*                                                                               
VREC122  SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         CHI   RF,2                Must be at least 2, max 3                    
         BL    IVALIPUT            Need to have a number                        
*                                                                               
         BCTR  RF,0                Less one for character (Row / Col)           
         LA    RE,FVIFLD+1                                                      
VREC124  CLI   0(RE),C'0'                                                       
         BL    IVALIPUT                                                         
         LA    RE,1(,RE)           Next number if any                           
         BCT   RF,VREC124                                                       
*                                                                               
         IC    RF,FVXLEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,FVIFLD+1(0)                                                
         CVB   R4,APDUB                                                         
         LTR   R4,R4                                                            
         BZ    IVALIPUT                                                         
***********************************************************************         
* still need to Validate row or column number against what exists               
***********************************************************************         
*                                                                               
         LR    RE,R2                                                            
         AH    RE,DATADISP                                                      
VREC130  CLI   0(RE),EOR           End of record                                
         BE    IVALIPUT                                                         
         CLC   APBYTE,0(RE)        Match on row or col element                  
         BNE   VREC132                                                          
         CLM   R4,1,2(RE)                                                       
         BE    VREC135                                                          
*                                                                               
VREC132  SR    RF,RF                                                            
         IC    RF,1(,RE)                                                        
         AR    RE,RF                                                            
         B     VREC130                                                          
*                                                                               
VREC135  GOTO1 SCR2ELM,APPARM,EXTTT1H,((R3),(R4))                               
         BH    VREC999                                                          
         BL    IVALIPUT                                                         
         GOTO1 ADDEL,(R2)                                                       
         BNE   VREC999                                                          
*                                                                               
VREC150  GOTO1 SCR2ELM,APPARM,EXTTRLH,('XTRTRL',0)                              
         BH    VREC999                                                          
         BL    VREC400                                                          
         GOTO1 ADDEL,(R2)                                                       
         BNE   VREC999                                                          
*                                                                               
VREC400  LA    R1,IOADD+IOACCFIL+IO1                                            
         TM    APINDS,APIOKADD           ADD A RECORD?                          
         BO    VREC480                                                          
         LA    R1,IOWRITE+IOACCFIL+IO1                                          
         TM    APINDS,APIOKCHA           CHANGE A RECORD?                       
         BO    VREC480                                                          
         DC    H'00'                     WHAT THE HELL?                         
                                                                                
VREC480  GOTO1 AIO                                                              
         BE    VREC999                                                          
         TM    IOERR,IOEDUP        DOES RECORD EXIST BUT DELETED                
         BO    *+6                                                              
         DC    H'00'               BAD WRITE OR SOMETHING DUDE                  
         MVC   FVMSGNO,=AL2(FVFCCDR)                                            
                                                                                
VREC999  CLC   FVMSGNO,=AL2(FVFOK)                                              
         BE    DISREC                                                           
         B     EXIT                                                             
         DROP  R9                                                               
         EJECT ,                                                                
***********************************************************************         
*        Left justify what is in FVIFLD                               *         
***********************************************************************         
                                                                                
LEFTJFY  ST    RE,SVRE                                                          
         SR    RF,RF                                                            
         IC    RF,FVILEN           Length of data                               
         LA    RE,FVIFLD                                                        
         CLI   0(RE),C' '                                                       
         BH    LEFTJXIT            Already left justified                       
*                                                                               
LEFTJ10  CLI   0(RE),C' '          Find first real char                         
         BH    LEFTJ20             Found                                        
         LA    RE,1(,RE)           Next field                                   
         BCT   RF,LEFTJ10                                                       
         B     LEFTJXIT            No data                                      
*                                                                               
LEFTJ20  BCTR  RF,0                                                             
         EXMVC RF,FVIFLD,0(RE)                                                  
         STC   RF,FVXLEN           Save new EX length                           
         LA    RE,1(,RF)                                                        
         IC    RF,FVILEN           Old length                                   
         STC   RE,FVILEN           Save new length                              
         SR    RF,RE                                                            
         BCTR  RF,0                                                             
         LA    RE,FVIFLD(RE)       Point to end of data                         
         EXMVC RF,0(RE),SPACES                                                  
                                                                                
LEFTJXIT L     RE,SVRE                                                          
         BR    RE                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISKEY                                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING RESRECD,R2                                                       
DISKEY   LA    R2,APRECKEY                                                      
         MVC   EXTCODE,RESKFORM                                                 
         NI    TWASWPST,TURNOFF-TWASWAP                                         
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
*  DISREC                                                             *         
***********************************************************************         
         SPACE 1                                                                
PRO      USING RPFELD,PROFELEM                                                  
*                                                                               
         USING RESRECD,R2                                                       
DISREC   DS    0H                                                               
         L     R5,ATWA             Reload - A(Screen base)                      
         L     R2,AIOAREA1                                                      
         TWAXC EXTNMEH,EXTENDH                                                  
         GOTO1 GETNAME,APPARM,(R2),EXTNMEH                                      
         GOTO1 GETPER,APPARM,(R2),EXTOWNH                                       
                                                                                
***********************************************************************         
*        Column heading 1                                                       
***********************************************************************         
         USING XTRELD,R3                                                        
         LR    R3,R2                                                            
         AH    R3,DATADISP                                                      
DREC050  CLI   0(R3),EOR           End of record ?                              
         BE    DREC900                                                          
         CLI   0(R3),XTRELQ        Extract element                              
         BNE   DREC300                                                          
*                                                                               
DREC100  CLI   XTRTYPE,XTRHDR      Header ?                                     
         BNE   DREC120                                                          
         CLI   XTRNUM,1                                                         
         BNE   *+8                                                              
         LA    R4,EXTHDR1H         Header line                                  
         CLI   XTRNUM,2                                                         
         BNE   *+8                                                              
         LA    R4,EXTHDR2H         Header line                                  
         CLI   XTRNUM,3                                                         
         BNE   *+8                                                              
         LA    R4,EXTHDR3H         Header line                                  
*                                                                               
DREC120  MVC   APWORK,SPACES                                                    
         SR    RF,RF                                                            
         IC    RF,XTRNUM           Row or column number                         
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'                                                    
         UNPK  APWORK(1),APDUB                                                  
         CLI   XTRNUM,10                                                        
         BL    *+10                                                             
         UNPK  APWORK(2),APDUB                                                  
*                                                                               
         CLI   XTRTYPE,XTRRTOT     ROW Total Trailer                            
         BNE   DREC122                                                          
         LA    R4,EXTTT1H          Total trailer line                           
         MVC   EXTNUM(1),APROWCHR                                               
         MVC   EXTNUM+1(2),APWORK                                               
*                                                                               
DREC122  CLI   XTRTYPE,XTRCTOT     COL Total Trailer                            
         BNE   DREC124                                                          
         LA    R4,EXTTT1H          Total trailer line                           
         MVC   EXTNUM(1),APCOLCHR                                               
         MVC   EXTNUM+1(2),APWORK                                               
*                                                                               
DREC124  CLI   XTRTYPE,XTRTRL      Trailer                                      
         BNE   *+8                                                              
         LA    R4,EXTTRLH          Trailer line                                 
         OI    6(R4),FVOXMT        Re-Transmit                                  
         GOTO1 ELM2SCR,APPARM,(R3),(R4)                                         
*                                                                               
DREC300  SR    RF,RF                                                            
         IC    RF,1(,R3)                                                        
         AR    R3,RF                                                            
         B     DREC050                                                          
*                                                                               
DREC900  DS    0H                                                               
         DROP  R3                                                               
*                                                                               
DRXIT    CLI   APACTN,ACTCHA                                                    
         BE    EXIT                                                             
*        LA    RE,COLNUMH          PLACE CURSOR ON COLUMN NUMBER                
*        ST    RE,APCURSOR                                                      
*        SR    RE,RE               SET CONCODE TO YES                           
         B     EXIT                                                             
         DROP  R2                                                               
         EJECT ,                                                                
         USING XTRELD,R9                                                        
SCR2ELM  NTR1                                                                   
         L     R4,0(,R1)                                                        
         LR    R2,R1               Save parameter address                       
         MVC   FVMSGNO,=AL2(FVFOK)                                              
         XC    APELEM,APELEM                                                    
         LA    R9,APELEM                                                        
         MVI   XPANDFLD,C' '                                                    
         MVC   XPANDFLD+1(L'XPANDFLD-1),XPANDFLD                                
*                                                                               
         GOTO1 AFVAL,(R4)                                                       
         BNE   SCR2LOW                                                          
         MVI   XTREL,XTRELQ        X'C6' extract element                        
         MVI   XTRLN,XTRLNQ        Length default                               
         MVC   XTRTYPE,4(R2)       Get type                                     
         MVC   XTRNUM,7(R2)        Get row / col # / hdr #                      
         LA    R3,XTRSUBEL                                                      
*                                                                               
         USING XTRSUBEL,R3         Sub-element                                  
         SR    RF,RF                                                            
         IC    RF,FVXLEN           Ex length of data                            
         EXMVC RF,XPANDFLD,FVIFLD                                               
         LA    R6,XPANDFLD+1(RF)   Point to end                                 
         MVI   0(R6),C'+'                                                       
         ST    R6,XPANDEND         Save off end location                        
         IC    RF,0(,R4)                                                        
         AR    R4,RF               Point to 2nd field                           
*                                                                               
         GOTO1 AFVAL,(R4)                                                       
         BNE   SCR2E010                                                         
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EXMVC RF,0(R6),FVIFLD     Append 2nd field to 1st                      
         LA    R6,1(RF,R6)         Point to end                                 
         MVI   0(R6),C'+'                                                       
         ST    R6,XPANDEND         Save off end location                        
*                                                                               
SCR2E010 LA    RF,XPANDFLD                                                      
         SR    R6,RF               Figure out length of string                  
         STC   R6,FVILEN                                                        
         BCTR  R6,0                                                             
         STC   R6,FVXLEN                                                        
*                                                                               
         LA    R6,XPANDFLD                                                      
         XC    ALASTSUB,ALASTSUB   Last text sub-element                        
         MVI   CONCAT,NO           Not concatenated string                      
         XC    ERRIND,ERRIND                                                    
         XC    ELEMENT,ELEMENT                                                  
         MVI   ELEMENT+C'+',X'FF'  Concatenation symbol                         
         MVI   LASTTIME,NO                                                      
*                                                                               
SCR2E050 SR    R1,R1                                                            
         SR    RF,RF                                                            
         IC    RF,FVILEN           Length of string to scan                     
         EX    RF,*+8                                                           
         B     *+10                                                             
         TRT   0(0,R6),ELEMENT                                                  
*                                                                               
SCR2E051 ST    R1,LASTPLUS         R1 = location of C'+'                        
         SR    R1,R6                                                            
         STC   R1,STRLEN           Save string length                           
         LR    RF,R1                                                            
         BP    SCR2E054                                                         
         BZ    *+6                 1st char was a C'+'                          
         DC    H'00'               Can't be negative                            
*                                                                               
         MVI   STRLEN,1            Save string length                           
         LA    RF,1                Default to length 1                          
         LA    R1,1(,R6)                                                        
         ZIC   R5,FVILEN           Case of multiple C'+' i.e. ++++              
*                                                                               
SCR2E052 C     R1,XPANDEND         At end of string                             
         BNE   SCR2E300            Yes                                          
         MVI   LASTTIME,YES                                                     
         B     SCR2E300                                                         
*                                                                               
SCR2E053 CLI   0(R1),C'+'          Find 1st non-C'+' or end                     
         BNE   SCR2E300                                                         
         ST    R1,LASTPLUS                                                      
         LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)           Count size of string                         
         STC   RF,STRLEN           Save string length                           
         BCT   R5,SCR2E052                                                      
         B     SCR2E300            No length, C'+' treat as text data           
*                                                                               
SCR2E054 CHI   RF,11               Maximum length of keyword or row/col         
         BH    SCR2E300            Must be text data                            
         CHI   RF,1                Minimum length of keyword or row/col         
         BE    SCR2E300            Must be text data                            
*                                                                               
         LA    RE,0(RF,R6)         Point to end                                 
         BCTR  RE,0                Look at last character                       
         CLC   APCLSPRN,0(RE)      Check for closed parrenthisis                
         BE    SCR2E056            Yes                                          
         CHI   RF,7                Min. len for &Keyword                        
         BH    SCR2E300            To large, must be text data                  
         B     SCR2E065            Ok so far                                    
*                                                                               
SCR2E056 CHI   RF,5                Min. len for keyword or row/col w/()         
         BL    SCR2E300            Must be text data                            
         LA    R1,2                Number of 1 to 99                            
         SHI   RE,3                                                             
         CLC   APOPNPRN,0(RE)      Find open parrenthisis                       
         BE    SCR2E058                                                         
         LA    R1,1                Number 1 to 9                                
         LA    RE,1(,RE)                                                        
         CLC   APOPNPRN,0(RE)      Find open parrenthisis                       
         BNE   SCR2E300            Must be text data                            
*                                                                               
SCR2E058 LA    RE,1(,RE)           Point to number                              
         SR    RF,R1               R1 = length of number                        
         SHI   RF,2                Less 2 for "(" and ")"                       
         LR    R0,R1                                                            
         LR    R5,RE                                                            
         MVC   ERRIND,=AL2(0003)   Set to possible error                        
*                                                                               
SCR2E060 CLI   0(R5),C'0'          Validate that it is a number                 
         BL    SCR2E065            Possible error                               
         LA    R5,1(,R5)                                                        
         BCT   R0,SCR2E060                                                      
*                                                                               
         MVC   ERRIND,=AL2(2098)                                                
         BCTR  R1,0                Convert number to biniary                    
         EX    R1,*+8              R1 = length of number string - 1             
         B     *+10                                                             
         PACK  APDUB,0(0,RE)                                                    
         CVB   R0,APDUB                                                         
         LTR   R0,R0               Can't be zero                                
         BZ    SCR2E065            Possible error                               
         STC   R0,XTRSWDTH         Store possible width                         
         XC    ERRIND,ERRIND       This is ok                                   
*                                                                               
SCR2E065 CLI   0(R6),C'&&'                                                      
         BNE   SCR2E100            Not a keyword                                
*        BCTR  RF,0                1 less for C'&'                              
*                                                                               
         LR    R1,RF                                                            
         BCTR  R1,0                One less for C'&'                            
         LA    R5,APWORK                                                        
         MVC   APWORK(6),SPACES                                                 
         BCTR  R1,0                                                             
         EXMVC R1,APWORK,1(R6)                                                  
*                                                                               
         USING KEYWRD,RE           See if keyword                               
         L     RE,=A(VALIDKYW)                                                  
         A     RE,APRELO                                                        
SCR2E068 CLI   KEYESC,EOT          End of table ?                               
         BE    SCR2E300            Not a keyword, must be text data             
         CLC   KEYWORD,APWORK                                                   
         BE    SCR2E070            Found                                        
         LA    RE,KEYWLNQ(,RE)                                                  
         B     SCR2E068                                                         
*                                                                               
SCR2E070 CLI   XTRSWDTH,0          Was this set already ?                       
         BNE   *+10                                                             
         MVC   XTRSWDTH,KEYWDTH    Set default width                            
         OC    ERRIND,ERRIND                                                    
         BNZ   SCR2ERRX                                                         
*                                                                               
         XC    ALASTSUB,ALASTSUB                                                
         OI    XTRSIND,XTRSKYW     Set to keyword                               
         MVI   XTRSUBLN,XTRSLN2Q                                                
         MVC   XTRSDD#,KEYDD#      Store dictionary number                      
         B     SCR2E400            Next                                         
         DROP  RE                                                               
*                                                                               
SCR2E100 CLI   XTRTYPE,XTRHDR                                                   
         BE    SCR2E300            Not valid in header, set to text             
         MVI   XTRSIND,XTRSROW     Set to row                                   
         MVI   APBYTE,RRWELQ       Set to row element                           
         CLC   APROWCHR,0(R6)                                                   
         BE    SCR2E102                                                         
         MVI   APBYTE,RCLELQ       Set to column element                        
         MVI   XTRSIND,XTRSCOL     Set to Column                                
         CLC   APCOLCHR,0(R6)                                                   
         BNE   SCR2E300                                                         
*                                                                               
SCR2E102 LR    R1,RF                                                            
         BCTR  R1,0                                                             
         CHI   R1,2                Number can only be two long                  
         BH    SCR2E300            Must be text data                            
*                                                                               
         LR    R0,R1                                                            
         LA    R5,1(,R6)           Bump past character                          
SCR2E105 CLI   0(R5),C'0'                                                       
         BL    SCR2E300            Must be a text data                          
         LA    R5,1(,R5)                                                        
         BCT   R0,SCR2E105                                                      
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  APDUB,1(0,R6)                                                    
         CVB   R6,APDUB                                                         
         STC   R6,XTRSNUM          Store row/col number                         
         MVI   XTRSUBLN,XTRSLN1Q   Set sub-element length                       
*                                                                               
         L     RE,AIOAREA1         Find element referring to                    
         AH    RE,DATADISP                                                      
SCR2E120 CLI   0(RE),EOR           End of record ?                              
         BE    SCR2ERR4            Invalid row or column number                 
         CLC   APBYTE,0(RE)                                                     
         BNE   SCR2E122                                                         
         CLC   XTRSNUM,2(RE)       Match on number                              
         BE    SCR2E125                                                         
*                                                                               
SCR2E122 SR    R0,R0                                                            
         IC    R0,1(,RE)                                                        
         AR    RE,R0                                                            
         B     SCR2E120                                                         
*                                                                               
         USING RRWELD,RE                                                        
SCR2E125 CLI   APBYTE,RRWELQ       Handle row                                   
         BNE   SCR2E135            Must be column                               
         CLI   XTRTYPE,XTRCTOT                                                  
         BE    SCR2E126            This is ok then                              
         CLC   XTRSNUM,XTRNUM                                                   
         BH    SCR2ERR6            Must be lower than total row                 
*                                                                               
SCR2E126 XC    ALASTSUB,ALASTSUB                                                
         OC    ERRIND,ERRIND                                                    
         BNZ   SCR2ERRX                                                         
         CLI   XTRSWDTH,0                                                       
         BH    SCR2E400                                                         
         MVI   XTRSWDTH,36         Default to 36                                
         B     SCR2E400            Finished                                     
*                                                                               
         USING RCLELD,RE                                                        
SCR2E135 TM    RCLOPT,RCLACCM      Is it an amount keyword ?                    
         BO    SCR2E140            Yes ok then                                  
         CLI   XTRTYPE,XTRRTOT     Row level                                    
         BE    SCR2ERR6            Not allowed                                  
         CLC   XTRSNUM,XTRNUM                                                   
         BH    SCR2ERR6                                                         
*                                                                               
SCR2E140 XC    ALASTSUB,ALASTSUB                                                
         OC    ERRIND,ERRIND                                                    
         BNZ   SCR2ERRX                                                         
         CLI   XTRSWDTH,0                                                       
         BNE   SCR2E400                                                         
         MVC   XTRSWDTH,RCLWDTH    Default is column width                      
         B     SCR2E400            Finished                                     
         DROP  RE                                                               
*                                                                               
SCR2E300 ZIC   RF,STRLEN                                                        
         ICM   RE,15,ALASTSUB      Load last sub-el address                     
         BNZ   SCR2E310                                                         
         MVI   CONCAT,NO           Not concatenated string                      
         MVI   XTRSIND,0           Plain old text data                          
         SHI   RF,1                                                             
         BNM   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         ST    R3,ALASTSUB         Set A(Last sub-el, text only)                
         CLI   0(R6),C'"'          See if quotation in front                    
         BNE   SCR2E308            No                                           
         LA    R1,0(RF,R6)                                                      
         CLI   0(R1),C'"'          See if quotation in back                     
         BNE   SCR2E308                                                         
         CHI   RF,2                                                             
         BNH   SCR2E308            Don't bother                                 
         SHI   RF,2                                                             
         OI    XTRSIND,XTRSQUOT    Quotations on                                
         XC    ALASTSUB,ALASTSUB   Build as sperate pieces                      
         LA    R6,1(,R6)           Remove quotations                            
*                                                                               
SCR2E308 EXMVC RF,XTRSDATA,0(R6)                                                
*                                                                               
         LR    R1,RF                                                            
         AHI   R1,(XTRSDATA-XTRSUBEL)+1                                         
         STC   R1,XTRSUBLN                                                      
         B     SCR2E400            Finished                                     
*                                                                               
SCR2E310 LR    R3,RE               Combine last sub-el with this one            
         SR    R1,R1                                                            
         IC    R1,XTRSUBLN         Get length of last sub-el                    
         AR    RE,R1               RE = end of last sub-el                      
         AR    R1,RF                                                            
         STC   R1,XTRSUBLN         New length                                   
         LR    R5,RE                                                            
         BCTR  R5,0                                                             
         CLI   0(R5),C'+'          Was last part also C'+'                      
         BE    SCR2E320            Yes, so don't adjust                         
         LR    R5,R6               See if there was a C'+'                      
         BCTR  R5,0                                                             
         CLI   0(R5),C'+'                                                       
         BNE   SCR2E320                                                         
         AHI   R1,1                                                             
         AHI   RF,1                                                             
         STC   R1,XTRSUBLN         Add back C'+'                                
         LR    R6,R5                                                            
*                                                                               
SCR2E320 BCTR  RF,0                                                             
         EXMVC RF,0(RE),0(R6)                                                   
         MVI   CONCAT,YES          Concatenate string                           
         B     SCR2E420                                                         
*                                                                               
SCR2E400 SR    R1,R1                                                            
         IC    R1,XTRSUB#          Increment sub-element count                  
         AHI   R1,1                                                             
         STC   R1,XTRSUB#                                                       
*                                                                               
SCR2E420 SR    R1,R1                                                            
         IC    R1,XTRSUBLN         Length of sub-element                        
         AR    R3,R1               Next location for next sub-element           
         L     R6,LASTPLUS         Location of C'+'                             
         L     RF,XPANDEND                                                      
         LA    R6,1(,R6)           Bump past C'+'                               
         SR    RF,R6               RF = length of string remaining              
         BM    SCR2E440            Must be at end                               
         BP    SCR2E424                                                         
         CLI   LASTTIME,YES                                                     
         BE    SCR2E440            Finished                                     
         BCTR  R6,0                Don't skip last character                    
         LA    RF,1                                                             
*                                                                               
SCR2E424 STC   RF,FVIFLD                                                        
         BCTR  RF,0                                                             
         STC   RF,FVXLEN                                                        
         B     SCR2E050                                                         
         DROP  R3                                                               
*                                                                               
SCR2E440 SR    RF,RF                                                            
         LA    R1,XTRLNQ           Initial length                               
         LA    R3,XTRSUBEL                                                      
*                                                                               
         USING XTRSUBEL,R3                                                      
SCR2E450 ICM   RF,1,XTRSUBLN                                                    
         BZ    SCR2EQU             Finished                                     
         AR    R1,RF               Add up sub-element lengths                   
         STC   R1,XTRLN            Element length                               
         AR    R3,RF                                                            
         B     SCR2E450                                                         
*                                                                               
SCR2ERR2 MVC   FVMSGNO,=AL2(0634)  Should be numeric                            
         B     SCR2HIGH                                                         
                                                                                
SCR2ERR4 MVC   FVMSGNO,=AL2(1410)  Invalid column number                        
         B     SCR2HIGH                                                         
                                                                                
SCR2ERR5 MVC   FVMSGNO,=AL2(1415)  Invalid row    number                        
         B     SCR2HIGH                                                         
                                                                                
SCR2ERR6 MVC   FVMSGNO,=AL2(0069)  Not valid with this type                     
         B     SCR2HIGH                                                         
                                                                                
SCR2ERRX MVC   FVMSGNO,ERRIND                                                   
         B     SCR2HIGH                                                         
*                                                                               
SCR2LOW  CLI   *,X'96'             Branch low                                   
         B     SCR2EXIT                                                         
                                                                                
SCR2EQU  CLI   *,X'95'             Branch equal                                 
         B     SCR2EXIT                                                         
                                                                                
SCR2HIGH CLI   *,X'94'             Branch high                                  
                                                                                
SCR2EXIT XIT1                                                                   
         DROP  R3,R9                                                            
         EJECT ,                                                                
         USING XTRELD,R9                                                        
ELM2SCR  NTR1                                                                   
         L     R9,0(,R1)           Element                                      
         L     R4,4(,R1)           Screen Header                                
         SR    R0,R0                                                            
         ICM   R0,1,XTRSUB#        Number of sub-elements                       
         BZ    ELM2EXIT                                                         
         LA    R6,XPANDFLD         Area to build data                           
         LA    R3,XTRSUBEL                                                      
*                                                                               
         USING XTRSUBEL,R3                                                      
ELM2S100 TM    XTRSIND,XTRSKYW     Keyword ?                                    
         BZ    ELM2S200            No                                           
         MVI   0(R6),C'&&'         Move marco symbol C'&'                       
         LA    R6,1(,R6)                                                        
         MVC   APWORK(7),SPACES                                                 
         MVI   APWORK,ESCLEFTJ        Left justified                            
         MVC   APWORK+1(2),XTRSDD#    Dictionary number                         
         MVI   APWORK+3,6             length of data                            
         GOTO1 VDICTAT,APPARM,C'SU  ',(6,APWORK),0                              
*                                                                               
         LA    R1,6                                                             
         LA    RE,APWORK+6                                                      
ELM2S102 CLI   0(RE),C' '          Figure out length of keyword                 
         BH    ELM2S105                                                         
         BCTR  RE,0                                                             
         BCT   R1,ELM2S102                                                      
         DC    H'00'               Can't be zero                                
*                                                                               
ELM2S105 EXMVC R1,0(R6),APWORK     Move in keyword                              
         LA    R6,1(R1,R6)                                                      
         B     ELM2S400            Add width characters "(nn)"                  
*                                                                               
ELM2S200 TM    XTRSIND,XTRSROW     From row                                     
         BZ    ELM2S210            No                                           
         MVC   0(1,R6),APROWCHR    C'R' for row                                 
         LA    R6,1(,R6)                                                        
         B     ELM2S212                                                         
*                                                                               
ELM2S210 TM    XTRSIND,XTRSCOL     From column                                  
         BZ    ELM2S300            No                                           
         MVC   0(1,R6),APCOLCHR    C'C' for column                              
         LA    R6,1(,R6)                                                        
*                                                                               
ELM2S212 SR    RF,RF                                                            
         IC    RF,XTRSNUM          Get row number                               
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'       Fix sign                                     
         UNPK  0(1,R6),APDUB                                                    
         LA    RF,1                                                             
         CLI   XTRSNUM,9                                                        
         BNH   ELM2S220                                                         
         UNPK  0(2,R6),APDUB                                                    
         LA    RF,2                                                             
*                                                                               
ELM2S220 AR    R6,RF                                                            
         B     ELM2S400                                                         
*                                                                               
ELM2S300 SR    RF,RF                                                            
         ICM   RF,1,XTRSUBLN       Length of sub-element                        
         BZ    ELM2S500                                                         
         SHI   RF,(XTRSDATA-XTRSUBEL)+1                                         
         BM    ELM2S500                                                         
         TM    XTRSIND,XTRSQUOT    Were quotations used ?                       
         BZ    ELM2S310                                                         
         MVI   0(R6),C'"'                                                       
         AHI   R6,1                                                             
*                                                                               
ELM2S310 EXMVC RF,0(R6),XTRSDATA                                                
         LA    R6,1(RF,R6)                                                      
         TM    XTRSIND,XTRSQUOT    Were quotations used ?                       
         BZ    ELM2S320                                                         
         MVI   0(R6),C'"'                                                       
         AHI   R6,1                                                             
*                                                                               
ELM2S320 CLM   R0,1,XTRSUB#        Is this the first sub-element                
         BNE   ELM2S420                                                         
         CLI   XTRSDATA,C'+'       Only if 1st char is C'+'                     
         BNE   ELM2S420                                                         
         B     ELM2S500                                                         
*                                                                               
ELM2S400 MVC   0(1,R6),APOPNPRN    C'('                                         
         LA    R6,1(,R6)           Go past parenthisis                          
         SR    RF,RF                                                            
         IC    RF,XTRSWDTH         Width of column                              
         CVD   RF,APDUB                                                         
         OI    APDUB+7,X'0F'       Fix sign                                     
         UNPK  0(1,R6),APDUB                                                    
         LA    RF,1                                                             
         CLI   XTRSWDTH,9                                                       
         BNH   ELM2S410                                                         
         UNPK  0(2,R6),APDUB                                                    
         LA    RF,2                                                             
*                                                                               
ELM2S410 AR    R6,RF                                                            
         MVC   0(1,R6),APCLSPRN    C')'                                         
         LA    R6,1(,R6)           Go past parenthisis                          
*                                                                               
ELM2S420 CHI   R0,1                Is this the last one ?                       
         BE    ELM2S500            Yes, so don't add a C'+'                     
         TM    XTRSIND,XTRSKYW+XTRSROW+XTRSCOL                                  
         BNZ   ELM2S424                                                         
         BCTR  R6,0                                                             
         CLI   0(R6),C'+'          Don't keep on adding C'+'                    
         BE    ELM2S424                                                         
         AHI   R6,1                                                             
*                                                                               
ELM2S424 MVI   0(R6),C'+'          Add concatenation sign                       
         LA    R6,1(,R6)           Go past plus sign                            
*                                                                               
ELM2S500 SR    RF,RF                                                            
         IC    RF,XTRSUBLN         Get sub-element length                       
         AR    R3,RF                                                            
         BCT   R0,ELM2S100         Process next                                 
*                                                                               
         LA    RF,XPANDFLD                                                      
         SR    R6,RF               Calculate length of data                     
         SR    R1,R1                                                            
         IC    R1,0(,R4)           Get field length                             
         SHI   R1,8                                                             
         TM    1(R4),FVAXTND       Extended field hdr                           
         BZ    *+8                                                              
         SHI   R1,8                                                             
*                                  R1 = field size, R6 = data size              
         STC   R6,FVILEN           Save off length                              
         CR    R1,R6               More data then field length ?                
         BH    *+6                 No                                           
         LR    R6,R1               Use field length                             
         SHI   R6,1                                                             
         BM    ELM2EXIT                                                         
         EXMVC R6,8(R4),XPANDFLD                                                
*                                                                               
         LA    RE,XPANDFLD+1(R6)   Point to end of what we moved                
         BNP   ELM2EXIT            No more to display                           
         IC    R6,FVILEN           Restore data length                          
         SR    R6,R1               R6 = len(data), R1 = len(Screen fld)         
         IC    R1,0(,R4)           Length of field & hdrs                       
         AR    R4,R1               Next field (Always two)                      
         IC    R1,0(,R4)           Length of field                              
         SHI   R1,8                                                             
         TM    1(R4),FVAXTND       Extended field hdr ?                         
         BZ    *+8                                                              
         SHI   R1,8                                                             
         CR    R1,R6               Is data greater than field ?                 
         BH    *+6                 No                                           
         LR    R6,R1                                                            
         SHI   R6,1                                                             
         BM    ELM2EXIT                                                         
         EXMVC R6,8(R4),0(RE)                                                   
*                                                                               
ELM2EXIT XIT1                                                                   
         DROP  R3,R9                                                            
         EJECT ,                                                                
IVALIPUT LHI   R1,-(X'FFFF'-FVFNOTV+1)     Invalid Input                        
         B     ERRXIT                                                           
                                                                                
IVALEKEY MVI   STSEQ,1                                                          
         LHI   R1,-(X'FFFF'-FVFEKEY+1)                                          
         B     ERRXIT                                                           
                                                                                
ERRXIT   STCM  R1,3,FVMSGNO                                                     
         CLC   FVMSGNO,=AL2(FVFOK) SET CONCODE                                  
         B     XIT                                                              
         EJECT ,                                                                
         LTORG                                                                  
         EJECT ,                                                                
***********************************************************************         
*       Table of valid keywords that can be used                      *         
***********************************************************************         
VALIDKYW DS    0H                                                               
         DCDD  AC#RSTDY,6                                                       
*&&US*&& DC    CL6' ',AL1(8)       Today                                        
*&&UK*&& DC    CL6' ',AL1(9)       Today                                        
         DCDD  AC#GENNO,6                                                       
         DC    CL6' ',AL1(12)      Generation number                            
         DCDD  AC#GENJN,6                                                       
         DC    CL6' ',AL1(18)      Generation job number                        
         DCDD  AC#RSAGN,6                                                       
         DC    CL6' ',AL1(36)      Agency name                                  
         DCDD  AC#RSONN,6                                                       
         DC    CL6' ',AL1(36)      Office name                                  
         DCDD  AC#RSBLK,6                                                       
         DC    CL6' ',AL1(8)       Blank                                        
         DCDD  AC#RSGAP,6                                                       
         DC    CL6' ',AL1(8)       Gap                                          
         DCDD  AC#RSFTC,6                                                       
         DC    CL6' ',AL1(8)       Format code                                  
         DCDD  AC#RSFTN,6                                                       
         DC    CL6' ',AL1(36)      Format name                                  
         DCDD  AC#TIME,6                                                        
         DC    CL6' ',AL1(8)       HH:MM:SS                                     
         DCDD  AC#RSCNT,6                                                       
         DC    CL6' ',AL1(8)       Detail + Total trailers                      
         DCDD  AC#RSHMS,6          HHMMSS                                       
         DC    CL6' ',AL1(6)       Hours minutes secounds                       
         DCDD  AC#RECCT,6          RCOUNT                                       
         DC    CL6' ',AL1(8)       Detail + all Total trailers                  
         DS    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
*        Validation table for Round / Decimals column option          *         
***********************************************************************         
DECMTAB  DS    0C                                                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
ESCLEN2  EQU   X'20'               Data dictionary len=2                        
ESCLEN3  EQU   X'21'               Data dictionary len=3                        
ESCLEFTJ EQU   X'22'               Data dictionary left justify                 
FVALOW   EQU   X'40'                                                            
*                                                                               
FLDNUMBR EQU   1                                                                
FLDWIDTH EQU   2                                                                
FLDXTRCT EQU   3                                                                
FLDPFKLN EQU   254                                                              
FLDLABEL EQU   255                                                              
         EJECT ,                                                                
***********************************************************************         
*  DSECT FOR LOCAL WORKING STORAGE                                    *         
***********************************************************************         
         SPACE 1                                                                
LWSD     DSECT                                                                  
SVRE     DS    A                   Save registar RE                             
AKEYWORD DS    A                   A(Keyword from DEFTAB ACSCR01 CORE)          
ASTYELEM DS    A                   STYELQ element address                       
XPANDEND DS    A                                                                
ALASTSUB DS    A                                                                
LASTPLUS DS    A                   Location of last plus C'+'                   
*                                                                               
MAXPARM  EQU   33                                                               
*                                                                               
LCINDS   DS    XL1                 Local indincation for read                   
LCADDREC EQU   X'80'               .   OK to add    record                      
LCCHAREC EQU   X'20'               .   OK to change record                      
LCADDELM EQU   X'08'               .   Added element                            
LCDELELM EQU   X'04'               .   Deleted element                          
*                                                                               
ELEMENT  DS    CL(L'APELEM)                                                     
BLOCK    DS    (MAXPARM)CL32       DATA BLOCK FOR SCANNER                       
SVKEY    DS    CL(L'IOKEY)         SAVE IOKEY                                   
SVELEM   DS    CL(L'APELEM)        SAVE APELEM                                  
SVTYPE   DS    XL1                 Save off type                                
EXTFLD#  DS    XL1                 Extended field number                        
CHECKUL  DS    CL1                 WILDCARD TO CHECK U/L                        
LDGLISTN DS    XL1                 # OF UNIT/LEDGER IN LIST                     
LDGLIST  DS    CL40                UNIT/LEDGER LIST                             
LASTROW# DS    XL1                 Last row    number                           
LASTCOL# DS    XL1                 Last column number                           
MAXSORT# DS    XL1                 Highest sort # used                          
MIDLINE# DS    XL1                 1st row midline number                       
COLARRAY DS    CL(MAXCOLS)                                                      
FAKEFLDH DS    XL8                                                              
FAKEFLD  DS    CL12                                                             
BYTE     DS    CL1                 One byte work area                           
SEQNUM   DS    XL1                 Column or Row number                         
CURSEQ#  DS    XL1                                                              
CONCAT   DS    CL1                 Concatenation      (Yes/No)                  
LASTTIME DS    CL1                 Last time in       (Yes/No)                  
STRLEN   DS    AL1                 Length of string                             
ERRIND   DS    AL2                 Error indication                             
*                                                                               
XLATIND  DS    XL1                 Translate indicator                          
XLATACTV EQU   X'80'               .   Keyword found and active                 
XLATKYWD EQU   X'40'               .   Keyword processed                        
*                                                                               
XPANDFLD DS    CL256                                                            
PROFELEM DS    XL(RPFLN2Q)         Saved off profile element                    
LWSX     DS    0C                                                               
         EJECT ,                                                                
KEYWRD   DSECT                                                                  
KEYESC   DS    XL1                                                              
KEYDD#   DS    XL1                                                              
KEYLEN   DS    XL1                                                              
         DS    CL3                                                              
KEYWORD  DS    CL6                                                              
KEYWDTH  DS    AL1                                                              
KEYWLNQ  EQU   *-KEYWRD                                                         
         EJECT ,                                                                
*ACSCRWRK                                                                       
       ++INCLUDE ACSCRWRK                                                       
         EJECT ,                                                                
TWAD     DSECT                                                                  
         ORG   SCROVLYH                                                         
       ++INCLUDE ACSCRD2D                                                       
         SPACE 3                                                                
*DDTWABLDD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDTWABLDD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'125ACSCR1C   09/02/15'                                      
         END                                                                    
