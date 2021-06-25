*          DATA SET RESFM14    AT LEVEL 009 AS OF 05/01/02                      
*PHASE T81814A,*                                                                
         TITLE 'T81814 - RESFM14 - FORECAST RECORD/ALL FUNCTIONS'               
*                                                                               
*********************************************************************           
*                                                                   *           
*      RESFM14 (T81814) --- BUDGET/FORECAST - ALL FUNCTIONS         *           
*                                                                   *           
* ----------------------------------------------------------------- *           
* UPDATE HISTORY:                                                   *           
*                                                                   *           
* JUN15/89  (SNS) -- INITIAL RELEASE                                *           
*                                                                   *           
* JUN23/89  (SNS) -- ADD MODE =LISTRECS                             *           
*                                                                   *           
* OCT06/89  (MRR) -- MAKE OFFICE AND PERIOD FILTERS ON LIST         *           
*                                                                   *           
* 27MAR90   (EFJ) -- USE OUTPUT OF DATVAL TO CHECK VALID DATE AND   *           
*                    GET RID OF USELESS MONTH TABLE, SINCE 'DC'     *           
*                    DOES NOTHING INSIDE A DSECT                    *           
*                                                                   *           
* 27MAY92   (SKU) -- FIX BUG OF NOT DELETING OLD COMMENT ELEMENTS   *           
*                                                                   *           
* 14SEP93   (SKU) -- CLEAR ELEM BEFORE ADDING COMMENT ELEMENT       *           
*                                                                   *           
*********************************************************************           
*                                                                               
T81814   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1814**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         SPACE                                                                  
         CLI   MODE,VALKEY         VALIDATE KEY                                 
         BE    VKEY                                                             
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VREC                                                             
         CLI   MODE,DISPKEY        DISPLAY KEY                                  
         BE    DKEY                                                             
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DREC                                                             
         CLI   MODE,LISTRECS       LIST RECORDS ONLINE                          
         BE    LREC                                                             
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              VALIDATE AND DISPLAY KEY                                         
         SPACE 3                                                                
VKEY     DS    0H                                                               
         XC    SVSTA,SVSTA        CLEAR OUT SAVE VALUES                         
         XC    SVOFF,SVOFF                                                      
         XC    SVADV,SVADV                                                      
         XC    SVPER,SVPER                                                      
*                                                                               
         LA    R2,SBBSTAH                                                       
         MVI   ERROR,1            MISSING STATION                               
         CLI   5(R2),0                                                          
         BE    TRAPERR                                                          
         GOTO1 VALISTA                                                          
         MVC   SVSTA,WORK                                                       
*                                                                               
         LA    R2,SBBADVH                                                       
         CLI   5(R2),0                                                          
         BNE   VKADV10                                                          
         CLI   ACTNUM,ACTLIST     NOT REQUIRED FOR LIST                         
         BE    VKOFF                                                            
         B     TRAPERR            MISSING ADVERTISER                            
*                                                                               
VKADV10  GOTO1 VALIADV                                                          
         MVC   SVADV,WORK                                                       
*                                                                               
VKOFF    LA    R2,SBBOFFH                                                       
         CLI   5(R2),0                                                          
         BNE   VKOFF05                                                          
         CLI   ACTNUM,ACTLIST     NOT REQUIRED FOR LIST                         
         BE    VKPER                                                            
         B     TRAPERR            MISSING OFFICE                                
*                                                                               
VKOFF05  CLI   1(RA),C'*'         DDS TERMINAL?                                 
         BE    VKOFF10            YES                                           
         USING TWAD,RE                                                          
         LR    RE,RA                                                            
         CLC   TWAACCS(2),=C'O= ' CHECK OFFICE RESTRICTION                      
         BNE   VKOFF10                                                          
         CLC   TWAACCS+2(2),8(R2)                                               
         BE    VKOFF10                                                          
         MVI   ERROR,55                                                         
         B     TRAPERR            OFFICE MUST MATCH                             
*                                                                               
         DROP  RE                                                               
VKOFF10  GOTO1 VALIOFF                                                          
         MVC   SVOFF,WORK                                                       
*                                                                               
VKPER    LA    R2,SBBPERH                                                       
         CLI   5(R2),0                                                          
         BNE   VKPER05                                                          
         CLI   ACTNUM,ACTLIST     NOT REQUIRED FOR LIST                         
         BE    VKBLD                                                            
         B     TRAPERR            MISSING OFFICE                                
*                                                                               
VKPER05  MVI   ERROR,2                                                          
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB,DMCB                                                        
         BZ    TRAPERR                                                          
* ONLY VALID PERIODS ARE JAN/APR/JUL AND OCT                                    
* 28MAR90 *** START ***                                                         
         CLC   =C'01',WORK+2                                                    
         BE    VKPER20                                                          
         CLC   =C'04',WORK+2                                                    
         BE    VKPER20                                                          
         CLC   =C'07',WORK+2                                                    
         BE    VKPER20                                                          
         CLC   =C'10',WORK+2                                                    
         BE    VKPER20                                                          
* 28MAR90 *** END ***                                                           
*         LA    R1,PERMONS         VALID MONTH TABLE                            
*VKPER10  CLC   0(L'PERMONS,R1),8(R2)                                           
*         BE    VKPER20                                                         
*         LA    R1,L'PERMONS(R1)                                                
*         CLC   =C'FFF',0(R1)                                                   
*         BNE   VKPER10                                                         
         MVI   ERROR,169                                                        
         B     TRAPERR                                                          
*                                                                               
VKPER20  GOTO1 DATCON,DMCB,(0,WORK),(2,SVPER)                                   
*                                                                               
VKBLD    LA    R4,KEY                                                           
         USING RSBBKEY,R4                                                       
         XC    KEY,KEY                                                          
         MVI   RSBBKTYP,X'2D'                                                   
         MVC   RSBBKREP,AGENCY                                                  
         MVC   RSBBKSTA,SVSTA                                                   
         MVC   RSBBKADV,SVADV                                                   
         MVC   RSBBKOFF,SVOFF                                                   
         MVC   RSBBKPER,SVPER                                                   
*                                                                               
VKEXT    B     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
VREC     DS    0H                                                               
         MVC   AIO,AIO1                                                         
         L     R6,AIO                                                           
*                                                                               
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   VREC10                                                           
         GOTO1 REMELEM                                                          
*                                                                               
VREC10   MVI   STAFLAG,0          INDICATE NO STAT AMT ENTERED                  
         XC    BUDTOT,BUDTOT                                                    
         LA    R2,SBBTOTH         STATION TOTAL                                 
         CLI   5(R2),0                                                          
         BE    VREC20             CHECK MARKET TOTAL                            
         MVI   ERROR,2                                                          
         TM    4(R2),X'08'                                                      
         BZ    TRAPERR            INVALID INPUT                                 
*                                                                               
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,VRECPK                                                        
         CVB   R1,DUB                                                           
         ST    R1,BUDTOT                                                        
         MVI   STAFLAG,1            STAT AMT ENTERED                            
*                                                                               
VREC20   DS    0H                                                               
         MVI   MKTFLAG,0          INDICTE NO MKT AMT ENTERED                    
         XC    MKTTOT,MKTTOT                                                    
         LA    R2,SBBMTOTH        MARKET TOTAL HEADER                           
         CLI   5(R2),0                                                          
         BE    VREC25             CHECK SHARE                                   
         MVI   ERROR,2                                                          
         TM    4(R2),X'08'                                                      
         BZ    TRAPERR            INVALID INPUT                                 
*                                                                               
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,VRECPK1                                                       
         CVB   R1,DUB                                                           
         ST    R1,MKTTOT                                                        
         MVI   MKTFLAG,1            MKT AMT ENTERED                             
*                                                                               
VREC25   DS    0H                                                               
         MVI   SHRTOT,0                                                         
         LA    R2,SBBSHRH         SHARE HEADER                                  
         CLI   5(R2),0                                                          
         BNE   VREC30             YES -SHARE AMT                                
*                                                                               
         CLI   MKTFLAG,0          NO SHARE ! WAS THERE MARKET?                  
         BE    VREC35             NO -- GOOD NEWS                               
         CLI   STAFLAG,1          NO SHR! MKT INPUTTED MUST BE STAT $           
         BE    VREC35                                                           
         LA    R2,SBBMTOTH        PT TO FIELD WITH THE INPUT                    
         MVI   ERROR,147          MUST ALSO ENTER SHARE OR STATION              
         B     TRAPERR                                                          
*                                                                               
VREC30   DS    0H                 SHARE AMT INPUTED                             
         CLI   MKTFLAG,0          SHR INPUTTED -MUST BE MKT OR STAT $           
         BE    VREC32             NO CHECK STATION $                            
         CLI   STAFLAG,1          YES HAVE MKT - SO MUST BE NO STAT $           
         BNE   VREC34                                                           
         MVI   ERROR,148          CAN ENTER ONLY 2 OUT OF 3                     
         B     TRAPERR                                                          
*                                                                               
VREC32   CLI   STAFLAG,1          IS THERE STATION $                            
         MVI   ERROR,149          IF NO - SHARE CAN'T BE ALL ALONE              
         BNE   TRAPERR            MUST ALSO ENTER STATION OR MARKET             
*                                                                               
VREC34   MVI   ERROR,2                                                          
         TM    4(R2),X'08'                                                      
         BZ    TRAPERR            INVALID INPUT                                 
         SR    R4,R4                                                            
         IC    R4,5(R2)                                                         
         BCTR  R4,0                                                             
         EX    R4,VRECPK2                                                       
         CVB   R1,DUB                                                           
         STC   R1,SHRTOT                                                        
*                                                                               
VREC35   XC    ELEM,ELEM          SET UP NEW ELEMENT                            
         MVC   ELEM(2),=XL2'010E'                                               
         MVC   ELEM+2(4),BUDTOT     STATION TOTAL TO ELEMENT                    
         MVC   ELEM+6(4),MKTTOT     MAKRET TOTAL TO ELEMENT                     
         MVC   ELEM+10(1),SHRTOT    SHARE PERCENTAGE TO ELEMENT                 
*                                                                               
         OC    ELEM+2(12),ELEM+2                                                
         BZ    VREC40                                                           
         GOTO1 ADDELEM                                                          
*                                                                               
VREC40   DS    0H                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   VREC45                                                           
*                          DELETE ALL COMMENT ELEMENTS                          
         GOTO1 REMELEM                                                          
VREC45   LA    R2,SBBCMTH                                                       
         LA    R3,4               NUMBER OF COMMENT LINES                       
         XC    ELEM,ELEM                                                        
         LA    R4,ELEM                                                          
         MVC   0(2,R4),=X'028E'                                                 
         LA    R4,2(R4)                                                         
*                                                                               
VREC60   CLI   5(R2),0            ANY COMMENTS?                                 
         BE    VREC80             NO                                            
         MVC   0(35,R4),8(R2)                                                   
         LA    R4,35(R4)                                                        
         SPACE                                                                  
         ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BCT   R3,VREC60          NEXT COMMENT LINE                             
*                                                                               
VREC80   DS    0H                                                               
         C     R3,=F'4'           ANY COMMENTS?                                 
         BE    XIT                                                              
         OC    ELEM+2(60),SPACES                                                
         OC    ELEM+62(80),SPACES                                               
         GOTO1 ADDELEM                                                          
         B     XIT                                                              
*                                                                               
VRECPK   PACK  DUB,SBBTOT(0)                                                    
VRECPK1  PACK  DUB,SBBMTOT(0)                                                   
VRECPK2  PACK  DUB,SBBSHR(0)                                                    
         EJECT                                                                  
*              DISPLAY KEY                                                      
DKEY     EQU   *                                                                
         L     R6,AIO1                                                          
         USING RSBBREC,R6                                                       
         LA    R2,SBBSTAH                                                       
         MVC   8(5,R2),RSBBKSTA                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SBBADVH                                                       
         MVC   8(4,R2),RSBBKADV                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SBBOFFH                                                       
         MVC   8(2,R2),RSBBKOFF                                                 
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R2,SBBPERH                                                       
         GOTO1 DATCON,DMCB,(2,RSBBKPER),(6,8(R2))                               
         OI    6(R2),X'80'                                                      
*                                                                               
         B     XIT                                                              
         EJECT                                                                  
*              DISPLAY RECORD                                                   
DREC     EQU   *                                                                
         GOTO1 CLRSCR,DMCB,(0,SBBTOTH),SBBFLGH                                  
         L     R6,AIO1                                                          
         USING RSBBREC,R6                                                       
         LA    R2,SBBTOTH         STATION TOTAL                                 
         MVI   ELCODE,X'01'                                                     
         BAS   RE,GETEL                                                         
         BNE   DREC20                                                           
         ICM   R1,15,2(R6)                                                      
         BZ    DREC10                                                           
         EDIT  (R1),(12,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
         OI    6(R2),X'80'                                                      
*                                                                               
DREC10   LA    R2,SBBMTOTH                                                      
         ICM   R1,15,6(R6)        MKT TOTAL                                     
         BZ    DREC15                                                           
         EDIT  (R1),(12,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                          
         OI    6(R2),X'80'                                                      
*                                                                               
DREC15   LA    R2,SBBSHRH         STATION SHARE                                 
         ZIC   R1,10(R6)                                                        
         LTR   R1,R1                                                            
         BZ    DREC20                                                           
         EDIT  (R1),(3,8(R2)),ALIGN=LEFT,ZERO=NOBLANK                           
*                                                                               
DREC20   L     R6,AIO1                                                          
         MVI   ELCODE,X'02'                                                     
         BAS   RE,GETEL                                                         
         BNE   DREXT                                                            
         LA    R2,SBBCMTH                                                       
         LA    R6,2(R6)           PT TO DATA                                    
         LA    R3,4                                                             
*                                                                               
DREC40   CLC   0(35,R6),SPACES                                                  
         BE    XIT                                                              
*                                                                               
         MVC   8(35,R2),0(R6)                                                   
         OI    6(R2),X'80'                                                      
         ZIC   R1,0(R2)                                                         
         AR    R2,R1              PT TO NXT LINE ON SCREEN                      
         LA    R6,35(R6)                                                        
         BCT   R3,DREC40                                                        
*                                                                               
DREXT    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
LREC     DS    0H                                                               
         OC    KEY(27),KEY        IS KEY ZERO                                   
         BNZ   LR10               NO, GET THIS KEY                              
         LA    R4,KEY                                                           
         USING RSBBKEY,R4                                                       
         MVI   RSBBKTYP,X'2D'                                                   
         MVC   RSBBKREP,AGENCY                                                  
         MVC   RSBBKSTA,SVSTA                                                   
         MVC   RSBBKADV,SVADV                                                   
         MVC   RSBBKOFF,SVOFF                                                   
         MVC   RSBBKPER,SVPER                                                   
         MVC   MYKEY,KEY                                                        
*                                                                               
LR10     LA    R3,18              MINIMUM KEY (-1 FOR EX)                       
         OC    SVADV,SVADV                                                      
         BZ    LR30               LISTING STATION ONLY                          
*                                                                               
         LA    R3,22                                                            
         OC    SVOFF,SVOFF                                                      
         BZ    LR30                                                             
*                                                                               
         LA    R3,24                                                            
         OC    SVPER,SVPER                                                      
         BZ    LR30                                                             
*                                                                               
         LA    R3,26                                                            
LR30     GOTO1 HIGH                                                             
LR35     EQU   *                                                                
         CLC   KEY(14),MYKEY       COMPARE KEYID AND REP                        
         BNE   LREXT               PAST, ALL DONE                               
         OC    SVSTA,SVSTA         STATION FILTER?A,SVSTA                       
         BZ    LR35A               NO, MOVE ON                                  
         CLC   KEY+14(5),SVSTA     YES, COMAPARE                                
         BNE   LREXT               NO EQUAL MEANS ALL DONE                      
LR35A    EQU   *                                                                
         OC    SVADV,SVADV         ADVERTIER FILTER                             
         BZ    LR35B               ZERO IS NO                                   
         CLC   KEY+19(4),SVADV     ELSE COMAPARE                                
         BNE   LR70                NO EQUAL IS KEEP READING                     
LR35B    EQU   *                                                                
         OC    SVOFF,SVOFF         OFF FILT - SUPPLIED OR SECURITY              
         BZ    LR35C1              NOT ON SCREEN, CHECK TWA                     
         CLC   KEY+23(2),SVOFF     COMAPARE                                     
         BNE   LR70                NO EQUAL IS KEEP READING                     
         B     LR35C2                                                           
LR35C1   EQU   *                                                                
         CLI   1(RA),C'*'          DDS TERMINAL?                                
         BE    LR35C2              DDS TERMINALS GET IT ALL                     
         LR    RE,RA                                                            
         USING TWAD,RE                                                          
         CLC   TWAACCS+2(2),KEY+23 COMPARE SIGN-ON OFFICE                       
         BE    LR35C2              EQUAL IS OK, NOT IS KEEP READING             
         B     LR70                                                             
         DROP  RE                                                               
LR35C2   EQU   *                                                                
         OC    SVPER,SVPER         PERIOD FILTER?                               
         BZ    LR40                NO, GET THE RECORD                           
         CLC   KEY+25(2),SVPER     COMAPRE                                      
         BNE   LR70                NOT EQUAL IS KEEP READING                    
         SPACE                                                                  
LR40     GOTO1 GETREC                                                           
         L     R4,AIO                                                           
         LR    R6,R4                                                            
         USING RSBBKEY,R6                                                       
         MVC   LISTAR,SPACES                                                    
         MVC   LADVCDE,RSBBKADV      MOVE ADVERT CODE IN LINE                   
         BAS   RE,GETADVNM                                                      
*                                                                               
         MVC   LOFFCDE+3(2),RSBBKOFF     MOVE OFFICE CODE IN LINE               
         GOTO1 DATCON,DMCB,(2,RSBBKPER),(6,LPER)                                
*                                                                               
         MVI   ELCODE,X'01'       FORECAST AMOUNT ELEMENT                       
         BAS   RE,GETEL                                                         
         BNE   LR42                                                             
         ICM   R1,15,2(R6)                                                      
         BZ    LR41M                                                            
         EDIT  (R1),(10,LSTAT),ZERO=NOBLANK                                     
*                                                                               
LR41M    ICM   R1,15,6(R6)        MARKET TOTAL                                  
         BZ    LR41S                                                            
         EDIT  (R1),(10,LMKT),ZERO=NOBLANK                                      
*                                                                               
LR41S    ZIC   R1,10(R6)          SHARE TOTAL                                   
         LTR   R1,R1                                                            
         BZ    LR42                                                             
         EDIT  (R1),(3,LSHR+2),ZERO=NOBLANK                                     
*                                                                               
LR42     LR    R6,R4              PT TO TOP OF AIO                              
         MVI   ELCODE,X'02'       COMMENT ELEMENT                               
         BAS   RE,GETEL                                                         
         BNE   LR60                                                             
         MVI   LCMT+1,C'*'                                                      
*                                                                               
LR60     GOTO1 LISTMON                                                          
LR70     GOTO1 SEQ                                                              
         B     LR35                                                             
LREXT    XIT                                                                    
         EJECT                                                                  
GETADVNM NTR1                                                                   
         MVC   SVKEY,KEY                                                        
         MVC   AIO,AIO2                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,X'08'                                                        
         MVC   KEY+21(4),LADVCDE                                                
         MVC   KEY+25(2),AGENCY                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(27),KEYSAVE                                                  
         BNE   GETADV20                                                         
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   LADV,36(R1)                                                      
         B     GETADV40                                                         
*                                                                               
GETADV20 MVC   LADV,=CL14'NAME NOT FOUND'                                       
         SPACE                                                                  
GETADV40 MVC   KEY(L'SVKEY),SVKEY         RESTORE KEY                           
         GOTO1 HIGH                                                             
         GOTO1 GETREC                                                           
         MVC   AIO,AIO1                                                         
         XIT                                                                    
         EJECT                                                                  
         GETEL R6,DATADISP,ELCODE                                               
         SPACE                                                                  
TRAPERR  GOTO1 ERREX                                                            
         SPACE 3                                                                
* CLEAR DISPLAY AREA OF SCREEN *                                                
* CLRSCR  - CLEAR AND FOUT FIELDS                                               
*                                                                               
* ON ENTRY                                                                      
*        P1    BYTE 0    = 0 UNPROTECTED FIELDS                                 
*                        = 1 PROTECTED FIELDS                                   
*              BYTES 1-3 = A(START FIELD HEADER)                                
*        P2    BYTES 1-3 = A(END FIELD HEADER)                                  
*                                                                               
CLRSCR   NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         SR    RE,RE                                                            
         LA    R4,X'10'            BRANCH CONDITION                             
         LA    R5,MOVESPA          CLEAR FIELD INSTRUCTION                      
         CLI   0(R1),0             TEST FOR UNPROTECTED FIELDS                  
         BE    *+12                YES                                          
         LA    R4,X'80'            SET BRANCH CONDITION AND CLEAR               
         LA    R5,ZEROFLD          INSTRUCTION FOR PROTECTED FIELDS             
         SPACE 1                                                                
CLRSCR2  IC    RE,0(R2)            LENGTH OF FIELD PLUS HEADER                  
         TM    1(R2),X'20'         TEST FOR PROTECTED FIELD                     
         EX    R4,TSTBRAN          BRANCH ACCORDINGLY                           
         LR    R1,RE                                                            
         SH    R1,=H'9'            SET EXECUTE LENGTH FOR FIELD DATA            
         TM    1(R2),X'02'         TEST FOR EXTENDED FIELD                      
         BZ    *+8                                                              
         SH    R1,=H'8'            LESS 8 MORE FOR EXTENDED FIELD               
         EX    R1,0(R5)            CLEAR FIELD                                  
         OI    4(R2),X'20'         SET VALIDITY BIT                             
         OI    6(R2),X'80'         SET TO TRANSMIT                              
         SPACE 1                                                                
CLRSCR4  LA    R2,0(RE,R2)                                                      
         CR    R2,R3               TEST IF END FIELD REACHED                    
         BL    CLRSCR2             NO-CONTINUE                                  
         B     XIT                 YES-ALL DONE                                 
         SPACE 1                                                                
MOVESPA  MVC   8(0,R2),SPACES                                                   
ZEROFLD  XC    8(0,R2),8(R2)                                                    
TSTBRAN  BC    0,CLRSCR4                                                        
         EJECT                                                                  
RELO     DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
* DDSPOOLD                                                                      
* DDSPLWORKD                                                                    
* RESFMFFD                                                                      
       ++INCLUDE DDSPOOLD                                                       
       ++INCLUDE DDSPLWORKD                                                     
       ++INCLUDE RESFMFFD                                                       
         ORG   CONTAGH                                                          
* RESFMFED                                                                      
       ++INCLUDE RESFMFED                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
* RESFMFDD                                                                      
       ++INCLUDE RESFMFDD                                                       
         EJECT                                                                  
* REGENSBB                                                                      
       ++INCLUDE REGENSBB                                                       
         EJECT                                                                  
* RESFMWORKD                                                                    
       ++INCLUDE RESFMWORKD                                                     
         ORG   SYSSPARE                                                         
BUDTOT   DS    F                                                                
MKTTOT   DS    F                                                                
*PERMONS  DS    0CL3                                                            
*         DC    C'JAN'                                                          
*         DC    C'APR'                                                          
*         DC    C'JUL'                                                          
*         DC    C'OCT'                                                          
*         DC    C'FFF'                                                          
*                                                                               
SVSTA    DS    CL5                                                              
SVADV    DS    CL4                                                              
SVOFF    DS    CL2                                                              
SVPER    DS    CL2                                                              
MKTFLAG  DS    XL1                                                              
STAFLAG  DS    XL1                                                              
SHRTOT   DS    XL1                                                              
DASH     DC    47C'-'                                                           
MYKEY    DS    CL32                                                             
SVKEY    DS    CL32                                                             
MYWORK   DS    CL142                                                            
* FATWA                                                                         
       ++INCLUDE FATWA                                                          
* ONLINE LIST LINE                                                              
GEND     DSECT                                                                  
         ORG   LISTAR                                                           
LADV     DS    CL20                                                             
         DS    CL1                                                              
LADVCDE  DS    CL4                                                              
         DS    CL1                                                              
LOFFCDE  DS    CL8                                                              
         DS    CL1                                                              
LPER     DS    CL6                                                              
         DS    CL1                                                              
LSTAT    DS    CL10                                                             
         DS    CL1                                                              
LMKT     DS    CL10                                                             
         DS    CL1                                                              
LSHR     DS    CL5                                                              
         DS    CL2                                                              
LCMT     DS    CL3                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009RESFM14   05/01/02'                                      
         END                                                                    
