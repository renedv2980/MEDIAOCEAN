*          DATA SET RENFI11    AT LEVEL 032 AS OF 05/01/02                      
*          DATA SET RENFI11    AT LEVEL 030 AS OF 06/22/98                      
*PHASE T83211A                                                                  
*INCLUDE MEDGET                                                                 
         TITLE 'SPOT KEY BUILDER'                                               
NFI11    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPNFI11*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING SAVED,R6                                                         
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
         SPACE 1                                                                
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    MVI   ERRFLG,FF           SET CC LOW                                   
         CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
         SPACE 2                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUBACT)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     CLI   CSACT,80                                                         
         BNE   EXITOK                                                           
         MVI   GSSMCODE,C'1'       SET SCREEN CODE FOR LIST OF FIELDS           
         MVC   KEYBLK(L'KEYFILL),KEYFILL                                        
         MVC   KEYBLK+1(L'KEYBLK-1),KEYBLK                                      
*                                                                               
         LH    RF,GSDIRDSP         DISPLACEMENT TO DIRECTORY DETAILS            
         A     RF,AXFILTAB                                                      
         ST    RF,RTDSPDIR         RECORD DIRECTORY DETAILS                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION         *         
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                  *         
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                              *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA04   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA04                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00010),AL4(RECDTA)    RECORD NAME                            
         DC    AL2(00011),AL4(FLDDTA)    FIELD NAME                             
         DC    AL2(00012),AL4(VALDTA)    FIELD VALUE                            
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
NFI11    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RECDTA   LA    RF,RECTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RECTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISREC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISREC   MVC   FVIFLD(L'TLKNAME),TLKNAME                                        
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FIELD NAME                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FLDDTA   LA    RF,FLDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FLDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFLD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FIELD NAME FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISFLD   MVC   FVIFLD(L'TLR1NAME),TLR1NAME                                      
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FIELD VALUE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDTA   LA    RF,VALTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
VALTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVAL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVAL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIELD VALUE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISVAL   MVC   FVIFLD(L'TLR1DATA),TLR1DATA                                      
         TM    TLR1STA2,FLDCPY                                                  
         BZ    EXITOK                                                           
         CLC   TLR1DATA,BCSPACES                                                
         BH    EXITOK                                                           
         MVC   FVIFLD(L'CUAALF),CUAALF                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD VALUE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
VALVAL   TM    TLR1STAT,FLDRTN     VALIDATION ROUTINE?                          
         BZ    VVAL01              NO                                           
         XR    RF,RF                                                            
         ICM   RF,3,TLR1RTN                                                     
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         B     EXIT                                                             
*                                                                               
VVAL01   MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
*                                                                               
         TM    TLR1STAT,FLDCHR     INPUT IS CHARACTERS?                         
         BZ    VVAL02              NO                                           
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),FVIFLD                                                   
         B     VVAL16                                                           
*                                                                               
VVAL02   TM    TLR1STAT,FLDHEX     INPUT IS HEX?                                
         BZ    VVAL06                                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    0(0,R3),0(R3)       ENSURE INPUT AREA IS ZERO FILLED             
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT TO FIELD                            
*                                                                               
         TM    TLR1STAT,FLDLEFT    DON'T RIGHT ALIGN HEX INPUT?                 
         BZ    VVAL04                                                           
         XR    R1,R1                                                            
         IC    R1,FVILEN           GET LENGTH OF INPUT                          
         SRL   R1,1                HEX IS REALLY HALF AS LONG                   
         XR    RF,RF                                                            
         IC    RF,TLR1LEN                                                       
*                                                                               
         SR    RF,R1               GET LENGTH TO DISPLACE                       
         BNM   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               TOO MUCH DATA INPUT                          
*                                                                               
         AR    R3,RF                                                            
*                                                                               
VVAL04   TM    FVIIND,FVIHEX       IS IT HEX INPUT?                             
         BO    *+14                YES                                          
         MVC   FVMSGNO,=AL2(GE$INHEX)                                           
         B     EXITL                                                            
*                                                                               
         TM    FVILEN,X'01'        HEX INPUT MUST HAVE EVEN NUMBER OF           
         BZ    *+14                CHARACTERS INPUT                             
         MVC   FVMSGNO,=AL2(GE$INHEX)                                           
         B     EXITL                                                            
*                                                                               
         XR    RF,RF                                                            
         IC    RF,FVILEN            LENGTH OF INPUT                             
*                                                                               
         GOTOX VHEXIN,BOPARM,FVIFLD,(R3),(RF)                                   
         OC    12(4,R1),12(R1)                                                  
         BZ    EXITNV                                                           
         B     VVAL16                                                           
*                                                                               
VVAL06   TM    TLR1STAT,FLDNUM     NUMERIC CHARACTERS FIELD?                    
         BZ    VVAL10                                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),BCSPACES    ENSURE INPUT AREA IS SPACE FILLED            
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT TO FIELD                            
*                                                                               
         TM    TLR1STAT,FLDLEFT    DON'T RIGHT ALIGN NUMERIC INPUT?             
         BZ    VVAL08                                                           
         XR    R1,R1                                                            
         IC    R1,FVILEN           GET LENGTH OF INPUT                          
         SRL   R1,1                NUMERIC IS REALLY HALF AS LONG               
         XR    RF,RF                                                            
         IC    RF,TLR1LEN                                                       
*                                                                               
         SR    RF,R1               GET LENGTH TO DISPLACE                       
         BNM   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               TOO MUCH DATA INPUT                          
*                                                                               
         AR    R3,RF                                                            
*                                                                               
VVAL08   TM    FVIIND,FVINUM       TEST VALID NUMERIC CHARACTER DATA            
         BZ    EXITNOTN                                                         
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),FVIFLD                                                   
         B     VVAL16                                                           
*                                                                               
VVAL10   TM    TLR1STAT,FLDBIN     NUMERIC BINARY DATA?                         
         BZ    VVAL12                                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    0(0,R3),0(R3)       ENSURE INPUT AREA IS ZERO FILLED             
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT TO FIELD                            
*                                                                               
         TM    FVIIND,FVINUM       TEST VALID BINARY NUMERIC DATA               
         BZ    EXITNOTN                                                         
*                                                                               
         LA    RF,L'BCFULL         NUMERIC DATA ALREADY CONVERTED HERE          
         XR    R1,R1                                                            
         IC    R1,TLR1LEN          R1=L'FIELD IN KEY                            
         SR    RF,R1               RF=DISPL. INTO DUB OF START OF DATA          
         LA    RF,BCFULL(RF)       RF=A(DATA)                                   
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),0(RF)                                                    
         B     VVAL16                                                           
*                                                                               
VVAL12   TM    TLR1STAT,FLDDTE     DATE FIELD?                                  
         BZ    VVAL16                                                           
*                                                                               
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         XC    0(0,R3),0(R3)       ENSURE INPUT AREA IS ZERO FILLED             
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO INPUT TO FIELD                            
*                                                                               
         GOTOX VDATVAL,BOPARM,(0,FVIFLD),BODUB1 VALIDATE FOR YMD                
         ICM   RE,15,0(R1)                                                      
         BNZ   VVAL14                          VALID                            
*                                                                               
         GOTOX (RF),(R1),(1,FVIFLD),BODUB1     ELSE VALIDATE FOR YM             
         ICM   RE,15,0(R1)                                                      
         BNZ   VVAL14                          VALID                            
         B     EXITNV                                                           
*                                  RETURN PWOS DATE                             
VVAL14   GOTOX VDATCON,(R1),(0,BODUB1),(1,(R3))                                 
*                                                                               
VVAL16   TM    TLR1STAT,FLDCOMP    TEST NEED TO COMPLEMENT                      
         BZ    VVAL18                                                           
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   BOWORK1(0),0(R3)                                                 
         XC    BOWORK1,HEXFFS                                                   
         EX    R1,*+4                                                           
         MVC   0(0,R3),BOWORK1                                                  
*                                                                               
VVAL18   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
* -------------                                                       *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                  *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (OUT)                      *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (BACK)                     *         
***********************************************************************         
         SPACE 1                                                                
XITOUT   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         DROP  R2                                                               
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMCODE    CHECK SCREEN CODE                            
         BE    LITER04                                                          
         LA    RF,OBJTABL(RF)                                                   
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(BLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(BLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(LSCRFRST),AL1(0,0,0),AL4(SCRFRST)                            
*                                                                               
         DC    AL1(LDEFCLM),AL1(0,0,0),AL4(DEFCLM)                              
         DC    AL1(LDEFCLM),AL1(0,0),C'1',AL4(DEFCLM)                           
*                                                                               
         DC    AL1(LINIT),AL1(0,0),C'1',AL4(ILST1)                              
         DC    AL1(LLSTFRST),AL1(0,0),C'1',AL4(FTFLST1)                         
         DC    AL1(LGETFRST),AL1(0,0),C'1',AL4(BLST1)                           
         DC    AL1(LGETNEXT),AL1(0,0),C'1',AL4(BLST1)                           
         DC    AL1(LTSARDIR),AL1(0,0),C'1',AL4(TSARDIR1)                        
         DC    AL1(LSCRFRST),AL1(0,0),C'1',AL4(SCRFRST1)                        
         DC    AL1(LSCRLAST),AL1(0,0),C'1',AL4(SCRLAST1)                        
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT COLUMNS FOR LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
DEFCLM   XC    LSFIXNUM,LSFIXNUM                                                
         LA    R1,LSFIXCLM                                                      
         USING DCTABD,R1                                                        
*                                                                               
         LA    RF,RECCLM                                                        
         CLI   CSACT,80                                                         
         BNE   *+8                                                              
         LA    RF,FLDCLM                                                        
*                                                                               
KCLM04   XC    0(DCTABL,R1),0(R1)  CLEAR COLUMN ENTRY                           
         CLC   =AL2(EOT),0(RF)     END OF LIST OF FIXED COLUMNS                 
         BE    KCLM06                                                           
*                                                                               
         MVC   DCTFLD#,0(RF)       SET FIELD NUMBER                             
         MVC   DCTINDS1,2(RF)      SET INDICATORS                               
         LH    RE,LSFIXNUM                                                      
         LA    RE,1(RE)                                                         
         STH   RE,LSFIXNUM         INCREMENT NUMBER OF FIXED COLUMNS            
         LA    R1,DCTABL(R1)                                                    
         LA    RF,3(RF)                                                         
         B     KCLM04                                                           
                                                                                
KCLM06   B     EXITOK                                                           
*                                                                               
RECCLM   DC    AL2(00010),AL1(0)          RECORD COLUMN                         
         DC    AL2(EOT)                                                         
*                                                                               
FLDCLM   DC    AL2(00011),AL1(0)          FIELD COLUMN                          
         DC    AL2(00012),AL1(DCTIOPEN)   FIELD COLUMN VALUE                    
         DC    AL2(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ILST     MVI   LSSUBLEN,3          SUB-ACTION FIELD LENGTH IS 3                 
         OI    LSSTAT1,(LSSBALL+LSSTSAR)                                        
         NI    LSSTAT1,FF-LSSENTK  NO NEED TO ENTER A KEY                       
         OI    LSSTAT2,LSSBDR+LSSSIZE                                           
         MVC   LSCOLROW,=AL2(COLS#Q)                                            
         MVC   LSCOLLIN,LSCOLROW                                                
         MVC   LSDSPROW,=AL2(3)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   XC    LINENOW,LINENOW     RESET CURRENT LINE NUMBER                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST OF RECORD TYPES                                          *         
***********************************************************************         
         SPACE 1                                                                
BLST     LH    RF,LINENOW                                                       
         MH    RF,=Y(RCOTABLQ)                                                  
         LA    RF,RCOTABL(RF)                                                   
         CLI   0(RF),0             END OF THE RECORD LIST?                      
         BE    EXITL               YES                                          
*                                                                               
         LH    RF,LINENOW                                                       
         LA    RF,1(RF)                                                         
         STH   RF,LINENOW                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR  L     R3,SVPARMS4         TSAR RECORD                                  
         USING TLSTD,R3                                                         
         LH    RF,LINENOW          GET CURRENT LIST LINE                        
         BCTR  RF,0                                                             
         MH    RF,=Y(RCOTABLQ)                                                  
         LA    RF,RCOTABL(RF)                                                   
         USING RCOTABLD,RF                                                      
*                                                                               
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         MVC   TLRDISP,RECDISP                                                  
         MVC   TLRFILL,RECFILL                                                  
         MVC   TLKNAME,BCSPACES                                                 
         MVC   TLKNAME(L'RECNAME),RECNAME                                       
         DROP  RF                                                               
*                                                                               
         GOTOX VDICTAT,BOPARM,C'SL  ',TLKNAME                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST  TM    GCINDS2,GCIXITS     JUST FROM FIELD LIST?                        
         BZ    SCRF02              NO                                           
         GOTOX AGEN,BOPARM,OSES,SXIT                                            
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR KEY BUILD SCREEN                                          *         
***********************************************************************         
         SPACE 1                                                                
SCRFRST1 LH    R1,LS1STLIN                                                      
         A     R1,ATWA                                                          
         LH    RF,LSFOODSP                                                      
         A     RF,ATWA                                                          
         XR    RE,RE                                                            
         USING FHD,R1                                                           
SCRF02   ICM   RE,1,0(R1)                                                       
         BZ    EXITOK                                                           
         TM    FHAT,FHATPR                                                      
         BO    SCRF04                                                           
         OI    FHAT,FHATMO                                                      
         NI    FHII,X'FF'-FHIIVA                                                
*                                                                               
SCRF04   BXLE  R1,RE,SCRF02                                                     
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST PAGE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
ILST1    MVI   LSSUBLEN,0          NO SUB-ACTION FIELD                          
         OI    LSSTAT1,(LSSBALL+LSSTSAR)                                        
         NI    LSSTAT1,FF-LSSENTK  NO NEED TO ENTER A KEY                       
         OI    LSSTAT2,LSSBDR+LSSSIZE                                           
         MVC   LSCOLROW,=AL2(COLS#Q)                                            
         MVC   LSCOLLIN,LSCOLROW                                                
         MVC   LSDSPROW,=AL2(3)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  XC    LINENOW,LINENOW     RESET CURRENT LINE NUMBER                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST OF FIELDS                                                *         
***********************************************************************         
         SPACE 1                                                                
BLST1    LH    RF,LINENOW                                                       
         MH    RF,=Y(FLDTABLQ)                                                  
         LA    RF,FLDTABL(RF)                                                   
         AH    RF,LISDISP                                                       
         CLI   0(RF),0             END OF THE RECORD LIST?                      
         BE    EXITL               YES                                          
*                                                                               
         LH    RF,LINENOW                                                       
         LA    RF,1(RF)                                                         
         STH   RF,LINENOW                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR RECORD FOR FIELDS                                       *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR1 L     R3,SVPARMS4         TSAR RECORD                                  
         USING TLSTD,R3                                                         
         LH    RF,LINENOW          GET CURRENT LIST LINE                        
         BCTR  RF,0                                                             
         MH    RF,=Y(FLDTABLQ)                                                  
         LA    RF,FLDTABL(RF)                                                   
         AH    RF,LISDISP                                                       
         USING FLDTABLD,RF                                                      
*                                                                               
         MVC   TLRLEN,=AL2(TLLNQ1)                                              
         MVC   TLR1DISP,FLDDISP                                                 
         MVC   TLR1LEN,FLDLEN                                                   
         MVC   TLR1STAT,FLDSTAT                                                 
         MVC   TLR1STA2,FLDSTAT2                                                
         MVC   TLR1RTN,FLDVRTN                                                  
         MVC   TLR1NAME,BCSPACES                                                
         MVC   TLR1NAME(L'FLDNAME),FLDNAME                                      
         DROP  RF                                                               
*                                                                               
         GOTOX VDICTAT,BOPARM,C'SL  ',TLR1NAME                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* LAST FOR SCREEN 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST1 TM    LSSCIND1,LSSCIINP   SCREEN INPUT?                                
         BZ    EXITOK                                                           
         CLI   ERRFLG,FF                                                        
         BE    EXITL                                                            
*                                                                               
BLDKEY   MVC   BOWORK1,KEYBLK      GET COPY OF KEY                              
         MVI   THISTYPE,C'X'       SET DEFAULT TO HEXADECIMAL                   
         MVC   LASTTYPE,THISTYPE                                                
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R0,R0                                                            
         IC    R0,NFIKEYL-NFITABD(RF)                                           
         XC    KEYBLK,KEYBLK       CLEAR DISPLAY BLOCK FOR KEY                  
         LA    R2,KEYBLK           R2=CURRENT FOR DISPLAY                       
         LA    R3,BOWORK1          R3=CURRENT IN KEY                            
*                                                                               
BKEY02   CLI   0(R3),C' '          IS THIS CHARACTER OR HEXADECIMAL?            
         BE    BKEY04                                                           
         CLI   0(R3),C'*'                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'?'                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'/'                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'\'                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'+'                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'='                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'<'                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'>'                                                       
         BE    BKEY04                                                           
         CLI   0(R3),C'A'                                                       
         BL    BKEY06                                                           
         CLI   0(R3),C'I'                                                       
         BNH   BKEY04                                                           
         CLI   0(R3),C'J'                                                       
         BL    BKEY06                                                           
         CLI   0(R3),C'R'                                                       
         BNH   BKEY04                                                           
         CLI   0(R3),C'S'                                                       
         BL    BKEY06                                                           
         CLI   0(R3),C'Z'                                                       
         BNH   BKEY04                                                           
         CLI   0(R3),C'0'                                                       
         BL    BKEY06                                                           
         CLI   0(R3),C'9'                                                       
         BNH   BKEY04                                                           
         B     BKEY06                                                           
*                                                                               
BKEY04   MVI   THISTYPE,C'C'       SET CHARACTER                                
         CLC   THISTYPE,LASTTYPE                                                
         BE    *+12                                                             
         MVI   0(R2),C'('                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         MVC   0(1,R2),0(R3)                                                    
         LA    R2,1(R2)                                                         
         B     BKEY08                                                           
*                                                                               
BKEY06   MVI   THISTYPE,C'X'       SET HEXADECIMAL                              
         CLC   THISTYPE,LASTTYPE                                                
         BE    *+12                                                             
         MVI   0(R2),C')'                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         GOTOX VHEXOUT,BOPARM,(R3),(R2),1,0                                     
         LA    R2,2(R2)                                                         
*                                                                               
BKEY08   MVC   LASTTYPE,THISTYPE                                                
         LA    R3,1(R3)                                                         
         BCT   R0,BKEY02                                                        
         CLI   LASTTYPE,C'C'                                                    
         BNE   *+8                                                              
         MVI   0(R2),C')'                                                       
*                                                                               
BKEY10   GOTOX AGEN,BOPARM,OSES,SXIT                                            
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* SUB-ACTION OBJECT                                                   *         
***********************************************************************         
         SPACE 1                                                                
SUBACT   LA    R1,SUBTAB                                                        
         USING OBJTABD,R1                                                       
         L     RE,SVPARMS2                                                      
*                                                                               
SUB02    CLI   OBJVERB,EOT                                                      
         BE    EXITH                                                            
         CLM   RE,1,OBJVERB        RE HOLDS EQUATED VERB                        
         BE    SUB04               MATCHED                                      
         LA    R1,OBJTABL(R1)                                                   
         B     SUB02               BUMP & LOOP                                  
*                                                                               
SUB04    ICM   RF,15,OBJADR                                                     
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  R1                                                               
*                                                                               
SUBTAB   DC    AL1(SAVAL),AL1(0,0,0),AL4(SUBVAL)                                
         DC    AL1(SAPROC),AL1(0,0,0),AL4(SHAC)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* VALIDATE SUB-ACTION INPUT FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SUBVAL   XC    LSSUBFRA,LSSUBFRA   CLEAR CURRENT SUB-ACTION ELEMENT             
         XC    LSSUBNUM,LSSUBNUM   RESET SUB-ACTION REPEAT COUNT                
*                                                                               
         LH    R2,LSCURLIN         DISPLACEMENT TO CURRENT LINE                 
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         TM    FHII,FHIIVA         FIELD INPUT?                                 
         BO    *+8                 NO                                           
         OI    LSSCIND1,LSSCIINP   YES - KEEP CURRENT SCREEN                    
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
*                                                                               
         CLI   FVIFLD,C'?'        WANT LIST OF VALID HELP?                      
         BNE   SVAL02                                                           
*                                                                               
         CLI   CSREC,O#SUBAC      CANNOT ALLOW THIS TO GO RECURSIVE             
         BNE   *+8                                                              
         CLI   PSREC,O#SUBAC                                                    
         BE    SVAL02                                                           
         CLI   PSREC,O#RTYP                                                     
         BE    SVAL02                                                           
         CLI   PSREC,O#ACT                                                      
         BE    SVAL02                                                           
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,O#SUBAC                                                   
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNISEL                                                 
         L     RF,ATLST           PASS CURRENT RECORD MASK                      
         MVC   N.SDATA(L'TLRMSK),TLRMSK-TLSTD(RF)                               
         GOTOX AGEN,BOPARM,OSES,SNTR                                            
         DROP  N                                                                
         DC    H'0'                                                             
*                                                                               
SVAL02   TM    LSSCIND1,LSSCIMEP+LSSCIMEL                                       
*                                                                               
SVAL10   CLI   FVILEN,0            SUB-ACTION ENTERED?                          
         BE    SVALOK              NO                                           
         CLI   FVIFLD,C'*'         IGNORE THIS FIELD?                           
         BE    SVALOK              YES                                          
*                                                                               
SVAL20   GOTOX AGENLST,BOPARM,OSUBACT,SAMATCH                                   
         BNE   SVALL               NO MATCH FOR THIS INPUT                      
*                                                                               
         GOTOX AOLY,BOPARM,OSUBACT,SAPROC                                       
         BNE   SVALL               ERROR ON PROCESS OF SUB-ACTION               
*                                                                               
SVALOK   OI    FHII,FHIIVA                                                      
         B     EXITOK                                                           
*                                                                               
SVALL    B     EXITL                                                            
         SPACE 1                                                                
         DROP  R2                                                               
***********************************************************************         
* PROCESS SUB-ACTION                                                  *         
***********************************************************************         
         SPACE 1                                                                
SHAC     LA    R1,SHACTAB                                                       
         USING OBJTABD,R1                                                       
         XR    RE,RE                                                            
         IC    RE,LSSUBFRA+(FRASACT-FRAELD)                                     
*                                                                               
SHAC02   CLI   OBJVERB,EOT                                                      
         BE    EXITH                                                            
         CLM   RE,1,OBJVERB        RE HOLDS EQUATED VERB                        
         BE    SHAC04              MATCHED                                      
         LA    R1,OBJTABL(R1)                                                   
         B     SHAC02              BUMP & LOOP                                  
*                                                                               
SHAC04   ICM   RF,15,OBJADR                                                     
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  R1                                                               
*                                                                               
SHACTAB  DC    AL1(SHNTR),AL1(0,0,0),AL4(SHANTR)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* SET UP FIELD LIST RECORD                                            *         
***********************************************************************         
         SPACE 1                                                                
N        USING SSAVD,NSSAV                                                      
SHANTR   MVC   N.SRECACT,LSSUBFRA+(FRANREC-FRAELD)                              
         OI    N.SNINDS1,SNIUSECR                                               
         LH    R0,LSSUBNUM                                                      
         STC   R0,N.SMPAGE                                                      
         MVI   GSFRPEL+(FRPTYPE-FRPELD),FRPTRFRS                                
         L     RF,ATLST                                                         
         USING TLSTD,RF                                                         
         MVC   LISDISP,TLRDISP                                                  
         MVC   KEYFILL,TLRFILL                                                  
         GOTOX AGEN,BOPARM,OSES,SNTR                                            
         DROP  RF,N                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY FOR AGENCY RECORD ONLY                              *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
VAGY     NTR1  ,               *** VALIDATE AGENCY (AGENCY RECORD ONLY)         
         CLI   FVILEN,0                                                         
         BNE   VAGY02              IF NO ENTRY - USE CONNECTED                  
         MVC   FVIFLD(L'CUAALF),CUAALF                                          
         MVI   FVILEN,L'CUAALF                                                  
*                                                                               
VAGY02   CLI   FVILEN,L'CUAALF     AGENCY MUST BE 2 CHARS.                      
         BE    VAGY04                                                           
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         BL    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
VAGY04   MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,6                                                          
         MVC   IOKEY+1(2),FVIFLD                                                
         L     R1,=AL4(XOREAD+XOSPTDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV                                                           
*                                                                               
         MVI   KEYBLK,06           FOR AGENCY RECORDS                           
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY FOR AGY/MEDIA                                       *         
***********************************************************************         
         SPACE 1                                                                
VAGENCY  NTR1  ,                                                                
         CLI   FVILEN,0                                                         
         BNE   VAGCY02             IF NO ENTRY - USE CONNECTED                  
         MVC   FVIFLD(L'CUAALF),CUAALF                                          
         MVI   FVILEN,L'CUAALF                                                  
*                                                                               
VAGCY02  CLI   FVILEN,L'CUAALF     AGENCY MUST BE 2 CHARS.                      
         BE    VAGCY04                                                          
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         BL    *+10                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
VAGCY04  MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,6                                                          
         MVC   IOKEY+1(2),FVIFLD                                                
         L     R1,=AL4(XOREAD+XOSPTDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV                                                           
         L     R1,=AL4(XOGET+XOSPTFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                WHY NO GETREC?                               
*                                                                               
         L     R3,AIO1                                                          
         USING AGYHDRD,R3                                                       
         MVC   AGYCTRY,AGYPROF+7   EXTRACT COUNTRY CODE                         
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE MEDIA FOR AGY/MEDIA & PLACE VALUE INTO RECORD KEY          *         
***********************************************************************         
         SPACE 1                                                                
VMEDIA   NTR1  ,                                                                
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         LA    R3,MEDIALST         GET MEDIA LIST                               
         LA    R0,L'MEDIALST                                                    
*                                                                               
VMED02   CLC   0(1,R3),FVIFLD      CHECK IF FIELD IN LIST                       
         BE    VMED04                                                           
         LA    R3,1(R3)                                                         
         BCT   R0,VMED02                                                        
         B     EXITNV                                                           
*                                                                               
VMED04   GOTO1 VMEDGET,BODMCB,(0(R3),CUAALF),VDMGR,BOWORK1,RR=BORELO            
         CLI   8(R1),X'FF'                                                      
         BE    EXITNV                                                           
*                                                                               
         MVC   AGYMCD,FVIFLD                                                    
         MVC   AGYMED,BOWORK1                                                   
         MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),AGYMED                                                   
         B     EXITOK                                                           
*                                                                               
MEDIALST DC    CL5'TRNXC'          MEDIA LIST                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VCLIENT  NTR1  ,                                                                
         CLI   FVILEN,3                                                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         CLC   =C'ALL',FVIFLD      'ALL' CLIENT IS NULLS                        
         BE    VCLT02                                                           
*                                                                               
         XC    BODUB1,BODUB1                                                    
         MVC   BODUB1+4(4),=X'D9000A14' CLPACK                                  
         GOTO1 VCOLY,BODUB1                                                     
*                                                                               
         L     RF,BODUB1           A(CLIENT PACK ROUTINE)                       
         GOTO1 (RF),BOPARM,FVIFLD,BOWORK1                                       
         CLI   0(R1),0                                                          
         BNE   EXITNV                                                           
*                                                                               
VCLT02   MVC   AGYCLI,BOWORK1                                                   
         MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),AGYCLI                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT CODE (CHARACTER)                                   *         
***********************************************************************         
         SPACE 1                                                                
VPROD    NTR1  ,                                                                
         CLI   FVILEN,3                                                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY+1(L'AGYMED),AGYMED                                         
         MVC   IOKEY+2(L'AGYCLI),AGYCLI                                         
         L     R1,=AL4(XOREAD+XOSPTDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV                                                           
         L     R1,=AL4(XOGET+XOSPTFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                WHY NO GETREC?                               
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,CLIST-CLTHDRD(R3)                                             
VPROD02  CLI   0(R3),0             END OF TABLE?                                
         BE    EXITNV              NO MATCH ON PRODUCT                          
         CLC   FVIFLD(3),0(R3)                                                  
         BE    VPROD04                                                          
         LA    R3,4(R3)                                                         
         B     VPROD02                                                          
*                                                                               
VPROD04  MVC   AGYPRO,FVIFLD                                                    
         MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),AGYPRO                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT CODE (HEX)                                         *         
***********************************************************************         
         SPACE 1                                                                
VPRD     NTR1  ,                                                                
         CLI   FVILEN,0                                                         
         BNE   VPRD02                                                           
         MVC   FVIFLD(3),=C'POL'                                                
         MVI   FVILEN,3                                                         
*                                                                               
VPRD02   CLI   FVILEN,3                                                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY+1(L'AGYMED),AGYMED                                         
         MVC   IOKEY+2(L'AGYCLI),AGYCLI                                         
         L     R1,=AL4(XOREAD+XOSPTDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITNV                                                           
         L     R1,=AL4(XOGET+XOSPTFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                WHY NO GETREC?                               
*                                                                               
         L     R3,AIO1                                                          
         LA    R3,CLIST-CLTHDRD(R3)                                             
VPRD04   CLI   0(R3),0             END OF TABLE?                                
         BE    EXITNV              NO MATCH ON PRODUCT                          
         CLC   FVIFLD(3),0(R3)                                                  
         BE    VPRD06                                                           
         LA    R3,4(R3)                                                         
         B     VPRD04                                                           
*                                                                               
VPRD06   MVC   AGYPRO,FVIFLD                                                    
         MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R4,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R4),3(R3)                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION                                                    *         
***********************************************************************         
         SPACE 1                                                                
VSTATION NTR1  ,                                                                
         CLI   FVILEN,4                                                         
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL                                                            
*                                                                               
         TM    FVIIND,FVIALF       THIS INPUT ALPHABETIC?                       
         BZ    EXITNV                                                           
         MVC   AGYSTN,FVIFLD                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE BAND (ASSUME STATION IN AGYSTN ALREADY)                    *         
***********************************************************************         
         SPACE 1                                                                
VBAND    NTR1                                                                   
         CLI   AGYMCD,C'R'                                                      
         BE    VBAND02                                                          
         MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'AGYMCD),AGYMCD                                          
         MVI   FVILEN,1                                                         
         B     VBAND04                                                          
*                                                                               
VBAND02  CLI   FVIFLD,C'A'         AM?                                          
         BE    VBAND04                                                          
         CLI   FVIFLD,C'F'         FM?                                          
         BNE   EXITNV                                                           
*                                                                               
VBAND04  XC    IOKEY,IOKEY                                                      
         MVI   IOKEY,C'0'                                                       
         MVC   IOKEY+1(14),IOKEY   NEW STATION 15-BYTE KEY                      
         MVI   IOKEY,C'S'                                                       
         MVC   IOKEY+1(1),AGYMCD   MEDIA LETTER                                 
         MVC   IOKEY+2(4),AGYSTN   STATION CALL LETTERS                         
         MVC   IOKEY+6(1),FVIFLD   BAND IF ANY                                  
         MVC   IOKEY+7(2),CUAALF   AGENCY LETTERS                               
         MVC   IOKEY+9(3),AGYCLI   CLIENT LETTERS                               
*                                                                               
         L     R3,AIO1                                                          
         USING STATHDR,R3                                                       
         BAS   RE,STAHI                                                         
         CLC   IOKEY(15),0(R3)                                                  
         BE    VBAND06                                                          
         MVC   IOKEY+9(3),=3C'0'   CLIENT LETTERS                               
         BAS   RE,STAHI            READ THE STATION RECORD                      
         CLC   IOKEY(15),0(R3)                                                  
         BNE   EXITNV                                                           
*                                                                               
VBAND06  MVC   BODUB1(L'SMKT),SMKT                                              
         DROP  R3                                                               
*                                                                               
         XC    BODMCB(8),BODMCB                                                 
         MVC   BODMCB+4(4),=X'D9000A1B' MSPACK                                  
         GOTO1 VCOLY,BODMCB                                                     
         L     RF,BODMCB           A(MARKET/STATION PACK ROUTINE)               
         GOTO1 (RF),(R1),BODUB1,IOKEY+2,BOWORK1                                 
*                                                                               
         MVC   AGYBND,BOWORK1                                                   
         MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),BOWORK1                                                  
         B     EXITOK                                                           
*                                                                               
STAHI    NTR1  ,                                                                
         GOTO1 VDMGR,BODMCB,=C'DMRDHI',=C'STATION',IOKEY,AIO1                   
         XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* VALIDATE LINE                                                       *         
***********************************************************************         
         SPACE 1                                                                
VLINE    NTR1  ,                                                                
         XR    RF,RF                                                            
         CLI   FVILEN,0            IF NO ENTRY                                  
         BE    VLINE02                                                          
         TM    FVIIND,FVINUM       TEST IF NUMERIC                              
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'255'          BOUNDARY CHECK FOR 1-BYTE BINARY             
         BH    EXITNV                                                           
*                                                                               
VLINE02  STC   RF,BOWORK1                                                       
         MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),BOWORK1                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SPILL                                                      *         
***********************************************************************         
         SPACE 1                                                                
VSPILL   NTR1  ,                                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         XR    RF,RF                                                            
         CLI   FVIFLD,C'Y'                                                      
         BNE   *+12                                                             
         LA    RF,X'80'                                                         
         B     VSPILL02                                                         
*                                                                               
         CLI   FVIFLD,C'N'                                                      
         BNE   EXITNV                                                           
*                                                                               
VSPILL02 STC   RF,BOWORK1                                                       
         MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
         XR    RF,RF                                                            
         IC    RF,TLR1DISP                                                      
         LA    R3,KEYBLK(RF)       POINT TO INSERTION AREA                      
         XR    R1,R1                                                            
         IC    R1,TLR1LEN                                                       
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   0(0,R3),BOWORK1                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TABLE OF SUPPORTED RECORD KEYS                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0A                                                               
*C    C'GOAL    ',AL4(GOALREC)                                                  
*C    C'STABILL ',AL4(STABREC)                                                  
*C    C'BILL    ',AL4(BILLREC)                                                  
*C    C'BUY     ',AL4(BUYREC)                                                   
*C    C'NWH     ',AL4(NWHREC)                                                   
*C    C'NWD     ',AL4(NWDREC)                                                   
*C    C'STATION ',AL4(STATREC)                                                  
*C    C'MKT     ',AL4(MKTREC)                                                   
*C    C'REP     ',AL4(REPREC)                                                   
*C    C'SCHEME  ',AL4(SCHREC)                                                   
*C    C'PERIOD  ',AL4(PERREC)                                                   
*C    C'NSID    ',AL4(NSIDREC)                                                  
*C    C'DETAIL  ',AL4(DETREC)                                                   
*C    C'PW-MKT  ',AL4(PWMKTREC)                                                 
*C    C'PW-STA  ',AL4(PWSTAREC)                                                 
*C    C'NETDEF  ',AL4(NWKREC)                                                   
*C    C'DEMODEF ',AL4(AGYREC)                                                   
*ND                                                                             
RCOTABL  DCDDL SP#AGY,12,L         AGENCY RECORD                                
         DC    Y(FLDAGY-FLDTABL),X'00',X'00'                                    
         DCDDL SP#CLI,12,L         CLIENT RECORD                                
         DC    Y(FLDCLI-FLDTABL),X'00',X'00'                                    
         DCDDL SP#PRO,12,L         PRODUCT RECORD                               
         DC    Y(FLDPRO-FLDTABL),X'00',X'00'                                    
         DCDDL SP#EST,12,L         ESTIMATE RECORD                              
         DC    Y(FLDEST-FLDTABL),X'00',X'00'                                    
         DCDDL SP#BUY,12,L         BUY RECORD                                   
         DC    Y(FLDBUY-FLDTABL),X'00',X'00'                                    
*                                                                               
         DC    X'00'               END OF TABLE MARKER                          
*                                                                               
RCOTABLD DSECT                                                                  
RECNAME  DS    XL4                                                              
RECDISP  DS    H                                                                
RECFILL  DS    X                                                                
         DS    X                                                                
RCOTABLQ EQU   *-RCOTABLD                                                       
*                                                                               
NFI11    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF SUPPORTED RECORD FIELDS                                    *         
***********************************************************************         
         SPACE 1                                                                
FLDTABL  DS    0A                                                               
*                                                                               
*                FIELD     MIN   MAX   VAL                                      
*                NAME      LEN   LEN   ADD                                      
*              ------------------------------------                             
*GYREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGY)                                
*        DC    X'FF',X'0106'                                                    
*                                                                               
FLDAGY   DS    0F              *** AGENCY RECORD FIELDS                         
         DCDDL SP#AGY,20,R               AGENCY                                 
         DC    AL1(1,2,FLDRTN,FLDCPY)                                           
         DC    AL2(VAGY-NFI11)                                                  
         DC    X'00'                                                            
*                                                                               
FLDCLI   DS    0F              *** CLIENT RECORD FIELDS                         
         DCDDL SP#AGY,20,R               AGENCY                                 
         DC    AL1(1,2,FLDRTN,FLDCPY)                                           
         DC    AL2(VAGENCY-NFI11)                                               
         DCDDL SP#MED,20,R               MEDIA                                  
         DC    AL1(1,1,FLDRTN,0)                                                
         DC    AL2(VMEDIA-NFI11)                                                
         DCDDL SP#CLI,20,R               CLIENT                                 
         DC    AL1(2,2,FLDRTN,0)                                                
         DC    AL2(VCLIENT-NFI11)                                               
         DC    X'00'                                                            
*                                                                               
FLDPRO   DS    0F              *** PRODUCT RECORD FIELDS                        
         DCDDL SP#AGY,20,R               AGENCY                                 
         DC    AL1(1,2,FLDRTN,FLDCPY)                                           
         DC    AL2(VAGENCY-NFI11)                                               
         DCDDL SP#MED,20,R               MEDIA                                  
         DC    AL1(1,1,FLDRTN,0)                                                
         DC    AL2(VMEDIA-NFI11)                                                
         DCDDL SP#CLI,20,R               CLIENT                                 
         DC    AL1(2,2,FLDRTN,0)                                                
         DC    AL2(VCLIENT-NFI11)                                               
         DCDDL SP#PRO,20,R               PRODUCT                                
         DC    AL1(4,3,FLDRTN,0)                                                
         DC    AL2(VPROD-NFI11)                                                 
         DC    X'00'                                                            
*                                                                               
FLDEST   DS    0F              *** ESTIMATE RECORD FIELDS                       
         DCDDL SP#AGY,20,R               AGENCY                                 
         DC    AL1(1,2,FLDRTN,FLDCPY)                                           
         DC    AL2(VAGENCY-NFI11)                                               
         DCDDL SP#MED,20,R               MEDIA                                  
         DC    AL1(1,1,FLDRTN,0)                                                
         DC    AL2(VMEDIA-NFI11)                                                
         DCDDL SP#CLI,20,R               CLIENT                                 
         DC    AL1(2,2,FLDRTN,0)                                                
         DC    AL2(VCLIENT-NFI11)                                               
         DCDDL SP#PRO,20,R               PRODUCT                                
         DC    AL1(4,3,FLDRTN,0)                                                
         DC    AL2(VPROD-NFI11)                                                 
         DCDDL SP#EST,20,R               ESTIMATE                               
         DC    AL1(7,1,FLDBIN,0)                                                
         DC    AL2(0)                                                           
         DC    X'00'                                                            
*                                                                               
FLDBUY   DS    0F              *** BUY RECORD FIELDS                            
         DCDDL SP#AGY,20,R               AGENCY                                 
         DC    AL1(0,1,FLDRTN,FLDCPY),AL2(VAGENCY-NFI11)                        
         DCDDL SP#MED,20,R               MEDIA                                  
         DC    AL1(0,1,FLDRTN,0),AL2(VMEDIA-NFI11)                              
         DCDDL SP#CLI,20,R               CLIENT                                 
         DC    AL1(1,2,FLDRTN,0),AL2(VCLIENT-NFI11)                             
         DCDDL SP#PRO,20,R               PRODUCT                                
         DC    AL1(3,1,FLDRTN,0),AL2(VPRD-NFI11)                                
         DCDDL SP#STA,20,R               STATION                                
         DC    AL1(4,4,FLDRTN,0),AL2(VSTATION-NFI11)                            
         DCDDL SP#BAND,20,R              BAND                                   
         DC    AL1(4,5,FLDRTN,0),AL2(VBAND-NFI11)                               
         DCDDL SP#EST,20,R               ESTIMATE                               
         DC    AL1(09,1,FLDBIN,0),AL2(0)                                        
         DCDDL SP#LINE,20,R              LINE                                   
         DC    AL1(11,1,FLDRTN,0),AL2(VLINE-NFI11)                              
         DCDDL SP#SPIL1,20,R             SPILL                                  
         DC    AL1(10,1,FLDRTN,0),AL2(VSPILL-NFI11)                             
         DC    X'00'                                                            
*                                                                               
FLDTABLX DC    X'00'                     END OF SUPPORTED FIELDS                
*                                                                               
FLDTABLD DSECT                                                                  
FLDNAME  DS    XL4                 NAME OF FIELD                                
FLDDISP  DS    XL1                 DISP TO FIELD IN KEY                         
FLDLEN   DS    XL1                 LENGTH OF FIELD                              
FLDSTAT  DS    XL1                 STATUS 1 - TYPE OF INPUT                     
FLDCHR   EQU   X'80'               CHARACTER                                    
FLDNUM   EQU   X'40'               NUMERIC CHARACTER   (RIGHT ALIGNED)          
FLDHEX   EQU   X'20'               HEX                 (RIGHT ALIGNED)          
FLDBIN   EQU   X'10'               NUMERIC BINARY                               
FLDDTE   EQU   X'08'               DATE FIELD (PWOS)                            
FLDRTN   EQU   X'04'               A(VALIDATION ROUTINE) DEFINED                
FLDCOMP  EQU   X'02'               COMPLEMENT THIS FIELD                        
FLDLEFT  EQU   X'01'               DON'T RIGHT-ALIGN HEX/NUM FIELD              
*                                                                               
FLDSTAT2 DS    XL1                 STATUS 2                                     
FLD00S   EQU   X'80'               CLEAR FIELD TO X'00' S                       
FLD40S   EQU   X'40'               CLEAR FIELD TO X'40' S                       
FLDCPY   EQU   X'20'               COMPANY FIELD                                
FLDXKEY  EQU   X'10'               DON'T MOVE VALUE INTO KEY                    
*                                                                               
FLDVRTN  DS    AL2                 A(VALIDATION ROUTINE) - FLDRTN SET           
FLDTABLQ EQU   *-FLDTABLD                                                       
*                                                                               
NFI11    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
VMEDGET  DC    V(MEDGET)                                                        
HEXFFS   DC    80X'FF'                                                          
         SPACE 2                                                                
***********************************************************************         
* OTHER REQUIRED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
STATHDR  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
       ++INCLUDE SPNWSBYR          BUYER RECORD                                 
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
*                                                                               
ATHIS    DS    A                                                                
RTDSPDIR DS    A                                                                
LINENOW  DS    H                                                                
THISTYPE DS    X                                                                
LASTTYPE DS    X                                                                
ERRFLG   DS    X                                                                
*                                                                               
AGYMCD   DS    C                   MEDIA CODE (CHAR)                            
AGYMED   DS    X                   AGY/MEDIA (BIN)                              
AGYCLI   DS    XL2                 CLIENT CODE (BIN)                            
AGYPRO   DS    CL3                                                              
AGYSTN   DS    CL4                                                              
AGYBND   DS    XL5                                                              
AGYCTRY  DS    X                                                                
*                                                                               
         SPACE 2                                                                
*        SPFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE SPNFIWORK                                                      
         PRINT ON                                                               
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKNAME  DS    CL20                                                             
         ORG   TLDATA              RECORD LIST                                  
TLRDISP  DS    H                                                                
TLRFILL  DS    X                                                                
TLLNQ    EQU   *-TLSTD                                                          
*                                                                               
         ORG   TLDATA              FIELD LIST                                   
TLR1NAME DS    CL20                                                             
TLR1DISP DS    X                                                                
TLR1LEN  DS    X                                                                
TLR1STAT DS    X                                                                
TLR1STA2 DS    X                                                                
TLR1RTN  DS    XL2                                                              
TLR1DATA DS    CL50                                                             
TLLNQ1   EQU   *-TLSTD                                                          
*                                                                               
SAVED    DSECT                                                                  
KEYBLK   DS    CL150               KEY BUILD BLOCK TO PASS BACK                 
LISDISP  DS    H                                                                
KEYFILL  DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'032RENFI11   05/01/02'                                      
         END                                                                    
*          DATA SET GEKEY02    AT LEVEL 165 AS OF 11/07/96                      
***********************************************************************         
*                                                                     *         
*  TITLE: TF0502 - PFM INTERFACE OVERLAY FOR SPOT SYSTEM              *         
*                                                                     *         
*  CALLED FROM: PFM INTERFACE CONTROLLER (TF0500), WHICH CALLS        *         
*               DDGENCON (T00A30) WHICH CALLS THIS.                   *         
*                                                                     *         
*  OUTPUTS: KEY FOR THE RECORD TYPE ACCORDING TO USER INPUT           *         
*                                                                     *         
*  LOCALS: REGISTER USAGE                                             *         
*          R0 - WORK                                                  *         
*          R1 - WORK                                                  *         
*          R2 - POINTER TO SCREEN FLDH, ERREX WILL POSITION CURSOR    *         
*          R3 - WORK                                                  *         
*          R4 - WORK                                                  *         
*          R5 - WORK                                                  *         
*          R6 - USED FOR GETEL ELEMENT DSECT POINTER                  *         
*          R7 - WORK                                                  *         
*          R8 - SPOOLD                                                *         
*          R9 - SYSD                                                  *         
*          RA - TWA                                                   *         
*          RB - FIRST BASE                                            *         
*          RC - GEND                                                  *         
*          RD - SYSTEM                                                *         
*          RE - SYSTEM                                                *         
*          RF - SYSTEM                                                *         
*                                                                     *         
***********************************************************************         
         TITLE 'TF0502 - PFM INTERFACE OVERLAY FOR SPOT SYSTEM'                 
TF0502   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*TF0502*,R7,RR=R3                                              
         L     RC,0(R1)            STANDARD CODING                              
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN + OUR SCREEN                     
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         MVI   GETMSYS,23          USES GETMSG FOR SYSTEM 23                    
         MVC   RCPROG(2),=C'FM'    PREFIX FOR REPORT NO.                        
*                                                                               
         CLI   MODE,VALREC         VALIDATE RECORD?                             
         BE    VR                                                               
*                                                                               
EXIT02   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE SCREEN                                                 *         
***********************************************************************         
VR       DS    0H                                                               
         LA    R2,CONRCRDH         POINT TO RECORD FIELD FIRST                  
         CLI   5(R2),0             NO RECORD?                                   
         BE    MISSFLD             MISSING RECORD                               
         ZIC   R1,5(R2)                                                         
         BCTR  R1,0                                                             
         LA    R4,RECTABLE         POINT TO RECTABLE                            
         CLI   8(R2),C'?'          USER ASKED FOR HELP?                         
         BNE   VKRECLP             NOPE, CHECK ENTRY                            
*                                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         OI    STATFLAG,X'01'      HELP INVOKED                                 
         USING CONHEADH-64,RA                                                   
         XC    CONRCRD,CONRCRD                                                  
         OI    CONRCRDH+6,X'80'    TRANSMIT THE DATA                            
         GOTO1 CLRSCN,DMCB,CONP0H                                               
         GOTO1 DISPHELP,DMCB,RECTABLE                                           
         B     EXIT02                                                           
*                                                                               
VKRECLP  CLI   0(R4),X'FF'         END OF TABLE?                                
         BE    INVLFLD                                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   8(0,R2),0(R4)                                                    
         BE    VKPARTS             PARTS OF THE KEY                             
         LA    R4,L'RECTABLE(R4)                                                
         B     VKRECLP                                                          
         EJECT                                                                  
VKPARTS  DS    0H                                                               
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         TM    STATFLAG,X'01'      HELP INVOKED?                                
         USING CONHEADH-64,RA                                                   
         BZ    VKPARTS5                                                         
         GOTO1 CLRSCN,DMCB,CONP0H  CLEAR THE SCREEN                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
         NI    STATFLAG,X'FF'-X'01'  HELP INVOKED?                              
         USING CONHEADH-64,RA                                                   
VKPARTS5 L     R4,8(R4)            GET ADDRESS OF KEY COMPONENTS                
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    R4,R1                    TO RELOCATION FACTOR                    
         NI    6(R2),X'FF'-X'40'   DON'T PUT CURSOR HERE AGAIN                  
         LA    R2,CONP0H           POINT TO 1ST PROTECTED FIELD                 
         ZIC   R1,0(R2)            GET LENGTH TO NEXT FIELD                     
         AR    R1,R2               POINT TO THE 1ST INPUT FIELD                 
         OI    6(R1),X'40'         CURSOR SHOULD BE HERE                        
         LR    R3,R4               SAVE POSITION IN TABLE                       
VKPARTLP CLI   0(R4),X'FF'         NO MORE PARTS OF THE KEY?                    
         BNE   VKPART10            THERE'S MORE                                 
         LR    R4,R3               DONE, NOW VALIDATE COMPONENTS                
*                                                                               
VKCLR    DS    0H                                                               
         OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'20'         PROTECT FIELD                                
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         ZIC   R1,0(R2)            NO, BUMP TO INPUT FIELD                      
         AR    R2,R1                                                            
         XC    8(L'CONP0,R2),8(R2) CLEAR FIELD                                  
         OI    1(R2),X'20'         PROTECT FIELD                                
         OI    6(R2),X'80'         TRANSMIT                                     
         LA    R1,CONPFH           ARE WE AT THE END YET?                       
         CR    R2,R1                                                            
         BNL   VK00                YES, GO VALIDATE INPUT                       
         ZIC   R1,0(R2)                                                         
         AR    R2,R1               BUMP TO NEXT PROTECT FIELD                   
         B     VKCLR                                                            
*                                                                               
VKPART10 OI    6(R2),X'80'         TRANSMIT                                     
         OI    1(R2),X'08'         SET TO HIGH INTENSITY                        
         MVC   8(8,R2),0(R4)                                                    
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         NI    1(R2),X'FF'-X'20'   UNPROTECT FIELD                              
         ZIC   R1,0(R2)            GET LENGTH                                   
         AR    R2,R1               GOTO THE INPUT FIELD                         
         LA    R4,LTAB(R4)         NEXT COMNPONENT IN KEY                       
         B     VKPARTLP                                                         
         EJECT                                                                  
***********************************************************************         
* CODE TO EXPEDITE THE NEW TABLES FOR RECORD ENTRIES                  *         
***********************************************************************         
VK00     LA    R2,CONI0H                                                        
         XC    INTKEY,INTKEY                                                    
         MVI   CXFLAG,X'FF'        HEX FLAG ON                                  
         MVI   INSERT,X'00'        GET NEXT INSERT POSITION                     
*                                                                               
VK10     DS    0H                                                               
*                                                                               
         CLI   0(R4),X'FF'         LAST FIELD DONE?                             
         BNE   VK40                                                             
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(2),=C'K,'                                                
         CLI   1(R4),X'00'         NO RECORD CODE                               
         BH    VK20                                                             
         MVC   SAVEKEY+2(L'SAVEKEY-2),INTKEY   INSERT HEADER                    
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK20     CLI   1(R4),X'01'         1-BYTE RECORD CODE                           
         BH    VK30                                                             
         GOTO1 HEXOUT,DMCB,2(R4),SAVEKEY+2,1                                    
         MVC   SAVEKEY+4(L'SAVEKEY-4),INTKEY   INSERT HEADER                    
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
VK30     GOTO1 HEXOUT,DMCB,2(R4),SAVEKEY+2,2   2-BYTE REC. CODE                 
         MVC   SAVEKEY+6(L'SAVEKEY-6),INTKEY   INSERT HEADER                    
         XC    INTKEY,INTKEY                                                    
         MVC   INTKEY(L'INTKEY),SAVEKEY                                         
         B     VK50                                                             
*                                                                               
VK40     DS    0H                                                               
*                                                                               
         MVC   LENGTH,5(R2)        GET LENGTH OF FIELD                          
         CLC   LENGTH,8(R4)        INPUT LEN LESS THAN MIN LEN?                 
         BL    INVLFLD             YES, INVALID ENTRY                           
         CLC   LENGTH,9(R4)        INPUT LEN GREATER THAN MAX LEN?              
         BH    INVLFLD             YES, INVALID ENTRY                           
         MVC   PADNUM,9(R4)        GET DEFAULT NUMBER OF CHARS.                 
*                                                                               
         L     RF,10(R4)           GET A(BUILD ROUTINE)                         
         L     R1,RELO             ADDRESS IS RELATIVE                          
         AR    RF,R1                 TO RELOCATION FACTOR                       
         BASR  RE,RF               GET FIELD, INSERT INTO KEY                   
         LA    R4,LTAB(R4)         NEXT KEY COMPONENT                           
         LA    R2,DENTRY(R2)       BUMP TO NEXT SCREEN HEADER                   
         B     VK10                                                             
         USING PFMSAVED,RA         WHERE TO SAVE KEY AND DA                     
*                                                                               
VK50     DS    0H                                                               
*                                                                               
         MVC   DISKADDR(L'DISKA+L'INTKEY),DISKA                                 
         USING CONHEADH-64,RA                                                   
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* PREPARE KEY FOR INCOMING HEX FIELD                                  *         
***********************************************************************         
VXKEY    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         STCM  R3,15,SAVER3        SAVE R3                                      
         ZIC   R3,FLDLEN           GET FIELD LENGTH                             
         LA    R4,0(R3,R4)                                                      
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    EXIT02              YES RETURN                                   
         ICM   R3,15,SAVER3        RESTORE R3                                   
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
         STCM  R3,15,SAVER3        SAVE R3                                      
         BE    EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* INSERT CHARACTERS TO KEY                                            *         
***********************************************************************         
VCHAR    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'00'        CHAR. STRING SO FAR?                         
         BE    VCHAR10                                                          
         MVI   0(R3),C'('          NO, INSERT CHAR. HEADER                      
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VCHAR10  DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),SPACES      DEFAULT TO SPACES                            
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         SR    R3,R4               POIN R3 TO FIELD                             
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT02              STOP                                         
*                                                                               
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CHAR. NUMBERS                                        *         
***********************************************************************         
VNUM     NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BNE    VNUM10                                                          
         MVI   0(R3),C'('          YES, INSERT CHAR. HEADER                     
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'00'                                                     
VNUM10   DS    0H                                                               
         ZIC   R4,PADNUM                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),=10C'0'     DEFAULT TO ZEROS                             
         ZIC   R4,PADNUM                                                        
         LA    R3,0(R3,R4)                                                      
         MVI   0(R3),C')'                                                       
         ZIC   R5,INSERT                                                        
         LA    R5,0(R4,R5)         GET NEXT INSERT POSITION                     
         STC   R5,INSERT                                                        
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    EXIT02              STOP                                         
*                                                                               
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         SR    R3,R4                                                            
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),8(R2)       MOVE FIELD TO KEY                            
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH AGENCY                                               *         
***********************************************************************         
VAGY     NTR1                                                                   
         CLI   LENGTH,0                                                         
         BNE   VAGY10              IF NO ENTRY                                  
         MVC   8(2,R2),14(RA)      GET DEFAULT AGENCY                           
         MVI   5(R2),2                                                          
         MVC   LENGTH,5(R2)                                                     
         OI    6(R2),X'80'                                                      
VAGY10   DS    0H                                                               
         CLI   LENGTH,2            AGENCY MUST BE 2 CHARS.                      
         BNE   INVLFLD                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
         ST    R2,AGYHADD          STORE HEADER ADDRESS                         
         BAS   RE,VCHAR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE AGENCY ONLY, KEY WILL BE BUILT WITH AGENCY/MEDIA           *         
***********************************************************************         
VAGENCY  NTR1                                                                   
         CLI   LENGTH,0                                                         
         BNE   VAGENCY1            IF NO ENTRY                                  
         MVC   8(2,R2),14(RA)      GET DEFAULT AGENCY                           
         MVI   5(R2),2                                                          
         MVC   LENGTH,5(R2)                                                     
         OI    6(R2),X'80'                                                      
VAGENCY1 DS    0H                                                               
         CLI   LENGTH,2            AGENCY MUST BE 2 CHARS.                      
         BNE   INVLFLD                                                          
         XC    KEY,KEY                                                          
         MVI   KEY,6                                                            
         MVC   KEY+1(2),8(R2)                                                   
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
         ST    R2,AGYHADD          STORE HEADER ADDRESS                         
* NOW READ THE SF RECORD                                                        
         L     RE,AIO                                                           
         ST    RE,AREC                                                          
         GOTO1 GETREC              GET THE AGENCY RECORD                        
*                                                                               
         L     R3,AIO                                                           
         USING AGYHDRD,R3                                                       
         GOTO1 GETREC                                                           
         MVC   AGYCTRY,AGYPROF+7   EXTRACT COUNTRY CODE                         
         DROP  R3                                                               
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION AGENCY CODE                                  *         
***********************************************************************         
VMAGY    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         MVI   INTKEY+9,C'0'       PATCH KEY                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH REP AGENCY CODE                                      *         
***********************************************************************         
VRAGY    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         MVI   INTKEY+8,C'0'       PATCH KEY                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH SCHEME AGENCY CODE                                   *         
***********************************************************************         
VSAGY    NTR1                                                                   
         BAS   RE,VAGENCY                                                       
         L     R3,AGYHADD                                                       
         GOTO1 =V(MEDGET),DMCB,(C'T',8(R3)),DATAMGR,WORK,RR=RELO                
         CLI   8(R1),X'FF'                                                      
         BE    INVLFLD                                                          
*                                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY                                                         
         ICM   R3,15,SAVER3                                                     
         XC    SAVEKEY,SAVEKEY                                                  
         MVC   SAVEKEY(1),WORK     AGENCY & MEDIA                               
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH AGENCY/MEDIA                                         *         
***********************************************************************         
***********************************************************************         
* BUILD KEY WITH STATIOIN MEDIA                                       *         
***********************************************************************         
VMMED    NTR1                                                                   
         STCM  R2,15,MEDHADD                                                    
         MVC   INTKEY(2),=C'(M'    MARKET RECORD HEADER                         
         B     VSMED00                                                          
VRMED    NTR1                                                                   
         STCM  R2,15,MEDHADD                                                    
         MVC   INTKEY(2),=C'(R'    REP RECORD HEADER                            
         B     VSMED00                                                          
VSMED    NTR1                                                                   
         STCM  R2,15,MEDHADD                                                    
         MVC   INTKEY(2),=C'(S'    STATION RECORD HEADER                        
VSMED00  MVC   INTKEY+2(16),=16C'0' PAD WITH CHAR ZEROS                         
         MVI   INTKEY+18,C')'                                                   
         MVI   INSERT,X'02'                                                     
         MVI   CXFLAG,X'00'        CHAR FLAG ON                                 
         BAS   RE,VCHAR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION CALL LETTER                                  *         
***********************************************************************         
VCALL    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         L     R3,MEDHADD                                                       
         CLI   8(R3),C'T'                                                       
         BNE   EXIT02                                                           
         MVI   INTKEY+7,C'T'                                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION CLIENT                                       *         
***********************************************************************         
VSCLT    NTR1                                                                   
         BAS   RE,VCHAR                                                         
         MVI   INTKEY+13,C'0'      PATCH KEY                                    
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE SPOT LENGTH (GOAL)                                         *         
***********************************************************************         
VSPLEN   NTR1                                                                   
         ST    R2,SPLHADD          SAVE SPOT LENGTH HEADER ADDRESS              
         BAS   RE,VBIN                                                          
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BINARY (TOTAL SEC - GOAL)                            *         
***********************************************************************         
VTOTSEC  NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VTOTSEC5            THEN COPY SPOT LENGTH                        
         BAS   RE,VBIN                                                          
         B     EXIT02                                                           
VTOTSEC5 L     R4,SPLHADD          COPY SPOT LENGTH                             
         LR    R2,R4                                                            
         MVC   LENGTH,5(R2)        UPDATE LENGTH!                               
         BAS   RE,VBIN                                                          
         B     EXIT02                                                           
***********************************************************************         
* BUILD KEY WITH BINARY                                               *         
***********************************************************************         
VBIN     NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VBIN30                                                           
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         CLI   PADNUM,4                                                         
         BL    VBIN05                                                           
         C     R4,=F'65535'        BOUNDARY CHECK FOR 2-BYTE BINARY             
         BH    INVLFLD                                                          
         STCM  R4,3,WORK                                                        
         B     VBIN30                                                           
VBIN05   CLI   PADNUM,3                                                         
         BL    VBIN10                                                           
         CH    R4,=H'255'          BOUNDARY CHECK FOR 1-BYTE BINARY             
         BH    INVLFLD                                                          
         STC   R4,WORK                                                          
         B     VBIN20                                                           
VBIN10   CH    R4,=H'99'           BOUNDARY CHECK FOR YEAR                      
         BH    INVLFLD                                                          
VBIN20   STC   R4,WORK                                                          
*                                                                               
VBIN30   LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         CLI   PADNUM,4                                                         
         BL    VBIN40                                                           
         LA    R4,2(R4)            2-BYTE BINARY                                
VBIN40   LA    R4,2(R4)            1-BYTE BINARY                                
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BE    VBIN50                                                           
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
*                                                                               
VBIN50   DS    0H                                                               
         CLI   PADNUM,3                                                         
         BH    VBIN60                                                           
         MVC   0(2,R3),=C'00'                                                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
VBIN60   DS    0H                                                               
         MVC   0(4,R3),=C'0000'                                                 
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* GET MARKET NUMBER                                                   *         
***********************************************************************         
VMARKET  NTR1                                                                   
         ST    R2,MKTHADD          SAVE MARKET HEADER ADDRESS                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         TM    4(R2),X'08'         THIS INPUT NUMERIC?                          
         BZ    INVLFLD                                                          
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH MARKET/STATION                                       *         
***********************************************************************         
VSTAT    NTR1                                                                   
         L     R3,MKTHADD          IF NO MARKET                                 
         CLI   5(R3),0                                                          
         BH    VSTAT05                                                          
         CLI   LENGTH,0            AND NO STATION                               
         BE    EXIT02              THEN STOP                                    
         L     R2,MKTHADD                                                       
         B     INVLFLD             STATION W/O MARKET=ERROR                     
VSTAT05  CLI   LENGTH,0            ERROR IF MARKET IS ENTERED W/O               
         BE    INVLFLD             STATION                                      
         TM    4(R2),X'04'         MUST BE ALPHABETIC                           
         BZ    INVLFLD                                                          
         LA    R3,INTKEY           GET INTKEY                                   
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         LA    R4,10(R4)            1-BYTE BINARY                               
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   CXFLAG,X'FF'        HEX STRING SO FAR?                           
         BE    VSTAT10                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VSTAT10  DS    0H                                                               
         MVC   WORK1(4),=C'0000'   PAD FIELD WITH ZEROS                         
         L     R4,MKTHADD                                                       
         ZIC   R5,5(R4)            GET LENGTH OF MARKET                         
         BCTR  R5,0                SUBTRACT 1 FOR EX                            
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   WORK2(0),8(R4)      GET MARKET                                   
         LA    R4,WORK1                                                         
         LA    R4,3(R4)            RIGHT JUSTIFY                                
         SR    R4,R5                                                            
         EX    R5,*+8              R5 ALREADY SUBTRACTED BY 1                   
         B     *+10                                                             
         MVC   0(0,R4),WORK2       STORE MARKET                                 
*                                                                               
VSTAT30  DS    0H                                                               
         XC    WORK,WORK                                                        
         LA    R4,WORK                                                          
         USING STAPACKD,R4                                                      
*                                                                               
         MVI   STAPACT,C'U'                                                     
         MVC   STAPAGY,AGENCY                                                   
         MVC   STAPCTRY,AGYCTRY                                                 
*                                                                               
         L     R2,MEDHADD          GET MEDIA HEADER ADDRESS                     
         MVC   STAPMED,8(R2)       MOVE MEDIA CODE                              
*                                                                               
         MVC   STAPQMKT,WORK1      MOVE MARKET NUMBER                           
         MVC   STAPQSTA,8(R2)      MOVE STATION                                 
         MVC   STAPACOM,ACOMFACS                                                
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A7A' STAPACK                                   
         GOTO1 CALLOV,DMCB                                                      
         CLI   4(R1),X'FF'                                                      
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,DMCB             GET STAPACK ADDRESS                          
         GOTO1 (RF),(R4)                                                        
         CLI   STAPERR,0                                                        
         BNE   INVLFLD                                                          
*                                                                               
         GOTO1 HEXOUT,DMCB,STAPMKST,0(R3),5                                     
         B     EXIT02                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH MONTH                                                *         
***********************************************************************         
VMONTH   NTR1                                                                   
         CLI   LENGTH,0            IF NO ENTRY                                  
         BE    VMONTH30                                                         
         TM    4(R2),X'08'         TEST IF NUMERIC                              
         BNZ   VMONTH20                                                         
*                                                                               
         LA    R3,MONTHLST         CHECK IF MONTH LIST                          
         LA    R4,12               12 MONTHS IN A YEAR                          
VMONTH10 CLC   0(3,R3),8(R2)                                                    
         BE    VMONTH15                                                         
         LA    R3,3(R3)            BUMP TO NEXT MONTH                           
         BCT   R4,VMONTH10                                                      
         B     INVLFLD             MONTH NOT FOUND                              
VMONTH15 LA    R3,13               CALCUTE NUMERIC EQUIVALENT                   
         SR    R3,R4                                                            
         STC   R3,WORK                                                          
         B     VMONTH30                                                         
VMONTH20 ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)                                                   
         CVB   R4,DUB                                                           
         CH    R4,=H'12'           BOUNDARY CHECK FOR MONTH                     
         BH    INVLFLD                                                          
         CH    R4,=H'1'            BOUNDARY CHECK FOR MONTH                     
         BL    INVLFLD                                                          
         STC   R4,WORK                                                          
*                                                                               
VMONTH30 DS    0H                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         MVC   0(2,R3),=C'01'                                                   
         CLI   LENGTH,0                                                         
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BUYER                                                *         
***********************************************************************         
VBUYER   NTR1                                                                   
         OC    8(3,R2),SPACES      PAD WITH SPACE                               
         MVI   5(R2),3                                                          
         XC    KEY,KEY             CLEAR KEY                                    
         MVC   KEY(2),=X'0D65'     VERIFY BUYER                                 
         MVC   KEY+2(1),SAVEKEY    A/M                                          
         MVC   KEY+3(3),8(R2)                                                   
         GOTO1 HIGH                READ IN THE BUYER REC                        
         CLC   KEY(13),KEYSAVE                                                  
         BNE   INVLFLD                                                          
         L     R1,AIO                                                           
         ST    R1,AREC                                                          
         GOTO1 GETREC              READ THE RECORD IN                           
         L     R1,AIO                                                           
         USING BYRRECD,R1                                                       
         MVC   WORK(1),BYRCODE                                                  
         MVC   SAVEKEY+1(1),BYRCODE                                             
         DROP  R1                                                               
*                                                                               
         MVI   FLDLEN,2                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
VBUYER10 GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH CAMPAIGN                                             *         
***********************************************************************         
VCAMP    NTR1                                                                   
         SR    R3,R3               USED TO FIGURE OUT 1'S COMPLEMENT            
         CLI   LENGTH,0            NOTHING INPUTTED?                            
         BE    VCAMP10                                                          
         TM    4(R2),8             NUMERIC?                                     
         BZ    INVLFLD             NO                                           
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         C     R3,=F'65535'        > X'FFFF'?                                   
         BH    INVLFLD             YES                                          
*                                                                               
VCAMP10  X     R3,=F'65535'        1'S COMPLEMENT FOR HELP WORD                 
         STCM  R3,3,WORK                                                        
         STCM  R3,3,SAVEKEY+2                                                   
*                                                                               
         MVI   FLDLEN,4                                                         
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH NWH MARKET                                           *         
***********************************************************************         
VDMKT    NTR1                                                                   
         CLI   LENGTH,0            NOTHING INPUTTED?                            
         BE    VDMKT10                                                          
         TM    4(R2),8             NUMERIC?                                     
         BZ    INVLFLD             NO                                           
         ZIC   R4,LENGTH                                                        
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R3,DUB                                                           
         STCM  R3,3,SAVEKEY+4                                                   
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0D67'     VERIFY BUYER                                 
         MVC   KEY+2(6),SAVEKEY    A/M                                          
         GOTO1 HIGH                READ IN THE BUYER REC                        
         CLC   KEY(8),KEYSAVE                                                   
         BNE   INVLFLD                                                          
         MVC   WORK(2),KEY+8       COPY CAMP/MKT SEQ #                          
*                                                                               
VDMKT10  LA    R3,INTKEY+4         POINT TO INTKEY+4                            
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VDMKT20                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         MVI   CXFLAG,X'FF'                                                     
VDMKT20  DS    0H                                                               
         CLI   LENGTH,0                                                         
         BNE   VDMKT30                                                          
         MVC   0(4,R3),=C'0000'                                                 
         B     EXIT02                                                           
VDMKT30  GOTO1 HEXOUT,DMCB,WORK,0(R3),2                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH STATION/MARKET                                       *         
***********************************************************************         
VSSTA    NTR1                                                                   
         CLI   LENGTH,0            NO INPUT?                                    
         BNE   VSSTA03             YES, MOVE NULLS IN                           
         XC    WORK,WORK                                                        
         MVI   FLDLEN,10                                                        
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),5                                         
         B     EXIT02                                                           
*                                                                               
VSSTA03  TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    VSSTA05             NO, INPUT IS STATION                         
         ZIC   R4,LENGTH           YES, STICK MKT TO HIGH ORDER 2 BYTES         
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)      DECIMAL YEAR                                 
         CVB   R4,DUB                                                           
         C     R4,=F'65535'        MKT > X'FFFF'?                               
         BH    INVLFLD                                                          
         XC    WORK,WORK                                                        
         STCM  R4,3,WORK                                                        
         MVI   FLDLEN,10                                                        
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),5                                         
         B     EXIT02                                                           
*                                                                               
VSSTA05  DS    0H                                                               
         CLI   LENGTH,4            INPUT IS STATION                             
         BH    INVLFLD                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(16),KEY                                                    
         MVI   KEY,C'S'                                                         
         MVI   KEY+1,C'T'          MEDIA LETTER ALWAYS T                        
         OC    8(4,R2),SPACES      PAD WITH SPACE                               
         MVC   KEY+2(4),8(R2)      STATION CALL LETTERS                         
         MVI   KEY+6,C'T'          NO BAND                                      
         L     R3,AGYHADD                                                       
         MVC   KEY+7(2),8(R3)      AGENCY LETTERS                               
         L     R3,CLTHADD                                                       
         MVC   KEY+9(3),8(R3)      (SCHEME) CLIENT LETTERS                      
         BAS   RE,SSTAHI                                                        
         L     R3,AIO                                                           
         CLC   KEY(17),0(R3)                                                    
         BE    VSSTA10                                                          
         MVC   KEY+9(3),=3C'0'     (SCHEME) CLIENT LETTERS                      
         BAS   RE,SSTAHI            READ THE STATION RECORD                     
         L     R3,AIO                                                           
         CLC   KEY(17),0(R3)                                                    
         BNE   INVLFLD                                                          
*                                                                               
         USING STATHDR,R3                                                       
VSSTA10  MVC   DUB(L'SMKT),SMKT                                                 
         DROP  R3                                                               
*                                                                               
         XC    DMCB(8),DMCB                                                     
         MVC   DMCB+4(4),=X'D9000A1B' MSPACK                                    
         GOTO1 CALLOV,DMCB                                                      
         L     RF,DMCB             A(MARKET/STATION PACK ROUTINE)               
         GOTO1 (RF),(R1),DUB,KEY+2,WORK                                         
*                                                                               
         MVI   FLDLEN,10                                                        
         BAS   RE,VXKEY            PREPARE KEY                                  
         ICM   R3,15,SAVER3        RESTORE R3                                   
         GOTO1 HEXOUT,DMCB,WORK,0(R3),5                                         
         B     EXIT02                                                           
*                                                                               
SSTAHI   NTR1                      DONT READ                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'STATION',KEY,AIO                      
         XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH DAY PART COED                                        *         
***********************************************************************         
VDAYPT   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VDAYPT10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VDAYPT10 DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,2(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVC   0(2,R3),=C'00'      MOVE IN NULLS                                
         CLI   LENGTH,0            IF NO INPUT EXIT                             
         BE    EXIT02                                                           
         GOTO1 HEXOUT,DMCB,8(R2),0(R3),1                                        
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK YEAR (PERIOD)                                   *         
***********************************************************************         
VPBKYR   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VPBKYR10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VPBKYR10 DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,14(R4)                                                        
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVC   0(14,R3),=14C'0'                                                 
         BAS   RE,VBKYR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK YEAR (NSID)                                     *         
***********************************************************************         
VNBKYR   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VNBKYR10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VNBKYR10 DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,2(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVC   0(2,R3),=C'00'      DTP# ALWAYS NULL                             
         BAS   RE,VBKYR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK YEAR                                            *         
***********************************************************************         
VBKYR    NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VBKYR10                                                          
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,1(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VBKYR10  DS    0H                                                               
         ZIC   R4,INSERT                                                        
         LA    R4,2(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         CLI   LENGTH,0            ANY INPUT?                                   
         BNE   VBKYR20                                                          
         MVC   0(2,R3),=C'00'      NOPE, DEFAULT TO NULLS                       
         B     EXIT02                                                           
VBKYR20  TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH           GET LENGTH OF INPUT                          
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)      DECIMAL YEAR                                 
         CVB   R4,DUB                                                           
         LCR   R5,R4               GET 2'S COMPLEMENT                           
         BCTR  R5,0                CHANGE TO 1'S COMPLEMENT                     
         STC   R5,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH BOOK MONTH                                           *         
***********************************************************************         
VBKMON   NTR1                                                                   
         LA    R3,INTKEY           POINT TO INTKEY                              
         ZIC   R4,INSERT           POINT TO INSERT POSITION                     
         LA    R3,0(R3,R4)         COMPUTE INSERT POSITION                      
         CLI   CXFLAG,X'FF'        HEX. STRING SO FAR?                          
         BE    VBKMON10                                                         
         LA    R3,1(R3)            BUMP FORWARD                                 
         ZIC   R4,INSERT                                                        
         LA    R4,3(R4)                                                         
         STC   R4,INSERT           GET NEXT INSERT POSITION                     
         MVI   CXFLAG,X'FF'                                                     
VBKMON10 DS    0H                                                               
         CLI   LENGTH,0            ANY INPUT?                                   
         BNE   VBKMON20                                                         
         MVC   0(2,R3),=C'00'      NOPE, DEFAULT TO NULLS                       
         B     EXIT02                                                           
VBKMON20 TM    4(R2),X'08'         TEST IF NUMERIC                              
         BZ    INVLFLD                                                          
         ZIC   R4,LENGTH           GET LENGTH OF INPUT                          
         BCTR  R4,0                SUBTRACT 1 FOR EX                            
         EX    R4,*+8                                                           
         B     *+10                                                             
         PACK  DUB(8),8(0,R2)      DECIMAL YEAR                                 
         CVB   R4,DUB                                                           
         CH    R4,=H'12'           SOURCE VALID?                                
         BH    INVLFLD                                                          
         CH    R4,=H'1'            INPUT BETWEEN 1 AND 12?                      
         BL    INVLFLD                                                          
         STC   R4,WORK                                                          
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
         MVI   0(R3),C'8'                                                       
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH NETWORK (NETWORK DEFINITON RECORD)                   *         
***********************************************************************         
VNET     NTR1                                                                   
         CLI   LENGTH,3                                                         
         BL    INVLFLD                                                          
         CLI   LENGTH,4                                                         
         BH    INVLFLD                                                          
         XC    KEY,KEY                                                          
         LR    R5,R4                                                            
VN10     CLI   0(R5),X'FF'                                                      
         BE    VN20                                                             
         LA    R5,LTAB(R5)                                                      
         B     VN10                                                             
VN20     MVC   KEY(2),2(R5)                                                     
         MVC   KEY+2(2),14(RA)     GET DEFAULT AGENCY                           
         ZIC   R5,LENGTH                                                        
         BCTR  R5,0                                                             
         EX    R5,*+8                                                           
         B     *+10                                                             
         MVC   KEY+4(0),8(R2)                                                   
         OI    KEY+7,C' '                                                       
         GOTO1 HIGH                                                             
         CLC   KEY(8),KEYSAVE                                                   
         BNE   INVLFLD                                                          
         BAS   RE,VCHAR                                                         
         B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH AGENCY/MEDIA IN HEX (DEMO DEFINITION RECORD) *                 
***********************************************************************         
VDEMAGY  NTR1                                                                   
         BAS   RE,VAGENCY                                                       
         GOTO1 =V(MEDGET),DMCB,(C'N',AGENCY),DATAMGR,WORK,RR=RELO               
         LA    R3,INTKEY                                                        
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         LA    R4,2(R4)            NEXT INSERT POSITION                         
         STC   R4,INSERT                                                        
         GOTO1 HEXOUT,DMCB,WORK,0(R3),1                                         
VDEMAGYX B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH NETWORK (DEMO DEFINITION RECORD)                     *         
***********************************************************************         
VDEMNET  NTR1                                                                   
         CLI   LENGTH,3                                                         
         BL    INVLFLD                                                          
         CLI   LENGTH,4                                                         
         BH    INVLFLD                                                          
         XC    KEY,KEY                                                          
*                                                                               
         LA    R4,KEY                                                           
         USING NDEFRECD,R4         NETWORK DEF'N RECORD                         
         MVC   NDEFKTYP,=X'0D11'   RECORD ID                                    
         MVC   NDEFKAGY,AGENCY     AGENCY ID                                    
         OC    8(4,R2),=C'    '                                                 
         MVC   NDEFKNET,8(R2)      NETWORK (OFF SCREEN)                         
         GOTO1 HIGH                DOES NETWORK EXIST?                          
         CLC   KEY(8),KEYSAVE                                                   
         BNE   INVLFLD             NO - NETWORK DOESN'T EXIST                   
*                                                                               
         GOTO1 GETREC              GET HIGH NETWORK RECORD                      
         L     R3,AIO                                                           
         MVI   ELCODE,X'02'        NETWORK ELEMENT X'02'                        
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING NDEFEL02,R3                                                      
         MVC   SVNETSEQ,NDEFNET    NETWORK SEQUENCE NUMBER                      
         DROP  R3,R4                                                            
*                                                                               
         LA    R3,INTKEY                                                        
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         LA    R4,2(R4)            NEXT INSERT POSITION                         
         STC   R4,INSERT                                                        
         GOTO1 HEXOUT,DMCB,SVNETSEQ,0(R3),1                                     
*                                                                               
VALDEMX  B     EXIT02                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH SHOW                                                 *         
***********************************************************************         
VSHOW    NTR1                                                                   
         LA    R3,INTKEY                                                        
         ZIC   R4,INSERT           (SPARE) GET INSERT POSITION                  
         LA    R3,0(R3,R4)                                                      
         LA    R4,2(R4)            NEXT INSERT POSITION                         
         STC   R4,INSERT                                                        
*                                                                               
         MVC   0(2,R3),=C'00'    SPARE                                          
*                                                                               
         CLI   LENGTH,0            ANY SHOW?                                    
         BE    VSHOWX                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING NPGMRECD,R5                                                      
         MVC   NPGMKTYP,=X'0D12'   RECORD ID                                    
         MVC   NPGMKAGY,AGENCY     AGENCY ID                                    
         LA    R2,CONP0H                                                        
         BAS   RE,BMPFLD                                                        
         BAS   RE,BMPFLD                                                        
         BAS   RE,BMPFLD                                                        
         MVC   NPGMKNET,8(R2)      NETWORK (OFF SCREEN)                         
         BAS   RE,BMPFLD                                                        
         BAS   RE,BMPFLD                                                        
         BAS   RE,BMPFLD                                                        
         BAS   RE,BMPFLD                                                        
         MVC   NPGMKID,8(R2)       PROGRAM NAME (OFF SCREEN)                    
         GOTO1 HIGH                DOES PROGRAM EXIST?                          
         CLC   KEY(12),KEYSAVE                                                  
         BNE   INVLFLD             NO - NETWORK DOESN'T EXIST                   
*                                                                               
         BAS   RE,VCHAR                                                         
*                                                                               
VSHOWX   B     EXIT02                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD KEY WITH RATING SERVICE (DEMO DEFINITION RECORD)                        
***********************************************************************         
VRTS     NTR1                                                                   
         CLI   LENGTH,0            ANY SHOW?                                    
         BE    VRTSX                                                            
*                                                                               
         LA    R3,INTKEY                                                        
         ZIC   R4,INSERT           GET INSERT POSITION                          
         LA    R3,0(R3,R4)         COMPUTE ADDRESS OF INSERT POSITION           
         LA    R4,1(R4)                                                         
         STC   R4,INSERT                                                        
*                                                                               
         MVC   0(2,R3),=C'1)'                                                   
         CLC   =C'BBM',8(R2)                                                    
         BE    VRTSX                                                            
         CLC   =C'NSI',8(R2)                                                    
         BNE   INVLFLD                                                          
         MVC   0(2,R3),=C'0)'                                                   
*                                                                               
VRTSX    B     EXIT02                                                           
         EJECT                                                                  
                                                                                
***********************************************************************         
* BUMP TO NEXT FIELD ON SCREEN                                        *         
***********************************************************************         
BMPFLD   ZIC   R1,0(R2)                                                         
         AR    R2,R1                                                            
         BR    RE                                                               
         EJECT                                                                  
*                                                                               
RELO     DS    A                                                                
*                                                                               
* ERROR MESSAGES                                                                
*                                                                               
MYERROR  GOTO1 ERREX2                                                           
ERREXIT  GOTO1 ERREX                                                            
*                                                                               
MISSFLD  MVI   ERROR,MISSING                                                    
         B     ERREXIT                                                          
*                                                                               
INVLFLD  MVI   ERROR,INVALID                                                    
         B     ERREXIT                                                          
*                                                                               
         GETEL R3,DATADISP,ELCODE                                               
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
MONTHLST DC    C'JANFEBMARAPRMAYJUNJULAUGSEPOCTNOVDEC'                          
*                                                                               
*                                CL8 - NAME OF THE RECORD TYPE                  
*                                AL4 - ADDRESS OF KEY PORTIONS                  
*                                                                               
RECTABLE DS    0CL12                                                            
         DC    C'AGY     ',AL4(AGYREC)                                          
         DC    C'CLT     ',AL4(CLTREC)                                          
         DC    C'PRD     ',AL4(PRDREC)                                          
         DC    C'EST     ',AL4(ESTREC)                                          
         DC    C'GOAL    ',AL4(GOALREC)                                         
         DC    C'STABILL ',AL4(STABREC)                                         
         DC    C'BILL    ',AL4(BILLREC)                                         
         DC    C'BUY     ',AL4(BUYREC)                                          
         DC    C'NWH     ',AL4(NWHREC)                                          
         DC    C'NWD     ',AL4(NWDREC)                                          
         DC    C'STATION ',AL4(STATREC)                                         
         DC    C'MKT     ',AL4(MKTREC)                                          
         DC    C'REP     ',AL4(REPREC)                                          
         DC    C'SCHEME  ',AL4(SCHREC)                                          
         DC    C'PERIOD  ',AL4(PERREC)                                          
         DC    C'NSID    ',AL4(NSIDREC)                                         
         DC    C'DETAIL  ',AL4(DETREC)                                          
         DC    C'PW-MKT  ',AL4(PWMKTREC)                                        
         DC    C'PW-STA  ',AL4(PWSTAREC)                                        
         DC    C'NETDEF  ',AL4(NWKREC)                                          
         DC    C'DEMODEF ',AL4(DEMOREC)                                         
         DC    X'FF'                                                            
         EJECT                                                                  
*                                                                               
*                                CL8 - NAME OF THE FIELD TO INPUT               
*                                XL1 - MINIMUM LENGTH OF INPUT                  
*                                XL1 - MAXIMUM LENGTH OF INPUT                  
*                                AL4 - ADDRESS OF VALIDATING ROUTINE            
*                                                                               
*                                                                               
*                FIELD     MIN   MAX   VAL                                      
*                NAME      LEN   LEN   ADD                                      
*              ------------------------------------                             
GOALREC  DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'MARKET #',X'00',X'05',AL4(VBIN)                                
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'DAYPART ',X'00',X'01',AL4(VDAYPT)                              
         DC    C'SPOT LEN',X'00',X'03',AL4(VSPLEN)                              
         DC    C'TOT. SEC',X'00',X'03',AL4(VTOTSEC)                             
         DC    C'AGY CODE',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'0102'                                                    
STABREC  DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'MARKET #',X'00',X'04',AL4(VMARKET)                             
         DC    C'STATION ',X'00',X'05',AL4(VSTAT)                               
         DC    X'FF',X'020E01'                                                  
BILLREC  DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPROD)                               
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'YEAR SVC',X'00',X'02',AL4(VBIN)                                
         DC    C'MON. SVC',X'00',X'09',AL4(VMONTH)                              
         DC    C'BILL MON',X'00',X'09',AL4(VMONTH)                              
         DC    C'BILL #  ',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'0100'                                                    
NWHREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'BUYER   ',X'02',X'03',AL4(VBUYER)                              
         DC    C'CAMPAIGN',X'00',X'05',AL4(VCAMP)                               
         DC    C'MARKET  ',X'00',X'04',AL4(VBIN)                                
         DC    X'FF',X'020D67'                                                  
*                                                                               
NWDREC   DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'BUYER   ',X'02',X'03',AL4(VBUYER)                              
         DC    C'CAMPAIGN',X'00',X'05',AL4(VCAMP)                               
         DC    C'MARKET  ',X'00',X'04',AL4(VDMKT)                               
         DC    X'FF',X'020D68'                                                  
*                                                                               
STATREC  DC    C'MEDIA   ',X'01',X'01',AL4(VSMED)                               
         DC    C'CALL LET',X'00',X'05',AL4(VCALL)                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VCHAR)                               
         DC    C'CLIENT  ',X'00',X'03',AL4(VSCLT)                               
         DC    X'FF',X'00'                                                      
*                                                                               
MKTREC   DC    C'MEDIA   ',X'01',X'01',AL4(VMMED)                               
         DC    C'MKT CODE',X'00',X'04',AL4(VNUM)                                
         DC    C'AGY CODE',X'00',X'02',AL4(VMAGY)                               
         DC    X'FF',X'00'                                                      
*                                                                               
REPREC   DC    C'MEDIA   ',X'01',X'01',AL4(VRMED)                               
         DC    C'REP CODE',X'00',X'03',AL4(VNUM)                                
         DC    C'AGY CODE',X'00',X'02',AL4(VRAGY)                               
         DC    X'FF',X'00'                                                      
*                                                                               
SCHREC   DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    X'FF',X'010C'                                                    
*                                                                               
PERREC   DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    C'YEAR    ',X'00',X'02',AL4(VPBKYR)                              
         DC    X'FF',X'010C'                                                    
*                                                                               
NSIDREC  DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    C'STATION ',X'00',X'05',AL4(VSSTA)                               
         DC    C'DAYPART ',X'00',X'01',AL4(VDAYPT)                              
         DC    C'YEAR    ',X'00',X'02',AL4(VNBKYR)                              
         DC    C'PERIOD  ',X'00',X'02',AL4(VBKMON)                              
         DC    X'FF',X'010C'                                                    
*                                                                               
DETREC   DC    C'AGENCY  ',X'00',X'02',AL4(VSAGY)                               
         DC    C'SCHEME  ',X'00',X'03',AL4(VCLT)                                
         DC    C'STATION ',X'00',X'05',AL4(VSSTA)                               
         DC    C'DAYPART ',X'00',X'01',AL4(VDAYPT)                              
         DC    C'SEQ NUM ',X'00',X'03',AL4(VBIN)                                
         DC    C'YEAR    ',X'00',X'02',AL4(VBKYR)                               
         DC    C'PERIOD  ',X'00',X'02',AL4(VBKMON)                              
         DC    X'FF',X'010C'                                                    
*                                                                               
PWMKTREC DS    0X                                                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    C'MARKET #',X'00',X'04',AL4(VBIN)                                
         DC    X'FF',X'020D7A'                                                  
*                                                                               
PWSTAREC DS    0X                                                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VAGENCY)                             
         DC    C'MEDIA   ',X'01',X'01',AL4(VMEDIA)                              
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'PRODUCT ',X'02',X'03',AL4(VPRD)                                
         DC    C'ESTIMATE',X'01',X'03',AL4(VBIN)                                
         DC    C'STATION ',X'03',X'04',AL4(VSTATION)                            
         DC    C'BAND    ',X'00',X'01',AL4(VBAND)                               
         DC    X'FF',X'020D7A'                                                  
*                                                                               
NWKREC   DS    0X                                                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VAGY)                                
         DC    C'NETWORK ',X'03',X'04',AL4(VNET)                                
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'ESTIMATE',X'00',X'03',AL4(VBIN)                                
         DC    X'FF',X'020D11'                                                  
*                                                                               
DEMOREC  DS    0X                                                               
         DC    C'AGENCY  ',X'00',X'02',AL4(VDEMAGY)                             
         DC    C'NETWORK ',X'03',X'04',AL4(VDEMNET)                             
         DC    C'CLIENT  ',X'02',X'03',AL4(VCLT)                                
         DC    C'SHOW    ',X'00',X'04',AL4(VSHOW)                               
         DC    C'RTS SVC ',X'00',X'03',AL4(VRTS)                                
         DC    C'SEQ #   ',X'00',X'01',AL4(VBIN)                                
         DC    X'FF',X'020D17'                                                  
*                                                                               
         EJECT                                                                  
*                                                                               
DCODE    EQU   CONP1H-CONP0H                                                    
DENTRY   EQU   CONI1H-CONI0H                                                    
LTAB     EQU   STA1-STA                                                         
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENNPGM                                                      
       ++INCLUDE SPGENNDEF                                                      
       ++INCLUDE DDPERVALD                                                      
       ++INCLUDE FAFACTS                                                        
       ++INCLUDE FALANG                                                         
       ++INCLUDE DDSPOOLD          (GENERAL PRINT AREAS)                        
       ++INCLUDE DDSPLWORKD        (GENERAL CONTROLLER AREAS)                   
         PRINT ON                                                               
       ++INCLUDE GEKEYFFD          (BASE SCREEN FOR SYSTEM)                     
         EJECT                                                                  
       ++INCLUDE GEPFMSAVE         (OUR MAINTENANCE SCREEN OVERLAY)             
         PRINT OFF                                                              
       ++INCLUDE SPSTAPACKD                                                     
AGYHDRD  DSECT                                                                  
       ++INCLUDE SPGENAGY                                                       
       ++INCLUDE GEKEYWORKD        (SYSTEM AREAS)                               
*        PRINT ON                                                               
* MY STORAGE AREA                                                               
         ORG   SYSSPARE                                                         
DISKA    DS    XL4                                                              
INTKEY   DS    CL60                                                             
SAVEKEY  DS    CL(L'INTKEY)                                                     
PREVKEY  DS    CL(L'INTKEY)                                                     
AREC     DS    A                                                                
AGYHADD  DS    A                   AGENCY FIELD HEADER ADDRESS                  
MEDHADD  DS    A                   MEDIA FIELD HEADER ADDRESS                   
CLTHADD  DS    A                   CLIENT FIELD HEADER ADDRESS                  
MKTHADD  DS    A                   MARKET FIELD HEADER ADDRESS                  
STTHADD  DS    A                   STATION FIELD HEADER ADDRESS                 
SPLHADD  DS    A                   SPOT LENGTH HEADER ADDRESS                   
SAVER3   DS    F                   SAVES CONTENT OF REGISTER 3                  
LENGTH   DS    X                   INPUT FIELD LENGTH                           
PADNUM   DS    X                   NUMBER OF PADDING CHARS.                     
CXFLAG   DS    X                   X'00' IF CHAR, X'FF' IF HEX                  
INSERT   DS    X                   NEXT INSERT POSITION                         
FLDLEN   DS    X                   OUTPUT FIELD LENGTH                          
AGYCTRY  DS    C                   COUNTRY CODE                                 
SVNETSEQ DS    XL1                 SAVED NETWORK SEQUENCE NUMBER                
WORK1    DS    CL13                                                             
WORK2    DS    CL13                                                             
**PAN#1  CSECT                                                                  
         END                                                                    
