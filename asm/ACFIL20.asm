*          DATA SET ACFIL20    AT LEVEL 106 AS OF 08/16/00                      
*PHASE T62320A,*                                                                
         TITLE 'ACCOUNT KEY BUILDER'                                            
FIL20    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,ACFIL20*,R7,RR=RE                                              
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
EXITL    CLI   *,FF                SET CC LOW                                   
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
FIL20    CSECT                                                                  
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
         BZ    EXITOK                                                           
         GOTOX VHEXOUT,BOPARM,CUABIN,FVIFLD,L'CUABIN,0                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FIELD VALUE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
VALVAL   MVC   TLR1DATA,FVIFLD     SAVE FOR NEXT DISPLAY                        
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
*                                                                               
BLDKEY   MVC   BOWORK1,KEYBLK      GET COPY OF KEY                              
         MVI   THISTYPE,C'X'       SET DEFAULT TO HEXADECIMAL                   
         MVC   LASTTYPE,THISTYPE                                                
*                                                                               
         LA    R0,L'ACCKEY                                                      
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
* TABLE OF SUPPORTED RECORD KEYS                                      *         
***********************************************************************         
         SPACE 1                                                                
         DS    0A                                                               
RCOTABL  DCDDL AC#TRN,12,L         TRANSACTION RECORD                           
         DC    Y(FLDTRN-FLDTABL),C' ',X'00'                                     
*                                                                               
         DCDDL AC#ACC,12,L         ACCOUNT RECORD                               
         DC    Y(FLDACC-FLDTABL),C' ',X'00'                                     
*                                                                               
         DCDDL AC#CTR,12,L         CONTRA ACCOUNT RECORD                        
         DC    Y(FLDCAC-FLDTABL),C' ',X'00'                                     
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
FIL20    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* TABLE OF SUPPORTED RECORD FIELDS                                    *         
***********************************************************************         
         SPACE 1                                                                
FLDTABL  DS    0A                                                               
*                                                                               
FLDTRN   DS    0F              *** TRANSACTION RECORD FIELDS                    
         DCDDL AC#CPY,20,R               COMPANY                                
         DC    AL1(0,1,FLDHEX,FLDCPY)                                           
         DCDDL AC#RSULC,20,R             UNIT/LEDGER                            
         DC    AL1(1,2,FLDCHR,0)                                                
         DCDDL AC#ACC,20,R               ACCOUNT                                
         DC    AL1(3,12,FLDCHR,0)                                               
         DCDDL AC#OFFWC,20,R             OFFICE/WORKCODE                        
         DC    AL1(15,2,FLDCHR,0)                                               
         DCDDL AC#CPY,20,R               COMPANY                                
         DC    AL1(17,1,FLDHEX,FLDCPY)                                          
         DCDDL AC#CTRUL,20,R             CONTRA UNIT/LEDGER                     
         DC    AL1(18,2,FLDCHR,0)                                               
         DCDDL AC#CTR,20,R               CONTRA ACCOUNT                         
         DC    AL1(20,12,FLDCHR,0)                                              
         DCDDL AC#DATE,20,R              DATE                                   
         DC    AL1(32,3,FLDDTE,0)                                               
         DCDDL AC#REF,20,R               REFERENCE                              
         DC    AL1(35,6,FLDCHR,0)                                               
         DCDDL AC#SEQ,20,R               SEQUENCE NUMBER                        
         DC    AL1(41,1,FLDHEX,0)                                               
         DC    X'00'                                                            
*                                                                               
FLDCAC   DS    0F              *** CONTRA ACCOUNT FIELDS                        
         DCDDL AC#CPY,20,R               COMPANY                                
         DC    AL1(0,1,FLDHEX,FLDCPY)                                           
         DCDDL AC#RSULC,20,R             UNIT/LEDGER                            
         DC    AL1(1,2,FLDCHR,0)                                                
         DCDDL AC#ACC,20,R               ACCOUNT                                
         DC    AL1(3,12,FLDCHR,0)                                               
         DCDDL AC#OFFWC,20,R             OFFICE/WORKCODE                        
         DC    AL1(15,2,FLDCHR,0)                                               
         DCDDL AC#CPY,20,R               COMPANY                                
         DC    AL1(17,1,FLDHEX,FLDCPY)                                          
         DCDDL AC#CTRUL,20,R             CONTRA UNIT/LEDGER                     
         DC    AL1(18,2,FLDCHR,0)                                               
         DCDDL AC#CTR,20,R               CONTRA ACCOUNT                         
         DC    AL1(20,12,FLDCHR,0)                                              
         DCDDL AC#BUCKT,20,R             BUCKET TYPE                            
         DC    AL1(35,1,FLDCHR,0)                                               
         DCDDL AC#SUBIT,20,R             SUB TYPE                               
         DC    AL1(36,3,FLDCHR,0)                                               
         DC    X'00'                                                            
*                                                                               
FLDACC   DS    0F              *** ACCOUNT RECORD FIELDS                        
         DCDDL AC#CPY,20,R               COMPANY                                
         DC    AL1(0,1,FLDHEX,FLDCPY)                                           
         DCDDL AC#RSULC,20,R             UNIT/LEDGER                            
         DC    AL1(1,2,FLDCHR,0)                                                
         DCDDL AC#ACC,20,R               ACCOUNT                                
         DC    AL1(3,12,FLDCHR,0)                                               
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
FLDTABLQ EQU   *-FLDTABLD                                                       
*                                                                               
FIL20    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
HEXFFS   DC    80X'FF'                                                          
         SPACE 2                                                                
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
LINENOW  DS    H                                                                
THISTYPE DS    X                                                                
LASTTYPE DS    X                                                                
*                                                                               
         SPACE 2                                                                
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
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
TLR1DATA DS    CL50                                                             
TLLNQ1   EQU   *-TLSTD                                                          
*                                                                               
SAVED    DSECT                                                                  
KEYBLK   DS    CL150               KEY BUILD BLOCK TO PASS BACK                 
LISDISP  DS    H                                                                
KEYFILL  DS    X                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'106ACFIL20   08/16/00'                                      
         END                                                                    
*                                                                               
         EJECT                                                                  
