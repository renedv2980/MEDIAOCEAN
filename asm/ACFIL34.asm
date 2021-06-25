*          DATA SET ACFIL34    AT LEVEL 004 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T62334C,*                                                                
         TITLE 'GLRULES OBJECT VERSON'                                          
         SPACE 2                                                                
FIL34    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL34**,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    RB,SAVRB                                                         
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING TWUSER,R6                                                        
                                                                                
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
         L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
EXIT     XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
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
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CPYRECD,R2                                                       
INIT     NI    BIT,X'FF'-NEWOFF                                                 
         TM    BCCPYST4,CPYSOFF2                                                
         BZ    *+8                                                              
         OI    BIT,NEWOFF          NEW OFFICE SYSTEM                            
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
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
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING ACTRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   MVC   ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING ACTRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS+8        GET GLOBAL VERB                              
         LA    RF,DTATABL                                                       
         B     ITER                                                             
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
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING ACTRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(GLR#LDG),AL4(DOLEDG)   LEDGER                                
         DC    AL2(GLR#SOFF),AL4(SUBOFF) SUB OFFICE                             
         DC    AL2(GLR#GOFF),AL4(GLOFF) G/L OFFICE                              
         SPACE 2                                                                
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL34    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEDGER                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DOLEDG   LA    RF,LEDGTBL                                                       
         B     ITER                                                             
*                                                                               
LEDGTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEDG)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLEDG)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDLDG)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVLDG)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTLDG)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LEDGER                                                            
***********************************************************************         
         SPACE 1                                                                
DISLEDG  MVC   FVIFLD(L'ACTKLDG),ACTKLDG                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LEDGER                                                           
***********************************************************************         
         SPACE 1                                                                
VALLEDG  CLI   FVILEN,0            NO INPUT MEANS GLRULES STORED ON THE         
         BE    EXITOK              COMPANY RECORD                               
         MVI   ACTKUNT,C'S'        IF LEDGER ENTERED WILL ALWAYS BE FOR         
         ZIC   R1,FVXLEN           UNIT S                                       
         EXMVC R1,BCWORK,FVIFLD                                                 
         BAS   RE,GETLEDG                                                       
         BNE   EXITL                                                            
         MVC   ACTKLDG,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LEDGER AS A FILTER                                                
***********************************************************************         
         SPACE 1                                                                
DFDLDG   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FVIFLD,FLTIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LEDGER AS A FILTER                                               
***********************************************************************         
         SPACE 1                                                                
DFVLDG   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         MVI   ACTKUNT,C'S'        IF LEDGER ENTERED WILL ALWAYS BE FOR         
         ZIC   R1,FVXLEN           UNIT S.                                      
         EXMVC R1,FLTIFLD,FVIFLD                                                
         EXMVC R1,ACTKLDG,FVIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LEDGER                                                       
***********************************************************************         
         SPACE 1                                                                
DOFLTLDG CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTER ON                         
         CLC   ACTKLDG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* GET LEDGER RECORD                                                             
***********************************************************************         
T        USING ACTRECD,R5                                                       
GETLEDG  NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVI   T.ACTKUNT,C'S'        ALWAYS UNIT S                              
         MVC   T.ACTKLDG,BCWORK      MOVE IN LEDGER                             
         GOTO1 AGETACT,0           GET ACCOUNT/TEST SECURITY                    
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUBSIDIARY OFFICE                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SUBOFF   LA    RF,SOFFTBL                                                       
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
SOFFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSOFF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE SUBSIDIARY OFFICE                                                 
***********************************************************************         
         SPACE 1                                                                
DISSOFF  CLC   TLKSOFF,EFFS        IS THIS A DUMMY ENTRY?                       
         BE    EXITOK              THEN DON'T SHOW                              
         MVC   FVIFLD(L'TLKSOFF),TLKSOFF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE SUBSIDIARY OFFICE                                                
***********************************************************************         
         SPACE 1                                                                
VALSOFF  NI    BIT,X'FF'-NEWENTRY                                               
         OC    TLKSOFF,TLKSOFF     IS THERE ALREADY AN OFFICE?                  
         BNZ   *+8                 NO                                           
         OI    BIT,NEWENTRY        YES MUST BE CHANGING AN EXISTING 1           
         MVC   TLKSOFF,BCSPACES                                                 
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   VSOF10                                                           
         OI    LSLNIND1,LSLNIDEL                                                
         B     EXITOK                                                           
*                                                                               
VSOF10   TM    BIT,NEWENTRY        NEW OFFICE BEING ENTERED?                    
         BZ    VSOF15              NO DON'T CHK FOR MAX #                       
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1          FIRST LIST REC #                             
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP FOR REAL NUMBER                      
         CHI   RE,51               51 OFFICES MAX                               
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXRC)                                           
         B     EXITL                                                            
*                                                                               
VSOF15   TM    BIT,NEWOFF          ON 2 BYTE OFFICES                            
         BZ    VSOF20              NO                                           
         CLI   FVILEN,2                                                         
         BNE   EXITNV                                                           
         BAS   RE,VAL2OFF          VALIDATE 2 BYTE OFFICE                       
         BNE   EXITL                                                            
         B     VSOF30                                                           
*                                                                               
VSOF20   CLI   FVILEN,1            MUST BE 1 BYTE OFFICE THAN                   
         BNE   EXITNV                                                           
         BAS   RE,VAL1OFF          VALIDATE 1 BYTE OFFICE                       
         BNE   EXITL                                                            
*                                                                               
VSOF30   MVC   TLKSOFF,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR GENERAL LEDGER OFFICE                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
GLOFF    LA    RF,GOFFTBL                                                       
         B     ITER                                                             
*                                                                               
GOFFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISGOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGOFF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE GENERAL LEDGER OFFICE                                             
***********************************************************************         
         SPACE 1                                                                
DISGOFF  CLC   TLKGOFF,EFFS        IS THIS A DUMMY ENTRY?                       
         BE    EXITOK              THEN DON'T SHOW                              
         MVC   FVIFLD(L'TLKGOFF),TLKGOFF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE GENERAL LEDGDR OFFICE                                            
***********************************************************************         
         SPACE 1                                                                
VALGOFF  NI    BIT,X'FF'-NEWENTRY                                               
         OC    TLKGOFF,TLKGOFF     IS THERE ALREADY AN OFFICE?                  
         BNZ   *+8                 NO                                           
         OI    BIT,NEWENTRY        YES MUST BE CHANGING AN EXISTING 1           
         MVC   TLKGOFF,BCSPACES                                                 
         CLI   FVILEN,0            ANY INPUT?                                   
         BNE   VGOF10                                                           
         CLC   TLKSOFF,EFFS        IF ENTERED SUB OFFICE THAN                   
         BE    EXITOK              MUST ALSO ENTER G/L OFFICE                   
         OC    TLKSOFF,TLKSOFF                                                  
         BZ    EXITOK                                                           
         B     EXITNO                                                           
*                                                                               
VGOF10   TM    BIT,NEWENTRY        ENTERING A NEW OFFICE?                       
         BZ    VGOF15              NO DON'T CHECK FOR MAX #                     
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1          FIRST LIST REC #                             
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP FOR REAL NUMBER                      
         CHI   RE,51               51 OFFICES MAX                               
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXRC)                                           
         B     EXITL                                                            
*                                                                               
VGOF15   TM    BIT,NEWOFF          ON 2 BYTE OFFICES                            
         BZ    VGOF20              NO                                           
         CLI   FVILEN,2                                                         
         BNE   EXITNV                                                           
         BAS   RE,VAL2OFF          VALIDATE 2 BYTE OFFICE                       
         BNE   EXITL                                                            
         B     VGOF30                                                           
*                                                                               
VGOF20   CLI   FVILEN,1            MUST BE 1 BYTE OFFICE THAN                   
         BNE   EXITNV                                                           
         BAS   RE,VAL1OFF          VALIDATE 1 BYTE OFFICE                       
         BNE   EXITL                                                            
*                                                                               
VGOF30   MVC   TLKGOFF,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 2 BYTE OFFICE (CANNOT BE AN OFFICE LIST)                             
***********************************************************************         
         SPACE 1                                                                
         USING OFFRECD,R5                                                       
VAL2OFF  NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   OFFKEY,BCSPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,FVIFLD                                                   
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$IVOFF)  INVALID OFFICE                           
         B     EXITL                                                            
*                                                                               
         TM    OFFKSTAT,OFFSLIST   IS THIS AN OFFICE LIST?                      
         BZ    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$OLNO) OFFICE LIST NOT ALLOWED                    
         B     EXITL                                                            
*                                                                               
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 1 BYTE OFFICE                                                        
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,R5                                                       
VAL1OFF  NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'2D'                            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,T.ACTKACT,FVIFLD                                              
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$IVOFF)  INVALID OFFICE                           
         B     EXITL                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING ACTRECD,R2                                                       
LAST     USING ACTRECD,R3                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(RF)                                                   
         B     LITER                                                            
*                                                                               
LITER04  ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 2                                                                
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CHAKEY),THIS.ACTRECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               MESS UP ON THE READ HIGH                     
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
T        USING ACTRECD,IOKEY                                                    
NLST     LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         ZIC   RF,T.ACTKACT        BUMP TO NEXT LEDGER                          
         AHI   RF,1                                                             
         STC   RF,T.ACTKACT                                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(L'ACTKCPY),THIS.ACTRECD  SAME CO                           
         BNE   EXITL                                                            
         CLI   T.ACTKLDG,C' '      IS THERE A LEDGER?                           
         BE    *+12                NO MUST BE COMPANY                           
         CLI   T.ACTKUNT,C'S'      IF A LEDGER MUST BE UNIT 'S'                 
         BNE   NLST                                                             
         CLC   T.ACTKACT,BCSPACES    DON'T WANT ACCOUNTS                        
         BNE   NLST                                                             
         LHI   R1,XOGET+XOACCMST+XIO1   MUST HAVE A G/L RULES ELEM              
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('GLRELQ',AIO1),0                  
         CLI   12(R1),0                                                         
         BNE   NLST                NO ELEM SO SKIP                              
*                                                                               
         CLI   T.ACTKLDG,C' '      FOR COMPANY REC DON'T CHK SECURITY           
         BNH   NLST04                                                           
         GOTO1 AGETACT,0           TEST SECURITY                                
         MVC   FVXTRA,BCSPACES     CLEAR OUT ANY MSG'S FROM GETACT              
         BNE   NLST                                                             
NLST04   MVC   THIS.ACTKEY(L'ACTKEY+L'ACTKSTA+L'ACTKDA),IOKEY                   
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   DS    0H                                                               
         OI    LSSTAT1,LSSBALL+LSSMULIN                                         
         OI    LSSTAT2,LSSNOSEQ+LSSADD                                          
         MVC   LSCOLLIN,=AL2(25)                                                
         MVC   LSLINROW,=AL2(3)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,ACTRFST-ACTRECD                                               
         STH   RF,RECDSP           SAVE DISPL TO FIRST ELEMENT                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,RECDSP           CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE ELMDSP INITIALISED                 
         BH    *+8                                                              
         LA    RF,ACTRFST(RF)      IT IS NOW.                                   
         XR    RE,RE                                                            
*                                                                               
         USING GLRELD,RF                                                        
FML02    CLI   GLREL,0             RECORD END?                                  
         BNE   *+14                                                             
         MVC   RECDSP,EFFS                                                      
         B     EXITL                                                            
         CLI   GLREL,GLRELQ        E6 ELEMENT                                   
         BE    FML04               NO                                           
         ZIC   R1,GLRLN                                                         
         AR    RF,R1                                                            
         B     FML02                                                            
                                                                                
FML04    LR    RE,RF                                                            
         S     RE,AIOREC                                                        
         STH   RE,RECDSP                                                        
         MVC   NUMOFF,GLRPRS       SAVE # OF OFFICE PAIRS                       
*                                                                               
         LR    RE,RF                                                            
         LR    R1,RF                                                            
         AH    RE,=Y(L'GLREL+L'GLRLN+L'GLRPRS)                                  
         SR    RE,R1                                                            
         STH   RE,ELMDSP          STORE DISPL INTO ELEMENT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
         SPACE 1                                                                
NLST1    CLC   RECDSP,EFFS         IF FF'S THEN DONE                            
         BE    EXITL                                                            
         LH    RF,RECDSP           CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         C     RF,AIOREC           MAKE SURE ELMDSP INITIALISED                 
         BH    NML04                                                            
         LA    RF,ACTRFST(RF)      IT IS NOW.                                   
*                                                                               
         USING GLRELD,RF                                                        
NML02    CLI   GLREL,0             RECORD END?                                  
         BE    EXITL                                                            
         CLI   GLREL,GLRELQ        E6 ELEMENT                                   
         BE    NML04               YES                                          
         IC    RE,GLRLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML04    CLI   NUMOFF,0            NO MORE OFFICES?                             
         BE    EXITL                                                            
         LR    RE,RF                                                            
         LR    R1,RF                                                            
         AH    RE,ELMDSP          ADD DISPL INTO ELEMENT                        
         LA    RE,L'GLROFFP(RE)    BUMP UP TO NEXT OFFICE PAIR                  
         SR    RE,R1                                                            
         STH   RE,ELMDSP                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         LH    RF,RECDSP                                                        
         A     RF,AIOREC                                                        
         AH    RF,ELMDSP                                                        
         USING GLRELD,RF                                                        
*                                                                               
         CLC   RECDSP,EFFS         IF ELMDSP IS EFFS                            
         BNE   TSARF05                                                          
         MVC   TLKSOFF,EFFS        CREATE A DUMMY TSAR RECORD TO                
         MVC   TLKGOFF,EFFS                                                     
         OI    LSLTIND1,LSLTIFVL   FORCE VAL OF LIST LINES                      
         B     EXITOK                                                           
*                                                                               
TSARF05  MVC   TLKSOFF,0(RF)       SUBSIDIARY OFFICE                            
         MVC   TLKGOFF,L'GLRSUBO(RF)  G/L OFFICE                                
         ZIC   R1,NUMOFF           DECREMENT # OF OFFICE PAIRS                  
         BCTR  R1,0                                                             
         STC   R1,NUMOFF                                                        
*                                                                               
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('GLRELQ',ACTRECD),0               
         MVC   BLDOFFC,BCSPACES    CLEAR TABLE OF OFFICES                       
         MVI   SVLEN,0             CLEAR DISPL INTO TABLE                       
         MVI   ELMLEN,3            MINIMUM LENGTH OF ELEMENT                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  LM    R2,R3,SVPARMS3                                                   
*                                                                               
         LA    R4,BLDOFFC          R4 POINTS TO ELEM WORK AREA                  
         ZIC   R1,SVLEN                                                         
         AR    R4,R1               R3 POINTS TO NEXT OPEN SPOT                  
         CLC   TLKSOFF,EFFS        IF THIS IS THE DUMMY ENTRY                   
         BNE   *+8                 THEN DON'T ADD TO ELEM                       
         B     EXITOK                                                           
         MVC   0(L'GLRSUBO,R4),TLKSOFF   MOVE IN SUB OFF TO BLDOFFC             
         MVC   L'GLRSUBO(L'GLRGNLO,R4),TLKGOFF  MOVE IN G/L OFF TOO             
         ZIC   R1,SVLEN                                                         
         LA    R1,L'GLROFFP(R1)   UPDATE LENGTH                                 
         STC   R1,SVLEN                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDLAST1 LM    R2,R3,SVPARMS3                                                   
*                                                                               
         CLC   BLDOFFC,BCSPACES    ANY OFFICES?                                 
         BE    EXITOK              NO                                           
*                                                                               
         USING GLRELD,R4                                                        
         LA    R4,BOELEM                                                        
         MVC   BOELEM,BCSPACES                                                  
         MVI   GLREL,GLRELQ                                                     
         ZIC   R1,SVLEN            R1=LENGTH OF OFFICE PAIRS                    
         LR    RE,R1                                                            
         BCTR  R1,0                                                             
         EXMVC R1,GLRSUBO,BLDOFFC  MOVE IN OFFICES                              
         LA    R1,1(R1)            RESTORE LENGTH                               
         SRA   R1,2                DIVIDE BY 4 TO FIND # OF OFFICE PRS          
         STC   R1,GLRPRS           NUMBER OF OFFICE PAIRS                       
         ZIC   RF,ELMLEN                                                        
         AR    RF,RE               ADD LEN OF OFFICES FOR TRUE LEN OF           
         STC   RF,GLRLN            ELEMENT                                      
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,BOELEM,0                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
EFFS     DC    50X'FF'                                                          
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#OFFC,6,L                                                      
         DCDDL AC#OFLST,8,L                                                     
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*ACFILWORK                                                                      
*DDSCANBLKD                                                                     
*CTGENFILE                                                                      
*CTGENEDICT                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
SVBACCH  DS    A                                                                
ELMDSP   DS    H                                                                
RECDSP   DS    H                                                                
*                                                                               
BLDOFFC  DS    CL240               BUILD OFFICE WORK AREA                       
SVLEN    DS    XL1                 SAVED LENGTH                                 
ELMLEN   DS    XL1                 ELEMENT LENGTH                               
NUMOFF   DS    XL1                 NUMBER OF OFFICES                            
BIT      DS    XL1                                                              
NEWOFF   EQU   X'80'               2 BYTE OFFICES IN USE                        
NEWENTRY EQU   X'40'               ENTERING A NEW OFFICE                        
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
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
SVADDR   DS    A                   SAVED ADDRESS                                
SAVRB    DS    F                                                                
         SPACE 1                                                                
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKSOFF  DS    CL2                 SUBSIDIARY OFFICE                            
         ORG   TLUSER                                                           
TLKGOFF  DS    CL2                 GENERAL LEDGER OFFICE                        
TLLNQ    EQU   *-TLSTD                                                          
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACFIL34   08/10/11'                                      
         END                                                                    
