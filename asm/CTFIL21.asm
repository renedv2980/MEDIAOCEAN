*          DATA SET CTFIL21    AT LEVEL 050 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE TA1321C                                                                  
         PRINT NOGEN                                                            
         TITLE 'NEW FILE SCRIPT SOURCE RECORDS'                                 
FIL21    START                                                                  
         NMOD1 0,CTFIL21*,R6,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         LH    R7,=Y(TWSAVE-TWAD)                                               
         A     R7,ATWA                                                          
         USING MYSAVED,R7                                                       
         ST    RE,BORELO                                                        
         ST    RB,BOBASE1                                                       
         ST    R6,BOBASE2                                                       
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
GSFRP    USING FRPELD,GSFRPEL                                                   
         SPACE 1                                                                
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       EXIT NOT WANTED FOR FILTER                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T                                        
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
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     OI    GCINDS3,GCINOACT    SUPPRESS ACTIVITY                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING FDRRECD,R2                                                       
         LA    RF,TABLKEY                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
TABLKEY  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KEYVAL)                                 
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KEYFVAL)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY VALIDATE                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING CT7KEY,R2                                                        
KEYVAL   MVI   CT7KTYP,CT7KTYPQ                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY FILTER VALIDATE                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CT7KEY,R2                                                        
KEYFVAL  MVI   CT7KTYP,CT7KTYPQ                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
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
         USING FDRRECD,R2                                                       
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
         USING CT7REC,R2                                                        
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(01001),AL4(NAMDTA)    BOOK NAME                              
         DC    AL2(01002),AL4(LVLDTA)    LEVEL                                  
         DC    AL2(01004),AL4(LINDTA)    DATA LINE                              
         DC    AL2(01005),AL4(DSPDTA)    LENGTH OF DATA ON LINE                 
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL21    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD NAME                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT7REC,R2                                                        
NAMDTA   LA    RF,NAMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNAM)                                 
         DC    AL1(DRDIS),AL1(0,0,0),AL4(DISNAM)                                
         DC    AL1(DRVAL),AL1(0,0,0),AL4(VALNAM)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTNAM)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTNAM)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTNAM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD NAME FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISNAM   MVC   FVIFLD(L'CT7KCODE),CT7KCODE                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD NAME                                              *         
***********************************************************************         
         SPACE 1                                                                
VALNAM   MVC   CT7KCODE,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD NAME FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTNAM  MVC   FVIFLD(L'CT7KCODE),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD NAME FILTER FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VFLTNAM  MVC   CT7KCODE,FVIFLD                                                  
         MVC   FLTIFLD(L'CT7KCODE),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO RECORD NAME FILTERING                                            *         
***********************************************************************         
         SPACE 1                                                                
DOFTNAM  CLC   CT7KCODE,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LEVEL                                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT7REC,R2                                                        
LVLDTA   LA    RF,LVLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LVLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLVL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD LEVEL                                              *         
***********************************************************************         
         SPACE 1                                                                
DISLVL   GOTO1 VHELLO,BOPARM,(C'G',CTFBIG),('CTPANDLQ',(R2)),0                  
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CTPAND,RF                                                        
         MVC   FVIFLD(CTPANLNQ-2),2(RF)                                         
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ONE LINE OF RECORD DATA                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
LINDTA   LA    RF,LINTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LINTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLIN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LINE OF RECORD DATA                                       *         
***********************************************************************         
         SPACE 1                                                                
DISLIN   XR    RF,RF                                                            
         ICM   RF,1,TLLLEN                                                      
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(0),TLLINE          MOVE DATA OUT TO LINE                  
         EX    RF,*-6                                                           
         EX    RF,*+8                                                           
         B     *+10                                                             
         TR    FVIFLD(0),TRTABO          TRANSLATE DATA                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD DESCRIPTION FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VALLIN   CLI   FVILEN,0            IS THIS LINE EMPTY?                          
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL                                                
         B     EXITOK                                                           
*                                                                               
         MVC   TLLLEN,FVILEN       CURRENT LINE NUMBER                          
         MVC   TLLINE,FVIFLD                                                    
         TR    TLLINE,TRTABI       TRANSLATE DATA                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LENGTH OF DATA ON THE LINE                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DSPDTA   LA    RF,DSPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LENGTH OF THE LINE                                      *         
***********************************************************************         
         SPACE 1                                                                
DISDSP   XR    RF,RF                                                            
         ICM   RF,3,TLCUM                                                       
         CURED (RF),(5,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         B     EXITOK                                                           
         SPACE 2                                                                
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
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
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
XITOUT   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   NSACT,A#LST                                                      
         BNE   EXITOK                                                           
         NI    SNINDS1,FF-SNIUSECR                                              
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
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING CT7REC,R2                                                        
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
         EJECT                                                                  
*                                                                               
LISTABL  DS    0A                                                               
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
*                                                                               
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
*        DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LSCRFRST),AL1(0,0,1),AL4(FSCR1)                              
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDDIR),AL1(0,0,1),AL4(UPDDIR1)                             
         DC    AL1(LNEWITEM),AL1(0,0,1),AL4(NEWITEM1)                           
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CT7KEY),THIS.CT7REC                                      
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5         EQUATED I/O AREA TO USE                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST04                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     MVC   IOKEY(L'CT7KEY),THIS.CT7REC                                      
*                                                                               
N        USING CT7REC,IOKEY                                                     
NLST02   XR    RF,RF               INCREMENT NAME FIELD                         
         ICM   RF,1,N.CT7KCODE+L'CT7KCODE-1                                     
         LA    RF,1(RF)                                                         
         STCM  RF,1,N.CT7KCODE+L'CT7KCODE-1                                     
         XC    N.CT7SEQNO,N.CT7SEQNO                                            
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5         EQUATED I/O AREA TO USE                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH UNHAPPY                            
         DROP  N                                                                
*                                                                               
NLST04   L     RF,SVPARMS5         CONTROL FILE RECORDS ARE RETURNED IN         
         SRL   RF,4-2              THE I/O AREA                                 
         L     RF,AIO1-L'AIO1(RF)                                               
*                                                                               
         CLC   0(CT7KCODE-CT7REC,RF),THIS.CT7REC                                
         BNE   EXITL                                                            
         MVC   THIS.CT7REC(CT7KEYL),0(RF)      WE WANT THIS RECORD              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT2,(LSSADD+LSSIUPD)                                         
         OI    LSSTAT1,(LSSTSAR+LSSBALL)                                        
         MVI   LSSUBLEN,SBLEN                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR SCREEN 1                                                  *         
***********************************************************************         
         SPACE 1                                                                
FSCR1    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST ON PAGE 1                                            *         
***********************************************************************         
         SPACE 1                                                                
FLST1    L     R3,AIOREC           FIRST RECORD IN AIOREC                       
         B     NLST102                                                          
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST ON PAGE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
NLST1    CLI   READFLAG,C'Y'       HAVE A RECORD?                               
         BE    BLST02              YES                                          
*                                                                               
N        USING CT7REC,IOKEY                                                     
         MVC   IOKEY,THIS.CT7REC                                                
         ICM   RF,3,N.CT7SEQNO                                                  
         LA    RF,1(RF)                                                         
         STCM  RF,3,N.CT7SEQNO                                                  
         ICM   R1,15,=AL4(XOREAD+XOCONFIL)                                      
         A     R1,SVPARMS5         EQUATED I/O AREA TO USE                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               NEXT RECORD NOT THERE - FINISHED             
         DROP  N                                                                
*                                                                               
         L     R3,SVPARMS5         CONTROL FILE RECORDS ARE RETURNED IN         
         SRL   R3,4-2              THE I/O AREA                                 
         L     R3,AIO1-L'AIO1(R3)                                               
         MVC   THIS.CT7KEY,0(R3)   SAVE THE KEY                                 
*                                                                               
NLST102  MVI   READFLAG,C'Y'                                                    
         XC    BUFFDISP,BUFFDISP   RESET DISPLACEMENT INTO BUFFER               
         LA    R0,BUFFER                                                        
         LH    R1,=Y(MAXLEN)                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE               CLEAR BUFFER                                 
*                                                                               
N        USING CT7REC,R3                                                        
         OC    N.CT7SEQNO,N.CT7SEQNO   0 OR 1..N SCRIPT CODE RECORDS?           
         BZ    *+8                                                              
         MVI   MANY,C'Y'           MORE THAN ONE RECORD                         
         DROP  N                                                                
*                                                                               
         LA    R3,CT7DATA-CT7REC(R3)                                            
         USING CTSCRD,R3                                                        
         XR    RF,RF                                                            
         LA    RE,BUFFER                                                        
*                                                                               
LOOP02   CLI   CTSCREL,0                                                        
         BE    BLST02                                                           
         CLI   CTSCREL,CTSCRELQ                                                 
         BE    LOOP06                                                           
*                                                                               
LOOP04   IC    RF,CTSCRLEN                                                      
         LA    R3,0(RF,R3)                                                      
         B     LOOP02                                                           
*                                                                               
LOOP06   IC    RF,CTSCRLEN                                                      
         SH    RF,=Y(CTSCRDTA-CTSCRD+1)                                         
         BNM   *+6                                                              
         DC    H'0'                BAD ELEMENT                                  
         MVC   0(0,RE),CTSCRDTA    MOVE DATA INTO BUFFER                        
         EX    RF,*-6                                                           
         LA    RE,1(RE,RF)                                                      
         B     LOOP04                                                           
*                                                                               
BLST02   LH    RF,BUFFDISP         REACHED END OF SINGLE BUFFER?                
         LA    RF,BUFFER(RF)                                                    
         CLI   0(RF),0                                                          
         BE    EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM DIRECTORY 1                                        *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR1 B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,SVPARMS4         A(TSAR BUFFER)                               
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
*                                                                               
         LA    RE,WIDTH                                                         
         LH    RF,BUFFDISP         WILL ALL THIS LINE FIT ON SCREEN?            
         CLM   RF,3,=AL2(MAXLEN-WIDTH)                                          
         BL    TDI102                                                           
*                                                                               
         MVI   READFLAG,C'N'       NEED NEXT RECORD OR FINISHED                 
         LH    RE,=Y(MAXLEN)                                                    
         SR    RE,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                WELL WHAT WENT WRONG?                        
*                                                                               
TDI102   LA    RF,BUFFER(RF)       WHERE TO MOVE FROM                           
         BCTR  RE,0                                                             
         MVC   TLLINE(0),0(RF)                                                  
         EX    RE,*-6                                                           
*                                                                               
         LA    RF,1(RE,RF)                                                      
         LA    RE,BUFFER+MAXLEN                                                 
         CR    RF,RE                                                            
         BL    *+8                 STILL WITHIN THE BUFFER                      
         MVI   READFLAG,C'N'                                                    
*                                                                               
         LA    R0,L'TLLINE         GET LENGTH IF NOT 'WIDTH'                    
         LA    RF,TLLINE+L'TLLINE-1                                             
         CLI   0(RF),0                                                          
         BNE   *+10                                                             
         BCTR  RF,0                                                             
         BCT   R0,*-10                                                          
         STC   R0,TLLLEN                                                        
*                                                                               
         LH    RF,CUMDISP                                                       
         STCM  RF,3,TLCUM                                                       
         AR    RF,R0                                                            
         STH   RF,CUMDISP                                                       
*                                                                               
         LH    RF,BUFFDISP                                                      
         LA    RF,WIDTH(RF)                                                     
         STH   RF,BUFFDISP                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING CTSCRD,BOELEM                                                    
UPDFRST1 GOTOX ADELEL,BOPARM,('CTSCRELQ',AIOREC),0                              
*                                                                               
         XC    BOELEM,BOELEM       FIRST TIME IN - CLEAR ELEMENT                
         MVI   CTSCREL,CTSCRELQ    ELEMENT CODE                                 
         XC    SEQUENCE,SEQUENCE   CLEAR SEQUENCE NUMBER                        
         MVC   CTSCRSEQ,SEQUENCE                                                
         LA    RF,CTSCRDTA-CTSCRD  SET FIXED LENGTH                             
         STC   RF,CTSCRLEN         STORE LENGTH IN ELEMENT                      
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING CTSCRD,BOELEM                                                    
UPDLAST1 XC    LSSTAT1,LSSTAT1                                                  
         XR    R1,R1                                                            
         ICM   R1,1,SEQUENCE       SEQUENCE NUMBER IS 0 OR 1...N                
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,CTSCRSEQ                                                      
*                                                                               
         L     R3,AIOREC                                                        
         USING CT7REC,R3                                                        
         XR    RF,RF               HAVE TO DO OWN SIZE CHECKS                   
         ICM   RF,1,CTSCRLEN       AS HELLO ADDS THEN CHECKS...                 
         XR    RE,RE                                                            
         ICM   RE,3,CT7LEN                                                      
         LA    RE,0(RF,RE)         NEW LENGTH IF WITH THIS ELEMENT              
         CLM   RE,3,=Y(MAXLEN)                                                  
         BL    *+8                 ELEMENT FITS                                 
         BAS   RE,RECPUT           WRITE THIS SEQUENCE AND GET NEXT             
*                                                                               
         GOTOX VHELLO,BOPARM,(C'P',CTFBIG),AIOREC,BOELEM,ADDEND                 
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         POP   USING                                                            
         EJECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* SET UP A NEW LINE IN THE LIST 1                                     *         
***********************************************************************         
         SPACE 1                                                                
NEWITEM1 L     R3,SVPARMS3         A(TSAR BUFFER)                               
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET NEW RECORD LENGTH                        
         MVI   TLLLEN,0                                                         
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* UPDATE DIRECTORY FROM TSAR RECORD 1                                 *         
* P3 = A (DIRECTORY RECORD)                                           *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING CTSCRD,BOELEM                                                    
UPDDIR1  L     R2,SVPARMS4                                                      
         USING TLSTD,R2                                                         
         XR    RF,RF                                                            
         ICM   RF,1,TLLLEN                                                      
         BZ    EXITOK                                                           
         OC    TLLINE,TLLINE                                                    
         BZ    EXITOK                                                           
*                                                                               
         XR    R0,R0                                                            
         IC    R0,CTSCRLEN         LENGTH OF ELEMENT AT PRESENT                 
         XC    ELNOWLQ,ELNOWLQ     CLEAR TEMP LENGTH HOLDER                     
         LA    R4,BOELEM                                                        
         AR    R4,R0               FIRST FREE DATA SLOT                         
         LA    R1,FF               AS MUCH AS CAN BE FITTED IN                  
         SR    R1,R0                                                            
         STC   R1,ELNOWLQ          HOW MUCH MORE CAN BE ADDED                   
         CR    R1,RF               CAN ALL THIS STUFF MOVE IN OK?               
         BL    UPDD02              NO                                           
*                                                                               
         BCTR  RF,0                OK TO MOVE IN ALL THIS LINE                  
         MVC   0(0,R4),TLLINE      MOVE IN LINE                                 
         EX    RF,*-6                                                           
         LR    RE,R0               R0 HOLDS CURRENT LENGTH REMEMBER             
         LA    RF,1(RE,RF)         ADD LENGTH TO TOTAL                          
         STC   RF,CTSCRLEN         STORE LENGTH IN ELEMENT                      
         CLI   CTSCRLEN,FF         EXACTLY FILLED ELEMENT?                      
         BNE   EXITOK              NO                                           
         XC    ELNOWLQ,ELNOWLQ     RESET AMOUNT TO MOVE INTO NEXT EL.           
         B     UPDD04                                                           
*                                                                               
UPDD02   SR    RF,R1               RF HOLDS WHAT MUST BE MOVED                  
         STC   RF,ELNOWLQ          STORE L'LEFT AFTER THIS MOVE                 
         LR    R5,R1               SAVE AMOUNT MOVED THIS TIME                  
         BCTR  R1,0                                                             
         MVC   0(0,R4),TLLINE      FILL THIS ELEMENT                            
         EX    R1,*-6                                                           
*                                                                               
UPDD04   MVI   CTSCRLEN,FF         SET ELEMENT LENGTH TO MAXIMUM                
         XR    R1,R1                                                            
         IC    R1,SEQUENCE         BUMP SEQUENCE NO.                            
         LA    R1,1(R1)                                                         
         STC   R1,SEQUENCE                                                      
         MVC   CTSCRSEQ,SEQUENCE   SET SEQUENCE NUMBER                          
*                                                                               
         L     R3,AIOREC                                                        
         USING CT7REC,R3                                                        
         XR    RF,RF               HAVE TO DO OWN SIZE CHECKS                   
         ICM   RF,1,CTSCRLEN       AS HELLO ADDS THEN CHECKS...                 
         XR    RE,RE                                                            
         ICM   RE,3,CT7LEN                                                      
         LA    RE,0(RF,RE)         NEW LENGTH IF WITH THIS ELEMENT              
         CLM   RE,3,=Y(MAXLEN)                                                  
         BL    *+8                 ELEMENT FITS                                 
         BAS   RE,RECPUT           WRITE THIS RECORD & GET NEXT                 
*                                                                               
         GOTOX VHELLO,BOPARM,(C'P',CTFBIG),AIOREC,BOELEM,ADDEND                 
         CLI   12(R1),0                                                         
         BE    UPDD06                                                           
         DC    H'0'                                                             
*                                                                               
UPDD06   XC    BOELEM,BOELEM       MOVE IN REMAINDER AND SET LENGTHS            
         MVI   CTSCREL,CTSCRELQ                                                 
         MVI   CTSCRLEN,3                                                       
         XR    RF,RF                                                            
         ICM   RF,1,ELNOWLQ        LENGTH LEFT TO MOVE IN                       
         BZ    EXITOK                                                           
*                                                                               
         BCTR  RF,0                                                             
         LA    RE,TLLINE(R5)       START OF NEXT PIECE OF DATA                  
         MVC   CTSCRDTA(0),0(RE)                                                
         EX    RF,*-6                                                           
         LA    RF,4(RF)            2 FOR FIXED, 1 FOR MVC                       
         STC   RF,CTSCRLEN         SAVE LENGTH                                  
         B     EXITOK                                                           
*                                                                               
UPDD08   XR    R1,R1                                                            
         ICM   R1,1,SEQUENCE       SEQUENCE NUMBER IS 0 OR 1...N                
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,CTSCRSEQ                                                      
*                                                                               
         L     R3,AIOREC                                                        
         USING CT7REC,R3                                                        
         XR    RF,RF               HAVE TO DO OWN SIZE CHECKS                   
         ICM   RF,1,CTSCRLEN       AS HELLO ADDS THEN CHECKS...                 
         XR    RE,RE                                                            
         ICM   RE,3,CT7LEN                                                      
         LA    RE,0(RF,RE)         NEW LENGTH IF WITH THIS ELEMENT              
         CLM   RE,3,=Y(MAXLEN)                                                  
         BL    *+8                 ELEMENT FITS                                 
         BAS   RE,RECPUT           WRITE THIS SEQUENCE AND GET NEXT             
*                                                                               
         GOTOX VHELLO,BOPARM,(C'P',CTFBIG),AIOREC,BOELEM,ADDEND                 
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         POP   USING                                                            
         EJECT                                                                  
RECPUT   DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         DROP  R7                  GET RID OF TWSAVE                            
         LTORG                                                                  
WIDTH    EQU   60                  DEFAULT AMOUNT DISPLAYED ONTO A LINE         
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SBLEN    EQU   0                   LENGTH OF A SUB-ACT FIELD                    
CTFBIG   DC    C'CTFBIG  '                                                      
ADDEND   DC    C'ADD=END '                                                      
MAXLEN   EQU   2000                MAX LENGTH OF CONTROL FILE RECORD            
*                                                                               
TRTABO   DC    XL16'40404040404040404040404040404040'  00-0F                    
         DC    XL16'40404040404040404040404040404040'  10-1F                    
         DC    XL16'40404040404040404040404040404040'  20-2F                    
         DC    XL16'40404040404040404040404040404040'  30-3F                    
         DC    XL16'6A4040404040404040404A4B4C4D4E4F'  40-4F 40-->6A            
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'606140404040404040406A6B6C6D6E6F'  60-6F                    
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'40818283848586878889404040404040'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040'  A0-AF                    
         DC    XL16'40404040404040404040404040404040'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-DF                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  F0-FF                    
         SPACE 2                                                                
TRTABI   DC    XL16'00000000000000000000000000000000'  00-0F                    
         DC    XL16'00000000000000000000000000000000'  10-1F                    
         DC    XL16'00000000000000000000000000000000'  20-2F                    
         DC    XL16'00000000000000000000000000000000'  30-3F                    
         DC    XL16'004040404040404040404A4B4C4D4E4F'  40-4F 40-->00            
         DC    XL16'504040404040404040405A5B5C5D5E5F'  50-5F                    
         DC    XL16'60614040404040404040406B6C6D6E6F'  60-6F 6A-->40            
         DC    XL16'404040404040404040797A7B7C7D7E7F'  70-7F                    
         DC    XL16'40818283848586878889404040404040'  80-8F                    
         DC    XL16'40919293949596979899404040404040'  90-9F                    
         DC    XL16'40A1A2A3A4A5A6A7A8A9404040404040'  A0-AF                    
         DC    XL16'40404040404040404040404040404040'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C9404040404040'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D9404040404040'  D0-DF                    
         DC    XL16'E040E2E3E4E5E6E7E8E9404040404040'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F9404040404040'  F0-FF                    
         SPACE 2                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
MYSAVED  DSECT                                                                  
SEQNUM   DS    H                   START SEQUENCE NUMBER                        
SEQLAST  DS    H                   PREVIOUS SEQUENCE NUMBER                     
SEQUENCE DS    XL1                                                              
ELNOWLQ  DS    XL1                                                              
*                                                                               
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
CUMDISP  DS    H                   CUMULATIVE DISPLACEMENT                      
MANY     DS    XL1                 ONE OR MANY RECORDS IN SEQUENCE              
READFLAG DS    XL1                 NEXT RECORD IN SEQUENCE REQUIRED             
*                                                                               
BUFFDISP DS    H                                                                
BUFFER   DS    XL(MAXLEN)          FORMATTED SCRIPT INFORMATION                 
*                                                                               
*     ** CTFILWORK ***                                                          
*        PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLUSER                                                           
TLLLEN   DS    XL1                 LINE LENGTH                                  
TLLINE   DS    XL80                DATA LINE                                    
TLCUM    DS    XL2                 CUMULATIVE LENGTH                            
TLLNQ    EQU   *-TLSTD                                                          
*                                                                               
*** CTGENFILE ***                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
*** CTMSGEQUS ***                                                               
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*** FAUTL ***                                                                   
         PRINT OFF                                                              
       ++INCLUDE FAUTL                                                          
         PRINT ON                                                               
*** DDFLDHDR ***                                                                
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'050CTFIL21   08/10/11'                                      
         END                                                                    
