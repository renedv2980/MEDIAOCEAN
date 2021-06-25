*          DATA SET CTFIL14    AT LEVEL 004 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1314A                                                                  
         TITLE 'OBJECT VERSION OF PFKEY RECORDS'                                
FIL14    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL14*,R6,R7,RR=RE                                           
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
         LH    R5,=Y(TWDICT-TWAD)                                               
         LA    R5,TWAD(R5)                                                      
         USING TWDICT,R5                                                        
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
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     L     RF,=A(VDIC)                                                      
         A     RF,BORELO                                                        
         ST    RF,AVDIC                                                         
         L     RF,=A(DDIC)                                                      
         A     RF,BORELO                                                        
         ST    RF,ADDIC                                                         
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R2,SVPARMS                                                    
         USING FRPRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(KHEIR),AL1(0,0,0),AL4(KEYHEIR)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKING PFKEY                            
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KFKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KFKFDIS)    DISPLAY FILTER             
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KFKDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    FRPKEY,FRPKEY       INITIALIZE KEY OF PFKEY RECORD               
         MVI   FRPKMIN,FRPKMINQ    SET MINOR SYSTEM                             
         MVI   FRPKTYP,FRPKTYPQ    SET PFKEY RECORD                             
         MVI   FRPKSUB,FF          PFKEY SUB-COUNTRY                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KFKFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    FRPKEY,FRPKEY       INITIALIZE KEY OF PFKEY RECORD               
         MVI   FRPKMIN,FRPKMINQ    SET MINOR SYSTEM                             
         MVI   FRPKTYP,FRPKTYPQ    SET PFKEY RECORD                             
         MVI   FRPKSUB,FF          PFKEY SUB-COUNTRY                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         R3=INVOKING PFKEY                            
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** LAST TIME FOR KEY OBJECT ***                  
*                                 ------------------------                      
KLTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KLKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(KFDIS),AL1(0,0,0),AL4(KLKFDIS)    DISPLAY FILTER             
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY OBJECT                               *         
***********************************************************************         
         SPACE 1                                                                
KLKDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KLKVAL   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A KEY FILTER                               *         
***********************************************************************         
         SPACE 1                                                                
KLKFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* BUILD KEY FOR HEIRARCHICAL RECORD GET                               *         
* P3 = A(BUILD AREA FOR KEY TO INHERIT FROM)                          *         
* P4 = A(KEY WHICH WILL INHERIT FOR REFERENCE)                        *         
***********************************************************************         
         SPACE 1                                                                
KEYHEIR  L     R3,SVPARMS4                                                      
X        USING FRPRECD,R3                                                       
Y        USING FRPRECD,IOKEY                                                    
         XC    Y.FRPKEY,Y.FRPKEY                                                
         MVI   Y.FRPKMIN,FRPKMINQ  SEE IF INHERIT FROM PROGRAM LEVEL            
         MVI   Y.FRPKTYP,FRPKTYPQ                                               
         MVC   Y.FRPKSYS,X.FRPKSYS                                              
         MVC   Y.FRPKPRG,X.FRPKPRG                                              
         MVC   Y.FRPKACT,X.FRPKACT                                              
         MVC   Y.FRPKCTRY,X.FRPKCTRY                                            
         MVC   Y.FRPKSUB,X.FRPKSUB                                              
         L     R1,=AL4(XOHI+XOGENDIR)                                           
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(FRPKCTRY-FRPRECD),IOKEYSAV                                 
         BNE   EXITH               NO KEY TO INHERIT                            
*                                                                               
         MVC   FRPKEY,IOKEY                                                     
         B     EXITOK              KEY TO INHERIT FROM                          
         DROP  X,Y                                                              
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
         BNZ   DATA02              PFKEY IS ON A DATA OBJECT                    
*                                                                               
         L     R2,SVPARMS4                                                      
         USING FRPRECD,R2                                                       
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
         USING FRPRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRPEL                                                        
         USING FRPELD,R3           R3=A(FRPEL ON RECORD)                        
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00001),AL4(SYSDTA)    SYSTEM                                 
         DC    AL2(00002),AL4(PRGDTA)    PROGRAM                                
         DC    AL2(00003),AL4(RCODTA)    RECORD                                 
         DC    AL2(00100),AL4(ACTDTA)    ACTION                                 
         DC    AL2(00085),AL4(PFNDTA)    PFKEY NUMBER                           
         DC    AL2(00150),AL4(CTRDTA)    COUNTRY FILTER                         
         DC    AL2(00101),AL4(TDRDTA)    PFKEY NAME                             
         DC    AL2(00102),AL4(TDDDTA)    PFKEY NAME DISPLAY ROUTINE             
         DC    AL2(00103),AL4(TDLDTA)    PFKEY NAME DISPLAY ROUTINE             
         DC    AL2(00022),AL4(FDODTA)    PFKEY IS DDS ONLY                      
         DC    AL2(00086),AL4(VRBDTA)    PFKEY TYPE                             
         DC    AL2(00087),AL4(RTGDTA)    RECORD TO GO TO                        
         DC    AL2(00088),AL4(ATGDTA)    ACTION TO GO TO                        
         DC    AL2(00095),AL4(NXTDTA)    NEXT TIME PFK                          
         DC    AL2(00116),AL4(TSTDTA)    TEST PHASE                             
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL14    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   XC    AFRPEL,AFRPEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRPELQ',FRPRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRPEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRPEL                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    AFRPEL,AFRPEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRPELQ',FRPRECD),0               
         CLI   12(R1),0                                                         
         BNE   DFVAL02             NO FRPEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRPEL                                                        
         B     EXITOK              ALREADY HAVE A FRPEL ON RECORD               
*                                                                               
TEMP     USING FRPELD,BOELEM                                                    
DFVAL02  XC    TEMP.FRPEL(FRPLNQ),TEMP.FRPEL                                    
         MVI   TEMP.FRPEL,FRPELQ                                                
         MVI   TEMP.FRPLN,FRPLNQ                                                
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),FRPRECD,TEMP.FRPELD                
         CLI   12(R1),0                                                         
         BE    *+6                 ERROR IN HELLO                               
         DC    H'0'                                                             
         L     RF,16(R1)           A(ELEMENT) HERE AFTER PUT                    
         ST    RF,AFRPEL                                                        
         B     EXITOK              FRPEL ADDED TO RECORD                        
         DROP  TEMP                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  XC    AFRPEL,AFRPEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRPELQ',FRPRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRPEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRPEL                                                        
         B     EXITOK              ALREADY HAVE A FRPEL ON RECORD               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  XC    AFRPEL,AFRPEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRPELQ',FRPRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRPEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRPEL                                                        
         B     EXITOK              ALREADY HAVE A FRPEL ON RECORD               
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3         VERB IN R1                                   
         LA    RF,DLTABL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DLDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DLDFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   DS    0H                                                               
         ICM   R3,15,AFRPEL                                                     
         BZ    DIE                                                              
X        USING FRPKSTAT,GSRECSTA                                                
         MVC   X.FRPKDICT,FRPDICT                                               
         MVC   X.FRPKIND1,FRPINDS1                                              
         MVC   FRPPFK#,FRPKPFK                                                  
         B     EXITOK                                                           
         DROP  X                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
DLDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DLDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SYSTEM                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SYSDTA   LA    RF,SYSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SYSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSYS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSYS)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSYS)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSYS)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSYS)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SYSTEM FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISSYS   L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
         SPACE 1                                                                
DSYS02   CLI   0(RE),0             TEST E-O-T                                   
         BE    DSYS04                                                           
         CLC   SYSLNUM,FRPKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DSYS04   CURED FRPKSYS,(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SYSTEM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALSYS   XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         L     R4,ASYSLST                                                       
         USING SYSLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
         SPACE 1                                                                
VSYS02   CLI   0(R4),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VSYS04                                                           
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VSYS06                                                           
         CLC   SYSLNAME(0),FVIFLD                                               
         SPACE 1                                                                
VSYS04   LA    R4,SYSLLEN(R4)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VSYS02                                                           
         SPACE 1                                                                
VSYS06   MVC   FRPKSYS,SYSLNUM     SYSTEM NUMBER                                
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SYSTEM FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTSYS  L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
         SPACE 1                                                                
DFSYS02  CLI   0(RE),0             TEST E-O-T                                   
         BE    DFSYS04                                                          
         CLC   SYSLNUM,FLTIFLD     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DFSYS02                                                          
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DFSYS04  XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A SYSTEM FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTSYS  XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         L     R4,ASYSLST                                                       
         USING SYSLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
         SPACE 1                                                                
VFSYS02  CLI   0(R4),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VFSYS04                                                          
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VFSYS06                                                          
         CLC   SYSLNAME(0),FVIFLD                                               
         SPACE 1                                                                
VFSYS04  LA    R4,SYSLLEN(R4)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VFSYS02                                                          
         SPACE 1                                                                
VFSYS06  MVC   FRPKSYS,SYSLNUM     SYSTEM NUMBER INTO KEY                       
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM    - AND FILTER                       
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DO SYSTEM FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLC   FRPKSYS,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROGRAM                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PRGDTA   LA    RF,PRGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRG)                                 
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDPRG)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPRG)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPRG)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPRG)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISPRG   OC    FRPKPRG,FRPKPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRPKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,FRPKPRG                                                   
         BNE   DPRG04                                                           
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    DPRG06                                                           
         CLC   PGMCTRY,CUCTRY                                                   
         BE    DPRG06                                                           
DPRG04   BXLE  R1,RE,DPRG02                                                     
         B     DPRG08                                                           
         SPACE 1                                                                
DPRG06   MVC   FVIFLD(L'PGMNAME),PGMNAME                                        
         B     EXITOK                                                           
         SPACE 1                                                                
DPRG08   CURED FRPKPRG,(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVI   FRPKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRPKSYS                                                  
         BE    VPRG02                                                           
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)  INVALID SYSTEM                            
         B     EXITL                                                            
         SPACE 1                                                                
VPRG02   L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         XR    R3,R3                                                            
         IC    R3,FVXLEN           R3=INPUT LENGTH-1                            
         SPACE 1                                                                
VPRG04   CLI   PGMCTRY,0           MATCH ON COUNTRY                             
         BE    *+14                                                             
         CLC   PGMCTRY,CUCTRY                                                   
         BNE   VPRG06                                                           
         EX    R3,*+8                                                           
         BE    VPRG08                                                           
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
         SPACE 1                                                                
VPRG06   BXLE  R1,RE,VPRG04                                                     
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#PGNVS) INVALID PROGRAM                           
         B     EXITL                                                            
         SPACE 1                                                                
VPRG08   MVC   FRPKPRG,PGMNUM      PROGRAM NUMBER                               
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* CREATE A PROGRAM HEADLINE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
HEDPRG   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PROGRAM FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTPRG  OC    FLTIFLD(L'FRPKPRG),FLTIFLD PROGRAM ENTERED?                      
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRPKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DFPRG08                                                          
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DFPRG02  CLC   PGMNUM,FLTIFLD                                                   
         BNE   DFPRG04                                                          
         CLI   PGMCTRY,0           MATCH ON COUNTRY CODE                        
         BE    DFPRG06                                                          
         CLC   PGMCTRY,CUCTRY                                                   
         BE    DFPRG06                                                          
DFPRG04  BXLE  R1,RE,DFPRG02                                                    
         B     DFPRG08                                                          
         SPACE 1                                                                
DFPRG06  MVC   FVIFLD(L'PGMNAME),PGMNAME                                        
         B     EXITOK                                                           
         SPACE 1                                                                
DFPRG08  XR    RF,RF                                                            
         ICM   RF,1,FLTIFLD                                                     
         CURED (RF),(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         DROP  R1                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTPRG  MVI   FRPKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         MVI   FLTIFLD,0                                                        
         CLI   FVILEN,0            TRYING TO SET TO SYSTEM LEVEL ONLY?          
         BE    EXITOK              YES                                          
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRPKSYS                                                  
         BE    VFPRG02                                                          
         BXLE  R1,RE,*-10                                                       
         MVC   FVMSGNO,=AL2(FVFESYS)  INVALID SYSTEM                            
         B     EXITL                                                            
         SPACE 1                                                                
VFPRG02  L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          R1=A(PROGRAMS LIST)                          
         XR    R3,R3                                                            
         IC    R3,FVXLEN           R3=INPUT LENGTH-1                            
         SPACE 1                                                                
VFPRG04  CLI   PGMCTRY,0           MATCH ON COUNTRY                             
         BE    *+14                                                             
         CLC   PGMCTRY,CUCTRY                                                   
         BNE   VFPRG06                                                          
         EX    R3,*+8                                                           
         BE    VFPRG08                                                          
         CLC   PGMNAME(0),FVIFLD   MATCH INPUT TO PROGRAM NAME                  
         SPACE 1                                                                
VFPRG06  BXLE  R1,RE,VFPRG04                                                    
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#PGNVS) INVALID PROGRAM                           
         B     EXITL                                                            
         SPACE 1                                                                
VFPRG08  MVC   FRPKPRG,PGMNUM      PROGRAM NUMBER                               
         MVC   FLTIFLD(L'PGMNUM),PGMNUM                                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FIELD                                      *         
***********************************************************************         
DOFTPRG  CLC   FRPKPRG,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RCODTA   LA    RF,RCOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RCOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRCO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRCO)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTRCO)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTRCO)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTRCO)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRCO   OC    FRPKREC,FRPKREC     GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         ICM   RF,2,FRPKREC                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,FVIFLD,FRPKREC,L'FRPKREC,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRCO   MVI   FRPKREC,0           IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITOK                                                           
         SPACE 1                                                                
         OC    FRPKPRG,FRPKPRG     TRYING TO OVERRIDE RECORD - MUST             
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FRPKREC                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCO  OC    FLTIFLD(L'FRPKREC),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         ICM   RF,2,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,FVIFLD,FRPKREC,L'FRPKREC,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCO  OC    FRPKPRG,FRPKPRG     TRYING TO FILTER RECORD - MUST               
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FRPKREC                                                     
         STCM  RF,2,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTRCO  CLC   FRPKREC,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION NUMBER                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
ACTDTA   LA    RF,ACTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ACTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISACT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALACT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISACT   OC    FRPKACT,FRPKACT     ACTION NUMBER                                
         BZ    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         ICM   RF,2,FRPKREC                                                     
         ICM   RF,1,FRPKACT                                                     
         GOTO1 AGEN,BOPARM,OACT,AXDIS,(RF)                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NAME                                                *         
***********************************************************************         
         SPACE 1                                                                
VALACT   TM    FVIIND,FVINUM       DID USER ENTER NUMBER?                       
         BZ    VACT02                                                           
         L     RF,BCFULL                                                        
         CH    RF,=H'255'                                                       
         BH    EXITNV              ONLY 255 ACTIONS ALLOWED                     
*                                                                               
         STC   RF,FRPKACT                                                       
         PUSH  USING                                                            
         USING FRARECD,IOKEY                                                    
         XC    FRAKEY,FRAKEY       READ ACTION DIRECTORY RECORD                 
         MVI   FRAKMIN,FRAKMINQ                                                 
         MVI   FRAKTYP,FRAKTYPQ                                                 
         MVC   FRAKSYS,FRPKSYS                                                  
         MVC   FRAKPRG,FRPKPRG                                                  
         MVC   FRAKREC,FRPKREC                                                  
         MVC   FRAKACT,FRPKACT                                                  
         L     R1,=AL4(XOGENDIR+XOHI+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   FRARECD(FRAKCTRY-FRARECD),IOKEYSAV                               
         BE    EXITOK              ACTION FOUND                                 
         MVC   FVMSGNO,=AL2(CE#INVAC)                                           
         B     EXITL                                                            
*                                                                               
VACT02   XR    RF,RF                                                            
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         ICM   RF,2,FRPKREC                                                     
         GOTO1 AGEN,BOPARM,OACT,AXVAL,(RF)                                      
         BNE   EXITL                                                            
         L     RF,8(R1)                                                         
         STC   RF,FRPKACT                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ACTION                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTACT  CLC   FRPKACT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PFKEY NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PFNDTA   LA    RF,PFNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PFNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPFN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPFN)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPFN)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPFN)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPFN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY NUMBER                                                *         
***********************************************************************         
         SPACE 1                                                                
DISPFN   OC    FRPKPFK,FRPKPFK     PFKEY NUMBER                                 
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FRPKPFK                                                       
         CURED (RF),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PFKEY NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
VALPFN   TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'24'                                                        
         BH    EXITNV                                                           
         STC   RF,FRPKPFK                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY NUMBER FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTPFN  OC    FLTIFLD(L'FRPKPFK),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PFKEY NUMBER FIELD FOR FILTERING                           *         
***********************************************************************         
         SPACE 1                                                                
VFLTPFN  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'24'                                                        
         BH    EXITNV                                                           
         STC   RF,FRPKPFK                                                       
         STC   RF,FLTIFLD                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PFKEY NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTPFN  CLC   FRPKPFK,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COUNTRY FILTER                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CTRDTA   LA    RF,CTRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CTRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCTR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCTR)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCTR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCTR)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCTR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COUNTRY FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCTR   MVC   BOBYTE1,FRPKCTRY    COUNTRY CODE IS 1`S COMP IN KEY              
         XC    BOBYTE1,BCEFFS                                                   
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,BORELO                                                        
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
*                                                                               
DCTR02   CLC   CTRYCODE,BOBYTE1    MATCH ON CODE                                
         BNE   *+12                                                             
         TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BO    DCTR04                                                           
         BXLE  R1,RE,DCTR02                                                     
         DC    H'0'                COUNTRY CODE IN KEY IS INVALID               
*                                                                               
DCTR04   MVC   FVIFLD(L'CTRYNAM),CTRYNAM   MOVE IN UK NAME FOR CTRY             
         CLI   CUCTRY,CTRYHOMQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'CTRYNAMN),CTRYNAMN MOVE IN NATIVE NAME FOR CTRY         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A COUNTRY FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCTR   CLI   FVILEN,0            ANY INPUT                                    
         BNE   VCTR01                                                           
         MVC   FRPKCTRY,BCEFFS     SET DEFAULT TO NATIVE COUNTRY                
         B     EXITOK                                                           
*                                                                               
VCTR01   XR    R4,R4                                                            
         IC    R4,FVXLEN                                                        
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,BORELO                                                        
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
*                                                                               
VCTR02   TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BZ    VCTR04                                                           
*                                                                               
         CLC   FVIFLD(0),CTRYNAM   FULL UK NAME FOR CTRY                        
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
         CLC   FVIFLD(0),CTRYNAMN  FULL NATIVE NAME FOR CTRY                    
*                                                                               
         CLI   FVILEN,3            LENGTH OK FOR SHORT NAME?                    
         BH    VCTR04                                                           
*                                                                               
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
         CLC   FVIFLD(0),CTRYSHR   SHORT UK NAME FOR CTRY                       
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
         CLC   FVIFLD(0),CTRYSHRN  SHORT NATIVE NAME FOR COUNTRY                
         EX    R4,*+8                                                           
         BE    VCTR06                                                           
*                                                                               
VCTR04   BXLE  R1,RE,VCTR02                                                     
         B     EXITNV              COUNTRY CODE IN KEY IS INVALID               
*                                                                               
VCTR06   MVC   FRPKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         XC    FRPKCTRY,BCEFFS     1`S COMPLIMENT IT                            
                                                                                
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A COUNTRY FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTCTR  LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,BORELO                                                        
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
         CLC   CTRYCODE,FLTIFLD    MATCH ON CODE                                
         BNE   *+12                                                             
         TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BO    DFCTR02                                                          
         BXLE  R1,RE,*-10                                                       
         DC    H'0'                COUNTRY CODE IN KEY IS INVALID               
*                                                                               
DFCTR02  MVC   FVIFLD(L'CTRYNAM),CTRYNAM   MOVE IN UK NAME FOR CTRY             
         CLI   CUCTRY,CTRYHOMQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'CTRYNAMN),CTRYNAMN MOVE IN NATIVE NAME FOR CTRY         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A COUNTRY FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTCTR  CLI   FVILEN,0            ANY INPUT                                    
         BNE   *+16                                                             
         MVI   FRPKCTRY,FF         SET DEFAULT TO NATIVE COUNTRY                
         B     EXITOK                                                           
*                                                                               
         XR    R4,R4                                                            
         IC    R4,FVXLEN                                                        
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         A     RF,BORELO                                                        
         LA    R1,6(R1)                                                         
         USING CTRYTABD,R1                                                      
*                                                                               
VFCTR02  TM    CTRYTYPE,CTRYTYCO   MAKE SURE IT IS A COUNTRY                    
         BZ    VFCTR04                                                          
*                                                                               
         CLC   FVIFLD(0),CTRYNAM   FULL UK NAME FOR CTRY                        
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
         CLC   FVIFLD(0),CTRYNAMN  FULL NATIVE NAME FOR CTRY                    
*                                                                               
         CLI   FVILEN,3            LENGTH OK FOR SHORT NAME?                    
         BH    VFCTR04                                                          
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
         CLC   FVIFLD(0),CTRYSHR   SHORT UK NAME FOR CTRY                       
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
         CLC   FVIFLD(0),CTRYSHRN  SHORT NATIVE NAME FOR COUNTRY                
         EX    R4,*+8                                                           
         BE    VFCTR06                                                          
*                                                                               
VFCTR04  BXLE  R1,RE,VFCTR02                                                    
         B     EXITNV              COUNTRY CODE IN KEY IS INVALID               
*                                                                               
VFCTR06  MVC   FRPKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         MVC   FLTIFLD(L'CTRYCODE),CTRYCODE                                     
         XC    FRPKCTRY,BCEFFS     1`S COMPLIMENT CODE IN KEY                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO COUNTRY FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFTCTR  MVC   BOBYTE1,FVIFLD                                                   
         XC    BOBYTE1,BCEFFS                                                   
         CLC   FRPKCTRY,BOBYTE1                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PFKEY DICTIONARY ENTRY                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TDRDTA   LA    RF,TDRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TDRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTDR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTDR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN PFKEY DICTIONARY ENTRY                                   *         
***********************************************************************         
         SPACE 1                                                                
DISTDR   LTR   R3,R3               FRPEL?                                       
         BZ    EXITOK                                                           
         MVC   BOHALF1,FRPDICT     RECORD NAME IN BOHALF1 FOR DDIC              
         GOTO1 ADDIC                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN PFKEY DICTIONARY ENTRY                                  *         
***********************************************************************         
         SPACE 1                                                                
VALTDR   LTR   R3,R3               FRPEL?                                       
         BZ    DIE                                                              
         GOTO1 AVDIC               VALIDATE RECORD NAME                         
         BNE   EXITL               VDIC SETS OWN ERROR MESSAGES                 
*                                                                               
         MVC   FRPDICT,BOHALF1     RECORD NAME IN BOHALF1 FROM VDIC             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG DICTIONARY FIELD DISPLAY                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TDDDTA   LA    RF,TDDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TDDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTDD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY NAME FROM DICTIONARY EQUATE                           *         
***********************************************************************         
         SPACE 1                                                                
DISTDD   LTR   R3,R3               FRPEL?                                       
         BZ    EXITOK                                                           
         MVI   FVIFLD,DD#ESCL      LEFT ALIGNED                                 
         MVI   FVIFLD+3,8          LENGTH=8                                     
         MVC   FVIFLD+1(L'FRPDICT),FRPDICT                                      
         ICM   RF,15,=C'SU  '                                                   
         ICM   RF,2,FRPKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TAG DICTIONARY FIELD DISPLAY - LOWER CASE           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TDLDTA   LA    RF,TDLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TDLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTDL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY NAME FROM DICTIONARY EQUATE - LOWER CASE              *         
***********************************************************************         
         SPACE 1                                                                
DISTDL   LTR   R3,R3               FRPEL?                                       
         BZ    EXITOK                                                           
         MVI   FVIFLD,DD#ESCL      LEFT ALIGNED                                 
         MVI   FVIFLD+3,8          LENGTH=8                                     
         MVC   FVIFLD+1(L'FRPDICT),FRPDICT                                      
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FRPKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PFKEY IS DDS ONLY PFKEY                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FDODTA   LA    RF,FDOTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FDOTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFDO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFDO)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY IS DDS ONLY                                           *         
***********************************************************************         
         SPACE 1                                                                
DISFDO   LTR   R3,R3               FRPEL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'GL@NO),GL@NO                                            
         TM    FRPINDS1,FRP1DDS                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'GL@YES),GL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PFKEY DICTIONARY ENTRY                                     *         
***********************************************************************         
         SPACE 1                                                                
VALFDO   LTR   R3,R3               FRPEL?                                       
         BZ    DIE                                                              
         NI    FRPINDS1,FF-(FRP1DDS)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),GU@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),GL@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VFDO02                                                           
         CLC   FVIFLD(0),GU@YES                                                 
         EX    RE,*+8              YES                                          
         BNE   VFDO04                                                           
         CLC   FVIFLD(0),GL@YES                                                 
*                                                                               
VFDO02   OI    FRPINDS1,FRP1DDS                                                 
         B     EXITOK                                                           
*                                                                               
VFDO04   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PFKEY VERB                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
VRBDTA   LA    RF,VRBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
VRBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVRB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVRB)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTVRB)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTVRB)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTVRB)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY NUMBER                                                *         
***********************************************************************         
         SPACE 1                                                                
DISVRB   LTR   R3,R3                                                            
         BZ    EXITOK                                                           
         OC    FRPTYPE,FRPTYPE     PFKEY NUMBER                                 
         BZ    EXITOK                                                           
         CURED FRPTYPE,(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PFKEY NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
VALVRB   LTR   R3,R3                                                            
         BZ    DIE                                                              
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'255'                                                       
         BH    EXITNV                                                           
         STC   RF,FRPTYPE                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY VERB FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTVRB  OC    FLTIFLD(L'FRPTYPE),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PFKEY NUMBER FIELD FOR FILTERING                           *         
***********************************************************************         
         SPACE 1                                                                
VFLTVRB  TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'255'                                                       
         BH    EXITNV                                                           
         STC   RF,FLTIFLD                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PFKEY NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTVRB  CLC   FRPTYPE,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD TO GO TO                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RTGDTA   LA    RF,RTGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RTGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRTG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRTG)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD TO GOT TO FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISRTG   OC    FRPREC,FRPREC       GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         ICM   RF,2,FRPREC                                                      
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,FVIFLD,FRPREC,L'FRPREC,0                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD TO GO TO FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALRTG   MVI   FRPREC,0            IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BNE   VALRTG04                                                         
*                                                                               
         CLI   FRPTYPE,FRPTRAC     RECORD ACTION?                               
         BE    VALRTG02                                                         
         CLI   FRPTYPE,FRPTRACN    RECORD ACTION WITH NTRSES?                   
         BE    VALRTG02                                                         
         CLI   FRPTYPE,FRPTPOSN    POSITIONAL ON LIST?                          
         BE    VALRTG02                                                         
         B     EXITOK                                                           
*                                                                               
VALRTG02 MVC   FRPREC,FRPKREC      USE CURRENT RECORD AS DEFAULT                
         B     EXITOK                                                           
*                                                                               
VALRTG04 XR    RF,RF                                                            
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FRPREC                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ACTION NUMBER TO GO TO                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
ATGDTA   LA    RF,ATGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ATGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISATG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALATG)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NAME TO GO TO                                        *         
***********************************************************************         
         SPACE 1                                                                
DISATG   OC    FRPACT,FRPACT       ACTION NUMBER                                
         BZ    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         ICM   RF,2,FRPREC                                                      
         ICM   RF,1,FRPACT                                                      
         GOTO1 AGEN,BOPARM,OACT,AXDIS,(RF)                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NUMBER/NAME TO GO TO                                *         
***********************************************************************         
         SPACE 1                                                                
VALATG   MVI   FRPACT,0            IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BNE   VALATG02                                                         
*                                                                               
         CLI   FRPTYPE,FRPTRAC     RECORD ACTION?                               
         BE    EXITNO                                                           
         CLI   FRPTYPE,FRPTRACN    RECORD ACTION WITH NTRSES?                   
         BE    EXITNO                                                           
         CLI   FRPTYPE,FRPTRACN    RECORD ACTION WITH NTRSES?                   
         BE    EXITNO                                                           
         B     EXITOK                                                           
*                                                                               
VALATG02 XR    RF,RF                                                            
         ICM   RF,8,FRPKSYS                                                     
         ICM   RF,4,FRPKPRG                                                     
         ICM   RF,2,FRPREC                                                      
         GOTO1 AGEN,BOPARM,OACT,AXVAL,(RF)                                      
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,1,FRPACT                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NEXT TIME AUTO PFKEY                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NXTDTA   LA    RF,NXTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NXTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNXT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNXT)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTNXT)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTNXT)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTNXT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY NUMBER                                                *         
***********************************************************************         
         SPACE 1                                                                
DISNXT   OC    FRPNEXT,FRPNEXT     PFKEY NUMBER                                 
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FRPKPFK                                                       
         CURED (RF),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PFKEY NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
VALNXT   MVI   FRPNEXT,0           FIELD IS OPTIONAL                            
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'24'                                                        
         BH    EXITNV                                                           
         STC   RF,FRPNEXT                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PFKEY NUMBER FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTNXT  OC    FLTIFLD(L'FRPNEXT),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(2,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PFKEY NUMBER FIELD FOR FILTERING                           *         
***********************************************************************         
         SPACE 1                                                                
VFLTNXT  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'24'                                                        
         BH    EXITNV                                                           
         STC   RF,FRPNEXT                                                       
         STC   RF,FLTIFLD                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PFKEY NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTNXT  CLC   FRPNEXT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TEST PHASE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
TSTDTA   LA    RF,TSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTST)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTST)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY TEST VERSION                                                *         
***********************************************************************         
         SPACE 1                                                                
DISTST   OC    FRPKTEST,FRPKTEST                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FRPKTEST),FRPKTEST                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TEST VERSION                                               *         
***********************************************************************         
         SPACE 1                                                                
VALTST   XC    FRPKTEST,FRPKTEST                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'A'                                                      
         BNE   *+12                                                             
         MVI   FRPKTEST,C'A'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'B'                                                      
         BNE   *+12                                                             
         MVI   FRPKTEST,C'B'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'C'                                                      
         BNE   *+12                                                             
         MVI   FRPKTEST,C'C'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'X'                                                      
         BNE   *+12                                                             
         MVI   FRPKTEST,C'X'                                                    
         B     EXITOK                                                           
*                                                                               
         B     EXITNV                                                           
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
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    B     EXITOK                                                           
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
THIS     USING FRPRECD,R2                                                       
LAST     USING FRPRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'FRPKEY),THIS.FRPRECD                                     
         ICM   R1,15,=AL4(XIO11+XOGENDIR+XOHI)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               FUCK UP ON THE READ HIGH                     
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     ICM   R1,15,=AL4(XIO11+XOGENDIR+XOSEQ)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(FRPKSYS-FRPRECD),THIS.FRPRECD                              
         BNE   EXITL                                                            
*                                                                               
         MVC   THIS.FRPKEY(FRPKLEN),IOKEY  WE WANT THIS KEY HERE...             
         B     EXITOK                                                           
         DROP  THIS,LAST                                                        
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE A DICTIONARY EQUATE                             *         
*                                                                     *         
* RETURNS DICTIONARY NUMBER IN BOHALF1                                *         
***********************************************************************         
         SPACE 1                                                                
         USING FRPRECD,R2                                                       
VDIC     NTR1  ,                                                                
         XC    BOHALF1,BOHALF1                                                  
         XC    BCFULL,BCFULL                                                    
         SPACE 1                                                                
         CLC   =C'GE#',FVIFLD      OVERRIDING WITH GEN SYSTEM EQUATE?           
         BE    VDIC04              (MUST BE SET EXPLICITLY)                     
         SPACE 1                                                                
         XR    RF,RF               SYSTEM FROM KEY                              
         IC    RF,FRPKSYS                                                       
         MH    RF,=Y(3)                                                         
         LA    RF,PFXTABLE(RF)     RF=PFXTABLE+(SYSTEM * 3)                     
         MVC   BCFULL(2),0(RF)                                                  
         MVI   BCFULL+2,C'#'       BCFULL(3)=DICTIONARY EQUATE PREFIX           
         SPACE 1                                                                
         CLC   FVIFLD(3),BCFULL    TEST USER ENTERED PREFIX                     
         BE    VDIC04                                                           
         SPACE 1                                                                
         CLI   FVILEN,5            NO - SEE IF CAN INSERT IT                    
         BNH   VDIC02                                                           
         MVC   FVMSGNO,=AL2(CE#EQPRE)                                           
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVXTRA(3),BCFULL                                                 
         B     EXITL                                                            
*                                                                               
VDIC02   MVC   BOWORK1,FVIFLD      SAVE THE INPUT                               
         MVC   FVIFLD(3),BCFULL    PREFIX INPUT WITH THE PREFIX                 
         MVC   FVIFLD+3(5),BOWORK1 MOVE INPUT AFTER THE PREFIX                  
         XR    RF,RF                                                            
         IC    RF,FVILEN           INCREMENT INPUT LENGTH BY L'PREFIX           
         LA    RF,3(RF)                                                         
         STC   RF,FVILEN           SAVE LENGTH                                  
         BCTR  RF,0                                                             
         STC   RF,FVXLEN           SAVE LENGTH-1                                
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
VDIC04   XC    GQKEY,GQKEY         READ EQUATE NAME PASSIVE                     
         MVI   GQKREC,GQKRECQ                                                   
         MVC   GQKQNAME,FVIFLD                                                  
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   *+14                                                             
         CLC   GQKEY(GQKMNUM-GQKEY),IOKEYSAV                                    
         BE    VDIC06                                                           
         MVI   FVOSYS,GCSYSGEN     CONTROL SYSTEM                               
         MVC   FVMSGNO,=AL2(CE#EQNOF)                                           
         B     EXITL                                                            
*                                                                               
VDIC06   MVC   BOHALF1,GQKMNUM     RETURN REFERENCE # IN BOHALF1                
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 1                                                                
***********************************************************************         
* ROUTINE TO DISPLAY A DICTIONARY REFERENCE                           *         
*                                                                     *         
* BOHALF1 HOLDS DICTIONARY REFERENCE ON ENTRY                         *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
DDIC     NTR1  ,                                                                
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTGDIC                                                  
         MVC   GMKSYS,FRPKSYS      SET UP THE SYSTEM                            
         XR    RF,RF                                                            
         ICM   RF,3,BOHALF1        GET DICTIONARY NUMBER                        
         TM    BOHALF1,X'40'       IS IT A GENERAL DD ENTRY?                    
         BZ    *+12                                                             
         SH    RF,=Y(GE#GEN)                                                    
         MVI   GMKSYS,GENSYS       MOVE IN GENERAL SYSTEM NUMBER                
*                                                                               
         STCM  RF,3,GMKMSG         SAVE DICTIONARY NUMBER                       
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DDICN                                                            
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BNE   DDICN                                                            
         L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DDICN                                                            
         L     R1,AIO1                                                          
         LA    R3,GMFIRST(R1)                                                   
         USING GMQSYD,R3                                                        
         XR    RF,RF                                                            
         CLI   GMQSYEL,GMQSYELC                                                 
         BE    *+12                                                             
         IC    RF,GMQSYELL                                                      
         BXH   R3,RF,*-12                                                       
         MVC   FVIFLD(L'GMQSYSYM),GMQSYSYM                                      
         B     EXITOK                                                           
*                                                                               
DDICN    MVI   FVIFLD,C'('         DICTIONARY REF# NOT ON FILE                  
         LH    RF,BOHALF1                                                       
         CVD   RF,BODUB1                                                        
         OI    BODUB1+7,X'0F'                                                   
         UNPK  FVIFLD+1(5),BODUB1                                               
         MVI   FVIFLD+6,C'*'                                                    
         MVI   FVIFLD+7,C')'                                                    
         B     EXITOK                                                           
         POP   USING                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
GENSYS   EQU   X'0F'                                                            
         SPACE 1                                                                
       ++INCLUDE DDPFXTBLE                                                      
         SPACE 1                                                                
       ++INCLUDE FACTRYTAB                                                      
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
DCLIST   DS    0D                                                               
         DCDDL GE#UP,8,L                                                        
         DCDDL GE#DOWN,8,L                                                      
         DCDDL GE#LEFT,8,L                                                      
         DCDDL GE#RIGHT,8,L                                                     
         DCDDL GE#YES,4,L                                                       
         DCDDL GE#NO,4,L                                                        
         DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL25                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
AVDIC    DS    A                                                                
ADDIC    DS    A                                                                
AFRPEL   DS    A                                                                
AFVADDR  DS    A                                                                
DSLISTU  DS    0F                                                               
GU@UP    DS    CL8                                                              
GU@DOWN  DS    CL8                                                              
GU@LEFT  DS    CL8                                                              
GU@RIGHT DS    CL8                                                              
GU@YES   DS    CL4                                                              
GU@NO    DS    CL4                                                              
DSLISTL  DS    0F                                                               
GL@UP    DS    CL8                                                              
GL@DOWN  DS    CL8                                                              
GL@LEFT  DS    CL8                                                              
GL@RIGHT DS    CL8                                                              
GL@YES   DS    CL4                                                              
GL@NO    DS    CL4                                                              
*                                                                               
         SPACE 2                                                                
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        FAFACTS                                                                
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
*        FASYSLSTD                                                              
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
*        DDDDEQUS                                                               
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
*        CTMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*        FASELIST                                                               
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
*        FAPGMLST                                                               
         PRINT OFF                                                              
       ++INCLUDE FAPGMLST                                                       
         PRINT ON                                                               
*        FASYSFAC                                                               
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
*        DDSCANBLKD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        FACTRY                                                                 
         PRINT OFF                                                              
       ++INCLUDE FACTRY                                                         
         PRINT ON                                                               
*        FACTRYEQUS                                                             
         PRINT OFF                                                              
       ++INCLUDE FACTRYEQUS                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004CTFIL14   08/22/00'                                      
         END                                                                    
