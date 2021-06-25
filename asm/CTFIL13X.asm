*          DATA SET CTFIL13X   AT LEVEL 003 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1313A                                                                  
         TITLE 'OBJECT VERSION OF ACTION RECORDS'                               
FIL13    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL13*,R6,R7,RR=RE                                           
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
         GOTOX VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTOX (RF),(R1),C'LL  ',,DSLISTL                                       
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
         USING FRARECD,R2                                                       
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
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
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
KFKVAL   XC    FRAKEY,FRAKEY       INITIALIZE KEY OF ACTION RECORD              
         MVI   FRAKMIN,FRAKMINQ    SET MINOR SYSTEM                             
         MVI   FRAKTYP,FRAKTYPQ    SET ACTION RECORD                            
         MVI   FRAKSUB,FF          ACTION SUB-COUNTRY                           
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
KFKFVAL  XC    FRAKEY,FRAKEY       INITIALIZE KEY OF ACTION RECORD              
         MVI   FRAKMIN,FRAKMINQ    SET MINOR SYSTEM                             
         MVI   FRAKTYP,FRAKTYPQ    SET ACTION RECORD                            
         MVI   FRAKSUB,FF          ACTION SUB-COUNTRY                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         R3=INVOKING ACTION                           
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
X        USING FRARECD,R3                                                       
         XC    FRAKEY,FRAKEY                                                    
         MVI   FRAKMIN,FRAKMINQ    SEE IF INHERIT FROM PROGRAM LEVEL            
         MVI   FRAKTYP,FRAKTYPQ                                                 
         MVC   FRAKSYS,X.FRAKSYS                                                
         MVC   FRAKPRG,X.FRAKPRG                                                
         MVC   FRAKACT,X.FRAKACT                                                
         MVC   FRAKCTRY,X.FRAKCTRY                                              
         MVC   FRAKSUB,X.FRAKSUB                                                
         MVC   IOKEY(L'FRAKEY),FRAKEY                                           
         L     R1,=AL4(XOHIGH+XOGENDIR)                                         
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   IOKEY(FRAKCTRY-FRARECD),IOKEYSAV                                 
         BE    EXITOK                                                           
*                                                                               
         XC    FRAKEY,FRAKEY                                                    
         B     EXITH               NO KEY TO INHERIT FROM                       
         DROP  X                                                                
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
         USING FRARECD,R2                                                       
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
         USING FRARECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRAEL                                                        
         USING FRAELD,R3           R3=A(FRAEL ON RECORD)                        
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
         DC    AL2(00101),AL4(TDRDTA)    ACTION NAME                            
         DC    AL2(00102),AL4(TDDDTA)    ACTION NAME DISPLAY ROUTINE            
         DC    AL2(00103),AL4(TDLDTA)    ACTION NAME DISPLAY ROUTINE            
         DC    AL2(00022),AL4(FDODTA)    ACTION IS DDS ONLY                     
         DC    AL2(00105),AL4(SELDTA)    ACTION TYPE                            
         DC    AL2(00150),AL4(CTRDTA)    COUNTRY FILTER                         
         DC    AL2(00107),AL4(HLPDTA)    ACTION DOES NOT APPEAR ON HELP         
         DC    AL2(00159),AL4(UDTDTA)    ACTION IS UPDATIVE                     
         DC    AL2(00109),AL4(VRBDTA)    ACTION VERB                            
         DC    AL2(00162),AL4(DMNDTA)    ACTION HELP NUMBER                     
         DC    AL2(00163),AL4(DMSDTA)    ACTION HELP NUMBER DISPLAY             
         DC    AL2(00108),AL4(SUBDTA)    ACTION IS ABLE TO BE SUBACTION         
         DC    AL2(00110),AL4(RTGDTA)    SUBACT RECORD TO GO TO                 
         DC    AL2(00111),AL4(ATGDTA)    SUBACT ACTION TO GO TO                 
         DC    AL2(00112),AL4(SVBDTA)    SUBACT VERB                            
         DC    AL2(00113),AL4(OPHDTA)    OVERRIDE PHASE TO USE                  
         DC    AL2(00114),AL4(LENDTA)    VALIDATION LENGTH                      
         DC    AL2(00115),AL4(SBLDTA)    SUB-ACTION VALIDATION LENGTH           
         DC    AL2(00164),AL4(SMNDTA)    SUB-ACTION HELP NUMBER                 
         DC    AL2(00165),AL4(SMSDTA)    SUB-ACTION HELP NUMBER DISPLAY         
         DC    AL2(00116),AL4(TSTDTA)    TEST PHASE                             
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL13    CSECT                                                                  
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
DFDDIS   XC    AFRAEL,AFRAEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRAELQ',FRARECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRAEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRAEL                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    AFRAEL,AFRAEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRAELQ',FRARECD),0               
         CLI   12(R1),0                                                         
         BNE   DFVAL02             NO FRAEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRAEL                                                        
         B     EXITOK              ALREADY HAVE A FRAEL ON RECORD               
*                                                                               
TEMP     USING FRAELD,BOELEM                                                    
DFVAL02  XC    TEMP.FRAEL(FRALNQ),TEMP.FRAEL                                    
         MVI   TEMP.FRAEL,FRAELQ                                                
         MVI   TEMP.FRALN,FRALNQ                                                
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),FRARECD,TEMP.FRAELD                
         CLI   12(R1),0                                                         
         BE    *+6                 ERROR IN HELLO                               
         DC    H'0'                                                             
         L     RF,16(R1)           A(ELEMENT) HERE AFTER PUT                    
         ST    RF,AFRAEL                                                        
         B     EXITOK              FRAEL ADDED TO RECORD                        
         DROP  TEMP                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  XC    AFRAEL,AFRAEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRAELQ',FRARECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRAEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRAEL                                                        
         B     EXITOK              ALREADY HAVE A FRAEL ON RECORD               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  XC    AFRAEL,AFRAEL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRAELQ',FRARECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRAEL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRAEL                                                        
         B     EXITOK              ALREADY HAVE A FRAEL ON RECORD               
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
         ICM   R3,15,AFRAEL                                                     
         BZ    DIE                                                              
X        USING FRAKSTAT,GSRECSTA                                                
         MVC   X.FRAKDICT,FRADICT                                               
         MVC   X.FRAKIND1,FRAINDS1                                              
         DROP  X                                                                
*                                                                               
         MVC   FRANUM,FRAKACT                                                   
         B     EXITOK                                                           
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
         CLC   SYSLNUM,FRAKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DSYS04   CURED FRAKSYS,(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
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
VSYS06   MVC   FRAKSYS,SYSLNUM     SYSTEM NUMBER                                
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
VFSYS06  MVC   FRAKSYS,SYSLNUM     SYSTEM NUMBER INTO KEY                       
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM    - AND FILTER                       
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DO SYSTEM FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLC   FRAKSYS,FLTIFLD                                                  
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
DISPRG   OC    FRAKPRG,FRAKPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRAKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,FRAKPRG                                                   
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
DPRG08   CURED FRAKPRG,(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVI   FRAKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRAKSYS                                                  
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
VPRG08   MVC   FRAKPRG,PGMNUM      PROGRAM NUMBER                               
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
DFLTPRG  OC    FLTIFLD(L'FRAKPRG),FLTIFLD PROGRAM ENTERED?                      
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRAKSYS                                                  
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
VFLTPRG  MVI   FRAKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
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
         CLC   SEOVSYS,FRAKSYS                                                  
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
VFPRG08  MVC   FRAKPRG,PGMNUM      PROGRAM NUMBER                               
         MVC   FLTIFLD(L'PGMNUM),PGMNUM                                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FIELD                                      *         
***********************************************************************         
DOFTPRG  CLC   FRAKPRG,FLTIFLD                                                  
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
DISRCO   OC    FRAKREC,FRAKREC     GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         ICM   RF,2,FRAKREC                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRCO   MVI   FRAKREC,0           IF NO RECORD                                 
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BE    EXITOK                                                           
         SPACE 1                                                                
         OC    FRAKPRG,FRAKPRG     TRYING TO OVERRIDE RECORD - MUST             
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FRAKREC                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCO  OC    FLTIFLD(L'FRAKREC),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         ICM   RF,2,FLTIFLD                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCO  OC    FRAKPRG,FRAKPRG     TRYING TO FILTER RECORD - MUST               
         BZ    EXITNV              INPUT THE PROGRAM                            
         SPACE 1                                                                
         XR    RF,RF                                                            
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FRAKREC                                                     
         STCM  RF,2,FLTIFLD                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTRCO  CLC   FRAKREC,FLTIFLD                                                  
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
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTACT)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTACT)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTACT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
DISACT   OC    FRAKACT,FRAKACT     ACTION NUMBER                                
         BZ    EXITOK                                                           
         CURED FRAKACT,(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NUMBER                                              *         
***********************************************************************         
         SPACE 1                                                                
VALACT   TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'255'                                                       
         BH    EXITNV                                                           
         STC   RF,FRAKACT                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION NUMBER FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTACT  OC    FLTIFLD(L'FRAKACT),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NUMBER FIELD FOR FILTERING                          *         
***********************************************************************         
         SPACE 1                                                                
VFLTACT  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'255'                                                       
         BH    EXITNV                                                           
         STC   RF,FRAKACT                                                       
         STC   RF,FLTIFLD                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ACTION NUMBER                                      *         
***********************************************************************         
         SPACE 1                                                                
DOFTACT  CLC   FRAKACT,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION DICTIONARY ENTRY                             *         
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
* DISPLAY AN ACTION DICTIONARY ENTRY                                  *         
***********************************************************************         
         SPACE 1                                                                
DISTDR   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         MVC   BOHALF1,FRADICT     RECORD NAME IN BOHALF1 FOR DDIC              
         GOTO1 ADDIC                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN ACTION DICTIONARY ENTRY                                 *         
***********************************************************************         
         SPACE 1                                                                
VALTDR   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
         GOTO1 AVDIC               VALIDATE RECORD NAME                         
         BNE   EXITL               VDIC SETS OWN ERROR MESSAGES                 
*                                                                               
         MVC   FRADICT,BOHALF1     RECORD NAME IN BOHALF1 FROM VDIC             
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
* DISPLAY ACTION NAME FROM DICTIONARY EQUATE                          *         
***********************************************************************         
         SPACE 1                                                                
DISTDD   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         MVI   FVIFLD,DD#ESCL      LEFT ALIGNED                                 
         MVI   FVIFLD+3,8          LENGTH=8                                     
         MVC   FVIFLD+1(L'FRADICT),FRADICT                                      
         ICM   RF,15,=C'SU  '                                                   
         ICM   RF,2,FRAKSYS                                                     
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
* DISPLAY ACTION NAME FROM DICTIONARY EQUATE - LOWER CASE             *         
***********************************************************************         
         SPACE 1                                                                
DISTDL   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         MVI   FVIFLD,DD#ESCL      LEFT ALIGNED                                 
         MVI   FVIFLD+3,8          LENGTH=8                                     
         MVC   FVIFLD+1(L'FRADICT),FRADICT                                      
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FRAKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR ACTION IS DDS ONLY ACTION                           *         
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
* DISPLAY ACTION IS DDS ONLY                                          *         
***********************************************************************         
         SPACE 1                                                                
DISFDO   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'UC@NO),UC@NO                                            
         TM    FRAINDS1,FRA1DDS                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'UC@YES),UC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION DICTIONARY ENTRY                                    *         
***********************************************************************         
         SPACE 1                                                                
VALFDO   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
         NI    FRAINDS1,FF-(FRA1DDS)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UC@NO                                                  
*                                                                               
VFDO02   EX    RE,*+8              YES                                          
         BNE   VFDO04                                                           
         CLC   FVIFLD(0),UC@YES                                                 
         OI    FRAINDS1,FRA1DDS                                                 
         B     EXITOK                                                           
*                                                                               
VFDO04   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION IS MAINTENANCE TYPE                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SELDTA   LA    RF,SELTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SELTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSEL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION IS MAINTENANCE                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         CLI   FRATYPE,0                                                        
         BE    EXITOK                                                           
         CLI   FRATYPE,FRATMNT                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'GL@MAINT),GL@MAINT                                      
         B     EXITOK                                                           
*                                                                               
         CLI   FRATYPE,FRATLIST                                                 
         BNE   *+14                                                             
         MVC   FVIFLD(L'GL@LIST),GL@LIST                                        
         B     EXITOK                                                           
*                                                                               
         CLI   FRATYPE,FRATREP                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'GL@REP),GL@REP                                          
         B     EXITOK                                                           
*                                                                               
         CLI   FRATYPE,FRATDWN                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'GL@DLOAD),GL@DLOAD                                      
         B     EXITOK                                                           
*                                                                               
         DC    H'0'                FRATYPE CORRUPTED                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION IS MAINTENANCE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
         MVI   FRATYPE,0                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NOTHING ??                        
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              MAINTAIN                                     
         BE    VSEL02                                                           
         CLC   FVIFLD(0),GU@MAINT                                               
         EX    RE,*+8              MAINTAIN                                     
         BE    VSEL02                                                           
         CLC   FVIFLD(0),GL@MAINT                                               
         B     VSEL04                                                           
*                                                                               
VSEL02   MVI   FRATYPE,FRATMNT                                                  
         B     EXITOK                                                           
*                                                                               
VSEL04   EX    RE,*+8              LIST                                         
         BE    VSEL06                                                           
         CLC   FVIFLD(0),GU@LIST                                                
         EX    RE,*+8              LIST                                         
         BE    VSEL06                                                           
         CLC   FVIFLD(0),GL@LIST                                                
         B     VSEL08                                                           
*                                                                               
VSEL06   MVI   FRATYPE,FRATLIST                                                 
         B     EXITOK                                                           
*                                                                               
VSEL08   EX    RE,*+8              REPORT                                       
         BE    VSEL10                                                           
         CLC   FVIFLD(0),GU@REP                                                 
         EX    RE,*+8              REPORT                                       
         BE    VSEL10                                                           
         CLC   FVIFLD(0),GL@REP                                                 
         B     VSEL12                                                           
*                                                                               
VSEL10   MVI   FRATYPE,FRATREP                                                  
         B     EXITOK                                                           
*                                                                               
VSEL12   EX    RE,*+8              DOWNLOAD                                     
         BE    VSEL14                                                           
         CLC   FVIFLD(0),GU@DLOAD                                               
         EX    RE,*+8              DOWNLOAD                                     
         BE    VSEL14                                                           
         CLC   FVIFLD(0),GL@DLOAD                                               
         B     VSELNV                                                           
*                                                                               
VSEL14   MVI   FRATYPE,FRATDWN                                                  
         B     EXITOK                                                           
*                                                                               
VSELNV   B     EXITNV                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION IS UPDATIVE TYPE                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
UDTDTA   LA    RF,UDTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
UDTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUDT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD IS UPDATIVE TYPE                                     *         
***********************************************************************         
         SPACE 1                                                                
DISUDT   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'UC@NO),UC@NO                                            
         TM    FRAINDS1,FRA1UPD                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'UC@YES),UC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD IS UPDATIVE TYPE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALUDT   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
         NI    FRAINDS1,FF-(FRA1UPD)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BNE   VUDT02                                                           
         CLC   FVIFLD(0),UC@YES                                                 
         OI    FRAINDS1,FRA1UPD                                                 
         B     EXITOK                                                           
*                                                                               
VUDT02   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION IS SUB-ACTION ON LIST                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SUBDTA   LA    RF,SUBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SUBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSUB)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION IS SUB-ACTION ON LIST                                *         
***********************************************************************         
         SPACE 1                                                                
DISSUB   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'UC@NO),UC@NO                                            
         TM    FRAINDS1,FRA1SUB                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'UC@YES),UC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION IS SUB-ACTION ON LIST                               *         
***********************************************************************         
         SPACE 1                                                                
VALSUB   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         CLI   FVILEN,0                                                         
         BE    VSUB04              DEFAULT IS NO                                
*                                                                               
         EX    RE,*+8              YES                                          
         BNE   VSUB02                                                           
         CLC   FVIFLD(0),UC@YES                                                 
         OI    FRAINDS1,FRA1SUB                                                 
         OC    FRANREC,FRANREC     RESET SUB RECORD/ACTION                      
         BNZ   *+10                                                             
         MVC   FRANREC,FRAKREC                                                  
         OC    FRANACT,FRANACT                                                  
         BNZ   *+10                                                             
         MVC   FRANACT,FRAKACT                                                  
         B     EXITOK                                                           
*                                                                               
VSUB02   EX    RE,*+8              NO                                           
         BNE   VSUB06                                                           
         CLC   FVIFLD(0),UC@NO                                                  
*                                                                               
VSUB04   NI    FRAINDS1,FF-(FRA1SUB)                                            
         XC    FRANREC,FRANREC     RESET SUB RECORD/ACTION                      
         XC    FRANACT,FRANACT                                                  
         XC    FRASACT,FRASACT                                                  
         B     EXITOK                                                           
*                                                                               
VSUB06   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION DOES NOT DISPLAY ON HELP SCREEN              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
HLPDTA   LA    RF,HLPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HLPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHLP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHLP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION DOES NOT DISPLAY ON HELP SCREEN                      *         
***********************************************************************         
         SPACE 1                                                                
DISHLP   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'UC@NO),UC@NO                                            
         TM    FRAINDS1,FRA1NOH                                                 
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'UC@YES),UC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION DOES NOT DISPLAY ON HELP SCREEN                     *         
***********************************************************************         
         SPACE 1                                                                
VALHLP   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
         NI    FRAINDS1,FF-(FRA1NOH)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UC@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BNE   VHLP02                                                           
         CLC   FVIFLD(0),UC@YES                                                 
         OI    FRAINDS1,FRA1NOH                                                 
         B     EXITOK                                                           
*                                                                               
VHLP02   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACTION VERB                                         *         
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
* DISPLAY ACTION NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
DISVRB   OC    FRAVERB,FRAVERB     ACTION VERB                                  
         BZ    EXITOK                                                           
         CURED FRAVERB,(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NUMBER                                              *         
***********************************************************************         
         SPACE 1                                                                
VALVRB   LTR   R3,R3                                                            
         BZ    DIE                                                              
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'255'                                                       
         BH    EXITNV                                                           
         STC   RF,FRAVERB                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACTION VERB FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTVRB  OC    FLTIFLD(L'FRAVERB),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         XR    RF,RF                                                            
         IC    RF,FLTIFLD                                                       
         CURED (RF),(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NUMBER FIELD FOR FILTERING                          *         
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
* DO FILTERING FOR ACTION NUMBER                                      *         
***********************************************************************         
         SPACE 1                                                                
DOFTVRB  CLC   FRAVERB,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
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
DISCTR   MVC   BOBYTE1,FRAKCTRY    COUNTRY CODE IS 1`S COMP IN KEY              
         XC    BOBYTE1,BCEFFS                                                   
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
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
         MVC   FRAKCTRY,BCEFFS     SET DEFAULT TO NATIVE COUNTRY                
         B     EXITOK                                                           
*                                                                               
VCTR01   XR    R4,R4                                                            
         IC    R4,FVXLEN                                                        
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
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
VCTR06   MVC   FRAKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         XC    FRAKCTRY,BCEFFS     1`S COMPLIMENT IT                            
                                                                                
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
         MVI   FRAKCTRY,FF         SET DEFAULT TO NATIVE COUNTRY                
         B     EXITOK                                                           
*                                                                               
         XR    R4,R4                                                            
         IC    R4,FVXLEN                                                        
         LA    R1,CTRYTAB                                                       
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
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
VFCTR06  MVC   FRAKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         MVC   FLTIFLD(L'CTRYCODE),CTRYCODE                                     
         XC    FRAKCTRY,BCEFFS     1`S COMPLIMENT CODE IN KEY                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO COUNTRY FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFTCTR  MVC   BOBYTE1,FVIFLD                                                   
         XC    BOBYTE1,BCEFFS                                                   
         CLC   FRAKCTRY,BOBYTE1                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAY OF HELP MESSAGE NUMBER                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DMNDTA   LA    RF,DMNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DMNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDMN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDMN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD HELP MESSAGE NUMBER                                  *         
***********************************************************************         
         SPACE 1                                                                
DISDMN   LTR   R3,R3                                                            
         BZ    EXITOK                                                           
         OC    FRAHELP,FRAHELP                                                  
         BZ    EXITOK                                                           
         CURED FRAHELP,(5,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD HELP MESSAGE NUMBER                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDMN   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
*                                                                               
         XC    FRAHELP,FRAHELP                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK              HELP FIELD OPTIONAL (FOR NOW)                
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
*                                                                               
         ICM   RF,15,BCFULL                                                     
         LTR   RF,RF                                                            
         BZ    EXITNV                                                           
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTSCR      SET SCREEN RECORD                            
         MVC   GMKSYS,FRAKSYS      SET UP THE SYSTEM                            
         STCM  RF,3,GMKMSG         SET MESSAGE NUMBER                           
         STCM  RF,3,FRAHELP        SAVE HELP NUMBER IN ELEMENT                  
         MVC   GMKLANG,CULANG                                                   
         XI    GMKLANG,X'FF'                                                    
*                                                                               
         L     R1,=AL4(XOHIGH+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(CE#RECNF)                                           
         B     EXITL                                                            
         POP   USING                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAY OF HELP MESSAGE                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DMSDTA   LA    RF,DMSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DMSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDMS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD HELP MESSAGE                                         *         
***********************************************************************         
         SPACE 1                                                                
DISDMS   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         OC    FRAHELP,FRAHELP                                                  
         BZ    EXITOK                                                           
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTSCR      SET SCREEN RECORD                            
         MVC   GMKSYS,FRAKSYS      SET UP THE SYSTEM                            
         MVC   GMKMSG,FRAHELP      SET MESSAGE NUMBER                           
         MVC   GMKLANG,CULANG                                                   
         XI    GMKLANG,X'FF'                                                    
*                                                                               
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BNE   EXITOK               @@TEMP @@                                   
*        DC    H'0'                                                             
         L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         LA    R3,GMFIRST(R1)                                                   
         USING GMSGEL,R3                                                        
         XR    RF,RF                                                            
         CLI   GMSGEL,GMSGELC                                                   
         BE    *+12                                                             
         IC    RF,GMSGELL                                                       
         BXH   R3,RF,*-12                                                       
         IC    RF,GMSGELL                                                       
         SH    RF,=Y(GMSGTXT-GMSGEL+1)                                          
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),GMSGTXT                                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAY OF SUB-ACTION HELP MESSAGE NUMBER           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SMNDTA   LA    RF,SMNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SMNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSMN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSMN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUB-ACTION HELP MESSAGE NUMBER                              *         
***********************************************************************         
         SPACE 1                                                                
DISSMN   LTR   R3,R3                                                            
         BZ    EXITOK                                                           
         OC    FRASHELP,FRASHELP                                                
         BZ    EXITOK                                                           
         CURED FRASHELP,(5,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD HELP MESSAGE NUMBER                                 *         
***********************************************************************         
         SPACE 1                                                                
VALSMN   LTR   R3,R3               FRAEL?                                       
         BZ    DIE                                                              
*                                                                               
         XC    FRASHELP,FRASHELP                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK              HELP FIELD OPTIONAL (FOR NOW)                
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
*                                                                               
         ICM   RF,15,BCFULL                                                     
         LTR   RF,RF                                                            
         BZ    EXITNV                                                           
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTSCR      SET SCREEN RECORD                            
         MVC   GMKSYS,FRAKSYS      SET UP THE SYSTEM                            
         STCM  RF,3,GMKMSG         SET MESSAGE NUMBER                           
         STCM  RF,3,FRASHELP       SAVE HELP NUMBER IN ELEMENT                  
         MVC   GMKLANG,CULANG                                                   
         XI    GMKLANG,X'FF'                                                    
*                                                                               
         L     R1,=AL4(XOHIGH+XOGENDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(CE#RECNF)                                           
         B     EXITL                                                            
         POP   USING                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAY OF SUB-ACTION HELP MESSAGE                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SMSDTA   LA    RF,SMSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SMSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSMS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUB-ACTION HELP MESSAGE                                     *         
***********************************************************************         
         SPACE 1                                                                
DISSMS   LTR   R3,R3               FRAEL?                                       
         BZ    EXITOK                                                           
         OC    FRASHELP,FRASHELP                                                
         BZ    EXITOK                                                           
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTSCR      SET SCREEN RECORD                            
         MVC   GMKSYS,FRAKSYS      SET UP THE SYSTEM                            
         MVC   GMKMSG,FRASHELP     SET MESSAGE NUMBER                           
         MVC   GMKLANG,CULANG                                                   
         XI    GMKLANG,X'FF'                                                    
*                                                                               
         L     R1,=AL4(XOHI+XOGENDIR+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BNE   EXITOK               @@TEMP @@                                   
*        DC    H'0'                                                             
         L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         LA    R3,GMFIRST(R1)                                                   
         USING GMSGEL,R3                                                        
         XR    RF,RF                                                            
         CLI   GMSGEL,GMSGELC                                                   
         BE    *+12                                                             
         IC    RF,GMSGELL                                                       
         BXH   R3,RF,*-12                                                       
         IC    RF,GMSGELL                                                       
         SH    RF,=Y(GMSGTXT-GMSGEL+1)                                          
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),GMSGTXT                                                
         B     EXITOK                                                           
         POP   USING                                                            
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
DISRTG   OC    FRANREC,FRANREC     GLOBAL FIELD?                                
         BZ    EXITOK                                                           
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         ICM   RF,2,FRANREC                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXDIS,(RF)                                   
         BE    EXITOK                                                           
         B     EXITOK          @@ TEMP @@                                       
         DC    H'0'                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD TO GO TO FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALRTG   MVI   FRANREC,0           IF NO RECORD                                 
         TM    FRAINDS1,FRA1SUB    SUB-ACTION?                                  
         BZ    EXITOK                                                           
*                                                                               
         CLI   FVILEN,0            FIELD SET AT LEVELS ABOVE                    
         BNE   VALRTG04                                                         
         MVC   FRANREC,FRAKREC     DEFAULT IS KEY RECORD                        
         B     EXITOK                                                           
*                                                                               
VALRTG02 MVC   FRANREC,FRAKREC     USE CURRENT RECORD AS DEFAULT                
         B     EXITOK                                                           
*                                                                               
VALRTG04 XR    RF,RF                                                            
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         GOTO1 AGEN,BOPARM,ORTYPE,RTXVAL,(RF)                                   
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,2,FRANREC                                                     
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
DISATG   OC    FRANACT,FRANACT     ACTION NUMBER                                
         BZ    EXITOK                                                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         ICM   RF,2,FRANREC                                                     
         ICM   RF,1,FRANACT                                                     
         GOTO1 AGEN,BOPARM,OACT,AXDIS,(RF)                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACTION NUMBER/NAME TO GO TO                                *         
***********************************************************************         
         SPACE 1                                                                
VALATG   MVI   FRANACT,0                                                        
         TM    FRAINDS1,FRA1SUB    SUB-ACTION                                   
         BZ    EXITOK                                                           
*                                                                               
         CLI   FVILEN,0                                                         
         BNE   VALATG02                                                         
         MVC   FRANACT,FRAKACT     DEFAULT IS KEY ACTION                        
         B     EXITOK                                                           
*                                                                               
VALATG02 XR    RF,RF                                                            
         ICM   RF,8,FRAKSYS                                                     
         ICM   RF,4,FRAKPRG                                                     
         ICM   RF,2,FRANREC                                                     
         GOTO1 AGEN,BOPARM,OACT,AXVAL,(RF)                                      
         BNE   EXITL               NOT FOUND                                    
         L     RF,8(R1)                                                         
         STCM  RF,1,FRANACT                                                     
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUB-ACTION VERB                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SVBDTA   LA    RF,SVBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SVBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSVB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSVB)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUB VERB NUMBER                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSVB   OC    FRASACT,FRASACT     ACTION VERB                                  
         BZ    EXITOK                                                           
         CURED FRASACT,(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUB VERB NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALSVB   MVI   FRASACT,0           RESET NUMBER                                 
         CLI   FVILEN,0            ANY INPUT?                                   
         BE    EXITOK                                                           
         TM    FRAINDS1,FRA1SUB    ONLY VALID IF SUB-ACTION SET                 
         BZ    EXITOK                                                           
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'255'                                                       
         BH    EXITNV                                                           
         STC   RF,FRASACT                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR OVERRIDE PHASE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
OPHDTA   LA    RF,OPHTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
OPHTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOPH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOPH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OVERRIDE PHASE FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISOPH   OC    FRAOACT,FRAOACT                                                  
         BZ    EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,FRAOACT,FVIFLD,L'FRAOACT,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALOPH   MVI   FRAOACT,0                                                        
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         TM    FVIIND,FVIHEX                                                    
         BZ    EXITNV                                                           
         CLI   FVILEN,2            X'NN' IS ONLY VALID INPUT                    
         BNE   EXITNV                                                           
         GOTO1 VHEXIN,BOPARM,FVIFLD,FRAOACT,2                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR VALIDATION LENGTH                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LENDTA   LA    RF,LENTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LENTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLEN)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFLTLEN)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VALIDATE LENGTH                                             *         
***********************************************************************         
         SPACE 1                                                                
DISLEN   MVC   BOBYTE1,GSRECSTA    LENGTH IN STATUS AREA                        
         NI    BOBYTE1,FRAKLVAL                                                 
         CURED BOBYTE1,(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                      
         CLI   FVIFLD,C'0'                                                      
         BNE   *+8                                                              
         MVI   FVIFLD,C'1'                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE VALIDATE LENGTH                                            *         
***********************************************************************         
         SPACE 1                                                                
VALLEN   TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'1'                                                         
         BL    EXITNV                                                           
         CH    RF,=H'8'                                                         
         BH    EXITNV                                                           
         NI    GSRECSTA,FF-(FRAKLVAL)                                           
         STCM  RF,1,BOBYTE1                                                     
         OC    GSRECSTA(1),BOBYTE1                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY VALIDATE LENGTH DEFAULT                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTLEN  OI    GSRECSTA,X'01'                                                   
         MVI   FVIFLD,C'1'                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SUB-ACTION VALIDATE LENGTH                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SBLDTA   LA    RF,SBLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SBLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSBL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSBL)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFLTSBL)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUB-ACTION VALIDATE LENGTH                                  *         
***********************************************************************         
         SPACE 1                                                                
DISSBL   MVC   BOBYTE1,GSRECSTA    LENGTH IN STATUS AREA                        
         NI    BOBYTE1,FRAKSVAL                                                 
         XR    RF,RF                                                            
         ICM   RF,1,BOBYTE1                                                     
         SRL   RF,3                                                             
         CURED (RF),(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         CLI   FVIFLD,C'0'                                                      
         BNE   *+8                                                              
         MVI   FVIFLD,C'1'                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUB-ACTION VALIDATE LENGTH                                 *         
***********************************************************************         
         SPACE 1                                                                
VALSBL   TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
         L     RF,BCFULL                                                        
         CH    RF,=H'1'                                                         
         BL    EXITNV                                                           
         CH    RF,=H'8'                                                         
         BH    EXITNV                                                           
         SLL   RF,3                                                             
         NI    GSRECSTA,FF-(FRAKSVAL)                                           
         STCM  RF,1,BOBYTE1                                                     
         OC    GSRECSTA(1),BOBYTE1                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUB-ACTION VALIDATE LENGTH DEFAULT                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTSBL  OI    GSRECSTA,X'08'                                                   
         MVI   FVIFLD,C'1'                                                      
         B     EXITOK                                                           
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
DISTST   OC    FRAKTEST,FRAKTEST                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FRAKTEST),FRAKTEST                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TEST VERSION                                               *         
***********************************************************************         
         SPACE 1                                                                
VALTST   XC    FRAKTEST,FRAKTEST                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'A'                                                      
         BNE   *+12                                                             
         MVI   FRAKTEST,C'A'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'B'                                                      
         BNE   *+12                                                             
         MVI   FRAKTEST,C'B'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'C'                                                      
         BNE   *+12                                                             
         MVI   FRAKTEST,C'C'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'X'                                                      
         BNE   *+12                                                             
         MVI   FRAKTEST,C'X'                                                    
         B     EXITOK                                                           
*                                                                               
         B     EXITNV              NOTHING ELSE IS VALID                        
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
THIS     USING FRARECD,R2                                                       
LAST     USING FRARECD,R3                                                       
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
FLST     MVC   IOKEY(L'FRAKEY),THIS.FRARECD                                     
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
NLST02   CLC   IOKEY(FRAKSYS-FRARECD),THIS.FRARECD                              
         BNE   EXITL                                                            
*                                                                               
         MVC   THIS.FRAKEY(FRAKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE A DICTIONARY EQUATE                             *         
*                                                                     *         
* RETURNS DICTIONARY NUMBER IN BOHALF1                                *         
***********************************************************************         
         SPACE 1                                                                
         USING FRARECD,R2                                                       
VDIC     NTR1  ,                                                                
         XC    BOHALF1,BOHALF1                                                  
         XC    BCFULL,BCFULL                                                    
         SPACE 1                                                                
         CLC   =C'GE#',FVIFLD      OVERRIDING WITH GEN SYSTEM EQUATE?           
         BE    VDIC04              (MUST BE SET EXPLICITLY)                     
         SPACE 1                                                                
         XR    RF,RF               SYSTEM FROM KEY                              
         IC    RF,FRAKSYS                                                       
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
         MVC   GMKSYS,FRAKSYS      SET UP THE SYSTEM                            
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
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   GMKEY(GMKLANG-GMKEY),IOKEYSAV                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,=AL4(XOGET+XOGENFIL+XIO1)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
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
*                                                                               
         DS    0D                                                               
DCLIST   DCDDL GE#MAINT,8,L                                                     
         DCDDL GE#LIS,8,L                                                       
         DCDDL GE#REP,8,L                                                       
         DCDDL GE#DLOAD,8,L                                                     
         DC    X'00'                                                            
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
AVDIC    DS    A                                                                
ADDIC    DS    A                                                                
AFRAEL   DS    A                                                                
AFVADDR  DS    A                                                                
DSLISTU  DS    0F                                                               
GU@MAINT DS    CL8                                                              
GU@LIST  DS    CL8                                                              
GU@REP   DS    CL8                                                              
GU@DLOAD DS    CL8                                                              
DSLISTL  DS    0F                                                               
GL@MAINT DS    CL8                                                              
GL@LIST  DS    CL8                                                              
GL@REP   DS    CL8                                                              
GL@DLOAD DS    CL8                                                              
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
**PAN#1  DC    CL21'003CTFIL13X  08/22/00'                                      
         END                                                                    
