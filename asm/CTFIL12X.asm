*          DATA SET CTFIL12X   AT LEVEL 004 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1312A                                                                  
         TITLE 'OBJECT VERSION OF RECORD RECORDS'                               
FIL12    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL12*,R6,R7,RR=RE                                           
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
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
*                                                                               
         L     RF,=A(VDIC)                                                      
         A     RF,BORELO                                                        
         ST    RF,AVDIC                                                         
         L     RF,=A(DDIC)                                                      
         A     RF,BORELO                                                        
         ST    RF,ADDIC                                                         
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
         USING FRRRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
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
KFKVAL   XC    FRRKEY,FRRKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FRRKMIN,FRRKMINQ    SET MINOR SYSTEM                             
         MVI   FRRKTYP,FRRKTYPQ    SET RECORD RECORD                            
         MVI   FRRKSUB,FF          SET RECORD SUB-COUNTRY                       
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
KFKFVAL  XC    FRRKEY,FRRKEY       INITIALIZE KEY OF FIELD RECORD               
         MVI   FRRKMIN,FRRKMINQ    SET MINOR SYSTEM                             
         MVI   FRRKTYP,FRRKTYPQ    SET FIELD RECORD                             
         MVI   FRRKSUB,FF          SET RECORD SUB-COUNTRY                       
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
         MVI   FRRKSUB,FF                                                       
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
         MVI   FRRKSUB,FF                                                       
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
         USING FRRRECD,R2                                                       
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
         USING FRRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRREL                                                        
         USING FRRELD,R3           R3=A(FRREL ON RECORD)                        
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
         DC    AL2(00003),AL4(RCODTA)    RECORD CODE                            
         DC    AL2(00150),AL4(CTRDTA)    COUNTRY FILTER                         
*        DC    AL2(00151),AL4(SCTDTA)    SUB-COUNTRY FILTER                     
         DC    AL2(00152),AL4(PHSDTA)    RECORD PHASE NUMBER                    
         DC    AL2(00153),AL4(TDRDTA)    RECORD NAME                            
         DC    AL2(00154),AL4(TDDDTA)    RECORD NAME DISPLAY UPPERCASE          
         DC    AL2(00155),AL4(TDLDTA)    RECORD NAME DISPLAY LOWERCASE          
         DC    AL2(00156),AL4(GNRDTA)    RECORD SYSTEM                          
         DC    AL2(00022),AL4(FDODTA)    RECORD IS DDS ONLY                     
         DC    AL2(00158),AL4(SELDTA)    RECORD IS SELECT ONLY                  
         DC    AL2(00159),AL4(UDTDTA)    RECORD IS UPDATED ON PFK PRESS         
         DC    AL2(00160),AL4(HERDTA)    RECORD IS HEIRARCHICAL                 
         DC    AL2(00161),AL4(GLBDTA)    RECORD IS GLOBAL TYPE                  
         DC    AL2(00162),AL4(DMNDTA)    HELP MESSAGE NUMBER                    
         DC    AL2(00163),AL4(DMSDTA)    HELP MESSAGE DISPLAY                   
         DC    AL2(00164),AL4(DIRDTA)    DIRECTORY ASSOCIATED                   
         DC    AL2(00165),AL4(USCDTA)    SCREEN CODE                            
         DC    AL2(00166),AL4(UKCDTA)    KEY CODE                               
         DC    AL2(00114),AL4(LENDTA)    VALIDATION LENGTH                      
         DC    AL2(00116),AL4(TSTDTA)    TEST PHASE                             
         DC    AL2(00150),AL4(CTRDTA)    COUNTRY CODE                           
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL12    CSECT                                                                  
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
DFDDIS   XC    AFRREL,AFRREL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRRELQ',FRRRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRREL                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    AFRREL,AFRREL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRRELQ',FRRRECD),0               
         CLI   12(R1),0                                                         
         BNE   DFVAL02             NO FRREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRREL                                                        
         B     EXITOK              ALREADY HAVE A FRREL ON RECORD               
*                                                                               
TEMP     USING FRRELD,BOELEM                                                    
DFVAL02  XC    TEMP.FRREL(FRRLNQ),TEMP.FRREL                                    
         MVI   TEMP.FRREL,FRRELQ                                                
         MVI   TEMP.FRRLN,FRRLNQ                                                
         MVC   TEMP.FRRCTRY,FRRKCTRY                                            
         MVC   TEMP.FRRSUB,FRRKSUB                                              
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),FRRRECD,TEMP.FRRELD,0              
         CLI   12(R1),0                                                         
         BE    *+6                 ERROR IN HELLO                               
         DC    H'0'                                                             
         L     RF,16(R1)           A(ELEMENT) HERE AFTER PUT                    
         ST    RF,AFRREL                                                        
         B     EXITOK              FRREL ADDED TO RECORD                        
         DROP  TEMP                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DFDFDIS  XC    AFRREL,AFRREL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRRELQ',FRRRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRREL                                                        
         B     EXITOK              ALREADY HAVE A FRREL ON RECORD               
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA FILTER                            *         
***********************************************************************         
         SPACE 1                                                                
DFDFVAL  XC    AFRREL,AFRREL                                                    
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('FRRELQ',FRRRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO FRREL FOUND                               
         L     RF,12(R1)                                                        
         ST    RF,AFRREL                                                        
         B     EXITOK              ALREADY HAVE A FRREL ON RECORD               
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
         ICM   R3,15,AFRREL                                                     
         BZ    DIE                                                              
         USING FRRELD,R3                                                        
X        USING FRRKSTAT,GSRECSTA                                                
         MVC   X.FRRKIND1,FRRINDS1 SAVE INDICATOR IN STATUS                     
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
         CLC   SYSLNUM,FRRKSYS     MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DSYS02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DSYS04   CURED FRRKSYS,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
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
VSYS06   MVC   FRRKSYS,SYSLNUM     SYSTEM NUMBER                                
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
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
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
VFSYS06  MVC   FRRKSYS,SYSLNUM     SYSTEM NUMBER INTO KEY                       
         MVC   FLTIFLD(L'SYSLNUM),SYSLNUM    - AND FILTER                       
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DO SYSTEM FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFTSYS  CLC   FRRKSYS,FLTIFLD                                                  
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
DISPRG   OC    FRRKPRG,FRRKPRG     WAS A PROGRAM ENTERED?                       
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRRKSYS                                                  
         BE    *+12                                                             
         BXLE  R1,RE,*-10                                                       
         B     DPRG08                                                           
         SPACE 1                                                                
         L     R1,SEPGMS                                                        
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING PGMLSTD,R1          FIND PGMLST ENTRY                            
DPRG02   CLC   PGMNUM,FRRKPRG                                                   
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
DPRG08   CURED FRRKPRG,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VALPRG   MVI   FRRKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
         CLI   FVILEN,0            TRYING TO SET TO SYSTEM LEVEL ONLY?          
         BE    EXITOK              YES                                          
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRRKSYS                                                  
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
VPRG08   MVC   FRRKPRG,PGMNUM      PROGRAM NUMBER                               
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
DFLTPRG  OC    FLTIFLD(L'FRRKPRG),FLTIFLD PROGRAM ENTERED?                      
         BZ    EXITOK              NO                                           
         SPACE 1                                                                
         L     R1,ASYSFAC                                                       
         L     R1,VSELIST-SYSFACD(R1)                                           
         LH    RE,0(R1)                                                         
         L     RF,2(R1)                                                         
         LA    R1,6(R1)                                                         
         USING SELISTD,R1          FIND SELIST ENTRY                            
         CLC   SEOVSYS,FRRKSYS                                                  
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
         CURED (RF),(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         DROP  R1                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PROGRAM FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTPRG  MVI   FRRKPRG,0           DEFAULT IS SYSTEM LEVEL ONLY                 
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
         CLC   SEOVSYS,FRRKSYS                                                  
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
VFPRG08  MVC   FRRKPRG,PGMNUM      PROGRAM NUMBER                               
         MVC   FLTIFLD(L'PGMNUM),PGMNUM                                         
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PROGRAM FIELD                                      *         
***********************************************************************         
DOFTPRG  CLC   FRRKPRG,FLTIFLD                                                  
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
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISRCO   GOTO1 VHEXOUT,BOPARM,FRRKREC,FVIFLD,L'FRRKREC,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALRCO   TM    FVIIND,FVIHEX                                                    
         BZ    EXITNV                                                           
         CLI   FVILEN,2            X'NN' IS ONLY VALID INPUT                    
         BNE   EXITNV                                                           
         GOTO1 VHEXIN,BOPARM,FVIFLD,FRRKREC,2                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CREATE A RECORD HEADLINE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
HEDRCO   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTRCO  OC    FLTIFLD(L'FRRKREC),FLTIFLD RECORD FIELD?                         
         BZ    EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,FLTIFLD,FVIFLD,L'FRRKREC,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTRCO  CLI   FVILEN,0                                                         
         BE    EXITOK              NO RECORD SET                                
         OC    FRRKPRG,FRRKPRG     TRYING TO OVERRIDE RECORD - MUST             
         BZ    EXITNV              INPUT THE PROGRAM                            
*                                                                               
         CLI   FVILEN,2            X'NN' IS ONLY VALID INPUT                    
         BNE   EXITNV                                                           
         GOTO1 VHEXIN,BOPARM,FVIFLD,FRRKREC,2                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR RECORD                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTRCO  CLC   FRRKREC,FLTIFLD                                                  
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
DISCTR   MVC   BOBYTE1,FRRKCTRY    COUNTRY CODE IS 1`S COMP IN KEY              
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
         B     EXITOK              FIX TILL FUCKUPS FIXED                       
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
         MVC   FRRKCTRY,BCEFFS     SET DEFAULT TO NATIVE COUNTRY                
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
VCTR06   MVC   FRRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         XC    FRRKCTRY,BCEFFS     1`S COMPLIMENT IT                            
                                                                                
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
         MVI   FRRKCTRY,FF         SET DEFAULT TO NATIVE COUNTRY                
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
VFCTR06  MVC   FRRKCTRY,CTRYCODE   MOVE IN COUNTRY CODE                         
         MVC   FLTIFLD(L'CTRYCODE),CTRYCODE                                     
         XC    FRRKCTRY,BCEFFS     1`S COMPLIMENT CODE IN KEY                   
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DO COUNTRY FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFTCTR  MVC   BOBYTE1,FVIFLD                                                   
         XC    BOBYTE1,BCEFFS                                                   
         CLC   FRRKCTRY,BOBYTE1                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PHASE NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PHSDTA   LA    RF,PHSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PHSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPHS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPHS)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PHASE NUMBER                                              *         
***********************************************************************         
         SPACE 1                                                                
DISPHS   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         GOTO1 VHEXOUT,BOPARM,FRRPHASE,FVIFLD,L'FRRPHASE,0                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PHASE NUMBER                                             *         
***********************************************************************         
         SPACE 1                                                                
VALPHS   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         TM    FVIIND,FVIHEX                                                    
         BZ    EXITNV                                                           
         CLI   FVILEN,2            X'NN' IS ONLY VALID INPUT                    
         BNE   EXITNV                                                           
         GOTO1 VHEXIN,BOPARM,FVIFLD,FRRPHASE,2                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR RECORD DICTIONARY ENTRY                             *         
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
         DC    AL1(DHED),AL1(0,0,0),AL4(HEDTDR)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTTDR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTTDR)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD DICTIONARY ENTRY                                   *         
***********************************************************************         
         SPACE 1                                                                
DISTDR   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   BOHALF1,FRRDICT     RECORD NAME IN BOHALF1 FOR DDIC              
         GOTO1 ADDIC                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A RECORD NAME FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALTDR   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         GOTO1 AVDIC               VALIDATE RECORD NAME                         
         BNE   EXITL               VDIC SETS OWN ERROR MESSAGES                 
*                                                                               
         MVC   FRRDICT,BOHALF1     RECORD NAME IN BOHALF1 FROM VDIC             
X        USING FRRKSTAT,GSRECSTA                                                
         MVC   X.FRRKDICT,FRRDICT  SAVE IT IN STATUS TOO                        
         DROP  X                                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CREATE A TAG DICTIONARY REFERENCE HEADLINE FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
HEDTDR   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TAG DICTIONARY REFERENCE FILTER FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTTDR  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TAG DICTIONARY REFERENCE FILTER FIELD                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTTDR  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
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
* DISPLAY RECORD NAME FROM DICTIONARY EQUATE                          *         
***********************************************************************         
         SPACE 1                                                                
DISTDD   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVI   FVIFLD,CT#ESCL      LEFT ALIGNED                                 
         MVI   FVIFLD+3,8          LENGTH=8                                     
         MVC   FVIFLD+1(L'FRRDICT),FRRDICT                                      
         ICM   RF,15,=C'SU  '                                                   
         ICM   RF,2,FRRKSYS                                                     
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
* DISPLAY RECORD NAME FROM DICTIONARY EQUATE - LOWER CASE             *         
***********************************************************************         
         SPACE 1                                                                
DISTDL   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVI   FVIFLD,CT#ESCL      LEFT ALIGNED                                 
         MVI   FVIFLD+3,8          LENGTH=8                                     
         MVC   FVIFLD+1(L'FRRDICT),FRRDICT                                      
         ICM   RF,15,=C'SL  '                                                   
         ICM   RF,2,FRRKSYS                                                     
         GOTO1 VDICTAT,BOPARM,(RF),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR RECORD IS DDS ONLY RECORD                           *         
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
* DISPLAY RECORD IS DDS ONLY                                          *         
***********************************************************************         
         SPACE 1                                                                
DISFDO   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         TM    FRRINDS1,FRR1DDS                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD DICTIONARY ENTRY                                    *         
***********************************************************************         
         SPACE 1                                                                
VALFDO   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         NI    FRRINDS1,FF-(FRR1DDS)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FL@NO                                                  
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
VFDO02   EX    RE,*+8              YES                                          
         BE    VFDO04                                                           
         CLC   FVIFLD(0),FL@YES                                                 
         EX    RE,*+8                                                           
         BE    VFDO04                                                           
         CLC   FVIFLD(0),FU@YES                                                 
         B     EXITNV              NOTHING ELSE IS VALID                        
*                                                                               
VFDO04   OI    FRRINDS1,FRR1DDS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR RECORD USES SCREEN CODES                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
USCDTA   LA    RF,USCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
USCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUSC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUSC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD USES SCREEN CODES                                    *         
***********************************************************************         
         SPACE 1                                                                
DISUSC   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         TM    FRRINDS1,FRR1USC                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD USES SCREEN CODES                                   *         
***********************************************************************         
         SPACE 1                                                                
VALUSC   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         NI    FRRINDS1,FF-(FRR1USC)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FL@NO                                                  
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
VUSC02   EX    RE,*+8              YES                                          
         BE    VUSC04                                                           
         CLC   FVIFLD(0),FL@YES                                                 
         EX    RE,*+8                                                           
         BE    VUSC04                                                           
         CLC   FVIFLD(0),FU@YES                                                 
         B     EXITNV              NOTHING ELSE IS VALID                        
*                                                                               
VUSC04   OI    FRRINDS1,FRR1USC                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD USES KEY CODES                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
UKCDTA   LA    RF,UKCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
UKCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUKC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUKC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD USES SCREEN CODES                                    *         
***********************************************************************         
         SPACE 1                                                                
DISUKC   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         TM    FRRINDS1,FRR1UKC                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD USES SCREEN CODES                                   *         
***********************************************************************         
         SPACE 1                                                                
VALUKC   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         NI    FRRINDS1,FF-(FRR1UKC)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FL@NO                                                  
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
VUKC02   EX    RE,*+8              YES                                          
         BE    VUKC04                                                           
         CLC   FVIFLD(0),FL@YES                                                 
         EX    RE,*+8                                                           
         BE    VUKC04                                                           
         CLC   FVIFLD(0),FU@YES                                                 
         B     EXITNV              NOTHING ELSE IS VALID                        
*                                                                               
VUKC04   OI    FRRINDS1,FRR1UKC                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD IS SELECT ONLY TYPE                          *         
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
* DISPLAY RECORD IS SELECT ONLY TYPE                                  *         
***********************************************************************         
         SPACE 1                                                                
DISSEL   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         TM    FRRINDS1,FRR1SEL                                                 
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD IS SELECT ONLY TYPE                                 *         
***********************************************************************         
         SPACE 1                                                                
VALSEL   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         NI    FRRINDS1,FF-(FRR1SEL)                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FL@NO                                                  
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VSEL02                                                           
         CLC   FVIFLD(0),FL@YES                                                 
         EX    RE,*+8                                                           
         BE    VSEL02                                                           
         CLC   FVIFLD(0),FU@YES                                                 
         B     EXITNV              NOTHING ELSE IS VALID                        
*                                                                               
VSEL02   OI    FRRINDS1,FRR1SEL                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD IS UPDATIVE TYPE                             *         
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
DISUDT   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         TM    FRRINDS1,FRR1UPDT                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD IS UPDATIVE TYPE                                    *         
***********************************************************************         
         SPACE 1                                                                
VALUDT   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         NI    FRRINDS1,FF-(FRR1UPDT)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FL@NO                                                  
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VUDT02                                                           
         CLC   FVIFLD(0),FL@YES                                                 
         EX    RE,*+8                                                           
         BE    VUDT02                                                           
         CLC   FVIFLD(0),FU@YES                                                 
         B     EXITNV              NOTHING ELSE IS VALID                        
*                                                                               
VUDT02   OI    FRRINDS1,FRR1UPDT                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD IS HEIRARCHICAL                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
HERDTA   LA    RF,HERTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HERTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHER)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHER)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD IS HEIRARCHICAL                                      *         
***********************************************************************         
         SPACE 1                                                                
DISHER   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         TM    FRRINDS1,FRR1HEIR                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD IS HEIRARCHICAL                                     *         
***********************************************************************         
         SPACE 1                                                                
VALHER   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         NI    FRRINDS1,FF-(FRR1HEIR)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FL@NO                                                  
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VHER02                                                           
         CLC   FVIFLD(0),FL@YES                                                 
         EX    RE,*+8                                                           
         BE    VHER02                                                           
         CLC   FVIFLD(0),FU@YES                                                 
         B     EXITNV                                                           
*                                                                               
VHER02   OI    FRRINDS1,FRR1HEIR                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD SYSTEM                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
GNRDTA   LA    RF,GNRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
GNRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISGNR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGNR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD SYSTEM                                               *         
***********************************************************************         
         SPACE 1                                                                
DISGNR   LTR   R3,R3                                                            
         BZ    EXITOK                                                           
         L     RE,ASYSLST                                                       
         LA    RE,6(RE)                                                         
         USING SYSLSTD,RE          RE=A(SYSTEM LIST)                            
         SPACE 1                                                                
DGNR02   CLI   0(RE),0             TEST E-O-T                                   
         BE    DGNR04                                                           
         CLC   SYSLNUM,FRRSYS      MATCH ON SYSTEM NUMBER                       
         BE    *+12                                                             
         LA    RE,SYSLLEN(RE)      NO - BUMP TO NEXT TABLE ENTRY                
         B     DGNR02                                                           
         SPACE 1                                                                
         MVC   FVIFLD(L'SYSLNAME),SYSLNAME                                      
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 1                                                                
DGNR04   CURED FRRSYS,(3,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD SYSTEM                                              *         
***********************************************************************         
         SPACE 1                                                                
VALGNR   LTR   R3,R3                                                            
         BZ    DIE                                                              
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         L     R4,ASYSLST                                                       
         USING SYSLSTD,R4                                                       
         LA    R4,6(R4)            R4=A(SYSTEM LIST)                            
         SPACE 1                                                                
VGNR02   CLI   0(R4),0             TEST E-O-T                                   
         BE    EXITNV                                                           
         CLI   SYSLNUM,1           IGNORE SERVICE SYSTEM                        
         BE    VGNR04                                                           
         EX    RF,*+8              MATCH INPUT TO TABLE                         
         BE    VGNR06                                                           
         CLC   SYSLNAME(0),FVIFLD                                               
         SPACE 1                                                                
VGNR04   LA    R4,SYSLLEN(R4)      NO - BUMP TO NEXT TABLE ENTRY                
         B     VGNR02                                                           
         SPACE 1                                                                
VGNR06   MVC   FRRSYS,SYSLNUM      SYSTEM NUMBER                                
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD IS GLOBAL TYPE                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
GLBDTA   LA    RF,GLBTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
GLBTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISGLB)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGLB)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD IS GLOBAL TYPE                                       *         
***********************************************************************         
         SPACE 1                                                                
DISGLB   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@NO),FL@NO                                            
         TM    FRRINDS1,FRR1GLOB                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FL@YES),FL@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD IS GLOBAL TYPE                                      *         
***********************************************************************         
         SPACE 1                                                                
VALGLB   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         NI    FRRINDS1,FF-(FRR1GLOB)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FL@NO                                                  
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),FU@NO                                                  
*                                                                               
         EX    RE,*+8              YES                                          
         BE    VGLB02                                                           
         CLC   FVIFLD(0),FL@YES                                                 
         EX    RE,*+8                                                           
         BE    VGLB02                                                           
         CLC   FVIFLD(0),FU@YES                                                 
         B     EXITNV                                                           
*                                                                               
VGLB02   OI    FRRINDS1,FRR1GLOB                                                
         B     EXITOK                                                           
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
         OC    FRRHELP,FRRHELP                                                  
         BZ    EXITOK                                                           
         CURED FRRHELP,(5,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD HELP MESSAGE NUMBER                                 *         
***********************************************************************         
         SPACE 1                                                                
VALDMN   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
*                                                                               
         XC    FRRHELP,FRRHELP                                                  
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
         MVC   GMKSYS,FRRKSYS      SET UP THE SYSTEM                            
         STCM  RF,3,GMKMSG         SET MESSAGE NUMBER                           
         STCM  RF,3,FRRHELP        SAVE HELP NUMBER IN ELEMENT                  
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
DISDMS   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         OC    FRRHELP,FRRHELP                                                  
         BZ    EXITOK                                                           
*                                                                               
         PUSH  USING                                                            
         USING GMSGD,IOKEY                                                      
         XC    GMKEY,GMKEY                                                      
         MVI   GMKREC,GMKRECQ                                                   
         MVI   GMKTYP,GMKTSCR      SET SCREEN RECORD                            
         MVC   GMKSYS,FRRKSYS      SET UP THE SYSTEM                            
         MVC   GMKMSG,FRRHELP      SET MESSAGE NUMBER                           
         MVC   GMKLANG,CULANG                                                   
         XI    GMKLANG,X'FF'                                                    
*                                                                               
         L     R1,=AL4(XOHIGH+XOGENDIR+XIO1)                                    
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
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DIRECTORY ASSOCIATED WITH FILE                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DIRDTA   LA    RF,DIRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DIRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDIR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDIR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DIRECTORY ASSOCIATED WITH FILE                              *         
***********************************************************************         
         SPACE 1                                                                
DISDIR   LTR   R3,R3               FRREL?                                       
         BZ    EXITOK                                                           
         OC    FRRDIR,FRRDIR                                                    
         BZ    EXITOK                                                           
*                                                                               
         L     RF,AXFILTAB         FILES HERE                                   
         USING NFITABD,RF                                                       
DDIR02   CLI   0(RF),EOT                                                        
         BE    EXITOK                                                           
         CLC   FRRSYS,NFIOSE                                                    
         BNE   DDIR04                                                           
         CLC   FRRDIR,NFINUM                                                    
         BNE   DDIR04                                                           
         MVC   FVIFLD(L'NFINAME),NFINAME                                        
         B     EXITOK                                                           
*                                                                               
DDIR04   LA    RF,NFITABL(RF)                                                   
         B     DDIR02                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DIRECTORY ASSOCIATED WITH FILE                             *         
***********************************************************************         
         SPACE 1                                                                
VALDIR   LTR   R3,R3               FRREL?                                       
         BZ    DIE                                                              
         XC    FRRDIR,FRRDIR                                                    
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         L     RF,AXFILTAB         FILES HERE                                   
         USING NFITABD,RF                                                       
VDIR02   CLI   0(RF),EOT                                                        
         BE    EXITNV                                                           
         CLC   FRRSYS,NFIOSE       MATCH SYSTEM                                 
         BNE   VDIR04                                                           
         EX    RE,*+8                                                           
         BE    VDIR06                                                           
         CLC   FVIFLD(0),NFINAME   MATCH NAME                                   
*                                                                               
VDIR04   LA    RF,NFITABL(RF)                                                   
         B     VDIR02                                                           
*                                                                               
VDIR06   MVC   FRRDIR,NFINUM                                                    
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
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
         NI    BOBYTE1,FF-(X'80')                                               
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
         NI    GSRECSTA,FF-(FRRKLVAL)                                           
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
         SPACE 2                                                                
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
DISTST   OC    FRRKTEST,FRRKTEST                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'FRRKTEST),FRRKTEST                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE TEST VERSION                                               *         
***********************************************************************         
         SPACE 1                                                                
VALTST   XC    FRRKTEST,FRRKTEST                                                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'A'                                                      
         BNE   *+12                                                             
         MVI   FRRKTEST,C'A'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'B'                                                      
         BNE   *+12                                                             
         MVI   FRRKTEST,C'B'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'C'                                                      
         BNE   *+12                                                             
         MVI   FRRKTEST,C'C'                                                    
         B     EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'X'                                                      
         BNE   *+12                                                             
         MVI   FRRKTEST,C'X'                                                    
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
         PUSH  USING                                                            
         USING FSRRECD,GSRECKEY                                                 
NTROUT   DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TEMP     USING FRRRECD,GSRECKEY                                                 
         USING FSRRECD,RF                                                       
NTRIN    CLI   PSREC,R#SCRN                                                     
         BNE   EXITOK                                                           
         LA    RF,SDATA+2                                                       
         XC    TEMP.FRRKEY,TEMP.FRRKEY                                          
         MVI   TEMP.FRRKMIN,FRRKMINQ                                            
         MVI   TEMP.FRRKTYP,FRRKTYPQ                                            
         MVC   TEMP.FRRKSYS,FSRKSYS                                             
         MVC   TEMP.FRRKPRG,FSRKPRG                                             
         MVC   TEMP.FRRKREC,FSRKREC                                             
         B     EXITOK                                                           
         POP   USING                                                            
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
THIS     USING FRRRECD,R2                                                       
LAST     USING FRRRECD,R3                                                       
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
FLST     MVC   IOKEY(L'FRRKEY),THIS.FRRRECD                                     
         ICM   R1,15,=AL4(XIO11+XOHIGH+XOGENDIR)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH ERROR                              
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
NLST02   CLC   IOKEY(FRRKSYS-FRRRECD),THIS.FRRRECD                              
         BNE   EXITL                                                            
*                                                                               
         MVC   THIS.FRRKEY(FRRKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS,LAST                                                        
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO VALIDATE A DICTIONARY EQUATE                             *         
*                                                                     *         
* RETURNS DICTIONARY NUMBER IN BOHALF1                                *         
***********************************************************************         
         SPACE 1                                                                
         USING FRRRECD,R2                                                       
VDIC     NTR1  ,                                                                
         XC    BOHALF1,BOHALF1                                                  
         XC    BCFULL,BCFULL                                                    
         SPACE 1                                                                
         CLC   =C'GE#',FVIFLD      OVERRIDING WITH GEN SYSTEM EQUATE?           
         BE    VDIC04              (MUST BE SET EXPLICITLY)                     
         SPACE 1                                                                
         XR    RF,RF               SYSTEM FROM KEY                              
         IC    RF,FRRKSYS                                                       
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
         L     R1,=AL4(XOHIGH+XOGENDIR+XIO1)                                    
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
         MVC   GMKSYS,FRRKSYS      SET UP THE SYSTEM                            
         XR    RF,RF                                                            
         ICM   RF,3,BOHALF1        GET DICTIONARY NUMBER                        
         TM    BOHALF1,X'40'       IS IT A GENERAL DD ENTRY?                    
         BZ    *+12                                                             
         SH    RF,=Y(GE#GEN)                                                    
         MVI   GMKSYS,GENSYS       MOVE IN GENERAL SYSTEM NUMBER                
*                                                                               
         STCM  RF,3,GMKMSG         SAVE DICTIONARY NUMBER                       
         L     R1,=AL4(XOHIGH+XOGENDIR+XIO1)                                    
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
         DS    0D                                                               
DCLIST   DCDDL CT#YES,4,L                                                       
         DCDDL GE#NO,4,L                                                        
         DC    X'00'                                                            
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
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
AFRREL   DS    A                                                                
AFVADDR  DS    A                                                                
ESCLEN   DS    XL1                                                              
ESCCHAR  DS    XL2                                                              
*                                                                               
DSLISTU  DS    0H                                                               
FU@YES   DS    CL4                                                              
FU@NO    DS    CL4                                                              
*                                                                               
DSLISTL  DS    0H                                                               
FL@YES   DS    CL4                                                              
FL@NO    DS    CL4                                                              
*                                                                               
MYSCAN   DS    3CL(SCBLKLQ)                                                     
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
**PAN#1  DC    CL21'004CTFIL12X  08/22/00'                                      
         END                                                                    
