*          DATA SET ACFIL12    AT LEVEL 004 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T62312C,*                                                                
         TITLE 'INTEREST RATE RECORD - OBJECT VERSION'                          
         SPACE 2                                                                
* YNGX 035 16AUG99 USE LABLES TO REPRESENT FIELD NUMBERS                        
* YNGX 036 10JAN00 DISPLAY * IN LIST SCREEN TO INDICATE ALL OFF ENTRIES         
         SPACE 2                                                                
FIL12    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL12**,R6,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING TWUSER,R6                                                        
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
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
         B     EXITOK                                                           
         EJECT                                                                  
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
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
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
KEY      LM    R0,R3,SVPARMS                                                    
         USING RATRECD,R2                                                       
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
KEYFRST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   MVC   RATKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   RATKTYP,RATKTYPQ    RATE TYPE CODE                               
         MVI   RATKSUB,RATKSUBQ    RATE SUBTYPE                                 
         MVC   RATKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   RATKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   RATKTYP,RATKTYPQ    RATE TYPE CODE                               
         MVI   RATKSUB,RATKSUBQ    RATE SUBTYPE                                 
         MVC   RATKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCREEN   LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRTABL                                                       
         B     ITER                                                             
*                                                                               
SCRTABL  DC    AL1(SKSET),AL1(0,0,0),AL4(SCKSET)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET KEY SCREEN CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCKSET   OI    GSINDSL1,GSIXKEY    SUPPRESS ASKING FOR KEY                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                  *         
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA RATION         *         
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                  *         
* P3 BYTES 1-3 HOLDS EQUATED RATION VERB                              *         
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                 *         
*                                                                     *         
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                          *         
*                                                                     *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              RATION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING RATRECD,R2                                                       
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
         USING RATRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(INT#FDATE),AL4(FDATE)  FILTER DATE                           
         DC    AL2(INT#DATE),AL4(INTDATE) DATE                                  
         DC    AL2(INT#RATE),AL4(INTRATE) RATE                                  
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
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDDIS)     DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFDVAL)     VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   XC    LASTFLDT,LASTFLDT                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    LASTFLDT,LASTFLDT                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DLDFDIS)    DISPLAY FILTER             
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DLDVAL)     VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
DLDFDIS  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
DLDFVAL  DS    0H                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FILTER DATE                                         *         
*                                                                               
****FILTER DATE IS NO LONGER SUPPORTED BUT YOU NEED AT LEAST ON UN-   *         
****PROTECTED KEY FIELD IN NFILE (I THINK) SO I AM USING THIS ONE.    *         
****IT IS A ONE BYTE UNPROTECTED FIELD WHICH IS SET TO LOW INTENSITY  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FDATE    LA    RF,FDATBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FDATBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALFDAT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILTER DATE FIELD (NOTHING IS VALID)                       *         
***********************************************************************         
         SPACE 1                                                                
VALFDAT  CLI   FVILEN,0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
*                                                                               
*&&DO                                                                           
         CLI   CSACT,A#DIS                                                      
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$NOFLT)                                           
         B     EXITL                                                            
*                                                                               
         ZIC   R1,FVXLEN                                                        
         CLC   FVIFLD(L'AC@ALL),AC@ALL                                          
         BE    EXITOK                                                           
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 VDATVAL,BODMCB,(0,FVIFLD),BOWORK1                                
         OC    0(4,R1),0(R1)                                                    
         BNZ   VFDAT10                                                          
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXITL                                                            
*                                                                               
VFDAT10  GOTO1 VDATCON,BODMCB,(0,BOWORK1),(2,SVFLTDT)                           
         B     EXITOK                                                           
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DATE                                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
INTDATE  LA    RF,DTETABL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
DTETABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDTE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDTE)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DATE FROM TSAR RECORD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISDTE   DS    0H                                                               
         XC    BCWORK,BCWORK    XC'D DATE FOR ASCENDING SORT SO                 
         MVC   BCWORK(L'TLKTDTE),TLKTDTE    NOW MUST XC BACK TO GET             
         XC    BCWORK(L'TLKTDTE),=XL2'FFFF' REAL DATE                           
*                                                                               
*        NI    BIT,X'FF'-NOSHOW                                                 
*        OC    SVFLTDT,SVFLTDT     ANY DATE FILTER?                             
*        BZ    DISDT10                                                          
*        CLC   SVFLTDT,LASTFLDT    SAME FILTER AS LAST TIME?                    
*        BE    *+14                                                             
*        MVC   LASTFLDT,SVFLTDT                                                 
*        OI    LSSCIND1,LSSCIFLT+LSSCIBLD                                       
*        CLC   BCWORK(L'SVFLTDT),SVFLTDT      CHECK FILTER                      
*        BNH   *+12                                                             
*        OI    BIT,NOSHOW          SET BIT TO CHECK FOR RATE                    
*        B     EXITOK                                                           
DISDT10  GOTO1 VDATCON,BOPARM,(2,BCWORK),(17,FVIFLD),0                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DATE AND SAVE ON TSAR RECORD                               *         
***********************************************************************         
         SPACE 1                                                                
VALDTE   DS    0H                                                               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   VALED05                                                          
         CLI   CSACT,A#ADD                                                      
         BE    EXITNO                                                           
         XC    TLKTDTE,TLKTDTE                                                  
         OI    LSLNIND1,LSLNIDEL                                                
         B     EXITOK                                                           
*                                                                               
VALED05  ZIC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,AC@DEL    ENTERED 'DELETE'?                            
         BNE   VALED10                                                          
         XC    TLKTDTE,TLKTDTE                                                  
         XC    TLKRATE,TLKRATE                                                  
         OI    LSLNIND1,LSLNIDEL   YES SO DELETE THIS DATE AND RATE             
         B     EXITOK                                                           
*                                                                               
VALED10  OC    TLKTDTE,TLKTDTE     IF DATE IS THERE MUST BE CHANGING            
         BNZ   VALED15             AN EXISTING 1 SO DON'T CHK MAX #             
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,MAXRATE          51 RATES MAX                                 
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXRC)                                           
         B     EXITL                                                            
*                                                                               
VALED15  XC    TLKTDTE,TLKTDTE                                                  
         GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXITL               INVAILD DATE                                 
         GOTO1 VDATCON,BODMCB,BODUB1,(2,TLKTDTE)                                
         XC    TLKTDTE,=XL2'FFFF'  TO SORT DESCENDING NOT ASCENDING             
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RATE                                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
INTRATE  LA    RF,RTETABL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
RTETABL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRTE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRTE)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RATE                                                                  
***********************************************************************         
         SPACE 1                                                                
DISRTE   TM    BIT,NOSHOW          IF THIS BIT ON THAN FILTERING OUT            
         BO    EXITOK              THIS ENTRY                                   
         CURED TLKRATE,(7,FVIFLD),4,DMCB=BODMCB                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RATE                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALRTE   CLI   FVILEN,0                                                         
         BNE   VRAT05                                                           
         CLI   CSACT,A#ADD                                                      
         BE    EXITNO                                                           
         OC    TLKTDTE,TLKTDTE     IF ENTERED AN EFFECTIVE DATE                 
         BNZ   EXITNO              THAN MUST ENTER A RATE ELSE                  
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
VRAT05   ZIC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,AC@DEL    ENTERED 'DELETE'?                            
         BNE   VRAT10                                                           
         XC    TLKTDTE,TLKTDTE                                                  
         XC    TLKRATE,TLKRATE                                                  
         OI    LSLNIND1,LSLNIDEL   YES SO DELETE THIS DATE AND RATE             
         B     EXITOK                                                           
*                                                                               
VRAT10   OC    TLKRATE,TLKRATE  IF RATE ALREADY THERE MUST BE CHANGING          
         BNZ   VRAT15           AN EXISTING RATE SO DON'T CHK FOR MAX           
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,MAXRATE          51 RATES MAX                                 
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXRC)                                           
         B     EXITL                                                            
VRAT15   SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(4,FVIFLD),(RF)                                  
         CLI   BODMCB,FF                                                        
         BE    EXITNOTN            NOT A NUMERIC                                
         L     RF,BODMCB+4                                                      
*        CVD   RF,BODUB1                                                        
*        CP    BODUB1,=P'999999'   MAXIMUM IS 9,999.99                          
*        BNH   *+14                TOO HIGH - ERROR                             
*        MVC   FVMSGNO,=AL2(AE$INAMT)  INVALID AMOUNT                           
*        B     EXITL                                                            
*                                                                               
         MVC   TLKRATE,BODMCB+5                                                 
         B     EXITOK                                                           
         POP   USING                                                            
         DROP  R2                                                               
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
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING RATRECD,R2                                                       
LAST     USING RATRECD,R3                                                       
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
LISTABL  DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR1)                           
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDDIR),AL1(0,0,1),AL4(UPDDIR1)                             
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   DS    0H                                                               
         OI    LSSTAT1,LSSBALL+LSSMULIN                                         
         OI    LSSTAT2,LSSNOSEQ+LSSADD                                          
         MVC   LSCOLLIN,=AL2(19)                                                
         MVC   LSLINROW,=AL2(4)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,RATRFST-RATRECD                                               
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
***********************************************************************         
         SPACE 1                                                                
FLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    RF,RATRFST-RATRECD(RF)      IT IS NOW.                           
         XR    RE,RE                                                            
*                                                                               
         USING RTEELD,RF                                                        
FML02    CLI   RTEEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   RTEEL,RTEELQ        '6E' ELEMENT?                                
         BNE   NML04               NO                                           
                                                                                
FML04    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
         SPACE 1                                                                
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML04                                                            
         LA    RF,RATRFST-RATRECD(RF)      IT IS NOW.                           
*                                                                               
         USING RTEELD,RF                                                        
NML02    CLI   RTEEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   RTEEL,RTEELQ        RTEEL?                                       
         BE    NML06               YES                                          
                                                                                
NML04    IC    RE,RTELN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML06    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
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
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         LH    RF,MNTDISP                                                       
         A     RF,AIOREC                                                        
         USING RTEELD,RF                                                        
*                                                                               
         MVC   TLKTDTE,RTEDATE                                                  
         XC    TLKTDTE,=XL2'FFFF'  SO SORTS DESCENDING NOT ASCENDING            
         MVC   TLKRATE,RTERATE                                                  
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('RTEELQ',AIOREC),0                
         B     EXITOK              DELETE ALL OLD RTEELS                        
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* UPDATE DIRECTORY FROM TSAR RECORD 1                                 *         
* P3 = A (DIRECTORY RECORD)                                           *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDDIR1  B     EXITOK                                                           
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDREC1  LM    R2,R3,SVPARMS3                                                   
         USING RATRECD,R2                                                       
         USING TLSTD,R3                                                         
*                                                                               
         USING RTEELD,BOELEM                                                    
         XC    RTEELD(RTELNQ),RTEELD                                            
         MVI   RTEEL,RTEELQ                                                     
         MVI   RTELN,RTELNQ                                                     
         XC    BCWORK,BCWORK     XC'D DATE FOR SORTING SO MUST                  
         MVC   BCWORK(L'TLKTDTE),TLKTDTE   XC BACK BEFORE WRITING               
         XC    BCWORK(L'TLKTDTE),=XL2'FFFF'  TO ELEMENT                         
         MVC   RTEDATE,BCWORK                                                   
         MVC   RTERATE,TLKRATE                                                  
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFF      EQU   X'FFFF'                                                          
MAXRATE  EQU   51                  MAXIMUM NUMBER OF RATES                      
DCLIST   DS    0D                                                               
         DCDDL AC#ALL,3,L                                                       
         DCDDL AC#DEL,6,L                                                       
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
MNTDISP  DS    H                                                                
SVFLTDT  DS    XL(L'RTEDATE)       SAVED DATE FILTER                            
LASTFLDT DS    XL(L'RTEDATE)       LAST DATE FILTER ENTERED                     
BIT      DS    XL1                                                              
NOSHOW   EQU   X'80'                                                            
*                                                                               
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
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
         SPACE 2                                                                
DSLISTU  DS    0F                                                               
AC@ALL   DS    CL3                                                              
AC@DEL   DS    CL6                                                              
OVERWRKN EQU   *-OVERWRKD                                                       
         SPACE 1                                                                
***********************************************************************         
* TSAR DSECT                                                                    
***********************************************************************         
         SPACE 1                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKTDTE  DS    XL(L'RTEDATE)                                                    
         ORG   TLUSER                                                           
TLKRATE  DS    XL(L'RTERATE)                                                    
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACFIL12   08/10/11'                                      
         END                                                                    
