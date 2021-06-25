*          DATA SET ACFIL27    AT LEVEL 003 AS OF 06/05/08                      
*&&      SET   NOP=N                                                            
*PHASE T62327A,*                                                                
         TITLE 'SCHEME CODE RECORD OBJECT VERSION'                              
         SPACE 2                                                                
FIL27    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL27**,R7,RR=RE                                              
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
FLTFXL   MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTFXE   MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTFXH   MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTFXX   MVI   SVPARMS,DFLTX       EXIT DEFINATELY NOT VALID                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         B     EXITOK                                                           
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
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
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
         USING WCORECD,R2                                                       
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
KFKVAL   MVC   WCOKEY,BCSPACES     INITIALIZE KEY OF WORK CODE RECORD           
         MVI   WCOKTYP,WCOKTYPQ    WORK-CODE RECORD TYPE                        
         MVC   WCOKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   WCOKEY,BCSPACES     INITIALIZE KEY OF WORK CODE RECORD           
         MVI   WCOKTYP,WCOKTYPQ    WORK-CODE RECORD TYPE                        
         MVC   WCOKCPY,CUABIN      CONNECTED ID                                 
         MVI   WCOKUNT,X'41'                                                    
         MVI   WCOKLDG,X'41'                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                       *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(RECORD)                                                        *         
* P4 HOLDS SUB-ACTION VERB                                            *         
***********************************************************************         
         SPACE 1                                                                
RECORD   LM    R0,R3,SVPARMS                                                    
         USING WCORECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
RECFRST  L     R1,SVPARMS4                                                      
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RFTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RCPY),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
***********************************************************************         
         SPACE 1                                                                
RFADD    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',WCORECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    RFADD02                                                          
         GOTO1 AADDRST,WCORECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL               SOMETHING WRONG                              
         B     EXITOK                                                           
*                                                                               
RFADD02  L     RF,12(R1)                                                        
         USING RSTELD,RF                                                        
         MVC   RSTBDATE,BCTODAYP   SET TO TODAY                                 
         MVC   RSTTDATE,BCTODAYP                                                
         B     EXITOK                                                           
         DROP  RF                                                               
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
         USING WCORECD,R2                                                       
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
         USING WCORECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
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
KNOWTAB  DC    AL2(WC#UL),AL4(ULCDTA)       UNIT AND LEDGER                     
         DC    AL2(WC#WCODE),AL4(WCDTA)     SCHEME CODE                         
         DC    AL2(WC#LDGNM),AL4(LGNDTA)    LEGER NAME                          
         DC    AL2(WC#WCDES),AL4(WCDES)     SCHEME NAME                         
*        DC    AL2(WC#WCSTA),AL4(WCSTA)     STATUS (REMOVED FOR NOW BUT         
*                                           DON'T DELETE THE CODE)              
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL27    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   XC    SAVWCOEL,SAVWCOEL                                                
         GOTO1 AGETEL,BOPARM,('WCOELQ',WCORECD),0                               
         BNE   EXITOK                                                           
         MVC   SAVWCOEL,BOELEM      SAVE LEDGER ELEMENT                         
         B     EXITOK                                                           
         PUSH  USING                                                            
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    SAVWCOEL,SAVWCOEL                                                
         GOTO1 AGETEL,BOPARM,('WCOELQ',WCORECD),0                               
         BNE   EXITOK                                                           
         MVC   SAVWCOEL,BOELEM      SAVE LEDGER ELEMENT                         
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
         USING WCOELD,R4                                                        
DLDVAL   GOTO1 AREPEL,BOPARM,('WCOELQ',WCORECD),0,SAVWCOEL                      
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR UNIT/LEDGER CODE WCOKUNT+WCOKLDG                    *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ULCDTA   LA    RF,ULCTBL                                                        
         B     ITER                                                             
*                                                                               
ULCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISULC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALULC)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTULC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALULC)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTULC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISULC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISULC   MVC   FVIFLD(L'WCOKUNT+L'WCOKLDG),WCOKUNT                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LEDGER FIELD                                             *         
* ONLY ACCEPTING UNIT 3                                                         
***********************************************************************         
         SPACE 1                                                                
VALULC   CLI   FVIFLD,C'3'             UNIT 3 ONLY                              
         BE    VULC02                                                           
         CLC   =C'SJ',FVIFLD                                                    
         BNE   EXITNV                                                           
         MVC   FVMSGNO,=AL2(AE$UPROD)  USE =PROD FOR SJ SCHEME CODES            
         B     EXITL                                                            
*                                                                               
VULC02   MVC   WCOKUNT(L'WCOKUNT+L'WCOKLDG),FVIFLD                              
         MVC   FLTIFLD(L'WCOKUNT+L'WCOKLDG),FVIFLD  MOVE TO FILTER TOO          
         USING ACTRECD,RF                                                       
         LA    RF,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),WCOKUNT                             
         GOTO1 AGETACT,0           READ ACCOUNT AND TEST SECURITY               
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTULC  MVC   FVIFLD(L'WCOKUNT+L'WCOKLDG),FLTIFLD                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LEDGER                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTULC  CLI   WCOKLDG,C' '        IS THERE A LEDGER TO COMPARE ON?             
         BNH   FLTFXX              NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLI   WCOKUNT,C'3'        FILTER OUT ALL SCHEME CODES THAT ARE         
         BNE   FLTFXX              NOT UNIT 3                                   
         CLC   WCOKUNT(L'WCOKUNT+L'WCOKLDG),FLTIFLD                             
         BL    FLTFXL                                                           
         BE    FLTFXE                                                           
         BH    FLTFXH                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR WORK CODE WCOKWRK                                   *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
WCDTA    LA    RF,WCTBL                                                         
         B     ITER                                                             
*                                                                               
WCTBL    DC    AL1(DDIS),AL1(0,0,0),AL4(DISWC)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWC)                                  
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTWC)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALWC)                                 
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTWC)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISWC)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORK CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISWC    MVC   FVIFLD(L'WCOKWRK),WCOKWRK                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A WORK CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALWC    MVC   SVSCHEME,BCSPACES                                                
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,WCOKWRK,FVIFLD                                                
         OC    WCOKWRK,BCSPACES                                                 
         EXMVC R1,FLTIFLD,FVIFLD  MOVE TO FILTER FIELD TOO                      
         EXMVC R1,SVSCHEME,FVIFLD      SAVE SCHEME CODE                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORK CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTWC   MVC   FVIFLD(L'WCOKWRK),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR WORK CODE                                          *         
***********************************************************************         
         SPACE 1                                                                
DOFTWC   CLC   WCOKWRK,BCSPACES    IS THERE A WORK CODE TO COMPARE ON?          
         BNH   FLTFXX              NO - WE DON`T WANT IT THEN                   
         CLC   WCOKWRK,FLTIFLD                                                  
         BL    FLTFXL                                                           
         BE    FLTFXE                                                           
         BH    FLTFXH                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A LEDGER NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LGNDTA   LA    RF,LGNTBL                                                        
         B     ITER                                                             
*                                                                               
LGNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLGN)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLGN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER NAME FIELD FROM THE KEY                            *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGRECD,RF                                                       
DISLGN   LA    RF,IOKEY                                                         
         MVC   IOKEY,BCSPACES       READ THE LEDGER RECORD FOR THIS ID          
         MVC   LDGKCPY,CUABIN       COMPANY                                     
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),2(R2)                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LEDGER NAME FIELD FROM THE FILTER                         *         
***********************************************************************         
         SPACE 1                                                                
         USING LDGRECD,RF                                                       
DFLGN    CLI   FLTIFLD,C' '                                                     
         BNH   EXITOK                                                           
         LA    RF,IOKEY                                                         
         MVC   IOKEY,BCSPACES       READ THE LEDGER RECORD FOR THIS ID          
         MVC   LDGKCPY,CUABIN       COMPANY                                     
         MVC   LDGKUNT(L'LDGKUNT+L'LDGKLDG),2(R2)                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R1,AIO1                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR WORK CODE DESCRIPTION                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
WCDES    LA    RF,WCDTBL                                                        
         B     ITER                                                             
*                                                                               
WCDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISWCN)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWCN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL WORK CODE DESCRIPTION                                 *         
***********************************************************************         
         SPACE 1                                                                
DISWCN   GOTO1 AGETEL,BOPARM,('WCOELQ',WCORECD),0                               
         BNE   EXITOK                                                           
         PUSH  USING                                                            
         USING WCOELD,BOELEM                                                    
         MVC   FVIFLD(L'WCODESC),WCODESC                                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WORK CODE DESCRIPTION                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING WCOELD,RE                                                        
VALWCN   LA    RE,SAVWCOEL         BUILD WORK CODE ELEM IN SAVED                
         MVI   WCOEL,WCOELQ        STORAGE AREA FIRST                           
         MVI   WCOLN,WCOLNQ                                                     
         MVC   WCOCODE,SVSCHEME                                                 
         CLI   FVILEN,0                                                         
         BE    EXITNO              ERROR - NO DESCRIPTION                       
         MVC   WCODESC,BCSPACES                                                 
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,WCODESC,FVIFLD                                                
         OC    WCODESC,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  RE                                                               
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* DATA OBJECT FOR WORK CODE STATUS LINE                               *         
*                                                                     *         
* IMPORTANT - REMOVED THE STATUS FOR NOW SINCE NOBODY SEEMED TO KNOW  *         
* WHAT THY WERE USED FOR.                                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
WCSTA    LA    RF,WCSTBL                                                        
         B     ITER                                                             
*                                                                               
WCSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WORK CODE STATUS                                            *         
* BUILD BLOCK IN AIO5 FOR UNSCAN.  UNSCAN USES FIXED 20 BYTE FIELDS.  *         
* 10 BYTES FOR RHS AND 10 BYTES FOR LHS UNLESS YOU SPECIFY OTHERWISE  *         
***********************************************************************         
         SPACE 1                                                                
         USING WCOELD,R4                                                        
DISSTA   L     R5,AIO5                                                          
         LR    RE,R5               CLEAR IO5                                    
         LA    RF,IOAREALN                                                      
         XCEF                                                                   
         MVI   BCBYTE1,0           KEEP TRACK OF # OF ENTRIES                   
         LA    R4,SAVWCOEL                                                      
         TM    WCOSTAT,WCOSSIND    DETAIL=NO                                    
         BZ    DSSTA10                                                          
         MVC   0(L'AC@DTL,R5),AC@DTL                                            
         MVC   10(L'AC@NO,R5),AC@NO                                             
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DSSTA10  TM    WCOSTAT,WCOSSDP1     P1DETAIL=NO                                 
         BZ    DSSTA20                                                          
         MVC   0(L'AC@P1DTL,R5),AC@P1DTL                                        
         MVC   10(L'AC@NO,R5),AC@NO                                             
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DSSTA20  TM    WCOSTAT,WCOSEXPT     TRAVEL=YES                                  
         BZ    DSSTA30                                                          
         MVC   0(L'AC@TRVL,R5),AC@TRVL                                          
         MVC   10(L'AC@YES,R5),AC@YES                                           
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DSSTA30  TM    WCOSTAT,WCOSEXPE     ENTERTAIN=YES                               
         BZ    DSSTA40                                                          
         MVC   0(L'AC@ENTN,R5),AC@ENTN                                          
         MVC   10(L'AC@YES,R5),AC@YES                                           
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DSSTA40  TM    WCOSTAT,WCOSHCOE     ORIGINAL=HOURS                              
         BZ    DSSTA50                                                          
         MVC   0(L'AC@ORGL,R5),AC@ORGL                                          
         MVC   10(L'AC@HOURS,R5),AC@HOURS                                       
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DSSTA50  TM    WCOSTAT,WCOSDP14     ESTPRINT=NO                                 
         BZ    DSSTA60                                                          
         MVC   0(L'AC@ESPRT,R5),AC@ESPRT                                        
         MVC   10(L'AC@NO,R5),AC@NO                                             
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DSSTA60  CLI   WCORMED,0           ANY MEDIA FOR RETAIL SCHEME?                 
         BZ    DSSTA70                                                          
         MVC   0(L'AC@MED,R5),AC@MED                                            
         MVC   10(L'WCORMED,R5),WCORMED                                         
         LA    R5,20(R5)           BUMP TO NEXT SLOT FOR UNSCAN                 
         ZIC   RF,BCBYTE1                                                       
         LA    RF,1(RF)                                                         
         STC   RF,BCBYTE1                                                       
*                                                                               
DSSTA70  ZIC   RF,BCBYTE1          # OF ENTRIES                                 
         GOTO1 VUNSCAN,BOPARM,((RF),AIO5),FVIHDR                                
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE WORK CODE STATUS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING SCANBLKD,R3                                                      
VALSTA   CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         LA    R3,BLOCK                                                         
         LR    RE,R3               CLEAR SCANBLK                                
         LA    RF,SCANMAX*SCBLKLQ                                               
         XCEF                                                                   
*                                                                               
         USING WCOELD,R4                                                        
         LA    R4,SAVWCOEL                                                      
         MVI   WCOSTAT,0           CLEAR VALUES                                 
         MVI   WCORMED,0           CLEAR MEDIA FOR RETAIL SCHEMES               
         GOTO1 VSCANNER,BOPARM,FVIHDR,BLOCK                                     
         CLI   BOPARM+4,0                                                       
         BE    EXITNV                                                           
         ZIC   RF,BOPARM+4         RF=# OF SCANNER LINES                        
         STC   RF,SVSCLINE                                                      
         LA    R3,BLOCK                                                         
         USING SCANBLKD,R3                                                      
VSTA05   CLC   SC1STFLD(L'AC@DTL),AC@DTL     DETAIL=NO                          
         BNE   VSTA10                                                           
         CLC   SC2NDFLD(L'AC@NO),AC@NO                                          
         BNE   EXITNV                                                           
         OI    WCOSTAT,WCOSSIND                                                 
         B     VSTANX                                                           
*                                                                               
VSTA10   CLC   SC1STFLD(L'AC@P1DTL),AC@P1DTL   P1DETAIL=NO                      
         BNE   VSTA20                                                           
         CLC   SC2NDFLD(L'AC@NO),AC@NO                                          
         BNE   EXITNV                                                           
         OI    WCOSTAT,WCOSSDP1                                                 
         B     VSTANX                                                           
*                                                                               
VSTA20   CLC   SC1STFLD(L'AC@TRVL),AC@TRVL    TRAVEL=YES                        
         BNE   VSTA30                                                           
         CLC   SC2NDFLD(L'AC@YES),AC@YES                                        
         BNE   EXITNV                                                           
         OI    WCOSTAT,WCOSEXPT                                                 
         B     VSTANX                                                           
*                                                                               
VSTA30   CLC   SC1STFLD(L'AC@ENTN),AC@ENTN    ENTERTAIN=YES                     
         BNE   VSTA40                                                           
         CLC   SC2NDFLD(L'AC@YES),AC@YES                                        
         BNE   EXITNV                                                           
         OI    WCOSTAT,WCOSEXPE                                                 
         B     VSTANX                                                           
*                                                                               
VSTA40   CLC   SC1STFLD(L'AC@ORGL),AC@ORGL ORIGINAL=HOURS                       
         BNE   VSTA50                                                           
         CLC   SC2NDFLD(L'AC@HOURS),AC@HOURS                                    
         BNE   EXITNV                                                           
         OI    WCOSTAT,WCOSHCOE                                                 
         B     VSTANX                                                           
*                                                                               
VSTA50   CLC   SC1STFLD(L'AC@ESPRT),AC@ESPRT ESTPRINT=NO                        
         BNE   VSTA60                                                           
         CLC   SC2NDFLD(L'AC@NO),AC@NO                                          
         BNE   EXITNV                                                           
         OI    WCOSTAT,WCOSDP14                                                 
         B     VSTANX                                                           
*                                                                               
VSTA60   DS    0H                                                               
*        CLC   SC1STFLD(L'AC@XFR),AC@XFR   TRANSFER=YES                         
*        BNE   VSTA70                                                           
*        CLC   SC2NDFLD(L'AC@XFR),AC@XFR                                        
*        BNE   EXITNV                                                           
*        OI    WCOSTAT,WCOSHCOE                                                 
*        B     VSTANX                                                           
*                                                                               
VSAT70   CLC   SC1STFLD(L'AC@MED),AC@MED      MEDIA=                            
         BNE   EXITNV                                                           
         ZIC   R1,SC2NDLEN                                                      
         CHI   R1,1                                                             
         BNE   EXITNV                                                           
         MVC   WCORMED,SC2NDFLD                                                 
         B     VSTANX                                                           
*                                                                               
VSTANX   LA    R3,SCBLKLQ(R3)  BUMP TO NEXT SCANNER LINE                        
         ZIC   R1,SVSCLINE                                                      
         AHI   R1,-1                                                            
         STC   R1,SVSCLINE                                                      
         BNZ   VSTA05                                                           
         B     EXITOK                                                           
         EJECT                                                                  
*&&                                                                             
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
THIS     USING WCORECD,R2                                                       
LAST     USING WCORECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'WCOKEY),THIS.WCORECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         BL    EXITL               HARD I/O ERROR                               
         TM    IOERR,IOEDEL        IF ERROR IS THAT REC IS DELETED              
         BO    NLST                READ NEXT INSTEAD OF EXIT                    
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
TEMP     USING WCORECD,IOKEY                                                    
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   TEMP.WCOKCPY(L'WCOKCPY),THIS.WCOKCPY                             
         BNE   EXITL               CHANGE COMPANY                               
         CLC   TEMP.WCOKUNT(L'WCOKUNT+L'WCOKLDG),BCSPACES                       
         BNH   NLST                MUST HAVE AN UNIT AND LEDGER                 
         CLI   TEMP.WCOKUNT,C'3'                                                
         BNE   NLST                FILTER OUT ALL UL'S OTHER THAN UNT 3         
         CLC   TEMP.WCOKWRK,BCSPACES                                            
         BNH   NLST                MUST HAVE AN WORK CODE                       
*                                                                               
         MVC   THIS.WCOKEY(ACCKLEN),IOKEY                                       
         B     EXITOK                                                           
         DROP  TEMP                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SCANMAX  EQU   10                  MAX NUMBER OF SCANNER ENTRIES                
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#NO,L'AC@NO,L         NO                                       
         DCDDL AC#YES,L'AC@YES,L       YES                                      
         DCDDL AC#DTL,L'AC@DTL,L       DETAIL                                   
         DCDDL AC#P1DTL,L'AC@P1DTL,L   P1DETAIL                                 
         DCDDL AC#TRVL,L'AC@TRVL,L     TRAVEL                                   
         DCDDL AC#ENTN,L'AC@ENTN,L     ENTERTAIN                                
         DCDDL AC#ORGL,L'AC@ORGL,L     ORIGINAL                                 
         DCDDL AC#HOURS,L'AC@HOURS,L   HOURS                                    
         DCDDL AC#ESPRT,L'AC@ESPRT,L   ESTPRINT                                 
         DCDDL AC#XFR,L'AC@XFR,L       TRANSFER                                 
         DCDDL AC#MED,L'AC@MED,L       MEDIA                                    
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*        FALANG                                                                 
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
*        ACFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
SAVWCOEL DS    CL(WCOLNQ+1)        SAVE WORK CODE ELEMENT                       
SVSCLINE DS    XL1                 SAVED # OF SCANNER LINES                     
SVSCHEME DS    CL(L'WCOKWRK)       SAVED SCHEME CODE                            
BLOCK    DS    (SCANMAX)CL(SCBLKLQ)                                             
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
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
DSLISTU  DS    0D                  DICTIONARY EQUATES USED                      
AC@NO    DS    CL2                 NO                                           
AC@YES   DS    CL3                 YES                                          
AC@DTL   DS    CL6                 DETAIL                                       
AC@P1DTL DS    CL8                 P1DETAIL                                     
AC@TRVL  DS    CL6                 TRAVEL                                       
AC@ENTN  DS    CL9                 ENTERTAIN                                    
AC@ORGL  DS    CL8                 ORIGINAL                                     
AC@HOURS DS    CL5                 HOURS                                        
AC@ESPRT DS    CL8                 ESTPRINT                                     
AC@XFR   DS    CL8                 TRANSFER                                     
AC@MED   DS    CL5                 MEDIA                                        
OVERWRKN EQU   *-OVERWRKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'003ACFIL27   06/05/08'                                      
         END                                                                    
