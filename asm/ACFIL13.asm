*          DATA SET ACFIL13    AT LEVEL 007 AS OF 03/23/10                      
*&&      SET   NOP=N                                                            
*PHASE T62313B,*                                                                
         TITLE 'BANK RECORD - OBJECT VERSION'                                   
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* AIO3 IS USED BY DDGETBANK AS THE BANK BLOCK SO DON'T USE IT                   
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 2                                                                
FIL13    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL13**,R7,RR=RE                                              
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         L     RA,ATWA                                                          
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
         LH    R5,=Y(TWUSER-TWAD)                                               
         AR    R5,RA                                                            
         USING TWUSER,R5                                                        
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
ERRNOCHG MVC   FVMSGNO,=AL2(AE$CHANA)                                           
         B     EXITL                                                            
ERRACBRF MVC   FVMSGNO,=AL2(AE$ACBRF)                                           
         B     EXITL                                                            
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
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     L     R2,=A(DCLIST)                                                    
         A     R2,BORELO                                                        
         GOTO1 VDICTAT,BOPARM,C'LU  ',(R2),DSLIST                               
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
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCREEN)                                
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
         USING BNKRECD,R2                                                       
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
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   MVC   BNKKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   BNKTYP,BNKTYPQ      X'2D'                                        
         MVI   BNKSUB,BNKSUBQ      X'08'                                        
         MVC   BNKCPY,CUABIN       CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   BNKKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   BNKTYP,BNKTYPQ      X'2D'                                        
         MVI   BNKSUB,BNKSUBQ      X'08'                                        
         MVC   BNKCPY,CUABIN       CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
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
         USING BNKRECD,R2                                                       
         LA    RF,SCRTABL                                                       
         B     ITER                                                             
*                                                                               
SCRTABL  DC    AL1(SSET),AL1(0,0,0),AL4(SCRMSET)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET DATA SCREEN CODE BASED ON EDI TYPE                              *         
***********************************************************************         
         SPACE 1                                                                
SCRMSET  MVI   GSSMCODE,C'A'       EFT GETS SCREEN CODE A                       
         CLI   SVEDITY,TYPEEFT                                                  
         BE    EXITOK                                                           
         MVI   GSSMCODE,C'B'       POSPAY/ACR GET SCREEN CODE B                 
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING BNKRECD,R2                                                       
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
         USING BNKRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(BNK#BNCDE),AL4(BNCDE)         BANK CODE                      
         DC    AL2(BNK#BNNME),AL4(BNNME)         BANK NAME                      
         DC    AL2(BNK#BRCDE),AL4(BRCDE)         BRANCH CODE                    
         DC    AL2(BNK#BRNME),AL4(BRNME)         BRANCH NAME                    
         DC    AL2(BNK#EDI),AL4(EDITY)           EDI TYPE                       
         DC    AL2(BNK#ROUTN),AL4(RTNN)          ROUTING NUMBER                 
         DC    AL2(BNK#CSNME),AL4(CSNM)          CLIENT SERVICE NAME            
         DC    AL2(BNK#CSPH1),AL4(CSPH)          CS PHONE                       
         DC    AL2(BNK#CSEXT),AL4(CSEXT)         CS PHONE EXTENSION             
         DC    AL2(BNK#CSEM),AL4(CSEM)           CLIENT SERVICE EMAIL           
         DC    AL2(BNK#TCNME),AL4(TCNM)          TECHNICAL CONTACT NAME         
         DC    AL2(BNK#TCPH1),AL4(TCPH)          TECH CONTACT PHONE             
         DC    AL2(BNK#TCEXT),AL4(TCEXT)         TC PHONE EXTENSION             
         DC    AL2(BNK#TCEM),AL4(TCEM)           TECH CONTACT EMAIL             
         DC    AL2(BNK#ADUID),AL4(ADID)          ADVANTIS USER ID (USE)         
         DC    AL2(BNK#ADACC),AL4(ADACC)         ADVANTIS ACCOUNT (ACC)         
         DC    AL2(BNK#MSGCL),AL4(MSGC)          MESSAGE CLASS (CLA)            
         DC    AL2(BNK#CHRG),AL4(CHRG)           CHARGE (CHA)                   
         DC    AL2(BNK#TRNT),AL4(TRNT)           TRANSMISSION TYPE              
         DC    AL2(BNK#TRNK),AL4(TRNK)           TRANSMISSION KEY               
         DC    AL2(BNK#DSNM),AL4(DSNM)           DATASET NAME                   
         DC    AL2(BNK#FMKY),AL4(FMKY)           FORMAT KEY                     
         DC    AL2(BNK#CSMSG),AL4(CSMSG)         CLIENT SERV MSG FIELD          
         DC    AL2(BNK#TCMSG),AL4(TCMSG)         TECH CONTACT MSG FIELD         
         DC    AL2(BNK#FNAME),AL4(FNDF)          FILENAME DEFINITION            
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL13    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DFDDIS   L     R3,AIO3             KEEP R3 POINTED TO THE BANK BLOCK            
         BRAS  RE,GETBANK                                                       
*                                                                               
         GOTO1 AGETEL,BOPARM,('BNKELQ',BNKRECD),0                               
         BNE   EXITOK                                                           
         XC    SVBNKEL,SVBNKEL                                                  
         MVC   SVBNKEL,BOELEM      SV BANK ELEMENT                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATION OF A DATA OBJECT                          *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   L     R3,AIO3             KEEP R3 POINTED TO THE BANK BLOCK            
         BRAS  RE,GETBANK                                                       
         XC    SVBNKEL,SVBNKEL                                                  
         GOTO1 AGETEL,BOPARM,('BNKELQ',BNKRECD),0                               
         BE    DFDV20                                                           
T        USING BNKELD,BOELEM                                                    
         MVI   T.BNKEL,BNKELQ      IF NO BNKEL, BUILD A BLANK ONE               
         MVI   T.BNKLN,BNK1LNQ     FOR NOW                                      
         GOTO1 AREPEL,BOPARM,('BNKELQ',BNKRECD),0,BOELEM                        
DFDV20   MVC   SVBNKEL,BOELEM      SAVE BANK ELEMENT                            
         B     EXITOK                                                           
         DROP  T                                                                
         LTORG                                                                  
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL                                                        
         B     ITER                                                             
*                                                                               
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   DS    0H                                                               
         LA    R3,SVBNKEL                                                       
T        USING BNKELD,R3                                                        
         MVI   T.BNKLN,BNK1LNQ       MAKE SURE LONGEST LENGTH                   
         GOTO1 AREPEL,BOPARM,('BNKELQ',BNKRECD),0,SVBNKEL                       
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR BANK CODE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BNCDE    LA    RF,BNCDTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BNCDTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBNC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBNC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BANK CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISBNC   MVC   FVIFLD(L'BNKBANK),BNKBANK                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A BANK CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALBNC   CLI   FVILEN,3                                                         
         BNE   EXITNV                                                           
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BNKBANK,FVIFLD                                                
         EXMVC R1,SVBANK,FVIFLD                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BANK CODE NAME                                      *         
* READ ONLY FIELD                                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BNNME    LA    RF,BNNMTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BNNMTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BANK CODE NAME                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DISBNM   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVC   FVIFLD(L'BNKNME),BNKNME                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BRANCH CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BRCDE    LA    RF,BRCDTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BRCDTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBRC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBRC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BRANCH CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISBRC   MVC   FVIFLD(L'BNKBRNCH),BNKBRNCH                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A BRANCH CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
VALBRC   ZIC   R1,FVXLEN                                                        
         EXMVC R1,BNKBRNCH,FVIFLD                                               
         EXMVC R1,SVBRCH,FVIFLD                                                 
         BRAS  RE,GETBANK                                                       
         TM    FLAG,FLGBNF                                                      
         BO    ERRACBRF            ADD BANK RECD ON CONTROL FRST                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BRANCH CODE NAME                                    *         
* READ ONLY FIELD                                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
BRNME    LA    RF,BRNMTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BRNMTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBRNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A BRANCH CODE NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DISBRNM  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVC   FVIFLD(L'BRNNME),BRNNME                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EDI TYPE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EDITY    LA    RF,EDITB            TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
EDITB    DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE EDI TYPE                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TYPTABD,RE                                                       
DISEDT   LA    RE,EDITYTAB                                                      
DISEDT10 CLI   0(RE),0                                                          
         BE    EXITOK                                                           
         CLC   TYPFLD#,BNKETYP     MATCH ON ELEMENT#                            
         BE    *+12                                                             
         AHI   RE,TYPLNQ                                                        
         B     DISEDT10                                                         
*                                                                               
         MVC   FVIFLD(6),TYPFLD                                                 
         B     EXITOK                                                           
         DROP  RE                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE EDI TYPE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALEDT   SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
*                                                                               
         USING TYPTABD,RE                                                       
         LA    RE,EDITYTAB                                                      
VALED10  CLI   0(RE),EOF                                                        
         BE    EXITNV                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),TYPFLD                                                 
         BE    *+12                                                             
         AHI   RE,TYPLNQ                                                        
         B     VALED10                                                          
*                                                                               
         MVC   SVEDITY,TYPFLD#                                                  
         MVC   BNKETYP,TYPFLD#                                                  
*                                                                               
VEDTX    B     EXITOK                                                           
         DROP  RE                                                               
*                                                                               
EDITYTAB DS    0CL11                                                            
         DC    CL10'POSPAY',AL1(BNKPOS)                                         
         DC    CL10'ACR',AL1(BNKACR)                                            
         DC    CL10'EFT',AL1(BNKEFT)                                            
         DC    CL10'820',AL1(BNK820)                                            
         DC    AL1(EOF)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ROUTING NUMBER                                      *         
* READ ONLY FIELD                                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
RTNN     LA    RF,RTNNTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
RTNNTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRTN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE BRANCH ROUTING NUMBER                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DISRTN   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVC   FVIFLD(L'BANRNO),BANRNO                                          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR THE PROTECTED MESSAGE FIELD FOR THE CLIENT SERVICE  *         
* SECTION.  IF ONE OF THE ENTRIES IN THE CLIENT SERVICE CONTACT INFO  *         
* USES THE DEFAULT DISPLAY '(DEFAULT CONTACT INFORMATION)'            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CSMSG    LA    RF,CSMSGTB          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CSMSGTB  DS    0C                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISCSMSG)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE MESSAGE '(DEFAULT CONTACT INFORMATION)'                 *         
* WAIT TO PROCESS ALL THE CONTACT INFORMATION BEFORE FILLING IN THE   *         
* FIELD SO SAVE THE FIELD FOR NOW.                                    *         
***********************************************************************         
         SPACE 1                                                                
DISCSMSG MVC   ACSMFLD,FVADDR                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CLIENT SERVICE NAME                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CSNM     LA    RF,CSNMTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CSNMTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCNM)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFCNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT CLIENT SERVICE NAME FROM BANK BLOCK              *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFCNM   L     R3,AIO3                                                          
         MVC   FVIFLD(L'BANCCNME),BANCCNME  DISPLAY FROM CFILE/BANK REC         
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE CLIENT SERVICE NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISCNM   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKCSB1,X'FF'-CSNMDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISCNM10 CLI   0(R4),0                                                          
         BE    DISCNM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISCNM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISCNM20                                                         
         CLI   FFTITYP,FFTCLI                                                   
         BE    DISCNM30                                                         
DISCNM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISCNM10                                                         
*                                                                               
DISCNM30 CLC   FFTNAME,BCSPACES                                                 
         BNH   DISCNM40            NO NAME SO GET IT FROM CFILE                 
         MVC   FVIFLD(L'FFTNAME),FFTNAME    DISPLAY FROM ACC/BANK REC           
         B     DISCNMX                                                          
*                                                                               
DISCNM40 MVC   FVIFLD(L'BANCCNME),BANCCNME  DISPLAY FROM CFILE/BANK REC         
         OI    BNKCSB1,CSNMDEF     SET BIT THAT CS NAME IS THE DEFAULT          
*                                                                               
DISCNMX  BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE CLIENT SERVICE NAME                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALCNM   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKCSB1,X'FF'-CSNMDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALCNM10 CLI   0(R4),0                                                          
         BE    VALCNM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALCNM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALCNM20                                                         
         CLI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO?                 
         BE    VALCNM30                                                         
VALCNM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALCNM10                                                         
*                                                                               
VALCNM30 OI    ELEMFLAG,ELREP                                                   
         MVC   BOWORK1(L'FFTPHONE),FFTPHONE    SAVE PHONE                       
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE                                                            
         BZ    VALCNM35                                                         
         STC   RE,BOWORK2          STORE LENGTH IN 1ST BYTE OF BOWORK1          
         BCTR  RE,0                SO WE KNOW WHAT LENGTH TO PUT BACK           
         EXMVC RE,BOWORK2+1,FFTEMAIL                                            
VALCNM35 MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALCNM40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKCSB1,CSNMDEF     SET BIT THAT CS NAME IS THE DEFAULT          
         B     VALCNM50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANCCNME   COMPARE NAME ENTERED W/ DEFAULT             
         BNE   *+12                                                             
         OI    BNKCSB1,CSNMDEF     SET BIT THAT CS NAME IS THE DEFAULT          
         B     VALCNM50                                                         
         CLI   FVIFLD,C' '         CANNOT BEGIN WITH A SPACE                    
         BE    EXITNV                                                           
         OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALCNM50 TM    ELEMFLAG,ELNEW+ELREP     ELEMENT NEEDED?                         
         BZ    VCNMX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO                  
         TM    BNKCSB1,CSNMDEF     IS CS NAME THE DEFAULT?                      
         BO    VALCNM60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FFTNAME,FVIFLD   MOVE NAME TO ELEMENT                         
VALCNM60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALCNM65            NO THAN DON'T RESTORE FIELDS                 
         MVC   FFTPHONE,BOWORK1    RESTORE PHONE                                
         ZIC   R1,BOWORK2          GET LENGTH OF EMAIL                          
         LTR   R1,R1               IS THERE AN EMAIL?                           
         BZ    VALCNM65            NO                                           
         ZIC   RE,FFTLN                                                         
         AR    RE,R1                                                            
         STC   RE,FFTLN            UPDATE LENGTH OF ELEM W/ EMAIL               
         ZIC   RE,FFTDLEN                                                       
         AR    RE,R1                                                            
         STC   RE,FFTDLEN          UPDATE DATA LENGTH W/ EMAIL                  
         BCTR  R1,0                                                             
         EXMVC R1,FFTEMAIL,BOWORK2+1   RESTORE EMAIL                            
VALCNM65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VCNMX    BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT SERVICE PHONE NUMBER (AREA CODE INCLUDED)    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CSPH     LA    RF,CSPHTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CSPHTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPH)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFCPH)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT CLIENT SERVICE PHONE FROM BANK BLOCK             *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFCPH   L     R3,AIO3                                                          
         MVC   FVIFLD(L'BANCPNO),BANCPNO    DISPLAY FROM CFILE/BANK REC         
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE CLIENT SERVICE PHONE NUMBER (AREA CODE INCLUDED)        *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISCPH   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKCSB1,X'FF'-CSPHDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISCPH10 CLI   0(R4),0                                                          
         BE    DISCPH40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISCPH20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISCPH20                                                         
         CLI   FFTITYP,FFTCLI                                                   
         BE    DISCPH30                                                         
DISCPH20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISCPH10                                                         
*                                                                               
DISCPH30 CLC   FFTPHONE(10),BCSPACES    ANY PHONE ?                             
         BNH   DISCPH40                                                         
         MVC   FVIFLD(10),FFTPHONE      YES MOVE IN FROM ACC/BANK REC           
         B     DISCPHX                                                          
DISCPH40 MVC   FVIFLD(10),BANCPNO       NO MOVE IN FROM CFILE/BANK REC          
         OI    BNKCSB1,CSPHDEF                                                  
*                                                                               
DISCPHX  BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE CLIENT SERVICE PHONE                                   *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALCPH   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKCSB1,X'FF'-CSPHDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALCPH10 CLI   0(R4),0                                                          
         BE    VALCPH40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALCPH20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALCPH20                                                         
         CLI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO?                 
         BE    VALCPH30                                                         
VALCPH20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALCPH10                                                         
*                                                                               
VALCPH30 OI    ELEMFLAG,ELREP                                                   
         MVC   BOWORK1(L'FFTNAME),FFTNAME    SAVE NAME                          
         MVC   SVPHONE(5),FFTPHONE+10        SAVE EXTENSION                     
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE               ANY EMAIL?                                   
         BZ    VALCPH35                                                         
         STC   RE,BOWORK2          STORE LENGTH IN 1ST BYTE OF BOWORK1          
         BCTR  RE,0                SO WE KNOW WHAT LENGTH TO PUT BACK           
         EXMVC RE,BOWORK2+1,FFTEMAIL                                            
VALCPH35 MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALCPH40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKCSB1,CSPHDEF     SET BIT THAT CS PHONE IS THE DEFAULT         
         B     VALCPH50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANCPNO    COMPARE PHONE ENTERED W/ DEFAULT            
         BNE   *+12                                                             
         OI    BNKCSB1,CSPHDEF     SET BIT THAT CS PHONE IS THE DEFAULT         
         B     VALCPH50                                                         
         TM    BNKCSB1,CSNMDEF     IS THE CLI SERV NAME THE DEFAULT?            
         BO    ERRNOCHG            YES SO CAN'T CHANGE EMAIL HERE               
         OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALCPH50 TM    ELEMFLAG,ELNEW+ELREP        ELEMENT NEEDED?                      
         BZ    VCPHX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO                  
         TM    BNKCSB1,CSPHDEF     IS CS PHONE THE DEFAULT?                     
         BO    VALCPH60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FFTPHONE,FVIFLD  MOVE PHONE TO ELEMENT                        
VALCPH60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALCPH65                                                         
         MVC   FFTNAME,BOWORK1     RESTORE NAME                                 
         MVC   FFTPHONE+10(5),SVPHONE  RESTORE EXTENSION                        
         ZIC   R1,BOWORK2          GET LENGTH OF EMAIL                          
         LTR   R1,R1               ANY EMAIL TO RESTORE?                        
         BZ    VALCPH65                                                         
         ZIC   RE,FFTLN                                                         
         AR    RE,R1                                                            
         STC   RE,FFTLN            UPDATE LENGTH OF ELEM W/ EMAIL               
         ZIC   RE,FFTDLEN                                                       
         AR    RE,R1                                                            
         STC   RE,FFTDLEN          UPDATE DATA LENGTH W/ EMAIL                  
         BCTR  R1,0                                                             
         EXMVC R1,FFTEMAIL,BOWORK2+1   RESTORE EMAIL                            
VALCPH65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VCPHX    BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT SERVICE PHONE EXTENSION                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CSEXT    LA    RF,CSEXTTB          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CSEXTTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCEXT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCEXT)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFCEXT)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT CLIENT SERVICE EXTENSION FROM BANK BLOCK         *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFCEXT  L     R3,AIO3                                                          
         MVC   FVIFLD(5),BANCPNO+10     DISPLAY FROM CFILE/BANK REC             
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE CLIENT SERVICE PHONE EXTENSION                          *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISCEXT  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKCSB1,X'FF'-CSEXDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISCEX10 CLI   0(R4),0                                                          
         BE    DISCEX40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISCEX20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISCEX20                                                         
         CLI   FFTITYP,FFTCLI                                                   
         BE    DISCEX30                                                         
DISCEX20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISCEX10                                                         
*                                                                               
DISCEX30 CLC   FFTPHONE+10(5),BCSPACES     ANY EXTENSION?                       
         BNH   DISCEX40                                                         
         MVC   FVIFLD(5),FFTPHONE+10    YES MOVE IN FROM ACC/BANK REC           
         B     DISCEXX                                                          
DISCEX40 MVC   FVIFLD(5),BANCPNO+10     NO MOVE IN FROM CFILE/BANK REC          
         OI    BNKCSB1,CSEXDEF                                                  
*                                                                               
DISCEXX  BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE CLIENT SERVICE PHONE EXTENSION                         *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALCEXT  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKCSB1,X'FF'-CSEXDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALCEX10 CLI   0(R4),0                                                          
         BE    VALCEX40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALCEX20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALCEX20                                                         
         CLI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO?                 
         BE    VALCEX30                                                         
VALCEX20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALCEX10                                                         
*                                                                               
VALCEX30 OI    ELEMFLAG,ELREP      REPLACE ELEMENT                              
         MVC   BOWORK1(L'FFTNAME),FFTNAME    SAVE NAME                          
         MVC   SVPHONE(10),FFTPHONE    SAVE PHONE (W/O EXTENSION)               
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE               ANY EMAIL?                                   
         BZ    VALCEX35            NO                                           
         STC   RE,BOWORK2          STORE LENGTH IN 1ST BYTE OF BOWORK1          
         BCTR  RE,0                SO WE KNOW WHAT LENGTH TO PUT BACK           
         EXMVC RE,BOWORK2+1,FFTEMAIL                                            
VALCEX35 MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALCEX40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKCSB1,CSEXDEF     SET BIT THAT PHONE EX IS THE DEFAULT         
         B     VALCEX50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANCPNO+10 COMPARE PHONE EXT ENTERED W/ DEFLT          
         BNE   *+12                                                             
         OI    BNKCSB1,CSEXDEF     SET BIT THAT PHONE EX IS THE DEFAULT         
         B     VALCEX50                                                         
         TM    BNKCSB1,CSNMDEF     IS THE CLI SERV NAME THE DEFAULT?            
         BO    ERRNOCHG            YES SO CAN'T CHANGE EMAIL HERE               
         OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALCEX50 TM    ELEMFLAG,ELNEW+ELREP      ELEMENT NEEDED?                        
         BZ    VCEXX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO                  
         TM    BNKCSB1,CSEXDEF     IS CS EXTENSION THE DEFAULT?                 
         BO    VALCEX60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FFTPHONE+10,FVIFLD  MOVE PHONE EXT TO ELEMENT                 
VALCEX60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALCEX65            NO THAN NOTHING TO RESTORE                   
         MVC   FFTNAME,BOWORK1     RESTORE NAME                                 
         MVC   FFTPHONE(10),SVPHONE  RESTORE PHONE (W/O EXTENSION)              
         ZIC   R1,BOWORK2          GET LENGTH OF EMAIL                          
         LTR   R1,R1               ANY EMAIL?                                   
         BZ    VALCEX65            NO                                           
         ZIC   RE,FFTLN                                                         
         AR    RE,R1                                                            
         STC   RE,FFTLN            UPDATE LENGTH OF ELEM W/ EMAIL               
         ZIC   RE,FFTDLEN                                                       
         AR    RE,R1                                                            
         STC   RE,FFTDLEN          UPDATE DATA LENGTH W/ EMAIL                  
         BCTR  R1,0                                                             
         EXMVC R1,FFTEMAIL,BOWORK2+1   RESTORE EMAIL                            
VALCEX65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VCEXX    BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CLIENT SERVICE EMAIL                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CSEM     LA    RF,CSEMTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CSEMTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCSEM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCSEM)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFCEM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT CLIENT SERVICE EMAIL FROM BANK BLOCK             *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFCEM   L     R3,AIO3                                                          
         MVC   FVIFLD(L'BANCEML),BANCEML   DISPLAY FROM CFILE/BANK REC          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE CLIENT SERVICE EMAIL                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISCSEM  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKCSB1,X'FF'-CSEMDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISCEM10 CLI   0(R4),0                                                          
         BE    DISCEM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISCEM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISCEM20                                                         
         CLI   FFTITYP,FFTCLI                                                   
         BE    DISCEM30                                                         
DISCEM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISCEM10                                                         
*                                                                               
DISCEM30 ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE               ANY EMAIL?                                   
         BZ    DISCEM40            NO                                           
         CLC   FFTEMAIL,BCSPACES     ANY EMAIL?                                 
         BNH   DISCEM40                                                         
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         GET LENGTH OF EMAIL ADDRESS                  
         BCTR  RE,0                SUBTRACT 1 FOR EXMVC                         
         MVC   FVIFLD(0),FFTEMAIL                                               
         EX    RE,*-6                                                           
         B     DISCEMX                                                          
DISCEM40 MVC   FVIFLD(L'BANCEML),BANCEML NO MOVE IN FROM CFILE/BANK REC         
         OI    BNKCSB1,CSEMDEF                                                  
*                                                                               
DISCEMX  BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE CLIENT SERVICE EMAIL ADDRESS                           *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALCSEM  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKCSB1,X'FF'-CSEMDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALCEM10 CLI   0(R4),0                                                          
         BE    VALCEM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALCEM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALCEM20                                                         
         CLI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO?                 
         BE    VALCEM30                                                         
VALCEM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALCEM10                                                         
*                                                                               
VALCEM30 OI    ELEMFLAG,ELREP                                                   
         MVC   BOWORK1(L'FFTNAME),FFTNAME    SAVE NAME                          
         MVC   BOWORK2(L'FFTPHONE),FFTPHONE    SAVE PHONE                       
         MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALCEM40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKCSB1,CSEMDEF     SET BIT THAT EMAIL IS THE DEFAULT            
         B     VALCEM50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANCEML    COMPARE PHONE EXT ENTERED W/ DEFLT          
         BNE   *+12                                                             
         OI    BNKCSB1,CSEMDEF     SET BIT THAT EMAIL IS THE DEFAULT            
         B     VALCEM50                                                         
         TM    BNKCSB1,CSNMDEF     IS THE CLI SERV NAME THE DEFAULT?            
         BO    ERRNOCHG            YES SO CAN'T CHANGE EMAIL HERE               
         GOTO1 VEMAIL,BOPARM,FVIHDR,0      VALIDATE E-MAIL ADDRESS              
         CLI   0(R1),0                                                          
         BE    VALCEM45                                                         
         SR    RE,RE                                                            
         ICM   RE,7,BOPARM+1                                                    
         MVC   FVXTRA,0(RE)                                                     
         B     EXITNV                                                           
VALCEM45 OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALCEM50 TM    ELEMFLAG,ELNEW+ELREP      ANY ELEMENT NEEDED?                    
         BZ    VCEXX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTCLI      CLIENT SERVICE CONTACT INFO                  
         TM    BNKCSB1,CSEMDEF     IS CS EMAIL THE DEFAULT?                     
         BO    VALCEM60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   RE,FVILEN           GET INPUT LENGTH                             
         ZIC   R1,FFTLN                                                         
         AR    R1,RE                                                            
         STC   R1,FFTLN            UPDATE ELEMENT LENGTH                        
         ZIC   R1,FFTDLEN                                                       
         AR    R1,RE                                                            
         STC   R1,FFTDLEN          UPDATE DATA LENGTH                           
         BCTR  RE,0                                                             
         MVC   FFTEMAIL(0),FVIFLD  EMAIL ADDRESS                                
         EX    RE,*-6                                                           
VALCEM60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALCEM65            NO THAN NOTHING TO RESTORE                   
         MVC   FFTNAME,BOWORK1     RESTORE NAME                                 
         MVC   FFTPHONE,BOWORK2    RESTORE PHONE                                
VALCEM65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VCEMX    BRAS  RE,DCSMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR THE PROTECTED MESSAGE FIELD FOR THE TECH CONTACT    *         
* SECTION.  IF ONE OF THE ENTRIES IN THE TECH CONTACT SECTION INFO    *         
* USES THE DEFAULT VALID DISPLAY '(DEFAULT CONTACT INFORMATION)'      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TCMSG    LA    RF,TCMSGTB          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TCMSGTB  DS    0C                                                               
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCMSG)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE MESSAGE '(DEFAULT CONTACT INFORMATION)'                 *         
* WAIT TO PROCESS ALL THE CONTACT INFORMATION BEFORE FILLING IN THE   *         
* FIELD SO SAVE THE FIELD FOR NOW.                                    *         
***********************************************************************         
         SPACE 1                                                                
DISTCMSG MVC   ATCMFLD,FVADDR                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TECHNICAL CONTACT NAME                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TCNM     LA    RF,TCNMTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TCNMTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTNM)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT TECH CONTACT NAME FROM BANK BLOCK                *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFTNM   L     R3,AIO3                                                          
         MVC   FVIFLD(L'BANTCNME),BANTCNME  DISPLAY FROM CFILE/BANK REC         
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TECHNICAL CONTACT NAME                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISTNM   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKTCB1,X'FF'-TCNMDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISTNM10 CLI   0(R4),0                                                          
         BE    DISTNM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISTNM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISTNM20                                                         
         CLI   FFTITYP,FFTTECH                                                  
         BE    DISTNM30                                                         
DISTNM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISTNM10                                                         
*                                                                               
DISTNM30 CLC   FFTNAME,BCSPACES                                                 
         BNH   DISTNM40            NO NAME SO GET IT FROM CFILE                 
         MVC   FVIFLD(L'FFTNAME),FFTNAME    DISPLAY FROM ACC/BANK REC           
         B     DISTNMX                                                          
*                                                                               
DISTNM40 MVC   FVIFLD(L'BANCCNME),BANTCNME  DISPLAY FROM CFILE/BANK REC         
         OI    BNKTCB1,TCNMDEF     SET BIT THAT TC NAME IS THE DEFAULT          
*                                                                               
DISTNMX  BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TECHNICAL CONTACT NAME                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALTNM   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKTCB1,X'FF'-TCNMDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALTNM10 CLI   0(R4),0                                                          
         BE    VALTNM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALTNM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALTNM20                                                         
         CLI   FFTITYP,FFTTECH     TECHNICAL CONTACT INFO?                      
         BE    VALTNM30                                                         
VALTNM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALTNM10                                                         
*                                                                               
VALTNM30 OI    ELEMFLAG,ELREP                                                   
         MVC   BOWORK1(L'FFTPHONE),FFTPHONE    SAVE PHONE                       
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE                                                            
         BZ    VALTNM35                                                         
         STC   RE,BOWORK2          STORE LENGTH IN 1ST BYTE OF BOWORK1          
         BCTR  RE,0                SO WE KNOW WHAT LENGTH TO PUT BACK           
         EXMVC RE,BOWORK2+1,FFTEMAIL                                            
VALTNM35 MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALTNM40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKTCB1,TCNMDEF     SET BIT THAT TECH NAME IS THE DEFLT          
         B     VALTNM50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANTCNME   COMPARE NAME ENTERED W/ DEFAULT             
         BNE   *+12                                                             
         OI    BNKTCB1,TCNMDEF     SET BIT THAT TC NAME IS THE DEFAULT          
         B     VALTNM50                                                         
         CLI   FVIFLD,C' '         CANNOT BEGIN WITH A SPACE                    
         BE    EXITNV                                                           
         OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALTNM50 TM    ELEMFLAG,ELNEW+ELREP     ELEMENT NEEDED?                         
         BZ    VTNMX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTTECH     TECH CONTACT INFO                            
         TM    BNKTCB1,TCNMDEF     IS TC NAME THE DEFAULT?                      
         BO    VALTNM60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FFTNAME,FVIFLD   MOVE NAME TO ELEMENT                         
VALTNM60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALTNM65            NO THAN DON'T RESTORE FIELDS                 
         MVC   FFTPHONE,BOWORK1    RESTORE PHONE                                
         ZIC   R1,BOWORK2          GET LENGTH OF EMAIL                          
         LTR   R1,R1               IS THERE AN EMAIL?                           
         BZ    VALTNM65            NO                                           
         ZIC   RE,FFTLN                                                         
         AR    RE,R1                                                            
         STC   RE,FFTLN            UPDATE LENGTH OF ELEM W/ EMAIL               
         ZIC   RE,FFTDLEN                                                       
         AR    RE,R1                                                            
         STC   RE,FFTDLEN          UPDATE DATA LENGTH W/ EMAIL                  
         BCTR  R1,0                                                             
         EXMVC R1,FFTEMAIL,BOWORK2+1   RESTORE EMAIL                            
VALTNM65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VTNMX    BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TECHNICAL CONTACT PHONE # (AREA CODE INCLUDED)      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TCPH     LA    RF,TCPHTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TCPHTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTPH)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTPH)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTPH)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT TECH CONTACT PHONE FROM BANK BLOCK               *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFTPH   L     R3,AIO3                                                          
         MVC   FVIFLD(L'BANTPNO),BANTPNO    DISPLAY FROM CFILE/BANK REC         
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TECHNICAL CONTACT PHONE # (AREA CODE INCLUDED)          *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISTPH   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKTCB1,X'FF'-TCPHDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISTPH10 CLI   0(R4),0                                                          
         BE    DISTPH40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISTPH20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISTPH20                                                         
         CLI   FFTITYP,FFTTECH                                                  
         BE    DISTPH30                                                         
DISTPH20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISTPH10                                                         
*                                                                               
DISTPH30 CLC   FFTPHONE(10),BCSPACES    ANY PHONE ?                             
         BNH   DISTPH40                                                         
         MVC   FVIFLD(10),FFTPHONE      YES MOVE IN FROM ACC/BANK REC           
         B     DISTPHX                                                          
DISTPH40 MVC   FVIFLD(10),BANTPNO       NO MOVE IN FROM CFILE/BANK REC          
         OI    BNKTCB1,TCPHDEF                                                  
*                                                                               
DISTPHX  BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TECHNICAL CONTACT PHONE                                *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALTPH   L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKTCB1,X'FF'-TCPHDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALTPH10 CLI   0(R4),0                                                          
         BE    VALTPH40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALTPH20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALTPH20                                                         
         CLI   FFTITYP,FFTTECH     TECH CONTACT INFO?                           
         BE    VALTPH30                                                         
VALTPH20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALTPH10                                                         
*                                                                               
VALTPH30 OI    ELEMFLAG,ELREP                                                   
         MVC   BOWORK1(L'FFTNAME),FFTNAME    SAVE NAME                          
         MVC   SVPHONE(5),FFTPHONE+10        SAVE EXTENSION                     
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE               ANY EMAIL?                                   
         BZ    VALTPH35                                                         
         STC   RE,BOWORK2          STORE LENGTH IN 1ST BYTE OF BOWORK1          
         BCTR  RE,0                SO WE KNOW WHAT LENGTH TO PUT BACK           
         EXMVC RE,BOWORK2+1,FFTEMAIL                                            
VALTPH35 MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALTPH40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKTCB1,TCPHDEF     SET BIT THAT TC PHONE IS THE DEFAULT         
         B     VALCPH50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANTPNO    COMPARE PHONE ENTERED W/ DEFAULT            
         BNE   *+12                                                             
         OI    BNKTCB1,TCPHDEF     SET BIT THAT TC PHONE IS THE DEFAULT         
         B     VALTPH50                                                         
         TM    BNKTCB1,TCNMDEF     IS THE TECH CONTACT NAME THE DEFLT?          
         BO    ERRNOCHG            YES SO CAN'T CHANGE EMAIL HERE               
         OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALTPH50 TM    ELEMFLAG,ELNEW+ELREP        ELEMENT NEEDED?                      
         BZ    VTPHX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTTECH     TECH CONTACT INFO                            
         TM    BNKTCB1,TCPHDEF     IS TC PHONE THE DEFAULT?                     
         BO    VALTPH60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FFTPHONE,FVIFLD  MOVE PHONE TO ELEMENT                        
VALTPH60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALTPH65                                                         
         MVC   FFTNAME,BOWORK1     RESTORE NAME                                 
         MVC   FFTPHONE+10(5),SVPHONE  RESTORE EXTENSION                        
         ZIC   R1,BOWORK2          GET LENGTH OF EMAIL                          
         LTR   R1,R1               ANY EMAIL TO RESTORE?                        
         BZ    VALTPH65                                                         
         ZIC   RE,FFTLN                                                         
         AR    RE,R1                                                            
         STC   RE,FFTLN            UPDATE LENGTH OF ELEM W/ EMAIL               
         ZIC   RE,FFTDLEN                                                       
         AR    RE,R1                                                            
         STC   RE,FFTDLEN          UPDATE DATA LENGTH W/ EMAIL                  
         BCTR  R1,0                                                             
         EXMVC R1,FFTEMAIL,BOWORK2+1   RESTORE EMAIL                            
VALTPH65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VTPHX    BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TECHNICAL CONTACT PHONE EXTENSION                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TCEXT    LA    RF,TCEXTTB          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TCEXTTB  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTEXT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTEXT)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTEXT)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT TECHNICAL CONTACT EXTENSION FROM BANK BLOCK      *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFTEXT  L     R3,AIO3                                                          
         MVC   FVIFLD(5),BANTPNO+10     DISPLAY FROM CFILE/BANK REC             
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TECHNICAL CONTACT PHONE EXTENSION                       *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISTEXT  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKTCB1,X'FF'-TCEXDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISTEX10 CLI   0(R4),0                                                          
         BE    DISTEX40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISTEX20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISTEX20                                                         
         CLI   FFTITYP,FFTTECH                                                  
         BE    DISTEX30                                                         
DISTEX20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISTEX10                                                         
*                                                                               
DISTEX30 CLC   FFTPHONE+10(5),BCSPACES     ANY EXTENSION?                       
         BNH   DISCEX40                                                         
         MVC   FVIFLD(5),FFTPHONE+10    YES MOVE IN FROM ACC/BANK REC           
         B     DISCEXX                                                          
DISTEX40 MVC   FVIFLD(5),BANTPNO+10     NO MOVE IN FROM CFILE/BANK REC          
         OI    BNKTCB1,TCEXDEF                                                  
*                                                                               
DISTEXX  BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TECHNICAL CONTACT PHONE EXTENSION                      *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALTEXT  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKTCB1,X'FF'-TCEXDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALTEX10 CLI   0(R4),0                                                          
         BE    VALTEX40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALTEX20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALTEX20                                                         
         CLI   FFTITYP,FFTTECH     TECH CONTACT INFO?                           
         BE    VALTEX30                                                         
VALTEX20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALTEX10                                                         
*                                                                               
VALTEX30 OI    ELEMFLAG,ELREP      REPLACE ELEMENT                              
         MVC   BOWORK1(L'FFTNAME),FFTNAME    SAVE NAME                          
         MVC   SVPHONE(10),FFTPHONE    SAVE PHONE (W/O EXTENSION)               
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE               ANY EMAIL?                                   
         BZ    VALTEX35            NO                                           
         STC   RE,BOWORK2          STORE LENGTH IN 1ST BYTE OF BOWORK1          
         BCTR  RE,0                SO WE KNOW WHAT LENGTH TO PUT BACK           
         EXMVC RE,BOWORK2+1,FFTEMAIL                                            
VALTEX35 MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALTEX40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKTCB1,TCEXDEF     SET BIT THAT PHONE EX IS THE DEFAULT         
         B     VALTEX50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANTPNO+10 COMPARE PHONE EXT ENTERED W/ DEFLT          
         BNE   *+12                                                             
         OI    BNKTCB1,TCEXDEF     SET BIT THAT PHONE EX IS THE DEFAULT         
         B     VALTEX50                                                         
         TM    BNKTCB1,TCNMDEF     IS THE TECH CONTACT NAME THE DFLT?           
         BO    ERRNOCHG            YES SO CAN'T CHANGE EMAIL HERE               
         OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALTEX50 TM    ELEMFLAG,ELNEW+ELREP      ELEMENT NEEDED?                        
         BZ    VTEXX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTTECH     TECH CONTACT INFO                            
         TM    BNKTCB1,TCEXDEF     IS TC EXTENSION THE DEFAULT?                 
         BO    VALTEX60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FFTPHONE+10,FVIFLD  MOVE PHONE EXT TO ELEMENT                 
VALTEX60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALTEX65            NO THAN NOTHING TO RESTORE                   
         MVC   FFTNAME,BOWORK1     RESTORE NAME                                 
         MVC   FFTPHONE(10),SVPHONE  RESTORE PHONE (W/O EXTENSION)              
         ZIC   R1,BOWORK2          GET LENGTH OF EMAIL                          
         LTR   R1,R1               ANY EMAIL?                                   
         BZ    VALTEX65            NO                                           
         ZIC   RE,FFTLN                                                         
         AR    RE,R1                                                            
         STC   RE,FFTLN            UPDATE LENGTH OF ELEM W/ EMAIL               
         ZIC   RE,FFTDLEN                                                       
         AR    RE,R1                                                            
         STC   RE,FFTDLEN          UPDATE DATA LENGTH W/ EMAIL                  
         BCTR  R1,0                                                             
         EXMVC R1,FFTEMAIL,BOWORK2+1   RESTORE EMAIL                            
VALTEX65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VTEXX    BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR TECHNICAL CONTACT EMAIL                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TCEM     LA    RF,TCEMTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TCEMTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCEM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTCEM)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTCEM)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DEFAUT TECHNICAL CONTACT EMAIL FROM BANK BLOCK          *         
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
DDFTCEM  L     R3,AIO3                                                          
         MVC   FVIFLD(L'BANTEML),BANTEML   DISPLAY FROM CFILE/BANK REC          
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TECHNICAL CONTACT EMAIL                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
DISTCEM  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         NI    BNKTCB1,X'FF'-TCEMDEF                                            
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
DISTEM10 CLI   0(R4),0                                                          
         BE    DISTEM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   DISTEM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   DISTEM20                                                         
         CLI   FFTITYP,FFTTECH                                                  
         BE    DISTEM30                                                         
DISTEM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISTEM10                                                         
*                                                                               
DISTEM30 ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         SUBTRACT NAME AND PHONE TO GET EMAIL         
         LTR   RE,RE               ANY EMAIL?                                   
         BZ    DISTEM40            NO                                           
         CLC   FFTEMAIL,BCSPACES     ANY EMAIL?                                 
         BNH   DISTEM40                                                         
         ZIC   RE,FFTDLEN          GET LENGTH OF DATA                           
         SHI   RE,FFT75LNQ         GET LENGTH OF EMAIL ADDRESS                  
         BCTR  RE,0                SUBTRACT 1 FOR EXMVC                         
         MVC   FVIFLD(0),FFTEMAIL                                               
         EX    RE,*-6                                                           
         B     DISTEMX                                                          
DISTEM40 MVC   FVIFLD(L'BANTEML),BANTEML NO MOVE IN FROM CFILE/BANK REC         
         OI    BNKTCB1,TCEMDEF                                                  
*                                                                               
DISTEMX  BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TECHNICAL CONTACT EMAIL ADDRESS                        *         
***********************************************************************         
         SPACE 1                                                                
         USING FFTELD,R4                                                        
         USING BANKD,R3                                                         
VALTCEM  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         MVI   ELEMFLAG,0                                                       
         NI    BNKTCB1,X'FF'-TCEMDEF                                            
         XC    BOWORK1,BOWORK1                                                  
         XC    BOWORK2,BOWORK2                                                  
         LA    R4,BNKRECD+(BNKRFST-BNKRECD)                                     
VALTEM10 CLI   0(R4),0                                                          
         BE    VALTEM40                                                         
         CLI   0(R4),FFTELQ                                                     
         BNE   VALTEM20                                                         
         CLI   FFTTYPE,FFTTCNAM                                                 
         BNE   VALTEM20                                                         
         CLI   FFTITYP,FFTTECH     CLIENT SERVICE CONTACT INFO?                 
         BE    VALTEM30                                                         
VALTEM20 ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     VALTEM10                                                         
*                                                                               
VALTEM30 OI    ELEMFLAG,ELREP                                                   
         MVC   BOWORK1(L'FFTNAME),FFTNAME    SAVE NAME                          
         MVC   BOWORK2(L'FFTPHONE),FFTPHONE    SAVE PHONE                       
         MVI   0(R4),X'FF'         DELETE ELEMENT                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',BNKRECD),0,0                
*                                                                               
VALTEM40 CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    BNKTCB1,TCEMDEF     SET BIT THAT EMAIL IS THE DEFAULT            
         B     VALTEM50                                                         
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,BANTEML    COMPARE PHONE EXT ENTERED W/ DEFLT          
         BNE   *+12                                                             
         OI    BNKTCB1,TCEMDEF     SET BIT THAT EMAIL IS THE DEFAULT            
         B     VALTEM50                                                         
         TM    BNKTCB1,TCNMDEF     IS THE CLI SERV NAME THE DEFAULT?            
         BO    ERRNOCHG            YES SO CAN'T CHANGE EMAIL HERE               
         GOTO1 VEMAIL,BOPARM,FVIHDR,0      VALIDATE E-MAIL ADDRESS              
         CLI   0(R1),0                                                          
         BE    VALTEM45                                                         
         SR    RE,RE                                                            
         ICM   RE,7,BOPARM+1                                                    
         MVC   FVXTRA,0(RE)                                                     
         B     EXITNV                                                           
VALTEM45 OI    ELEMFLAG,ELNEW                                                   
         DROP  R4                                                               
*                                                                               
         USING FFTELD,RF                                                        
VALTEM50 TM    ELEMFLAG,ELNEW+ELREP      ANY ELEMENT NEEDED?                    
         BZ    VTEXX                                                            
         LA    RF,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   FFTEL,FFTELQ        DB ELEMENT                                   
         MVI   FFTLN,FFT75LNQ+FFTLN1Q+1  ADD OVERHEAD                           
         MVI   FFTTYPE,FFTTCNAM    CONTACT NAME AND PHONE # TYPE                
         MVI   FFTDLEN,FFT75LNQ    ACTUAL TEXT OF LENGTH                        
         MVI   FFTITYP,FFTTECH     TECH CONTACT INFO                            
         TM    BNKTCB1,TCEMDEF     IS TC EMAIL THE DEFAULT?                     
         BO    VALTEM60            THAN DON'T PUT IT BACK TO ELEMENT            
         ZIC   RE,FVILEN           GET INPUT LENGTH                             
         ZIC   R1,FFTLN                                                         
         AR    R1,RE                                                            
         STC   R1,FFTLN            UPDATE ELEMENT LENGTH                        
         ZIC   R1,FFTDLEN                                                       
         AR    R1,RE                                                            
         STC   R1,FFTDLEN          UPDATE DATA LENGTH                           
         BCTR  RE,0                                                             
         MVC   FFTEMAIL(0),FVIFLD  EMAIL ADDRESS                                
         EX    RE,*-6                                                           
VALTEM60 TM    ELEMFLAG,ELREP      REPLACING THE ELEMENT?                       
         BZ    VALTEM65            NO THAN NOTHING TO RESTORE                   
         MVC   FFTNAME,BOWORK1     RESTORE NAME                                 
         MVC   FFTPHONE,BOWORK2    RESTORE PHONE                                
VALTEM65 GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),BNKRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VTEMX    BRAS  RE,DTCMSG                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADVANTIS USER ID (USE)                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ADID     LA    RF,ADIDTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ADIDTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADID)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADID)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE ADVANTIS USER ID                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
DISADID  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         LA    RF,SVBNKEL                                                       
         CLC   BKADVID,BCSPACES    ANY ADVANTIS USER ID?                        
         BNH   DISAD10             NO GET FROM DEFAULT                          
         MVC   FVIFLD(L'BKADVID),BKADVID                                        
         B     EXITOK                                                           
*                                                                               
DISAD10  CLC   BANDFUSR,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   SVFIELD(L'BANDFUSR),BANDFUSR  PASS THE DEFAULT FIELD             
         OC    SVFIELD,BCSPACES                                                 
         MVI   SVLEN,L'BANDFUSR    PASS THE LENGTH                              
         BRAS  RE,DISDEF           DISPLAY THE DEFAULT                          
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE ADVANTIS USER ID                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
VALADID  LA    RF,SVBNKEL                                                       
         MVC   BKADVID,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'('         IS THIS THE DEFAULT?                         
         BE    EXITOK              THAN IGNORE                                  
*                                                                               
         CLI   FVILEN,7            MAX OF 7                                     
         BH    EXITNV                                                           
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BKADVID,FVIFLD                                                
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADVANTIS ACCOUNT (ACC)                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ADACC    LA    RF,ADACTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
ADACTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALADAC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE ADVANTIS ACCOUNT                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
DISADAC  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         LA    RF,SVBNKEL                                                       
         CLC   BKADVAC,BCSPACES    ANY ADVANTIS ACCOUNT?                        
         BNH   DISAC10             NO GET FROM DEFAULT                          
         MVC   FVIFLD(L'BKADVAC),BKADVAC                                        
         B     EXITOK                                                           
*                                                                               
DISAC10  CLC   BANDFACN,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   SVFIELD(L'BANDFACN),BANDFACN  PASS THE DEFAULT FIELD             
         OC    SVFIELD,BCSPACES                                                 
         MVI   SVLEN,L'BANDFACN    PASS THE LENGTH                              
         BRAS  RE,DISDEF           DISPLAY THE DEFAULT                          
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE ADVANTIS ACCOUNT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
VALADAC  LA    RF,SVBNKEL                                                       
         MVC   BKADVAC,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'('         IS THIS THE DEFAULT?                         
         BE    EXITOK              THAN IGNORE                                  
*                                                                               
         CLI   FVILEN,4            MAX OF 4                                     
         BH    EXITNV                                                           
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BKADVAC,FVIFLD                                                
         B     EXITOK              DON'T PUT ELEMENT YET                        
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MESSAGE CLASS (CLA)                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MSGC     LA    RF,MSGCTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MSGCTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMSGC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMSGC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE MESSAGE CLASS                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
DISMSGC  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         LA    RF,SVBNKEL                                                       
         CLC   BKMSGCL,BCSPACES    ANY MESSAGE CLASS?                           
         BNH   DISMSG10            NO GET FROM DEFAULT                          
         MVC   FVIFLD(L'BKMSGCL),BKMSGCL                                        
         B     EXITOK                                                           
*                                                                               
DISMSG10 CLC   BANDFCLS,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   SVFIELD(L'BANDFCLS),BANDFCLS  PASS THE DEFAULT FIELD             
         OC    SVFIELD,BCSPACES                                                 
         MVI   SVLEN,L'BANDFCLS    PASS THE LENGTH                              
         BRAS  RE,DISDEF           DISPLAY THE DEFAULT                          
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE MESSAGE CLASS                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
VALMSGC  LA    RF,SVBNKEL                                                       
         MVC   BKMSGCL,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'('         IS THIS THE DEFAULT?                         
         BE    EXITOK              THAN IGNORE                                  
*                                                                               
         CLI   FVILEN,8            MAX OF 8                                     
         BH    EXITNV                                                           
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BKMSGCL,FVIFLD                                                
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FILENAME DEFINITION                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNDF     LA    RF,FNDFTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FNDFTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNDF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFNDF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE FILENAME DEFINITON                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
DISFNDF  LA    RF,SVBNKEL                                                       
         TM    BKSTAT,BKFNID                                                    
         BZ    *+14                                                             
         MVC   FVIFLD(L'AC@ID),AC@ID                                            
         B     EXITOK                                                           
         TM    BKSTAT,BKFNNM                                                    
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'AC@NAME),AC@NAME                                        
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE FILENAME DEFINITION                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
VALFNDF  LA    RF,SVBNKEL                                                       
         NI    BKSTAT,X'FF'-(BKFNID+BKFNNM)                                     
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,AC@ID            ID                                    
         BNE   *+12                                                             
         OI    BKSTAT,BKFNID                                                    
         B     EXITOK                                                           
         EXCLC R1,FVIFLD,AC@NAME          NAME                                  
         BNE   EXITNV                                                           
         OI    BKSTAT,BKFNNM                                                    
         B     EXITOK                                                           
         DROP  RF                                                               
***********************************************************************         
* DATA OBJECT FOR CHARGE (CHA)                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CHRG     LA    RF,CHRGTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CHRGTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCHRG)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCHRG)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE CHARGE                                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
DISCHRG  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         LA    RF,SVBNKEL                                                       
         CLC   BKCHRG,BCSPACES     ANY CHARGE?                                  
         BNH   DISCHR10            NO GET FROM DEFAULT                          
         MVC   FVIFLD(L'BKCHRG),BKCHRG                                          
         B     EXITOK                                                           
*                                                                               
DISCHR10 CLC   BANDFCHR,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   SVFIELD(L'BANDFCHR),BANDFCHR  PASS THE DEFAULT FIELD             
         OC    SVFIELD,BCSPACES                                                 
         MVI   SVLEN,L'BANDFCHR    PASS THE LENGTH                              
         BRAS  RE,DISDEF           DISPLAY THE DEFAULT                          
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE CHARGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
VALCHRG  LA    RF,SVBNKEL                                                       
         MVC   BKCHRG,BCSPACES                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'('         IS THIS THE DEFAULT?                         
         BE    EXITOK              THAN IGNORE                                  
         CLI   FVIFLD,C'1'         ONLY 1, 2 OR 3 ARE VALID                     
         BE    VCHRG10                                                          
         CLI   FVIFLD,C'2'                                                      
         BE    VCHRG10                                                          
         CLI   FVIFLD,C'3'                                                      
         BNE   EXITNV                                                           
*                                                                               
VCHRG10  ZIC   R1,FVXLEN                                                        
         EXMVC R1,BKCHRG,FVIFLD                                                 
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TRANSMISSION TYPE                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TRNT     LA    RF,TRNTTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TRNTTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTRNT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTRNT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TRANSMISSION TYPE                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
DISTRNT  DS    0H                                                               
         NI    FLAG,X'FF'-FLGDEF                                                
         L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         LA    RF,SVBNKEL                                                       
*                                                                               
         LA    R1,BKTRNTY                                                       
         CLI   BKTRNTY,0           IS THERE ANY OVERRIDE?                       
         BH    DISTRT05                                                         
         LA    R1,BANDFTYP                                                      
         OI    FLAG,FLGDEF         DISPLAYING FROM DEFAULT                      
*                                                                               
         USING TYPTABD,RE                                                       
DISTRT05 LA    RE,TRNTYTAB                                                      
DISTRT10 CLI   0(RE),EOF                                                        
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLC   TYPFLD#,0(R1)                                                    
         BE    *+12                                                             
         AHI   RE,TYPLNQ                                                        
         B     DISTRT10                                                         
*                                                                               
         TM    FLAG,FLGDEF                ARE WE DIPLAYING DEFAULT              
         BO    DISTRT30                                                         
         MVC   FVIFLD(L'TYPFLD),TYPFLD                                          
         B     DISTRTX                    EXIT OK                               
*                                                                               
DISTRT30 NI    FLAG,X'FF'-FLGDEF          TURN OFF DISPLAYING DFLT BIT          
         MVC   SVFIELD(L'TYPFLD),TYPFLD   PASS THE DEFAULT FIELD                
         DROP  RE                                                               
*                                                                               
         OC    SVFIELD,BCSPACES                                                 
         MVI   SVLEN,L'BANDFTKY    PASS THE LENGTH                              
         BRAS  RE,DISDEF           DISPLAY THE DEFAULT                          
*                                                                               
DISTRTX  B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TRANSMISSION TYPE                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
*                                                                               
VALTRNT  LA    RF,SVBNKEL                                                       
         MVI   BKTRNTY,0                                                        
         L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         CLI   BANDFTYP,0                                                       
         BNE   VALTRT05                                                         
         DROP  R3                                                               
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITNV                                                           
VALTRT05 CLI   FVIFLD,C'('         IS THIS THE DEFAULT?                         
         BE    EXITOK              THAN IGNORE                                  
*                                                                               
         SR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
*                                                                               
         USING TYPTABD,RE                                                       
         LA    RE,TRNTYTAB                                                      
VALTRT10 CLI   0(RE),EOF                                                        
         BE    EXITNV                                                           
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   FVIFLD(0),TYPFLD                                                 
         BE    *+12                                                             
         AHI   RE,TYPLNQ                                                        
         B     VALTRT10                                                         
*                                                                               
         MVC   BKTRNTY,TYPFLD#                                                  
         B     EXITOK                                                           
         DROP  RE,RF                                                            
*                                                                               
TRNTYTAB DS    0C                                                               
         DC    CL10'EDICT',AL1(BKTEDT)                                          
         DC    CL10'MQ',AL1(BKTMQ)                                              
         DC    CL10' ',AL1(0)                                                   
         DC    AL1(EOF)                                                         
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TRANSMISSION KEY                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TRNK     LA    RF,TRNKTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TRNKTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTRNK)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTRNK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TRANSMISSION KEY                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
DISTRNK  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         LA    RF,SVBNKEL                                                       
         CLC   BKTRNKY,BCSPACES    ANY TRANSMISSION KEY?                        
         BNH   DISTRK10            NO GET FROM DEFAULT                          
         MVC   FVIFLD(L'BKTRNKY),BKTRNKY                                        
         B     EXITOK                                                           
*                                                                               
DISTRK10 CLC   BANDFTKY,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   SVFIELD(L'BANDFTKY),BANDFTKY  PASS THE DEFAULT FIELD             
         OC    SVFIELD,BCSPACES                                                 
         MVI   SVLEN,L'BANDFTKY    PASS THE LENGTH                              
         BRAS  RE,DISDEF           DISPLAY THE DEFAULT                          
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TRANSMISSION KEY                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
*                                                                               
VALTRNK  LA    RF,SVBNKEL                                                       
         MVC   BKTRNKY,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVIFLD,C'('         IS THIS THE DEFAULT?                         
         BE    EXITOK              THAN IGNORE                                  
         CLI   BKTRNTY,BKTEDT      IS THIS AN EDICT TYPE?                       
         BNE   VALTRK10            NO - IGNORE                                  
*                                                                               
         USING EDIKEYD,R3                                                       
         LA    R3,IOKEY                                                         
         XC    EDIKEY,EDIKEY                                                    
         MVI   EDIKSYS,EDIKSYSQ    X'05' - KEY SYSTEM FOR ALL SYSTEMS           
         MVI   EDITYPE,EDITYPEQ    X'07' - EDICT TYPE                           
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,EDINAME,FVIFLD                                                
         OC    EDINAME,BCSPACES                                                 
         LHI   R1,XOCONFIL+XOREAD+XIO1                                          
         GOTO1 AIO                                                              
         BE    VALTRK10                                                         
         MVC   FVMSGNO,=AL2(AE$NOEDI)  EDICT RECORD NOT SET UP                  
         B     EXITL                                                            
*                                                                               
VALTRK10 LA    RF,SVBNKEL                                                       
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BKTRNKY,FVIFLD                                                
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DATASET NAME                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DSNM     LA    RF,DSNMTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DSNMTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSNM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE DATASET NAME                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
DISDSNM  LA    RF,SVBNKEL                                                       
         CLC   BKDSNM,BCSPACES     ANY DATASET NAME?                            
         BNH   EXITOK                                                           
         MVC   FVIFLD(L'BKDSNM),BKDSNM                                          
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE DATASET NAME                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
VALDSNM  LA    RF,SVBNKEL                                                       
         MVC   BKDSNM,BCSPACES                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BKDSNM,FVIFLD                                                 
         OC    BKDSNM,BCSPACES                                                  
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FORMAT KEY                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FMKY     LA    RF,FMKYTB           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FMKYTB   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMKY)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFMKY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE FORMAT KEY                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
         USING BANKD,R3                                                         
DISFMKY  L     R3,AIO3             BANK BLOCK FROM GETBANK CALL                 
         LA    RF,SVBNKEL                                                       
         CLC   BKFMKY,BCSPACES     ANY FORMAT KEY?                              
         BNH   DISFMK10            NO GET FROM DEFAULT                          
         MVC   FVIFLD(L'BKFMKY),BKFMKY                                          
         B     EXITOK                                                           
*                                                                               
DISFMK10 CLC   BANDFFKY,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   SVFIELD(L'BANDFFKY),BANDFFKY  PASS THE DEFAULT FIELD             
         OC    SVFIELD,BCSPACES                                                 
         MVI   SVLEN,L'BANDFFKY    PASS THE LENGTH                              
         BRAS  RE,DISDEF           DISPLAY THE DEFAULT                          
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE FORMAT KEY                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING BNKELD,RF                                                        
VALFMKY  LA    RF,SVBNKEL                                                       
         MVC   BKFMKY,BCSPACES                                                  
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,BKFMKY,FVIFLD                                                 
         B     EXITOK                                                           
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
THIS     USING BNKRECD,R2                                                       
LAST     USING BNKRECD,R3                                                       
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
X        USING BNKRECD,IOKEY                                                    
FLST     MVC   X.BNKKEY,THIS.BNKRECD                                            
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTOX AIO                                                              
         BNE   EXITL               READ HIGH UNHAPPY                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(BNKBANK-BNKRECD),THIS.BNKRECD                              
         BNE   EXITL               DIFFERENT COMPANY                            
*                                                                               
         MVC   THIS.BNKKEY(ACCKLEN),IOKEY                                       
         B     EXITOK                                                           
         DROP  THIS,X,LAST                                                      
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE CLIENT SERVICE CONTACT INFORMATION MESSAGE                        
***********************************************************************         
         SPACE 1                                                                
DCSMSG   NTR1  BASE=*,LABEL=*                                                   
         CLI   BNKCSB1,0                                                        
         BE    DCSMSGX                                                          
         L     R4,ACSMFLD          POINT TO SAVED CLI SERV MSG FIELD            
         USING FHD,R4                                                           
         MVC   FHDA(L'AC@NF100),AC@NF100  MOVE IN MESSAGE                       
         MVI   FHIL,L'AC@NF100          SET LENGTH                              
         OI    FHOI,FHOITR              AND TRANSMIT                            
*                                                                               
DCSMSGX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY THE TECH SERVICE CONTACT INFORMATION MESSAGE                          
***********************************************************************         
         SPACE 1                                                                
DTCMSG   NTR1  BASE=*,LABEL=*                                                   
         CLI   BNKTCB1,0                                                        
         BE    DCTCSGX                                                          
         L     R4,ATCMFLD          POINT TO SAVED CLI SERV MSG FIELD            
         USING FHD,R4                                                           
         MVC   FHDA(L'AC@NF100),AC@NF100  MOVE IN MESSAGE                       
         MVI   FHIL,L'AC@NF100          SET LENGTH                              
         OI    FHOI,FHOITR              AND TRANSMIT                            
*                                                                               
DCTCSGX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* FORMAT THE DEFAULT FROM THE CONTROL FILE INTO FVIFLD SURROUNDED BY            
* PARENTHESES.                                                                  
* SVFIELD CONTAINS DEFAULT ENTRY FROM THE CONTROL FILE                          
* SVLEN CONTAINS THE MAX LENGTH OF THE ENTRY                                    
***********************************************************************         
         SPACE 1                                                                
DISDEF   NTR1  BASE=*,LABEL=*                                                   
         CLC   SVFIELD,BCSPACES                                                 
         BNH   DISDEFX                                                          
*                                                                               
         LA    RF,FVIFLD           POINT TO FVIFLD                              
         MVI   0(RF),C'('                                                       
         AHI   RF,1                BUMP PAST (                                  
         ZIC   R1,SVLEN            R1=MAX LENGTH OF FIELD                       
         LA    RE,SVFIELD                                                       
         AR    RE,R1               BUMP TO END OF FIELD                         
         BCTR  RE,0                -1 FOR REAL END                              
DISD20   CLI   0(RE),C' '                                                       
         BH    DISD30                                                           
         BCTR  RE,0                                                             
         BCT   R1,DISD20                                                        
DISD30   BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BM    DISDEFX                                                          
         EXMVC R1,FVIFLD+1,SVFIELD                                              
         AHI   R1,1                BUMP UP FOR REAL LENGTH                      
         AR    RF,R1               BUMP PAST THE DATA                           
         MVI   0(RF),C')'                                                       
DISDEFX  XIT1                                                                   
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CALL DDGETBANK TO GET BANK INFORMATION TO DISPLAY.                            
* DDGETBANK READS CONTROL/NFILE BANK RECORD AND AFM BANK RECORD                 
* USE AIO3 AS BANK BLOCK                                                        
***********************************************************************         
         SPACE 1                                                                
         USING BANKD,R3                                                         
GETBANK  NTR1 BASE=*,LABEL=*                                                    
         NI    FLAG,X'FF'-FLGBNF  RESET BANK NOT FOUND BIT                      
         L     RE,AIO3                                                          
         LHI   RF,2000                                                          
         SR    R1,R1                                                            
         SR    R0,R0                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,AIO3                                                          
         MVC   BANCPY,CUABIN       COMPANY CODE                                 
         MVC   BANCDE,BNKBANK      BANK CODE                                    
         MVC   BANBRN,BNKBRNCH     BRANCH CODE                                  
         GOTO1 VGETBANK,BOPARM,(X'01',AIO3),ACOM,0                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    BANKFLAG,BCDENFND+BHUBNFND+BBRANFND                              
         BZ    GETBANKX                                                         
         OI    FLAG,FLGBNF         BANK RECD NOT FOUND ON CONTROL FILE          
*                                                                               
GETBANKX XIT1                                                                   
         DROP  R3                                                               
         SPACE 2                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*        CHECK IF VALID EMAIL ADDRESS                                           
***********************************************************************         
         SPACE 1                                                                
EMVAL    NTR1  BASE=*,LABEL=*                                                   
         MVI   FLGEM,0             INIT FLAG                                    
         ZIC   R2,FVILEN           PICK UP THE LENGTH                           
         LR    R1,R2                                                            
         LA    R6,FVIFLD                                                        
         LR    R4,R6               EXTRA POINTER TO EMAIL ADDRS                 
         AR    R6,R2               CHECK FOR VALID .XXX                         
*                                                                               
EMVAL10  SHI   R6,1                                                             
         CLI   0(R6),C'.'          IS IT DOT SOMETHING                          
         BE    EMVAL20                                                          
         CLI   0(R6),C'A'          IS IT HIGHER THAN A                          
         BL    EMVALERR                                                         
         CLI   0(R6),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
         SHI   R2,1                                                             
         LR    R2,R2                                                            
         BNZ   EMVAL10                                                          
         B     EMVALERR            NO '.' FOUND                                 
*                                                                               
EMVAL20  CR    R2,R1                                                            
         BE    EMVALERR            ENDS WITH '.'                                
         LR    R2,R1               RESET LENGTH                                 
*                                                                               
         CLI   0(R4),C'.'          DOES USER NAME START WITH DOT                
         BE    EMVALERR            ERROR CAN'T START WITH DOT                   
EMVAL30  DS    0H                                                               
         CLI   0(R4),C'0'          IS IT LESS THAN F0                           
         BL    EMVAL40             YES CHK NEXT                                 
         CLI   0(R4),C'9'          IS IT HIGHER THAN F9                         
         BNH   EMVAL50             IT IS A NUMBER                               
         B     EMVALERR                                                         
EMVAL40  CLI   0(R4),C'A'          IS IT HIGHER THAN A                          
         BL    EMVAL60             CHECK FOR SPECIAL CHARS                      
         CLI   0(R4),C'Z'          SHOULD BE Z OR LOWER                         
         BH    EMVALERR                                                         
*                                                                               
EMVAL50  DS    0H                                                               
         LA    R4,1(R4)            GET NEXT CHAR                                
         BCT   R2,EMVAL30                                                       
         B     EMVALGD                                                          
*                                                                               
EMVAL60  DS    0H                                                               
         LA    R1,EXCTAB           POINT TO SPECIAL CHARS TABLE                 
EMVAL60A CLI   0(R1),X'FF'         DID WE FIND SPECIAL CHARS                    
         BE    EMVALERR            NO SPCL CHAR FND, ERROR.                     
         CLI   0(R4),C'@'          HAVE WE REACHED @ YET                        
         BNE   EMVAL6AA                                                         
         TM    FLGEM,ATFOUND       MAKE SURE NO MORE THAN ONE @ SIGN            
         BO    EMVALERR                                                         
         OI    FLGEM,ATFOUND       NOW WE HAVE ONE @ IN E-MAIL                  
EMVAL6AA CLC   0(1,R1),0(R4)       IS IT SPCL CHAR                              
         BE    EMVAL60C                                                         
EMVAL60B LA    R1,1(R1)            POINT TO NEXT TABLE ENTRY                    
         B     EMVAL60A                                                         
*                                                                               
EMVAL60C DS    0H                  MAKE SURE NO 2 SPCL CHRS APEAR TOGE          
         LA    RF,EXCTAB                                                        
EMVAL60D CLI   0(RF),X'FF'                                                      
         BE    EMVAL50                                                          
         CLC   1(1,R4),0(RF)       ARE BOTH SPECIAL CHARS                       
         BE    EMVALERR                                                         
         LA    RF,1(RF)            POINT TO NEXT CHAR IN TAB                    
         B     EMVAL60D                                                         
*                                                                               
EMVALGD  DS    0H                                                               
         TM    FLGEM,ATFOUND       SHOULD HAVE ONE @ IN E-MAIL                  
         BZ    EMVALERR            NO @ FOUND ERROR                             
         CR    RB,RB                                                            
         B     *+6                                                              
*                                                                               
EMVALERR CR    RB,RD                                                            
EMVALX   XIT1                                                                   
EXCTAB   DC    C'@'                @ SIGN                                       
         DC    C'_'                UNDERSCORE                                   
         DC    C'.'                DOT                                          
         DC    C'-'                DASH                                         
         DC    X'FF'               END OF TABLE                                 
* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -             
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
EOF      EQU   FF                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#YES,L'AC@YES,L           YES                                  
         DCDDL AC#NO,L'AC@NO,L             NO                                   
         DCDDL AC#NF100,L'AC@NF100,L       (DEFAULT CONTACT INFO)               
         DCDDL AC#ID,L'AC@ID,L              ID                                  
         DCDDL AC#NAME,L'AC@NAME,L          NAME                                
DCLISTX  DC    X'00'                                                            
*                                                                               
         SPACE 2                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
* DDGETBANKD                                                                    
* DDCOMFACS                                                                     
* CTGENEDICT                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDGETBANKD                                                     
       ++INCLUDE DDCOMFACS                                                      
       ++INCLUDE CTGENEDICT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
ACSFFTEL DS    A                   A(CLIENT SERVICE INFO DB ELEM)               
ATCFFTEL DS    A                   A(TECHNICAL CONTACT INFO DB ELEM)            
ACSMFLD  DS    A                   A(CLIENT SERVICE MESSAGE FIELD)              
ATCMFLD  DS    A                   A(TECHNICAL CONTACT MESSAGE FIELD)           
*                                                                               
SVPHONE  DS    CL15                                                             
SVBANK   DS    CL3                 BANK CODE                                    
SVBRCH   DS    CL4                 BRANCH CODE                                  
SVEDITY  DS    XL1                                                              
TYPEPOS  EQU   BNKPOS                                                           
TYPEACR  EQU   BNKACR                                                           
TYPEEFT  EQU   BNKEFT                                                           
TYPE820  EQU   BNK820                                                           
SVFIELD  DS    CL20                SAVED DEFAULT FIELD TO DISPLAY               
SVLEN    DS    XL1                 SAVED LENGTH OF DEFAULT FIELD                
FLGEM    DS    XL1                 FLAG FOR EMAIL VALIDATION                    
ATFOUND  EQU   X'80'               @ SIGN FOUND                                 
ELEMFLAG DS    XL1                                                              
ELNEW    EQU   X'80'               ADDING A NEW ELEMENT (NOT REPLACING)         
ELREP    EQU   X'40'               REPLACING AN EXISTING ELEMENT                
BNKCSB1  DS    XL1                 CLIENT SERVICE CONTACT INFO BYTE             
CSNMDEF  EQU   X'80'               CLIENT SERVICE NAME DEFAULT USED             
CSPHDEF  EQU   X'40'               CLIENT SERVICE PHONE DEFAULT USED            
CSEXDEF  EQU   X'20'               CLIENT SERVICE PHONE EXT DEF USED            
CSEMDEF  EQU   X'10'               CLIENT SERVICE EMAIL DEFAULT USED            
*                                                                               
BNKTCB1  DS    XL1                 TECH CONTACT INFO BYTE                       
TCNMDEF  EQU   X'80'               TECH CONTACT NAME DEFAULT USED               
TCPHDEF  EQU   X'40'               TECH CONTACT PHONE DEFAULT USED              
TCEXDEF  EQU   X'20'               TECH CONTACT PHONE EXT DEF USED              
TCEMDEF  EQU   X'10'               TECH CONTACT EMAIL DEFAULT USED              
SVBNKEL  DS    CL(BNK1LNQ)         SAVED BANK ELEMENT                           
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
         EJECT                                                                  
***********************************************************************         
* TYPE TABLE DSECT                                                    *         
***********************************************************************         
         SPACE 1                                                                
TYPTABD  DSECT                                                                  
TYPFLD   DS    CL10                INPUT FIELD VALUE                            
TYPFLD#  DS    XL1                 FIELD NUMBER                                 
TYPLNQ   EQU   *-TYPTABD                                                        
         SPACE 1                                                                
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
         SPACE 1                                                                
*                                                                               
RTFILDIR DS    A                   A(THIS SYSTEM DIRECTORY ENTRY)               
RTFILREC DS    A                   A(THIS SYSTEM FILE ENTRY)                    
*                                                                               
FLAG     DS    XL1                                                              
FLGDEF   EQU   X'80'               DISPLAYING DEFAULT                           
FLGBNF   EQU   X'40'               BANK RECD NOT FOUND ON CONTROL FILE          
*                                                                               
DSLIST   DS    0D                  DICTIONARY EQUATES USED                      
AC@YES   DS    CL3                                                              
AC@NO    DS    CL2                                                              
AC@NF100 DS    CL29                (DEFAULT CONTACT INFORMATION)                
AC@ID    DS    CL2                                                              
AC@NAME  DS    CL4                                                              
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACFIL13   03/23/10'                                      
         END                                                                    
