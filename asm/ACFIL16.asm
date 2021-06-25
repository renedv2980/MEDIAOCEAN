*          DATA SET ACFIL16    AT LEVEL 004 AS OF 08/10/11                      
*&&      SET   NOP=N                                                            
*PHASE T62316C,*                                                                
         SPACE 1                                                                
FIL16    TITLE 'SALES/USE TAX RECORD'                                           
         SPACE 2                                                                
FIL16    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL16**,R7,RR=RE                                              
         USING WORKD,R9                                                         
         L     RA,ATWA                                                          
         USING TWAD,RA                                                          
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
EXITNVRC MVC   FVMSGNO,=AL2(AE$INREC)     INVALID RECORD                        
         LH    R0,GSDSPREC                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR     SET CURSOR TO RECORD FIELDD                   
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
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     DS    0H                                                               
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
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
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
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
         USING SUTRECD,R2                                                       
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
KFKVAL   MVC   SUTKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   SUTKTYP,SUTKTYPQ    X'2D'                                        
         MVI   SUTKSUB,SUTKSUBQ    X'01'                                        
         MVC   SUTKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   SUTKEY,BCSPACES     INITIALIZE KEY OF RECORD                     
         MVI   SUTKTYP,SUTKTYPQ    X'2D'                                        
         MVI   SUTKSUB,SUTKSUBQ    X'01'                                        
         MVC   SUTKCPY,CUABIN      CONNECTED ID                                 
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
         USING SUTRECD,R2                                                       
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
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD                                  *         
***********************************************************************         
         SPACE 1                                                                
RFADD    GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',SUTRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    RFADD02                                                          
         GOTO1 AADDRST,SUTRECD     ADD A RSTEL IF IT DOESN`T EXIST              
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
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    DS    0H                                                               
         MVC   IOKEY(L'SUTKEY),0(R2)                                            
         MVC   FVMSGNO,=AL2(AE$ACNVD)                                           
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         LA    R1,11               11 IS 2D01+CO CODE+8 BYTE TAX CODE           
         LA    RF,10(R2)           RF POINTS TO END OF KEY TO BE DEL            
RFDEL10  CLI   0(RF),C' '                                                       
         BH    RFDEL20                                                          
         BCTR  RF,0                                                             
         BCT   R1,RFDEL10                                                       
         DC    H'0'                BAD KEY                                      
RFDEL20  BCTR  R1,0                                                             
         LA    R4,IOKEY            R3 POINTS TO NEXT RECORD                     
         EXCLC R1,0(R4),0(R2)                                                   
         BNE   EXITOK                                                           
         MVC   FVADDR,ATXCODE      POINT TO TAX CODE FIELD                      
         B     EXITL                                                            
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
         USING SUTRECD,R2                                                       
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
         USING SUTRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(TX#CODE),AL4(TXCODE)    TAX CODE                             
         DC    AL2(TX#CODNM),AL4(TXCDNM)   TAX CODE NAME                        
         DC    AL2(TX#CRACCT),AL4(CRACCT)  CREDIT ACCOUNT                       
         DC    AL2(TX#CRNAME),AL4(CRACTNM) CREDIT ACCOUNT NAME                  
         DC    AL2(TX#EFFDT),AL4(EFFDTE)   EFFECTIVE DATE                       
         DC    AL2(TX#RATE),AL4(TXRTE)     TAX RATE                             
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL16    CSECT                                                                  
*              VALID CREDIT U/L                                                 
CREDLST  DC    C'SBSCSVSXSE',X'FF'                                              
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
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   DS    0H                                                               
         MVI   COUNT,0                                                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAX CODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TXCODE   LA    RF,TXCDTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TXCDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCDE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCDE)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCDE)                                 
*        DC    AL1(DSET),AL1(0,0,0),AL4(DSETCDE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCDE)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCDE)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCDE)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
*SETCDE  DS    0H                                                               
*        B     FLTXX               UNPROTECT FIELD                              
*        SPACE 2                                                                
***********************************************************************         
* DISPLAY A TAX CODE                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISCDE   MVC   FVIFLD(L'SUTKLOC),SUTKLOC                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TAX CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALCDE   CLI   FVILEN,0            ANY INPUT ?                                  
         BE    EXITNO                                                           
         CLI   FVILEN,6            IF ENTERED 6 CHARS FINE                      
         BE    VCDE05              ELSE MAKE SURE THEY ENTERED AN EVEN          
         SR    RE,RE               # OF CHARACTERS BY DIVIDING BY THE           
         LA    RF,8                MAX WHICH IS 8.                              
         ZIC   R1,FVILEN                                                        
         DR    RE,R1               DIVIDE BY THE INPUT LENGTH                   
         LTR   RE,RE               IF REMAINDER THAN DIDN'T ENTER AN            
         BZ    VCDE05              EVEN # OF CHARACTERS.                        
         MVC   FVMSGNO,=AL2(AE$TAXLN)                                           
         B     EXITL                                                            
*                                                                               
VCDE05   NI    BIT,X'FF'-NEWKEY                                                 
         MVC   ATXCODE,FVADDR      SAVE A(TAX CODE FIELD)                       
         MVC   SVTXCODE,BCSPACES                                                
         MVC   SVTXCODE(2),FVIFLD    SAVE THE TAX CODE                          
         ZIC   R3,FVILEN                                                        
         AHI   R3,-2               IF 2 THAN FIRST LEVEL CODE                   
         BNP   VCDE10              SO DON'T READ FOR HIGHER LEVELS              
         BAS   RE,RDTAX            MAKE SURE HIGHER LVL CODE IS THERE           
         BNE   EXITL                                                            
*                                                                               
         MVC   SVTXCODE(4),FVIFLD                                               
         AHI   R3,-2               CHECK FOR NEXT LEVEL                         
         BNP   VCDE10                                                           
         BAS   RE,RDTAX                                                         
         BNE   EXITL                                                            
*                                                                               
         MVC   SVTXCODE(6),FVIFLD                                               
         AHI   R3,-2               CHECK FOR NEXT LEVEL                         
         BNP   VCDE10                                                           
         BAS   RE,RDTAX                                                         
         BNE   EXITL                                                            
*                                                                               
VCDE10   ZIC   RF,FVXLEN                                                        
         EXMVC RF,SUTKLOC,FVIFLD   NOW MOVE IN FULL TAX CODE TO KEY             
         CLC   SAVEKEY(L'SUTKEY),0(R2)                                          
         BE    *+8                                                              
         OI    BIT,NEWKEY                                                       
         MVC   SAVEKEY(L'SUTKEY),0(R2)                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TAX CODE FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTCDE  MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'SUTKLOC),FLTIFLD                                        
         MVC   SUTKLOC,FLTIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TAX CODE FILTER FIELD                                              
***********************************************************************         
         SPACE 1                                                                
VFLTCDE  MVC    FLTIFLD,BCSPACES                                                
         ZIC    R1,FVXLEN                                                       
         EXMVC  R1,FLTIFLD,FVIFLD                                               
         EXMVC  R1,SUTKLOC,FVIFLD                                               
         B      EXITOK                                                          
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTCDE  CLI   FLTIFLD,0           ANY FILTER?                                  
         BE    FLTXE                                                            
*                                                                               
         CLC   SUTKLOC,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* READ FOR HIGHER LEVEL TAX CODE                                                
***********************************************************************         
         SPACE 1                                                                
T        USING SUTRECD,R5                                                       
RDTAX    NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   IOKEY,BCSPACES                                                   
         MVI   T.SUTKTYP,SUTKTYPQ                                               
         MVI   T.SUTKSUB,SUTKSUBQ                                               
         MVC   T.SUTKCPY,CUABIN    COMPANY                                      
         MVC   T.SUTKLOC,SVTXCODE      MOVE IN SAVED TAX CODE                   
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$HLACM) HIGHER LEVEL ACCT MISSING                 
         B     EXITL                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A TAX CODE NAME                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
TXCDNM   LA    RF,TXCNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TXCNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXCN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTXCN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TAX CODE NAME                                             *         
***********************************************************************         
         SPACE 1                                                                
DISTXCN  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',SUTRECD),0               
         CLI   12(R1),0            NAME ON RECORD?                              
         BNE   EXITOK              NO                                           
*                                                                               
         USING NAMELD,RF                                                        
         L     RF,12(R1)                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EXMVC R1,FVIFLD,NAMEREC                                                
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A TAX CODE NAME                                            *         
***********************************************************************         
         SPACE 1                                                                
VALTXCN  MVC   ATXNAME,FVADDR      SAVE ADDRESS OF FIELD                        
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('NAMELQ',AIOREC),0                
         NI    BIT,X'FF'-GOTNAME                                                
         CLI   FVILEN,0            IF NO NAME ENTERED WILL USE THE              
         BE    EXITOK              NAME FROM THE CREDIT ACCT                    
*                                                                               
         ZIC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,UC@DEL                                                 
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVNM)    DELETE IS NOT A VALID NAME             
         B     EXITL                                                            
*                                                                               
         USING NAMELD,R4                                                        
         LA    R4,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   NAMEL,NAMELQ        X'20'                                        
         ZIC   R1,FVILEN                                                        
         AH    R1,=Y(NAMLN1Q)                                                   
         STC   R1,NAMLN                                                         
         BCTR  R1,0                                                             
         EXMVC R1,NAMEREC,FVIFLD                                                
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),SUTRECD,BOELEM,0                   
         OI    BIT,GOTNAME         SET BIT THAT NAME ENTERED                    
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CREDIT ACCOUNT                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CRACCT   LA    RF,CRTBL            TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CRTBL    DC    AL1(DDIS),AL1(0,0,0),AL4(DISCR)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCR)                                  
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCR)                                  
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCR)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALCR)                                 
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCR)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCR)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CREDIT ACCOUNT                                            *         
***********************************************************************         
         SPACE 1                                                                
DISCR    CLI   CSACT,A#ADD         CR ACCT IS NOT STORED IN TSAR REC            
         BNE   DISCR10             BUT IS NOT IT'S OWN ELEM SO FOR ACT          
         CLC   SVCRACC,BCSPACES    ADD MUST GET FROM SVCRACC BECAUSE            
         BE    EXITOK              SUTELQ ELEM DOES NOT EXIST YET               
         MVC   FVIFLD(L'SVCRACC),SVCRACC                                        
         B     EXITOK                                                           
*                                                                               
DISCR10  CLI   CSACT,A#DIS         IF ACTION DISPLAY OR LIST                    
         BE    DISCR12             GET FROM RECORD                              
         CLI   CSACT,A#LST                                                      
         BE    DISCR12                                                          
         TM    BIT,NEWKEY          CHANGE OF KEY?                               
         BO    DISCR12             THAN MUST GET FROM RECORD                    
         CLC   SVCRACC,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVCRACC),SVCRACC                                        
         B     EXITOK                                                           
*                                                                               
T        USING SUTRECD,R3                                                       
DISCR12  LR    R3,R2               R3 POINTS TO RECORD                          
         LA    R4,T.SUTRECD+(T.SUTRFST-T.SUTRECD)                               
*                                                                               
DISCR15  CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         CLI   0(R4),X'5F'         SALES TAX ELEMENT                            
         BE    DISCR20                                                          
DISCRNX  ZIC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DISCR15                                                          
*                                                                               
         USING SUTELD,R4                                                        
DISCR20  CLI   SUTLN,SUTLN2Q       IF THIS LEN THAN THIS HAS THE CR ACC         
         BNE   DISCRNX                                                          
*                                                                               
DISCR25  DS    0H                                                               
         USING ACTRECD,R5                                                       
         LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'ACTKULA),SUTACC                                        
         GOTO1 AGETACT,0           READ ACCT/TEST SECURITY                      
         BNE   EXITL                                                            
         MVC   FVIFLD(L'SUTACC),SUTACC MOVE IN CREDIT ACCT                      
         MVC   SVCRACC,SUTACC                                                   
         B     EXITOK                                                           
         DROP  T,R4,R5                                                          
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CREDIT ACCOUNT                                           *         
***********************************************************************         
         SPACE 1                                                                
VALCR    MVC   SVCRACC,BCSPACES                                                 
         LA    R1,CREDLST          TABLE OF VALID LEDGERS                       
         MVC   FVMSGNO,=AL2(AE$ULVRT) U/L NOT VALID FOR THIS REC TYPE           
VCR10    CLC   0(2,R1),FVIFLD      MATCH ON UNIT/LEDGER                         
         BE    VCR20                                                            
         LA    R1,2(R1)                                                         
         CLI   0(R1),X'FF'                                                      
         BE    EXITL                                                            
         B     VCR10                                                            
*                                                                               
         USING ACTRECD,R5                                                       
VCR20    LA    R5,IOKEY            VALIDATE THE ACCOUNT                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,ACTKUNT,FVIFLD                                                
         GOTO1 AGETACT,0           READ ACCT/TEST SECURITY                      
         BNE   EXITL                                                            
*        LHI   R1,XOREAD+XOACCDIR+XIO2                                          
*        GOTO1 AIO                                                              
*        BE    *+14                                                             
*        MVC   FVMSGNO,=AL2(AE$INACC) INVALID ACCOUNT                           
*        B     EXITL                                                            
*        LHI   R1,XOGET+XOACCMST+XIO2                                           
*        GOTO1 AIO                                                              
*        BE    *+6                                                              
*        DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('ABLELQ',AIO1),0                  
         CLI   12(R1),0            32 BALANCE ELEMENT?                          
         BE    VCR30               NO                                           
         MVC   FVMSGNO,=AL2(AE$INACP) INVALID ACCOUNT FOR POSTING               
         B     EXITL                                                            
*                                                                               
VCR30    ZIC   RF,FVXLEN                                                        
         EXMVC RF,SVCRACC,FVIFLD   SAVE THE CR ACCT                             
         OC    SVCRACC,BCSPACES                                                 
         ZIC   RF,FVXLEN                                                        
         EXMVC RF,FLTIFLD,FVIFLD   MOVE INTO FILTER FIELD TOO                   
         OC    FLTIFLD,BCSPACES                                                 
*                                                                               
         TM    BIT,GOTNAME         WAS THE NAME ENTERED?                        
         BO    EXITOK              YES-NO NEED TO GET IT FROM CR ACCT           
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0            NAME ON RECORD?                              
         BNE   EXITOK              NO                                           
*                                                                               
         XC    BOELEM,BOELEM                                                    
         USING NAMELD,RF                                                        
         L     RF,12(R1)                                                        
         ZIC   R1,NAMLN                                                         
         BCTR  R1,0                                                             
         EXMVC R1,BOELEM,0(RF)     COPY THE NAME ELEMENT                        
         CLI   CSACT,A#LST         FOR ACTION LIST DON'T MOVE NAME              
         BE    VCR40               TO SCREEN                                    
*                                                                               
         USING FHD,R3                                                           
         L     R3,ATXNAME          A(TAX NAME FIELD)                            
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EXMVC R1,FHDA,NAMEREC     MOVE IN NAME TO SCREEN                       
         LA    R1,1(R1)                                                         
         STC   R1,FHIL             SET LENGTH                                   
         OI    FHOI,FHOITR         AND TRANSMIT                                 
*                                                                               
VCR40    GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),SUTRECD,BOELEM,0                   
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CREDIT ACCOUNT                                          *         
***********************************************************************         
         SPACE 1                                                                
SRCHCR   CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              DON'T SEARCH(MESSES UP FILTERING)            
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,(0,0)            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CREDIT ACCOUNT FILTER FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTCR   MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'SUTACC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR CREDIT ACCOUNT                                     *         
***********************************************************************         
         SPACE 1                                                                
DOFTCR   CLI   FLTIFLD,0                 ANY FILTER?                            
         BE    FLTXE                     NO                                     
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SUTELQ',SUTRECD),0               
         CLI   12(R1),0            SALES TAX ELEMENT?                           
         BNE   FLTXE               NO                                           
*                                                                               
         USING SUTELD,RF                                                        
         L     RF,12(R1)                                                        
         CLC   SUTACC,FLTIFLD                                                   
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CREDIT ACCOUNT NAME                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CRACTNM  LA    RF,CRNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CRNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCRN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CREDIT ACCOUNT NAME                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING ACTRECD,R5                                                       
         USING FHD,R4                                                           
DISCRN   DS    0H                                                               
         LA    R5,IOKEY            VALIDATE THE ACCOUNT                         
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
*&&DO                                                                           
         ZIC   R1,FHIL                                                          
         BCTR  R1,0                                                             
         LTR   R1,R1                                                            
         BZ    EXITOK                                                           
         EXMVC R1,ACTKULA,FHDA                                                  
*&&                                                                             
         MVC   ACTKULA(L'SVCRACC),SVCRACC                                       
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0            NAME ON RECORD?                              
         BNE   EXITOK              NO                                           
*                                                                               
         USING NAMELD,RF                                                        
         L     RF,12(R1)                                                        
         ZIC   R1,NAMLN                                                         
         SH    R1,=Y(NAMLN1Q+1)                                                 
         EXMVC R1,FVIFLD,NAMEREC                                                
         B     EXITOK                                                           
         DROP  R4,R5                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR EFFECTIVE DATE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
EFFDTE   LA    RF,EFFTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
EFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDT)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A EFFECTIVE DATE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISEDT   OC    TLKTDTE,TLKTDTE                                                  
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BODMCB,(1,TLKTDTE),(8,FVIFLD)                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A EFFECTIVE DATE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALEDT   MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         XC    TLKTDTE,TLKTDTE                                                  
         B     EXITOK                                                           
*                                                                               
         ZIC   RF,FVXLEN                                                        
         EXCLC RF,FVIFLD,UC@DEL    ENTERED DELETE?                              
         BNE   VALEDT5                                                          
         XC    TLKTDTE,TLKTDTE                                                  
         XC    TLKTAMT,TLKTAMT                                                  
         OI    LSLNIND1,LSLNIDEL   YES SO DELETE THIS DATE AND RATE             
         B     EXITOK                                                           
*                                                                               
VALEDT5  OC    TLKTDTE,TLKTDTE     IF DATE IS THERE MUST BE CHANGING            
         BNZ   VALED10             AN EXISTING 1 SO DON'T CHK MAX #             
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1          FIRST LIST REC #                             
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,4                4 RATES MAX                                  
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXRC)                                           
         B     EXITL                                                            
*                                                                               
VALED10  XC    TLKTDTE,TLKTDTE                                                  
         GOTO1 VDATVAL,BODMCB,FVIFLD,BODUB1                                     
         OC    0(4,R1),0(R1)                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDAT)                                           
         B     EXITL               INVAILD DATE                                 
         GOTO1 VDATCON,BODMCB,BODUB1,(1,TLKTDTE)                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RATE FIELD                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
TXRTE    LA    RF,TXRTTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
TXRTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISRAT1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRAT1)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RATE                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRAT1  OC    TLKTAMT,TLKTAMT                                                  
         BZ    EXITOK                                                           
         EDIT  (P4,TLKTAMT),(7,FVIFLD),4,DROP=3,ALIGN=LEFT,DUB=BODUB1, +        
               WRK=BOWORK1                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RATE                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALRAT1  CLI   FVILEN,0                                                         
         BNE   VRAT10                                                           
         CLI   CSACT,A#ADD                                                      
         BE    EXITNO                                                           
         OC    TLKTDTE,TLKTDTE     IF ENTERED AN EFFECTIVE DATE                 
         BNZ   EXITNO              THAN MUST ENTERE A RATE ELSE                 
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
VRAT10   OC    TLKTDTE,TLKTDTE     IF ENTERED A RATE BUT NO DATE                
         BNZ   VRAT12              FILL IN TODAY'S DATE                         
         GOTO1 VDATCON,BODMCB,(5,0),(1,TLKTDTE) DEFAULT IS TODAY                
VRAT12   OC    TLKTAMT,TLKTAMT  IF RATE ALREADY THERE MUST BE CHANGING          
         BNZ   VRAT15           AN EXISTING RATE SO DON'T CHK FOR MAX           
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,4                4 RATES MAX                                  
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXRC)                                           
         B     EXITL                                                            
VRAT15   SR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         GOTO1 VCASHVAL,BODMCB,(4,FVIFLD),(RF)                                  
         CLI   BODMCB,FF                                                        
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXNM)                                           
         B     EXITL                                                            
         L     RF,BODMCB+4                                                      
         CVD   RF,BODUB1                                                        
         MVC   FVMSGNO,=AL2(AE$INAMT)  INVALID AMOUNT                           
         CP    BODUB1,=P'1'                                                     
         BNH   EXITL                                                            
         CP    BODUB1,=P'999999'                                                
         BH    EXITL                                                            
*                                                                               
         ZAP   TLKTAMT,BODUB1                                                   
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
*                                                                     *         
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
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#RATE         RATES RECORD                                 
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
         BNE   EXITH                                                            
         NI    SNINDS1,FF-SNIUSECR   TURN OFF USE CURRENT RECORD BIT            
         MVI   LSLTIND1,0          TURN OFF LIST INDICATORS                     
         OI    LSSCIND1,LSSCIBLD   AND REBUILD LIST                             
         B     EXITOK                                                           
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
         USING SUTRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING SUTRECD,R2                                                       
LAST     USING SUTRECD,R3                                                       
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
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'SUTKEY),THIS.SUTKEY                                      
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
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(SUTKLOC-SUTRECD),THIS.SUTRECD                              
         BNE   EXITL               CHANGE COMPANY                               
*                                                                               
         MVC   SAVEKEY,IOKEY         SAVE THE KEY                               
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 AGETEL,BOPARM,('SUTELQ',AIO1),0                                  
         BNE   NLST04                                                           
*                                                                               
         USING ACTRECD,R5                                                       
         USING SUTELD,RF                                                        
         LA    R5,IOKEY                                                         
         LA    RF,BOELEM                                                        
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'ACTKULA),SUTACC                                        
         GOTO1 AGETACT,0                 READ ACCT/TEST SECURITY                
         MVC   IOKEY,SAVEKEY             RESTORE KEY                            
         MVC   FVXTRA,BCSPACES     NO ACCESS SO READ NEXT RECORD                
         BE    NLST04                                                           
         LHI   R1,XOREAD+XOACCDIR+XIO1  RESTORE ORIGINAL READ                   
         GOTO1 AIO                                                              
         B     NLST                                                             
*                                                                               
NLST04   LHI   R1,XOREAD+XOACCDIR+XIO1  RESTORE ORIGINAL READ                   
         GOTO1 AIO                                                              
         MVC   THIS.SUTKEY(ACCKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS,LAST,R5,RF                                                  
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ+LSS1HEAD                                 
         MVC   LSCOLLIN,=AL2(80)   NUMBER OF COLUMNS PER LIST LINE              
         MVC   LSLINROW,=AL2(1)    NUMBER OF LIST LINES PER ROW                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1                                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,SUTRFST-SUTRECD                                               
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
         LA    RF,SUTRFST-SUTRECD(RF) IT IS NOW.                                
         XR    RE,RE                                                            
*                                                                               
         USING SUTELD,RF                                                        
FML02    CLI   SUTEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
         CLI   SUTEL,SUTELQ        SUTEL?                                       
         BNE   NML04               NO                                           
                                                                                
FML04    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
NLST1    LH    RF,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         XR    RE,RE                                                            
         C     RF,AIOREC           MAKE SURE MNTDISP INITIALISED                
         BH    NML04                                                            
         LA    RF,SUTRFST-SUTRECD(RF) IT IS NOW.                                
*                                                                               
         USING SUTELD,RF                                                        
NML02    CLI   SUTEL,0             RECORD END?                                  
         BE    EXITL               YES                                          
NML03    CLI   SUTEL,SUTELQ        SUTEL?                                       
         BE    NML06               YES                                          
                                                                                
NML04    IC    RE,SUTLN                                                         
         LA    RF,0(RE,RF)                                                      
         B     NML02                                                            
*                                                                               
NML06    S     RF,AIOREC                                                        
         STH   RF,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         LH    RF,MNTDISP                                                       
         A     RF,AIOREC                                                        
         USING SUTELD,RF           MOVE IN DETAILS FROM ELEMENT                 
         MVC   TLKTDTE,SUTEFF      EFFECTIVE DATE (PWOS)                        
         MVC   TLKTAMT,SUTRTE      CHARGE RATE (4DP)                            
         B     EXITOK                                                           
         DROP  RF,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYELEMS,NO         RESET BIT                                    
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BNE   EXITOK              A MAIN ACTION OF CHANGE                      
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('SUTELQ',AIOREC),0                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
UPDLAST1 DS    0H                                                               
         CLI   CSACT,A#CHA                                                      
         BE    *+12                                                             
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
         CLI   ANYELEMS,YES        ANY ELEMENTS IN THE RECORD                   
         BE    EXITOK              YES - GOOD                                   
         LH    R0,LS1STLIN                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         B     EXITNO              NO INPUT                                     
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING SUTRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  LM    R2,R3,SVPARMS3                                                   
         CLI   CSACT,A#CHA         ONLY UPDATE ELEMENT IF WE HAVE               
         BE    *+12                A MAIN ACTION OF CHANGE OR ADD               
         CLI   CSACT,A#ADD                                                      
         BNE   EXITOK                                                           
                                                                                
         MVI   ANYELEMS,YES                                                     
         USING SUTELD,R4                                                        
         LA    R4,BOELEM                                                        
         XC    BOELEM,BOELEM       MOVE IN DETAILS TO ELEMENT                   
         MVI   SUTEL,SUTELQ                                                     
         MVI   SUTLN,SUTLN1Q       LENGTH OF ELEM W/O CREDIT ACCT               
         CLI   COUNT,0             IF FIRST ELEM THAN USE BIGGER LEN            
         BNE   *+8                 TO ACCOMMODATE CREDIT ACCT                   
         MVI   SUTLN,SUTLN2Q                                                    
         MVC   SUTEFF,TLKTDTE      EFFECTIVE DATE (PWOS)                        
         MVC   SUTRTE,TLKTAMT      CHARGE RATE (4DP)                            
         MVC   SUTACC,SVCRACC                                                   
*        CLI   COUNT,0             IF FIRST ELEM ADD CREDIT ACCT                
*        BNE   *+10                                                             
         ZIC   R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
*                                                                               
         GOTO1 AADDEL,BOPARM,AIOREC                                             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
EFFALL   DC    32X'FF'                                                          
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#DEL,L'UC@DEL,L                                                
DCLISTX  DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORKA                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
ATXNAME  DS    A                   A(TAX CODE NAME FIELD)                       
ATXCODE  DS    A                   A(TAX CODE FIELD)                            
MNTDISP  DS    H                                                                
*                                                                               
SVCRACC  DS    CL14                SAVED CREDIT ACCOUNT                         
SVTXCODE DS    CL8                 SAVED TAX CODE                               
SVTXDTE  DS    CL(L'SUTEFF)        SAVED TAX DATE                               
ANYELEMS DS    CL1                 DO WE HAVE ANY ELEMENTS                      
COUNT    DS    XL1                                                              
BIT      DS    XL1                                                              
GOTNAME  EQU   X'80'               NAME ENTERED(DON'T USE FROM CR ACCT)         
NEWKEY   EQU   X'40'               NEW KEY ENTERED                              
SAVEKEY  DS    CL(L'IOKEY)                                                      
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
         SPACE 2                                                                
OVERWRKN EQU   *-OVERWRKD                                                       
DSLISTU  DS    0D                  UPPERCASE FOR MATCHING                       
UC@DEL   DS    CL6                                                              
         EJECT                                                                  
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKTDTE  DS    CL(L'SUTEFF)        EFFECTIVE DATE (PWOS)                        
         ORG   TLUSER                                                           
TLKTAMT  DS    XL(L'SUTRTE)        TAX RATE (4DP)                               
TLLNQ     EQU   *-TLSTD                                                         
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004ACFIL16   08/10/11'                                      
         END                                                                    
