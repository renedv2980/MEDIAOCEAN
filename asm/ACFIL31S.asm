*          DATA SET ACFIL31S   AT LEVEL 093 AS OF 04/17/00                      
*&&      SET   NOP=N                                                            
*PHASE T62331A,*                                                                
****************************************************************                
*      DISPLAYS,ADDS AND LISTS MI (MEDIA INTERFACE) RECORDS    *                
*      FEATURE ADD/CHANGE/DELETE/LIST                          *                
****************************************************************                
         TITLE 'MEDIA RECORD OBJECT VERSION'                                    
         SPACE 2                                                                
FIL31    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL31**,R7,RR=RE                                              
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
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
*                                                                               
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
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
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
*                                                                               
         USING CPYRECD,R2                                                       
INIT     LA    R2,IOKEY                                                         
         MVC   CPYKEY,BCSPACES   READ COMPANY RECORD                            
         MVC   CPYKCPY,CUABIN      CONNECTED ID                                 
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD COMPANY RECORD                           
         GOTO1 AGETEL,BOPARM,('CPYELQ',AIO1),0                                  
         BE    *+6                                                              
         DC    H'0'                NO COMPANY ELEMENT                           
         MVC   SVCPYEL,BOELEM      SAVE COMPANY ELEMENT                         
         B     EXITOK                                                           
         DROP  R2                                                               
*                                                                               
***********************************************************************         
* TABLE  ITERATION ROUTINE - EXPECTS R1 TO HOLD EQUATED VERB          *         
*                          - EXPECTS RF TO HOLD A(TABLE)              *         
***********************************************************************         
*                                                                               
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
*                                                                               
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                  *         
*                                                                     *         
* P1 HOLDS EQUATED VERB                                               *         
***********************************************************************         
*                                                                               
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
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
*                                                                               
KEY      LM    R0,R3,SVPARMS                                                    
         USING MINRECD,R2          MEDIA INTERFACE RECORD                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
*                                                                               
KEYFRST  L     R1,SVPARMS4         TABLE OF KNOWN INVOKERS                      
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
*                                                                               
KFKVAL   MVC   MINKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   MINKTYP,MINKTYPQ    X'08' RECORD                                 
         MVC   MINKCPY,CUABIN      CONNECTED ID AGENCY BINARY                   
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
*                                                                               
KFKFVAL  DS    0H                                                               
         MVC   MINKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   MINKTYP,MINKTYPQ    X'08' RECORD                                 
         MVC   MINKCPY,CUABIN      CONNECTED ID AGENCY BINARY                   
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
*                                                                               
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING MINRECD,R2                                                       
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
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING MINRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
         BR    RF                                                               
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
*                                                                               
KNOWTAB  DC    AL2(MI#MCODE),AL4(MCODE)    MEDIA CODE                           
         DC    AL2(MI#DESC),AL4(MDESC)     DESCRIPTION                          
         DC    AL2(MI#CMACC),AL4(CACC)     COMMISION ACCOUNT                    
         DC    AL2(MI#CMNAM),AL4(NAMDTA)    COMMISION ACCOUNT NAME              
         DC    AL2(MI#CDSACC),AL4(CDACC)   CASH DISC. ACCOUNT                   
         DC    AL2(MI#CDSNAM),AL4(NAMDTA)  CASH DISC. ACCOUNT NAME              
         DC    AL2(MI#LCDACC),AL4(LCDACC)  LOST C.D.  ACCOUNT                   
         DC    AL2(MI#LCDNAM),AL4(NAMDTA)  LOST C.D.  ACCOUNT NAME              
         DC    AL2(MI#CRACC),AL4(CRACC)    CASH RECEIPTS ACCOUNT                
         DC    AL2(MI#CRNAM),AL4(NAMDTA)   CASH RECEIPTS ACCOUNT NAME           
         DC    AL2(MI#CCRACC),AL4(CCRACC)  CANADIAN C.R. ACCOUNT                
         DC    AL2(MI#CCRNAM),AL4(NAMDTA)  CANADIAN C.R. ACCOUNT NAME           
         DC    AL2(MI#NPACC),AL4(NPACC)    NET POSTING   ACCOUNT                
         DC    AL2(MI#NPNAM),AL4(NAMDTA)   NET POSTING   ACCOUNT NAME           
         DC    AL2(MI#COACC),AL4(COACC)    COMMISSION ONLY ACCOUNT              
         DC    AL2(MI#CONAM),AL4(NAMDTA)   COMMISSION ONLY ACCOUNT NAME         
         DC    AL2(MI#ANLACC),AL4(ANLACC)  ANALYSIS ACCOUNT                     
*                                                                               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL31    CSECT                                                                  
*                                                                               
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
*                                                                               
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
*                                                                               
DFDVAL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         GOTO1 AADDRST,MINRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MEDIA CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
MCODE    LA    RF,MCODETBL                                                      
         B     ITER                                                             
*                                                                               
MCODETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISMCDE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMCDE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTMCDE)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTMCDE)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTMCDE)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY A MEDIA CODE                                                          
***********************************************************************         
*                                                                               
DISMCDE  MVC   FVIFLD(L'MINKMED),MINKMED                                        
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE THE MEDIA CODE                                                       
***********************************************************************         
*                                                                               
VALMCDE  DS    0H                                                               
*                                                                               
* READ RECORD WITH MEDIA CODE FROM THE SCREEN                                   
*                                                                               
         MVC   MINKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   MINKTYP,MINKTYPQ    X'08' RECORD                                 
         MVC   MINKCPY,CUABIN      CONNECTED ID AGENCY BINARY                   
         MVC   MINKMED,FVIFLD      MOVE IN MEDIA CODE TO READ                   
         MVC   SVMCODE,FVIFLD      SAVE OF MEDIA CODE                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LIST CODE FILTER FIELD                                              
***********************************************************************         
*                                                                               
DFLTMCDE MVC   FVIFLD(L'MINKMED),FLTIFLD                                        
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE A MEDIA CODE FILTER FIELD                                            
***********************************************************************         
*                                                                               
VFLTMCDE DS    0H                                                               
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN LIST CODE TO FILTER FIELD            
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DO FILTERING ON MEDIA CODE                                                    
***********************************************************************         
*                                                                               
DOFTMCDE CLC   MINKMED,BCSPACES  IS THERE A CODE TO COMPARE ON?                 
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   MINKMED,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DESCRIPTION                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
MDESC    LA    RF,DESCTBL                                                       
         B     ITER                                                             
*                                                                               
DESCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDESC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDESC)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE DESCRIPTION                                                       
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
DISDESC  GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVIFLD(L'MDIDESC),MDIDESC                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE DESCRIPTION                                     *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALDESC  DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         XC    BOELEM,BOELEM       CLEAR ELEMENT BUILD AREA                     
         MVI   MDIEL,MDIELQ        BUILD X'19' ELEMENT IN BOELEM                
         MVI   MDILN,MDILNQ        MOVE IN LENGTH                               
         MVC   MDICODE,SVMCODE     PUT IN MEDIA CODE TYPED IN BY USER           
*                                                                               
         CLI   FVILEN,0            DID USER INPUT DESCRIPTION                   
         BE    EXITNO                                                           
         MVC   MDIDESC,FVIFLD                                                   
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMISION ACCOUNT                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CACC     LA    RF,CACCTBL                                                       
         B     ITER                                                             
*                                                                               
CACCTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCACC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCACC)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY THE COMMISION ACCOUNT                                                 
***********************************************************************         
         USING MDIELD,R5           DSECT TO COVER MEDIA INTERFACE ELEM          
         USING ACTRECD,R4                                                       
*                                                                               
DISCACC  DS    0H                                                               
         LA    R5,BOELEM                                                        
         MVC   IOKEY,BCSPACES                                                   
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FVIFLD(L'MDICOMM),MDICOMM                                        
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICOMM),MDICOMM                                       
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE COMMISSION ACCOUNT                              *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALCACC  DS    0H                                                               
*                                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITNO                                                           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
*                                                                               
         MVC   MDICOMM,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CASH DISC ACCOUNT                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CDACC    LA    RF,CDACCTBL                                                      
         B     ITER                                                             
*                                                                               
CDACCTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCDACC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCDAC)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE CASH DISC ACCOUNT                                                 
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
*                                                                               
DISCDACC DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDICSHD,0           IS CASH DISCOUNT ACCOUNT EXIST               
         BE    EXITOK                                                           
         MVC   FVIFLD(L'MDICSHD),MDICSHD                                        
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICSHD),MDICSHD                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE CASH DISC  ACCOUNT                              *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALCDAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
         MVC   MDICSHD,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LOST C.D. ACCOUNT                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
LCDACC   LA    RF,LCDACTBL                                                      
         B     ITER                                                             
*                                                                               
LCDACTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCDAC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCDAC)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE LOST C.D. ACCOUNT                                                 
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
*                                                                               
DISLCDAC DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDILOST,0           IS LOST C.D.  ACCOUNT EXIST                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'MDILOST),MDILOST                                        
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDILOST),MDILOST                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE LOST C.D.  ACCOUNT                              *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALLCDAC DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
         MVC   MDILOST,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR      CASH RECEIPTS ACCOUNT                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CRACC    LA    RF,CRACTBL                                                       
         B     ITER                                                             
*                                                                               
CRACTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCRAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCRAC)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE CASH RECEIPTS ACCOUNT                                   *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
*                                                                               
DISCRAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDICSHR,0           IS CASH RECEIPTS ACCOUNT EXIST               
         BE    EXITOK                                                           
         MVC   FVIFLD(L'MDICSHR),MDICSHR                                        
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICSHR),MDICSHR                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE CASH RECEIPTS ACCOUNT                           *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALCRAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
         MVC   MDICSHR,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CANADIAN CASH RECEIPTS ACCOUNT                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
CCRACC   LA    RF,CCRACTBL                                                      
         B     ITER                                                             
*                                                                               
CCRACTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCCRAC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCCRAC)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE CANADIAN CASH RECEIPTS ACCOUNT                          *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
*                                                                               
DISCCRAC DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDICCSR,0            IS CASH RECEIPTS ACCOUNT EXIST              
         BE    EXITOK                                                           
         MVC   FVIFLD(L'MDICCSR),MDICCSR                                        
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICCSR),MDICCSR                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE CANADIAN CASH RECEIPTS ACCOUNT                  *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALCCRAC DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
         MVC   MDICCSR,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NET POSTING / MEDIA CONTROL ACCOUNT                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
NPACC    LA    RF,NPACTBL                                                       
         B     ITER                                                             
*                                                                               
NPACTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISNPAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNPAC)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE NET POSTING / MEDIA CONTROL ACCOUNT                     *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
*                                                                               
DISNPAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDICNTL,0            IS NET POSTING/MEDIA CONTROL EXIST          
         BE    EXITOK                                                           
         MVC   FVIFLD(L'MDICNTL),MDICNTL                                        
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICNTL),MDICNTL                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE NET POSTING / MEDIA CONTROL ACCOUNT             *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALNPAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
         MVC   MDICNTL,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COMMISSION ONLY CONTROL ACCOUNT                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
COACC    LA    RF,COACTBL                                                       
         B     ITER                                                             
*                                                                               
COACTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOAC)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE COMMISSION ONLY CONTROL ACCOUNT                         *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
*                                                                               
DISCOAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDICONL,0            IS COMMISSION ONLY ACCOUNT EXIST            
         BE    EXITOK                                                           
         MVC   FVIFLD(L'MDICONL),MDICONL                                        
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICONL),MDICONL                                       
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE COMMISSION ONLY CONTROL ACCOUNT                 *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALCOAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
         MVC   MDICONL,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR COST ANALYSIS ACCOUNT                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
ANLACC   LA    RF,ANLACTBL                                                      
         B     ITER                                                             
*                                                                               
ANLACTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISANLAC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALANLAC)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE COST ANALYSIS ACCOUNT                                   *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
*                                                                               
DISANLAC DS    0H                                                               
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDICOST,0            IS COST ANALYSIS ACCOUNT EXIST              
         BE    EXITOK                                                           
         MVC   FVIFLD(L'MDICOST),MDICOST                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE COMMISSION ONLY CONTROL ACCOUNT                 *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALANLAC DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         LA    R4,IOKEY                                                         
         MVI   ACTKUNT,C'1'        CHK UNIT 1                                   
         MVI   ACTKLDG,C'1'        CHK LDGR 1                                   
         MVC   ACTKACT,FVIFLD                                                   
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
*                                                                               
         MVI   ACTKUNT,C'1'        CHK UNIT 1                                   
         MVI   ACTKLDG,C'2'        CHK LDGR 2                                   
         MVC   ACTKACT,FVIFLD                                                   
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
*                                                                               
         MVC   MDICOST,FVIFLD      PUT ACCOUNT IN BOELEM                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),MINRECD,BOELEM,0                   
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING NAMES OF ACCOUNTS                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
NAMDTA   LA    RF,NAMTBL                                                        
         B     ITER                                                             
*                                                                               
NAMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNAM)                                 
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY ACCOUNT NAMES                                               *         
***********************************************************************         
*                                                                               
DISNAM   CLC   IOKEY,BCSPACES      ACCOUNT NOT PRESENT                          
         BE    EXITOK                                                           
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BE    *+6                 ACCOUNT NOT FOUND NOT POSSIBLE               
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         L     R1,AIO1             A(ACCOUNT RECORD)                            
         GOTOX AGETNAM             GET ACCOUNT NAME                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE ACCOUNT SUBROUTINE                                         *         
* CHECKS WETHER USER INPUT VALID ACCOUNT AND IF HE DID THEN WETHER    *         
* BALANCE ELEMENT EXIST IN ACCOUNT IF NOT THEN EXIT WITH ERROR        *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING MDIELD,BOELEM                                                    
VALACC   NTR1                                                                   
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('MDIELQ',MINRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         LA    R4,IOKEY            BUILD KEY TO READ VALID ACCOUNT              
         MVC   ACTKCPY,CUABIN                                                   
*                                                                               
         L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(MI#ANLACC)                                             
         BE    VALACC10                                                         
*                                                                               
         MVC   ACTKULA,FVIFLD      UNIT/LEDGER BY INPUTED BY USER               
VALACC10 MVC   IOKEYSAV,IOKEY                                                   
*                                                                               
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         BE    VALACC20                                                         
         MVC   FVMSGNO,=AL2(AE$INACC)  INVALID ACCOUNT                          
         B     VALACCX                                                          
*                                                                               
VALACC20 TM    ACTKSTAT,ACTSABLP    DOES ACCOUNT HAS BALANCE ELM X'32'          
         BO    VALACC30                                                         
         CR    RB,RD                                                            
         MVC   FVMSGNO,=AL2(AE$INACP)  INVALID FOR POSTING                      
         B     VALACCX                                                          
*                                                                               
VALACC30 DS    0H                                                               
         CR    RB,RB                                                            
         MVC   IOKEY,IOKEYSAV      RESTORE IOKEY TO READ NAME OF RECORD         
VALACCX  XIT1                                                                   
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
*                                                                               
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING MINRECD,R2                                                       
LAST     USING MINRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
*                                                                               
FLST     MVC   IOKEY(L'MINKEY),THIS.MINRECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               MESS UP ON THE READ HIGH                     
         B     NLST02                                                           
*                                                                               
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
*                                                                               
NLST     LHI   R1,XOSEQ+XOACCDIR+XIO1                                           
         GOTO1 AIO                                                              
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(MINKMED-MINRECD),THIS.MINRECD                              
*        BNE   EXITL               NO MORE FOR THIS COMPANY                     
         BE    *+8                                                              
         B     EXITL                                                            
         MVC   THIS.MINKEY(L'MINKEY+L'MINKSTA+L'MINKDA),IOKEY                   
         B     EXITOK                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
*                                                                               
FTFLST   B     EXITOK                                                           
*                                                                               
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SCANMAX  EQU   40                  40 MAX ENTRIES PER LINE                      
*                                                                               
         EJECT                                                                  
*                                                                               
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
*                                                                               
*ACFILWORK                                                                      
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
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
SVREG    DS    A                   SAVED REGISTER                               
SAVRB    DS    F                                                                
DISPERR  DS    A                                                                
SVELADDR DS    A                   A(THIS ELEMENT)                              
*                                                                               
SVACLEL  DS    CL100               SAVE ACCOUNT LENGTHS ELEMENT                 
SCANBLK  DS    (SCANMAX)CL(SCBLKLQ)                                             
ENTLIST  DS    CL256               LIST OF CODES (FOR 1 LINE AT A TIME)         
SVCPYEL  DS    CL(CPYLN2Q)         SAVE COMPANY ELEMENT                         
SVTODAY  DS    CL6                 TODAYS DATE(EBCDIC)                          
SVEDATE  DS    XL3                 SAVED EXPIRY DATE                            
SVELDISP DS    XL4                 DISPLACEMENT TO THIS ELEMENT                 
ELEMDISP DS    XL4                 DISPLACEMENT INTO THIS ELEMENT               
GOODEL   DS    C                                                                
FOUND    DS    C                                                                
*                                                                               
LCONTROL DS    0CL27                                                            
AHEADER  DS    A                   A(LINE IN USE)                               
AFIELD   DS    A                   8 BEYOND HEADER OF LINE IN USE               
LADATA   DS    A                   A(DATA IN ELEMENT)                           
         DS    2A                                                               
LUNIT    DS    C                                                                
LLEDGER  DS    C                                                                
LLEVEL   DS    C                                                                
LTYPE    DS    C                                                                
FLDLEN   DS    C                                                                
ELEMLEN  DS    C                   REMAINING LENGTH OF ELEMENT                  
SVELEN   DS    C                   SAVED REMAINING LENGTH OF ELEMENT            
ITEMLEN  DS    C                                                                
SVITEMLN DS    C                   SAVED LENGTH OF DATA                         
SVMCODE  DS    CL2                 SAVED MEDIA CODE TYPED BY USER               
LFSTEL   DS    X                                                                
LINCOUNT DS    XL1                 # OF ITEMS IN SCANBLK LINE                   
BIT      DS    XL1                                                              
LVLUSE   EQU   X'80'               TO CHECK FOR DUPLICATE LVL='S                
ULUSE    EQU   X'40'               TO CHECK FOR DUPLICATE UL='S                 
*                                  AND UNIT/LEDGERS IN LIST                     
FIRSTLN  EQU   X'20'               NOT DOING THE FIRST DATA LINE                
DOUNLDG  EQU   X'10'               DISPLAYING UNIT AND LEDGER                   
DOLVL    EQU   X'08'               DISPLAYING LEVEL                             
DOACCT   EQU   X'04'               DISPLAYING ACCOUNT CODES                     
DONELIST EQU   X'02'               DONE DISPLAYING LIST                         
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'093ACFIL31S  04/17/00'                                      
         END                                                                    
