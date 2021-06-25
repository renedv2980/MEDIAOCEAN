*          DATA SET ACFIL31    AT LEVEL 016 AS OF 07/25/12                      
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
         LH    R4,=Y(TWUSER-TWAD)                                               
         AR    R4,RA                                                            
         USING TWUSER,R4                                                        
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
EXITMIF  MVC   FVMSGNO,=AL2(AE$MISIF)                                           
         B     EXITL               EXIT WITH MISSING INPUT FIELD                
EXITINUL MVC   FVMSGNO,=AL2(AE$ULNVF)  UNIT/LEDGER NOT VALID                    
         B     EXITL                                                            
EXITHLAM MVC   FVMSGNO,=AL2(AE$HLACM) HIGHER LEVEL ACCOUNT MISSING              
         B     EXITL                                                            
EXITSEC  MVC   FVMSGNO,=AL2(AE$SECLK) SECURITY LOCKOUT                          
         B     EXITL                                                            
EXITRECN MVC   FVMSGNO,=AL2(AE$RECNF) RECORD NOT FOUND                          
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
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
*                                                                               
INIT     DS    0H                                                               
         XC    FLAG,FLAG                                                        
         GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
*                                                                               
T        USING CPYRECD,IOKEY                                                    
         MVC   T.CPYKEY,BCSPACES   GET SJ CLI/PRO/JOB LENGTH                    
         MVC   T.CPYKCPY,CUABIN                                                 
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO1                                          
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                MISSING COMPANY RECORD                       
                                                                                
         L     RE,AIO1             LOCATE COMPANY ELEMENT                       
         LA    RE,CPYRFST-CPYRECD(RE)                                           
         USING CPXELD,RE                                                        
INIT02   CLI   CPXEL,0             TEST EOR                                     
         BE    INITX                                                            
         CLI   CPXEL,CPXELQ        TEST COMPANY EXTRA ELEMENT                   
         BE    *+16                                                             
         LLC   R0,CPXLN                                                         
         AR    RE,R0                                                            
         B     INIT02                                                           
         MVC   SVCPXST1,CPXSTAT1                                                
         TM    CPXSTAT1,CPXOFFMI   IS AGENCY ON OFF LEVEL MI REOCRD?            
         BZ    INITX                                                            
         DROP  RE                                                               
*                                                                               
         MVI   GSSKCODE,C'1'       CHANGE KEY SCREEN CODE                       
         MVI   GSSMCODE,C'1'       CHANGE MAINTENNANCE SCREEN CODE              
         MVI   GSSLCODE,C'1'       CHANGE LIST SCREEN CODE                      
INITX    B     EXITOK                                                           
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
         DC    AL2(MI#OFFCDE),AL4(OFFCDE)  OFFICE CODE                          
         DC    AL2(MI#OFFNME),AL4(OFFNAM)  OFFICE NAME FIELD                    
         DC    AL2(MI#DESC),AL4(MCNAME)    MEDIA CODE NAME                      
         DC    AL2(MI#CMACC),AL4(CACC)     COMMISION ACCOUNT                    
         DC    AL2(MI#NPACC),AL4(NPACC)    NET POSTING   ACCOUNT                
*                                                                               
         DC    AL2(MI#ANLACC),AL4(ANLACC)  BILL/REVENUE ANALYSIS ACCT           
         DC    AL2(MI#CDSACC),AL4(CDACC)   CASH DISC. ACCOUNT                   
*                                                                               
         DC    AL2(MI#CRACC),AL4(CRACC)    US-CASH RCPT (CK) ACCT               
         DC    AL2(MI#CCRACC),AL4(CCRACC)  CAN-CASH RCPT (CK) ACCT              
         DC    AL2(MI#COACC),AL4(COACC)    COMMISSION ONLY ACCOUNT              
*                                                                               
         DC    AL2(MI#ACCNAM),AL4(NAMDTA)  ACCOUNT NAME FIELDS                  
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
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
*                                                                               
DFDDIS   LA    R2,IOKEY                   MUST HAVE VALID MI CODE LEVEL         
         XC    IOKEY,IOKEY                                                      
         MVC   MINKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   MINKTYP,MINKTYPQ    X'08' RECORD                                 
         MVC   MINKCPY,CUABIN      CONNECTED ID AGENCY BINARY                   
         MVC   MINKMED,SVMCODE     MOVE IN MEDIA CODE TO READ                   
         L     R1,=A(XOACCMST+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 AGETEL,BOPARM,('MDIELQ',AIO2),0                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVELEM,BOELEM       SAVE HIGHER LEVEL TO DISP DEF VALS           
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
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
*                                                                               
         CLC   MINKOFF,BCSPACES                                                 
         BH    EXITOK                                                           
         XC    IOKEY,IOKEY                                                      
         MVC   IOKEY(L'MINKEY),0(R2)                                            
         L     R1,=A(XOACCMST+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
*                                                                               
         GOTO1 AGETEL,BOPARM,('MDIELQ',AIO2),0                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVELEM,BOELEM       SAVE HIGHER LEVEL TO DISP DEF VALS           
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
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
OFFCDE   LA    RF,OCODETBL                                                      
         B     ITER                                                             
*                                                                               
OCODETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISOCDE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOCDE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOCDE)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTOCDE)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOCDE)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY AN OFFICE CODE                                                        
***********************************************************************         
*                                                                               
DISOCDE  MVC   FVIFLD(L'MINKOFF),MINKOFF                                        
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE THE OFFICE CODE                                                      
***********************************************************************         
*                                                                               
VALOCDE  DS    0H                                                               
*                                                                               
* READ RECORD WITH OFFICE CODE FROM THE SCREEN                                  
*                                                                               
         CLI   FVILEN,0                   KEY FIELD, BUT OPTIONAL               
         BE    EXITOK                                                           
         MVC   MINKOFF,FVIFLD                                                   
*                                                                               
         LA    R2,IOKEY                   MUST HAVE VALID MI CODE LEVEL         
         XC    IOKEY,IOKEY                                                      
         MVC   MINKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   MINKTYP,MINKTYPQ    X'08' RECORD                                 
         MVC   MINKCPY,CUABIN      CONNECTED ID AGENCY BINARY                   
         MVC   MINKMED,SVMCODE     MOVE IN MEDIA CODE TO READ                   
         L     R1,=A(XOACCMST+XOREAD+XIO2)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITHLAM            ERROR EXIT WITH HIGHER LVL MISSING.          
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE                                  
         BNE   EXITSEC             INVALID OFFICE                               
*                                                                               
         GOTO1 AGETEL,BOPARM,('MDIELQ',AIO2),0                                  
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVELEM,BOELEM       SAVE HIGHER LEVEL TO DISP DEF VALS           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LIST OFFICE CODE FILTER FIELD                                       
***********************************************************************         
*                                                                               
DFLTOCDE MVC   FVIFLD(L'MINKOFF),FLTIFLD                                        
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* VALIDATE A OFFICE CODE FILTER FIELD                                           
***********************************************************************         
*                                                                               
VFLTOCDE DS    0H                                                               
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN LIST CODE TO FILTER FIELD            
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE                                  
         BNE   EXITL               INVALID OFFICE                               
         B     EXITOK                                                           
*                                                                               
***********************************************************************         
* DO FILTERING ON OFFICE CODE                                                   
***********************************************************************         
*                                                                               
DOFTOCDE CLC   MINKOFF,BCSPACES  IS THERE A CODE TO COMPARE ON?                 
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   MINKOFF,FLTIFLD                                                  
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
MCNAME   LA    RF,MNAMTBL                                                       
         B     ITER                                                             
*                                                                               
MNAMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMNAM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMNAM)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE DESCRIPTION / MEDIA NAME                                          
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
DISMNAM  GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
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
VALMNAM  DS    0H                                                               
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
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY THE COMMISION ACCOUNT                                                 
***********************************************************************         
         USING MDIELD,R5           DSECT TO COVER MEDIA INTERFACE ELEM          
         USING ACTRECD,R6                                                       
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
DISCACC  MVC   IOKEY,BCSPACES                                                   
         CLC   MINKOFF,BCSPACES                                                 
         BNH   DISCAC02                                                         
         MVC   FVIFLD(L'MDICOMM),HIGH.MDICOMM                                   
         OI    FVATRB,FVAPROT                                                   
         OI    FVOIND,FVOXMT                                                    
         CLC   HIGH.MDICOMM,BCSPACES                                            
         BNH   EXITOK                                                           
*                                                                               
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICOMM),HIGH.MDICOMM                                  
*                                                                               
         GOTO1 AGETACT,0                                                        
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
DISCAC02 LA    R5,BOELEM                                                        
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICOMM),MDICOMM                                       
*                                                                               
         GOTO1 AGETACT,0                                                        
         BE    DISCAC05                                                         
         CLI   CSACT,A#LST                                                      
         BNE   EXITL                                                            
         MVC   FVXTRA,BCSPACES                                                  
         B     EXITL                                                            
*                                                                               
DISCAC05 MVC   FVIFLD(L'MDICOMM),MDICOMM                                        
         MVC   SVCOMM,MDICOMM      SAVE OFF COMM ACC FOR DEF ANALSIS AC         
*                                                                               
         ICM   RF,15,ACALDG        GETS SET BY EITHER GETACT OR GETLDG          
         USING LDGTABD,RF                                                       
         LHI   R0,LDGTMAXN         MAX # OF ENTRIES IN TABLE 20                 
DISCAC10 CLC   LDGTUL,FVIFLD       IS U/L SAME AS COMMISION ACC (SI)            
         BE    DISCAC20                                                         
         LA    RF,LDGTABL(RF)                                                   
         BCT   R0,DISCAC10                                                      
         DC    H'0'                SI SHOULD HAVE BEEN THERE                    
*                             IF NOT FOUND MEANS IT WAS OVERWRITTEN             
DISCAC20 MVC   LEVELS,LDGTLVA      LEVELS HAS LEN OF EACH LEVEL                 
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE COMMISSION ACCOUNT                              *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
HIGH     USING MDIELD,SVELEM                                                    
                                                                                
VALCACC  CLC   MINKOFF,BCSPACES                                                 
         BNH   VALCAC10                                                         
         MVC   FVIFLD(L'MDICOMM),HIGH.MDICOMM                                   
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
VALCAC10 MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITNO                                                           
*                                                                               
         CLC   =C'SI',FVIFLD       MUST BE UNIT LEDGER SI                       
         BNE   EXITINUL            INVALID UNIT LEDGER FOR THIS FIELD           
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
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY THE NET POSTING / MEDIA CONTROL ACCOUNT                     *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
DISNPAC  MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         CLC   MINKOFF,BCSPACES                                                 
         BNH   DISNP05                                                          
         OI    FVATRB,FVAPROT                                                   
         OI    FVOIND,FVOXMT                                                    
         MVC   SVCOMM,HIGH.MDICOMM                                              
         CLC   HIGH.MDICNTL,BCSPACES                                            
         BNH   DISNP10             DISPLAY DEFAULT ACCOUNT                      
         MVC   FVIFLD(L'MDICNTL),HIGH.MDICNTL DISPLAY FROM HIGHER LEVEL         
*                                                                               
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICNTL),HIGH.MDICNTL                                  
         GOTO1 AGETACT,0                                                        
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
DISNP05  GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCOMM,MDICOMM                                                   
*                                                                               
         CLI   MDICNTL,0            IS NET POSTING/MEDIA CONTROL EXIST          
         BE    DISNP10              DISPLAY DEFAULT ACCOUNT                     
*                                                                               
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICNTL),MDICNTL                                       
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
         MVC   FVIFLD(L'MDICNTL),MDICNTL                                        
         B     DISNPX                                                           
*                                                                               
DISNP10  GOTO1 =A(DDEFNP),BOPARM,RR=BORELO DISPLAY DEFAULT NET POST ACC         
DISNPX   B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE NET POSTING / MEDIA CONTROL ACCOUNT             *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
VALNPAC  CLC   MINKOFF,BCSPACES    OFFICE LEVEL                                 
         BNH   VALNP05             NO                                           
         MVC   SVCOMM,HIGH.MDICOMM                                              
         CLC   HIGH.MDICNTL,BCSPACES                                            
         BNH   *+14                DISPLAY DEFAULT ACCOUNT                      
         MVC   FVIFLD(L'MDICNTL),HIGH.MDICNTL DISPLAY FROM HIGHER LEVEL         
         B     EXITOK                                                           
         GOTO1 =A(DDEFNP),BOPARM,RR=BORELO                                      
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
VALNP05  MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BNE   VALNP10                                                          
*                                                                               
         GOTO1 =A(DDEFNP),BOPARM,RR=BORELO                                      
         BNE   EXITMIF                                                          
*                                                                               
VALNP10  CLI   FVIFLD,C'('                                                      
         BE    VALNPX                                                           
*                                                                               
         CLC   =C'SZ',FVIFLD                                                    
         BNE   EXITINUL            INVALID UNIT LEDGER FOR THIS FIELD           
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
VALNPX   B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BILL/REV ANALYSIS ACCOUNT                           *         
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
* DISPLAY BILL/REV ANALYSIS ACCOUNT                                   *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
DISANLAC CLC   MINKOFF,BCSPACES                                                 
         BNH   DISANL05                                                         
         OI    FVATRB,FVAPROT                                                   
         OI    FVOIND,FVOXMT                                                    
         CLC   HIGH.MDICOST,BCSPACES                                            
         BNH   DISANL10            DISPLAY DEFAULT ACCOUNT                      
         MVC   FVIFLD(L'MDICOST),HIGH.MDICOST DISPLAY FROM HIGHER LEVEL         
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
DISANL05 GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   SVCOMM,MDICOMM                                                   
         CLI   MDICOST,0            IS COST ANALYSIS ACCOUNT EXIST              
         BE    DISANL10                                                         
         MVC   FVIFLD(L'MDICOST),MDICOST                                        
         MVC   SVDEFANL,MDICOST                                                 
         B     DISANLX                                                          
DISANL10 GOTO1 =A(DDEFAN),BOPARM,RR=BORELO   DIS DEF ANALYSIS ACCOUNT           
*                                                                               
DISANLX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE BILL/REV ANALYSIS ACCOUNT                       *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
VALANLAC CLC   MINKOFF,BCSPACES    OFFICE LEVEL                                 
         BNH   VALANL05            NO                                           
         CLC   HIGH.MDICOST,BCSPACES                                            
         BNH   *+14                                                             
         MVC   FVIFLD(L'MDICOST),HIGH.MDICOST DISPLAY FROM HIGHER LEVEL         
         B     EXITOK                                                           
         GOTO1 =A(DDEFAN),BOPARM,RR=BORELO DISPLAY DEFAULT ACCOUNT              
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
VALANL05 MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BNE   VALANL10                                                         
         GOTO1 =A(DDEFAN),BOPARM,RR=BORELO   DIS DEF ANALYSIS ACCOUNT           
         BE    VALANL10                                                         
         GOTO1 =A(CHKCOST),BOPARM,RR=BORELO   DIS DEF ANALYSIS ACCOUNT          
         BE    EXITMIF                        NEW COST IN USE                   
         B     EXITOK                                                           
VALANL10 CLI   FVIFLD,C'('         IS IT A DEFAULT                              
         BE    EXITOK                                                           
*                                                                               
         LA    R6,IOKEY                                                         
         MVI   ACTKUNT,C'1'        CHK UNIT 1                                   
         MVI   ACTKLDG,C'1'        CHK LDGR 1                                   
         MVC   ACTKACT,FVIFLD                                                   
*                                                                               
         LHI   R1,2                                                             
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
         MVI   ACTKLDG,C'2'                                                     
         BCT   R1,*-12             CHECK U/L 11 AND 12                          
*                                                                               
         MVC   MDICOST,FVIFLD      PUT ACCOUNT IN BOELEM                        
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
* DATA OBJECT FOR CASH DISCOUNT ACCOUNT                               *         
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
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE CASH DISCOUNT ACCOUNT                                             
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
DISCDACC MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         CLC   MINKOFF,BCSPACES                                                 
         BNH   DISCD05                                                          
         OI    FVATRB,FVAPROT                                                   
         OI    FVOIND,FVOXMT                                                    
         CLC   HIGH.MDICSHD,BCSPACES                                            
         BNH   DISCD07             DISPLAY DEFAULT ACCOUNT                      
         MVC   FVIFLD(L'MDICSHD),HIGH.MDICSHD DISPLAY FROM HIGHER LEVEL         
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICSHD),HIGH.MDICSHD                                  
         GOTO1 AGETACT,0                                                        
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
DISCD05  GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   MDICSHD,0           IS CASH DISCOUNT ACCOUNT EXIST               
         BNE   DISCD10                                                          
DISCD07  GOTO1 =A(DDEFCD),BOPARM,RR=BORELO DISPLAY DEFAULT CASH DISC            
         B     DISCDX                                                           
*                                                                               
DISCD10  LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICSHD),MDICSHD                                       
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
         MVC   FVIFLD(L'MDICSHD),MDICSHD                                        
*                                                                               
DISCDX   B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE CASH DISCOUNT ACCOUNT                           *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
VALCDAC  CLC   MINKOFF,BCSPACES    OFFICE LEVEL                                 
         BNH   VALCD05             NO                                           
         CLC   HIGH.MDICSHD,BCSPACES                                            
         BNH   *+14                                                             
         MVC   FVIFLD(L'MDICSHD),HIGH.MDICSHD DISPLAY FROM HIGHER LEVEL         
         B     EXITOK                                                           
         GOTO1 =A(DDEFCD),BOPARM,RR=BORELO DISPLAY DEFAULT ACCOUNT              
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
VALCD05  MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BNE   VALCD10                                                          
*                                                                               
         GOTO1 =A(DDEFCD),BOPARM,RR=BORELO DISPLAY DEFAULT CASH DISC            
         BNE   EXITMIF                                                          
*                                                                               
VALCD10  CLI   FVIFLD,C'('                                                      
         BE    VALCDX                                                           
*                                                                               
         CLC   =C'SI',FVIFLD           MUST BE UNIT LEDGER SZ                   
         BNE   EXITINUL            INVALID UNIT LEDGER FOR THIS FIELD           
*                                                                               
         BAS   RE,VALACC           CHECK IF IT'S A VALID ACCOUNT                
         BNE   EXITL               ERROR W/ INVALID ACCOUNT/POSTING             
*                                                                               
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
VALCDX   B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR US-CASH RCPT (CK) ACCT                              *         
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
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE US-CASH RCPT (CK) ACCT                                 *          
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
DISCRAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BNE   DISRAC10                                                         
*                                                                               
         CLI   MDICSHR,0           IS CASH RECEIPTS ACCOUNT EXIST               
         BE    DISRAC10                                                         
*                                                                               
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICSHR),MDICSHR                                       
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
         MVC   FVIFLD(L'MDICSHR),MDICSHR                                        
         B     DISRACX                                                          
*                                                                               
DISRAC10 CLC   MINKOFF,BCSPACES                                                 
         BNH   DISRAC20                                                         
         CLC   HIGH.MDICSHR,BCSPACES                                            
         BNH   DISRAC20                                                         
         GOTO1 =A(ADDPAR),BOPARM,RR=BORELO,(L'MDICSHR,HIGH.MDICSHR)             
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICSHR),HIGH.MDICSHR                                  
         GOTO1 AGETACT,0                                                        
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
DISRAC20 MVC   FVIFLD(L'AC@A54PR),AC@A54PR    DEFAULT IS (A54 PROFILE)          
DISRACX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE US-CASH RCPT (CK) ACCT                          *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALCRAC  DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
*                                                                               
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'('         IS IT A DEFAULT                              
         BE    EXITOK                                                           
*                                                                               
         CLC   =C'SB',FVIFLD                                                    
         BE    *+14                                                             
         CLC   =C'SC',FVIFLD                                                    
         BNE   EXITINUL            INVALID UNIT LEDGER FOR THIS FIELD           
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
* DATA OBJECT FOR CAN-CASH RCPT (CK) ACCT                             *         
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
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE CAN-CASH RCPT (CK) ACCT                                 *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
DISCCRAC DS    0H                                                               
         MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BNE   DISCCR10                                                         
*                                                                               
         CLI   MDICCSR,0            IS CASH RECEIPTS ACCOUNT EXIST              
         BE    DISCCR10                                                         
*                                                                               
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICCSR),MDICCSR                                       
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
         MVC   FVIFLD(L'MDICCSR),MDICCSR                                        
         B     DISCCRX                                                          
*                                                                               
DISCCR10 CLC   MINKOFF,BCSPACES                                                 
         BNH   DISCCR20                                                         
         CLC   HIGH.MDICCSR,BCSPACES                                            
         BNH   DISCCR20                                                         
         GOTO1 =A(ADDPAR),BOPARM,RR=BORELO,(L'MDICCSR,HIGH.MDICCSR)             
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICCSR),HIGH.MDICCSR                                  
         GOTO1 AGETACT,0                                                        
         B     EXITOK                                                           
         DROP  HIGH                                                             
*                                                                               
DISCCR20 MVC   FVIFLD(L'AC@A54PR),AC@A54PR    DEFAULT IS (A54 PROFILE)          
*                                                                               
DISCCRX  B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE CAN-CASH RCPT (CK) ACCT                         *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
VALCCRAC DS    0H                                                               
         MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'('         IS IT DISPLAYING DEFAULT                     
         BE    EXITOK                                                           
*                                                                               
         CLC   =C'SB',FVIFLD                                                    
         BE    *+14                                                             
         CLC   =C'SC',FVIFLD                                                    
         BNE   EXITINUL            INVALID UNIT LEDGER FOR THIS FIELD           
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
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHACC)                               
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY THE COMMISSION ONLY CONTROL ACCOUNT                         *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM       DSECT TO COVER MEDIA INTERFACE ELEM          
HIGH     USING MDIELD,SVELEM                                                    
*                                                                               
DISCOAC  MVC   IOKEY,BCSPACES      INIT KEY TO READ ACCOUNT NAME                
         GOTO1 AGETEL,BOPARM,('MDIELQ',MINRECD),0                               
         BE    *+8                                                              
         B     EXITOK                                                           
         CLI   MDICONL,0            IS COMMISSION ONLY ACCOUNT EXIST            
         BE    DISCO10                                                          
*                                                                               
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICONL),MDICONL                                       
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
         MVC   FVIFLD(L'MDICONL),MDICONL                                        
         B     EXITOK                                                           
*                                                                               
DISCO10  CLC   MINKOFF,BCSPACES                                                 
         BNH   EXITOK                                                           
         CLC   HIGH.MDICONL,BCSPACES                                            
         BNH   EXITOK                                                           
         GOTO1 =A(ADDPAR),BOPARM,RR=BORELO,(L'MDICONL,HIGH.MDICONL)             
         LA    R6,IOKEY            BUILD KEY TO READ FOR NAME                   
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA(L'MDICONL),HIGH.MDICONL                                  
         GOTO1 AGETACT,0                                                        
         B     EXITOK                                                           
         DROP  HIGH                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* VALIDATE ADD/CHANGE COMMISSION ONLY CONTROL ACCOUNT                 *         
***********************************************************************         
         PUSH  USING                                                            
         USING MDIELD,BOELEM      DSECT TO COVER MEDIA INTERFACE ELEM           
HIGH     USING MDIELD,SVELEM                                                    
                                                                                
*                                                                               
VALCOAC  MVC   IOKEY,BCSPACES                                                   
         CLI   FVILEN,0            DID USER INPUT ANYTHING                      
         BE    EXITOK                                                           
*                                                                               
         CLC   MINKOFF,BCSPACES                                                 
         BNH   VALCO10                                                          
         CLI   FVIFLD,C'('         IS IT A DEFAULT                              
         BE    EXITOK                                                           
*                                                                               
VALCO10  CLC   =C'SZ',FVIFLD           MUST BE UNIT LEDGER SZ                   
         BNE   EXITINUL            INVALID UNIT LEDGER FOR THIS FIELD           
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
* SEARCH ON AN ACCOUNT FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
SRCHACC  CLI   CSACT,A#LST         FOR ACTION LIST                              
         BE    EXITOK              DON'T SEARCH(MESSES UP FILTERING)            
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,             +        
               ACOM,(0,0)                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
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
*                                                                               
         L     R1,AIO1             A(ACCOUNT RECORD)                            
         GOTOX AGETNAM             GET ACCOUNT NAME                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAYING OFFICE NAME                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
*                                                                               
OFFNAM   LA    RF,OFFNTBL                                                       
         B     ITER                                                             
*                                                                               
OFFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFFN)                                
         DC    AL1(EOT)                                                         
*                                                                               
***********************************************************************         
* DISPLAY OFFICE NAME                                                 *         
***********************************************************************         
*                                                                               
         PUSH  USING                                                            
         USING OFFRECD,R4                                                       
DISOFFN  DS    0H                                                               
         CLC   MINKOFF,BCSPACES                                                 
         BNH   EXITOK                                                           
*                                                                               
         LA    R4,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         MVC   OFFKEY,BCSPACES                                                  
         MVI   OFFKTYP,OFFKTYPQ    X'01' RECORD                                 
         MVC   OFFKCPY,CUABIN      CONNECTED ID AGENCY BINARY                   
         MVC   OFFKOFF,MINKOFF     MOVE IN MEDIA CODE TO READ                   
         L     R1,=A(XOACCMST+XOREAD+XIO1)                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO1                                                          
         GOTOX AGETNAM             GET ACCOUNT NAME                             
         B     EXITOK                                                           
         DROP  R4                                                               
         POP   USING                                                            
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
         LA    R6,IOKEY            BUILD KEY TO READ VALID ACCOUNT              
         MVC   ACTKCPY,CUABIN                                                   
*                                                                               
         L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(MI#ANLACC)                                             
         BE    VALACC10                                                         
*                                                                               
         MVC   ACTKULA,FVIFLD      UNIT/LEDGER INPUT BY USER                    
VALACC10 DS    0H                                                               
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   EXITL                                                            
*                                                                               
         L     R6,AIO1                                                          
         TM    ACTRSTAT,ACTSABLP    DOES ACCOUNT HAS BALANCE ELM X'32'          
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
*        DC    AL1(LINIT),AL1(0,0,0),AL4(INITL0)                                
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
         TM    SVCPXST1,CPXOFFMI   IS THE COMPANY ON OFFICE MI RECORD ?         
         BO    *+14                                                             
         CLC   IOKEY+(MINKOFF-MINKEY)(L'MINKOFF),BCSPACES                       
         BH    NLST                                                             
*                                                                               
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    EXITOK                                                           
         GOTO1 ATSTOFF,THIS.MINKOFF TEST OFFICE                                 
         MVC   IOKEY,THIS.MINKEY    RESTORE IOKEY                               
         BNE   NLST04               INVALID OFFICE GET NEXT RECORD              
         LHI   R1,XOHIGH+XOACCDIR+XIO1  RESTORE READ SEQUENCE                   
         GOTO1 AIO                   BEFORE READ SEQUENTIAL                     
         B     EXITOK                                                           
NLST04   LHI   R1,XOHIGH+XOACCDIR+XIO1  RESTORE READ SEQUENCE                   
         GOTO1 AIO                   BEFORE READ SEQUENTIAL                     
         B     NLST                                                             
*                                                                               
         B     EXITOK                                                           
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
FF       EQU   X'FF'                                                            
STMPSTRQ EQU   X'03'                                                            
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#A54PR,13,L                                                    
DCLISTX  DC    X'00'                                                            
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEFAULT CASH DISCOUNT ACCOUNT                               *         
***********************************************************************         
         SPACE 2                                                                
DDEFCD   NMOD1 0,**DDEFCD*                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    RF,IOKEY                                                         
T        USING ACTRECD,RF                                                       
         MVC   IOKEY,BCSPACES                                                   
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKULA,=CL14'SIMP'         IT IS THE DEFAULT                  
         DROP  T                                                                
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   DDEFCDNO                                                         
*                                                                               
         L     R6,AIO1                                                          
         TM    ACTRSTAT,ACTSABLP    DOES ACCOUNT HAS BALANCE ELM X'32'          
         BNO   DDEFCDNO                                                         
*                                                                               
         MVI   FVIFLD,C'('                                                      
         MVC   FVIFLD+1(L'ACTKULA),IOKEY+1                                      
         LHI   R1,L'ACTKULA                                                     
DDEFCD10 LA    RE,FVIFLD+1                                                      
         AR    RE,R1                                                            
         CLI   0(RE),C' '           ANYTHING THERE                              
         BNE   DDEFCD20                                                         
         BCT   R1,DDEFCD10                                                      
         DC    H'0'                                                             
*                                                                               
DDEFCD20 DS    0H                                                               
         LA    RE,1(RE)            BUMP TO AVAILABLE SPACE                      
         MVI   0(RE),C')'                                                       
         CR    RB,RB                                                            
         B     DDEFCDX                                                          
*                                                                               
DDEFCDNO DS    0H                                                               
         CR    RB,RD               SET CC LOW                                   
         MVC   FVXTRA,BCSPACES                                                  
         MVC   IOKEY,BCSPACES                                                   
DDEFCDX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS FOR DDEFCD NMOD                                *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEFAULT NET POSTING ACCOUNT                                 *         
***********************************************************************         
         SPACE 2                                                                
*        USING TWUSER,R4                                                        
DDEFNP   NMOD1 0,**DDEFNP*                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    RF,IOKEY                                                         
T        USING ACTRECD,RF                                                       
         MVC   IOKEY,BCSPACES                                                   
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'SZ'                            
         MVC   T.ACTKACT,SVCOMMAC    MOVE COMMISSION ACC W/O UNIT LEDG          
         DROP  T                                                                
*                                                                               
         GOTO1 AGETACT,0                                                        
         BNE   DDEFNPNO                                                         
*                                                                               
         MVI   FVIFLD,C'('                                                      
         MVC   FVIFLD+1(L'ACTKULA),IOKEY+1                                      
         LHI   R1,L'ACTKULA                                                     
DDEFNP10 LA    RE,FVIFLD+1                                                      
         AR    RE,R1                                                            
         CLI   0(RE),C' '           ANYTHING THERE                              
         BNE   DDEFNP20                                                         
         BCT   R1,DDEFNP10                                                      
         DC    H'0'                                                             
*                                                                               
DDEFNP20 DS    0H                                                               
         LA    RE,1(RE)            BUMP TO AVAILABLE SPACE                      
         MVI   0(RE),C')'                                                       
         CR    RB,RB                                                            
         B     DDEFNPX                                                          
*                                                                               
DDEFNPNO DS    0H                                                               
         CR    RB,RD               SET CC LOW                                   
         MVC   FVXTRA,BCSPACES                                                  
         MVC   IOKEY,BCSPACES                                                   
DDEFNPX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS FOR DDEFNP NMOD                                *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY DEFAULT ANALYSIS ACCOUNT FROM SI ACCOUNT ANAL POINTER       *         
***********************************************************************         
         SPACE 2                                                                
*        USING TWUSER,R4                                                        
DDEFAN   NMOD1 0,**DDEFAN*                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING ACTRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.ACTKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.ACTKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         MVC   TEMP.ACTKULA,SVCOMM    SAVED COMMISSION ACCOUNT                  
         DROP  TEMP                                                             
*                                                                               
DDEFAN10 DS    0H                                                               
         GOTO1 AGETACT,0                                                        
         BNE   DDEFANX                                                          
         L     R6,AIO1                                                          
         AH    R6,=Y(ACTRFST-ACTKEY)                                            
DDEFAN20 CLI   0(R6),0                                                          
         BE    DDEFAN40                                                         
         CLI   0(R6),SPAELQ        IS IT X'2C' ELEMENT                          
         BE    DDEFAN30                                                         
*                                                                               
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     DDEFAN20                                                         
*                                                                               
DDEFAN30 BAS   RE,GETANAL          GET ANALYSIS POINTER IF EXIST                
         BE    DDEFANX             DEFAULT IS FOUND                             
         TM    FLAG,FLGLDG         ARE WE AT THE LEDGER LEVEL                   
         BO    DDEFANNO            NO DEFAULT ACCOUNT FOUND                     
*                                                                               
DDEFAN40 DS    0H                                                               
         BAS   RE,READNXT          ADJUST KEY TO READ NXT HIGHER LVL            
         TM    FLAG,FLGLDG         ARE WE AT THE LEDGER LEVEL                   
         BNO   DDEFAN10                                                         
DDEFANNO CR    RB,RD                                                            
DDEFANX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE CHECKS SPAELQ ELEMENT FOR DEFAULT ANALYSIS ACCOUNT       *         
* R6=A(X'2C' ELEMENT)                                                           
***********************************************************************         
*                                                                               
         USING SPAELD,R6                                                        
GETANAL  NTR1                                                                   
         CLI   SPATYPE,SPATANAL     IS IT ANALYSIS ACCOUNT                      
         BNE   GETANNO                                                          
*                                                                               
         MVC   SVDEFANL,SPAAANAL                                                
         MVI   FVIFLD,C'('                                                      
         MVC   FVIFLD+1(L'SPAAANAL),SPAAANAL                                    
         LHI   R1,L'SPAAANAL                                                    
GETAN10  LA    RE,FVIFLD+1                                                      
         AR    RE,R1                                                            
         CLI   0(RE),C' '           ANYTHING THERE                              
         BNE   GETAN20                                                          
         BCT   R1,GETAN10                                                       
         DC    H'0'                                                             
*                                                                               
GETAN20  DS    0H                                                               
         LA    RE,1(RE)            BUMP TO AVAILABLE SPACE                      
         MVI   0(RE),C')'                                                       
*        MVI   FVIFLD+(L'SPAAANAL+1),C')'                                       
         CR    RB,RB               EXIT WITH DEF ANAL FOUND                     
         B     GETANX                                                           
GETANNO  CR    RB,RD               DEFAULT ANALYSIS ACCOUNT NOT FOUND           
GETANX   XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* SUBROUTINE ADJUSTS IOKEY TO READ NEXT HIGHER SI COMMISSION ACCOUNT  *         
***********************************************************************         
         SPACE 2                                                                
READNXT  NTR1                                                                   
         NI    FLAG,X'FF'-FLGSPC                                                
         LHI   R3,LEVLNQ           TOTAL NO. OF LEVELS -1                       
         LA    R1,LEVD                                                          
         LA    R2,LEVC                                                          
         LA    R5,IOKEYSAV                                                      
         LA    R5,L'ACTKCULA-1(R5)                                              
RDNXT10  DS    0H                                                               
         TM    FLAG,FLGSPC         WAS IOKEYSAV SPACED OUT THIS TIME            
         BO    RDNXT50                                                          
         ZIC   RF,0(R1)            LENGTH OF LAST LEVEL                         
         LTR   RF,RF                                                            
         BNZ   RDNXT20                                                          
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         BCT   R3,RDNXT10                                                       
         DC    H'0'                                                             
*                                                                               
RDNXT20  LA    R6,LEVA             FIRST LEVEL HAS INDIV LEN                    
         CR    R2,R6                                                            
*        BL    RDNXT40                                                          
         BNL   *+8                                                              
         B     RDNXT40                                                          
         ZIC   RE,0(R2)            LENGTH OF LAST-1 LEVEL                       
         SR    RF,RE               LENGTH OF INDIVIDUAL LEVEL                   
RDNXT40  CLI   0(R5),C' '          IS IT A SPACE IN IOKEYSAV                    
         BE    *+12                                                             
         MVI   0(R5),C' '          CLEAR THIS LEVEL                             
         OI    FLAG,FLGSPC         SPACED OUT THIS TIME                         
         BCTR  R5,0                                                             
         BCT   RF,RDNXT40          RF HAS IND LENGTH OF CURRENT LEVEL           
*                                                                               
         BCTR  R1,0                                                             
         BCTR  R2,0                                                             
         BCT   R3,RDNXT10                                                       
         OI    FLAG,FLGLDG         WE ARE AT  THE LEDGER LEVEL NOW              
RDNXT50  MVC   IOKEY,IOKEYSAV                                                   
READNXTX XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS FOR DDEFAN NMOD                                *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* CHECK TO SEE IF COMPANY IS ON NEW COST SYSTEM                       *         
***********************************************************************         
         SPACE 2                                                                
*        USING TWUSER,R4                                                        
CHKCOST  NMOD1 0,*CHKCOST*                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LA    RF,IOKEY                                                         
TEMP     USING CPYRECD,RF                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   TEMP.CPYKEY,BCSPACES   INITIALIZE KEY OF RECORD                  
         MVC   TEMP.CPYKCPY,CUABIN    CONNECTED ID AGENCY BINARY                
         DROP  TEMP                                                             
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                 ACCOUNT MUST EXIST                           
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD ACCOUNT RECORD                           
*                                                                               
         L     R6,AIO2                                                          
         AH    R6,=Y(CPYRFST-CPYKEY)                                            
         USING CPYELD,R6                                                        
CHKCST10 CLI   0(R6),0                                                          
         BE    CHKCSTNO                                                         
         CLI   0(R6),CPYELQ        IS IT X'10' COMPANY ELEMENT                  
         BE    CHKCST20                                                         
*                                                                               
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CHKCST10                                                         
*                                                                               
CHKCST20 TM    CPYSTAT5,CPYSNCST   IS NEW COST SYSTEM IN USE                    
         BO    CHKCSTYS            DEFAULT IS FOUND                             
*                                                                               
CHKCSTNO CR    RB,RD               NOT ON COST SYSTEM                           
         B     *+6                                                              
CHKCSTYS CR    RB,RB                                                            
CHKCSTX  XIT1                                                                   
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS FOR CHKCOST NMOD                               *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ADD PARENTHESIS AROUND THE DATA AND MOVE IT IN FVIFLD (SCCASH1MAIN) *         
* ON ENTRY                                                                      
* P1 BYTE 0   ( LENGTH OF DATA )                                      *         
* P1 BYTE 1-3 ( DATA )                                                *         
* ON EXIT                                                                       
* FVIFLD SET WITH DATA IN PARENTHESIS                                           
***********************************************************************         
         SPACE 2                                                                
ADDPAR   NMOD1 0,**ADDPAR*                                                      
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
*                                                                               
         LLC   RE,0(R1)             LENGTH OF DATA                              
         L     RF,0(R1)             DATA                                        
                                                                                
         BCTR  RE,0                                                             
         LTR   RE,RE                IS LENGTH ZERO ?                            
         BM    ADDPARX                                                          
         EX    RE,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BCSPACES      NOTHING PASSED IN DATA                     
         BNH   ADDPARX                                                          
                                                                                
         MVI   FVIFLD,C'('                                                      
         MVC   FVIFLD+1(0),0(RF)                                                
         EX    RE,*-6                                                           
         AHI   RE,1                                                             
         LA    R1,FVIFLD+1                                                      
         AR    R1,RE                                                            
ADDPAR10 CLI   0(R1),C' '                                                       
         BH    ADDPAR20                                                         
         BCTR  R1,0                                                             
         BCT   RE,ADDPAR10                                                      
ADDPAR20 LA    R1,1(R1)                                                         
         MVI   0(R1),C')'                                                       
                                                                                
ADDPARX  XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS FOR ADDPAR NMOD                                *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
*                                                                               
*ACFILWORK                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                               *         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
LEVELS   DS    0XL4                                                             
LEVA     DS    XL1                 LEVEL A LENGTH                               
LEVB     DS    XL1                 LEVEL B LENGTH (A+B)                         
LEVC     DS    XL1                 LEVEL C LENGTH (A+B+C)                       
LEVD     DS    XL1                 LEVEL D LENGTH (A+B+C+D)                     
LEVLNQ   EQU   *-LEVELS                                                         
SVCOMM   DS    0CL14               SAVED COMMISSION ACCOUNT                     
SVCOMMUL DS    CL2                 UNIT AND LEDGER                              
SVCOMMAC DS    CL12                ACCOUNT                                      
         ORG   TWUSER+L'TWUSER-(*-SAVEVALS)                                     
         EJECT                                                                  
*                                                                               
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
SAVRB    DS    F                                                                
SVELEM   DS    XL(MDILNQ)                                                       
*                                                                               
DSLIST   DS    0C                                                               
AC@A54PR DS    CL13                (A54 PROFILE)                                
*                                                                               
FLAG     DS    XL1                                                              
FLGLDG   EQU   X'80'               WE ARE AT THE LEDGER LEVEL                   
FLGSPC   EQU   X'40'               KEY SPACED OUT THIS TIME                     
*                                                                               
SVCPXST1 DS    XL(L'CPXSTAT1)      SAVED COMPANY EXTRA STATUS 1                 
SVMCODE  DS    CL2                 SAVED MEDIA CODE TYPED BY USER               
SVDEFANL DS    CL12                SAVED DEFAULT ANALYSIS                       
*                                  AND UNIT/LEDGERS IN LIST                     
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'016ACFIL31   07/25/12'                                      
         END                                                                    
