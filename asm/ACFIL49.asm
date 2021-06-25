*          DATA SET ACFIL49    AT LEVEL 005 AS OF 08/04/06                      
*PHASE T62349A,*                                                                
         SPACE 1                                                                
FIL49    TITLE 'ORDER NUMBER RECORD'                                            
         SPACE 2                                                                
* TKLU 002 12SEP05 EBUYER DEFECT: UKCR00004234 - ONCNXT# BUG FIX                
* TKLU 003 13SEP05 <DU01-4618> - INTERNAL ORDER SUPPORT (DD CHANGE)             
* YNGX 004 01FEB06 <TK# 1024895> CHANGE MIN LENGTH OF APP FLD TO 2 CHRS         
* DKEL 005 04AUG06 <LO01-5639> ENSURE ORDER CONTROL REC ADDED TO FILE           
FIL49    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL49**,R6,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
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
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLISTU,DSLISTU                           
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
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
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
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
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
         USING ONARECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
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
KFKVAL   XC    ONAKEY,ONAKEY       INITIALIZE KEY OF ORDER NUM RECORD           
         MVI   ONAKTYP,ONAKTYPQ    ORDER NUM RECORD TYPE                        
         MVI   ONAKSUB,ONAKSUBQ    ORDER NUM RECORD SUB-TYPE                    
         MVC   ONAKCPY,CUABIN      CONNECTED ID                                 
         MVC   ONAKOFF,BCSPACES    OFFICE                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    ONAKEY,ONAKEY       INITIALIZE KEY OF ORDER NUM RECORD           
         MVI   ONAKTYP,ONAKTYPQ    ORDER NUM RECORD TYPE                        
         MVI   ONAKSUB,ONAKSUBQ    ORDER NUM RECORD SUB-TYPE                    
         MVC   ONAKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KLTABL   DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  XC    CDOPTION,CDOPTION                                                
         GOTO1 AVALDOPT,0                                                       
         BL    EXITL                                                            
         CLC   CDOPTION,SDOPTION   HAS OPTIONS BEEN CHANGED?                    
         BE    EXITOK                                                           
         MVI   LSSCIND1,LSSCIFLT   REFRESH LIST                                 
         MVC   SDOPTION(SDOPTSL),CDOPTION                                       
         B     EXITOK                                                           
         DROP  R2                                                               
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
         USING ONARECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
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
RFTABL   DC    AL1(RRES),AL1(0,0,0),AL4(RFRES)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
***********************************************************************         
         SPACE 1                                                                
RFRES    GOTO1 AGETEL,BOPARM,('ONCELQ',ONARECD),0                               
         BE    *+6                                                              
         DC    H'0'                NO ONCEL!!!                                  
*                                                                               
         MVC   SVONCEL,BOELEM      SAVE ONCEL                                   
         BAS   RE,CHKDON           CHECK DUPLICATE ORDER NUMBER                 
         BNE   EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE                                                                  
RLADD    BAS   RE,BLDKEY                                                        
         MVI   PASACT,PASADDQ                                                   
         BAS   RE,ONAPAS           ADD PASSIVE POINTER                          
         BAS   RE,CHKCTRL          DOES ORDER CONTROL RECORD EXIST?             
         BL    EXITL               PROBLEM ADDING CONTROL REC                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE                                                                  
RLDEL    BAS   RE,BLDKEY                                                        
         MVI   PASACT,PASDELQ                                                   
         BAS   RE,ONAPAS           DELETE PASSIVE POINTER                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - CHANGE A RECORD                       *         
***********************************************************************         
         SPACE                                                                  
T        USING ONAPASD,IOKEY                                                    
RLWRT    BAS   RE,BLDKEY                                                        
         CLC   T.ONAPSTA#,SVONSTA#                                              
         BNE   *+14                                                             
         CLC   T.ONAPEND#,SVONEND#                                              
         BE    EXITOK              OK - ONCEL HAVEN'T BEEN CHANGED              
*                                                                               
         MVI   PASACT,PASADDQ                                                   
         BAS   RE,ONAPAS           ADD NEW PASSIVE POINTER                      
*                                                                               
         MVC   T.ONAPSTA#,SVONSTA#                                              
         MVC   T.ONAPEND#,SVONEND#                                              
         MVI   PASACT,PASDELQ                                                   
         BAS   RE,ONAPAS           DELETE OLD PASSIVE POINTER                   
         BAS   RE,CHKCTRL          DOES ORDER CONTROL RECORD EXIST?             
         BL    EXITL               PROBLEM ADDING CONTROL REC                   
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT ,                                                                
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
         USING ONARECD,R2                                                       
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
         LA    RF,KNOWLQ(,RF)                                                   
         B     DATA04                                                           
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING ONARECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(F#ORDN#APP),AL4(APPDTA)    APPLICATION                       
         DC    AL2(F#ORDN#CLS),AL4(CLSDTA)    CLASS (ORDER TYPE)                
         DC    AL2(F#ORDN#OFF),AL4(OFFDTA)    OFFICE                            
         DC    AL2(F#ORDN#OFFNM),AL4(OFFNDTA) OFFICE NAME                       
         DC    AL2(F#ORDN#STR#),AL4(STRDTA)   START NUMBER                      
         DC    AL2(F#ORDN#END#),AL4(ENDDTA)   END NUMBER                        
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL49    CSECT                                                                  
         EJECT ,                                                                
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
DFDDIS   GOTO1 AGETEL,BOPARM,('ONCELQ',ONARECD),0                               
         MVC   SVONCEL,BOELEM      SAVE ORDER NUMBER ELEMENT                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   GOTO1 AGETEL,BOPARM,('ONCELQ',ONARECD),0                               
         BE    DFDV02              ORDER NUMBER ELEMENT FOUND                   
T        USING ONCELD,BOELEM       SAVE ORDER NUMBER ELEMENT                    
         MVI   T.ONCEL,ONCELQ                                                   
         MVI   T.ONCLN,ONCOANQ                                                  
DFDV02   MVC   SVONCEL,BOELEM                                                   
         MVC   SVONSTA#,T.ONCSTA#                                               
         MVC   SVONEND#,T.ONCEND#                                               
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT ,                                                                
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
DLDVAL   GOTO1 AREPEL,BOPARM,('ONCELQ',ONARECD),0,SVONCEL                       
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR APPLICATION ONAKAPP                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPDTA   LA    RF,APPTBL                                                        
         B     ITER                                                             
*                                                                               
APPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPP)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPP)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISAPP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETAPP)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTAPP)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALAPP)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTAPP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETAPP  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A APPLICATION FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISAPP   CLI   ONAKAPP,ONAKAEQ                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@EBUY),UC@EBUY   EBUYER                               
         B     EXITOK                                                           
         CLI   ONAKAPP,ONAKAPQ                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@PRSTO),UC@PRSTO PRESTO                               
         B     EXITOK                                                           
         CLI   ONAKAPP,ONAKHSE                                                  
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'UC@HOUSE),UC@HOUSE  HOUSE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A APPLICATION FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALAPP   CLC   FVIFLD(2),UC@EBUY   EBUYER                                       
         BNE   *+12                                                             
         MVI   ONAKAPP,ONAKAEQ                                                  
         B     VAPP10                                                           
         CLC   FVIFLD(2),UC@PRSTO  PRESTO                                       
         BNE   *+12                                                             
         MVI   ONAKAPP,ONAKAPQ                                                  
         B     VAPP10                                                           
         CLC   FVIFLD(2),UC@HOUSE  HOUSE                                        
         BNE   *+12                                                             
         MVI   ONAKAPP,ONAKHSE                                                  
         B     VAPP10                                                           
*                                                                               
         MVC   FVMSGNO,=AL2(AE$IVAPP)                                           
         B     EXITL               INVALID APPLICATION                          
*                                                                               
VAPP10   MVC   FLTIFLD(L'ONAKAPP),ONAKAPP                                       
         CLI   ONAKAPP,ONAKAPQ     PRESTO ?                                     
         BNE   EXITOK                                                           
         CLI   CSACT,A#LST         PRESTO DOES THE MAINTENANCE ITSELF           
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$AMPRS)                                           
         B     EXITL               IT'S MAINTAINED IN PRESTO                    
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A APPLICATION FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DFLTAPP  CLI   FLTIFLD,ONAKAEQ                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@EBUY),UC@EBUY   EBUYER                               
         B     EXITOK                                                           
         CLI   FLTIFLD,ONAKAPQ                                                  
         BNE   *+14                                                             
         MVC   FVIFLD(L'UC@PRSTO),UC@PRSTO PRESTO                               
         B     EXITOK                                                           
         CLI   FLTIFLD,ONAKHSE                                                  
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'UC@HOUSE),UC@HOUSE  HOUSE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR APPLICATION                                        *         
***********************************************************************         
         SPACE 1                                                                
DOFTAPP  CLC   ONAKAPP,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR ORDER TYPE ONAKCLS                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CLSDTA   LA    RF,CLSTBL                                                        
         B     ITER                                                             
*                                                                               
CLSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCLS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCLS)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISCLS)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETCLS)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCLS)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALCLS)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCLS)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETCLS  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ORDER TYPE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISCLS   MVC   FVIFLD(L'UC@EXP),UC@EXP     EXPENSE                              
         CLI   ONAKCLS,ONAKCEQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@PRD),UC@PRD     PRODUCTION                           
         CLI   ONAKCLS,ONAKCPQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@ARTST),UC@ARTST ARTIST                               
         CLI   ONAKCLS,ONAKCAQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@INTOR),UC@INTOR INTERNAL                             
         CLI   ONAKCLS,ONAKCIQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@INTOR),BCSPACES NO ORDER TYPE                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A ORDER TYPE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALCLS   MVI   ONAKCLS,ONAKCDF                                                  
         CLI   FVILEN,0                                                         
         BE    VCLS10                                                           
         MVI   ONAKCLS,ONAKCEQ                                                  
         CLC   FVIFLD(1),UC@EXP    EXPENSE                                      
         BE    VCLS10                                                           
         MVI   ONAKCLS,ONAKCPQ                                                  
         CLC   FVIFLD(1),UC@PRD    PRODUCTION                                   
         BE    VCLS10                                                           
         MVI   ONAKCLS,ONAKCAQ                                                  
         CLC   FVIFLD(1),UC@ARTST  ARTIST                                       
         BE    VCLS10                                                           
         MVI   ONAKCLS,ONAKCIQ                                                  
         CLC   FVIFLD(1),UC@INTOR  INTERNAL                                     
         BE    VCLS10                                                           
         MVC   FVMSGNO,=AL2(AE$INVOT)                                           
         B     EXITL               INVALID ORDER TYPE                           
*                                                                               
VCLS10   MVC   FLTIFLD(L'ONAKCLS),ONAKCLS                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A ORDER TYPE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTCLS  CLI   FLTIFLD,ONAKCDF                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@EXP),UC@EXP     EXPENSE                              
         CLI   FLTIFLD,ONAKCEQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@PRD),UC@PRD     PRODUCTION                           
         CLI   FLTIFLD,ONAKCPQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@ARTST),UC@ARTST ARTIST                               
         CLI   FLTIFLD,ONAKCAQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@INTOR),UC@INTOR INTERNAL                             
         CLI   FLTIFLD,ONAKCIQ                                                  
         BE    EXITOK                                                           
         MVC   FVIFLD(L'UC@INTOR),BCSPACES ???                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR ORDER TYPE                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFTCLS  CLC   ONAKCLS,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFDTA   LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETOFF)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOFF)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFF)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOFF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHOFF)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETOFF  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   MVC   FVIFLD(L'ONAKOFF),ONAKOFF                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFICE CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   MVC   ONAKOFF,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         MVC   ONAKOFF,FVIFLD                                                   
         MVC   FLTIFLD(L'ONAKOFF),ONAKOFF                                       
*                                                                               
         CLI   CSACT,A#DIS         DON'T TEST OFFICE CODE                       
         BE    EXITOK              IF DISPLAY OR LIST                           
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE CODE                             
         BNE   EXITL               INVALID OFFICE                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFICE CODE FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  MVC   FVIFLD(L'ONAKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR OFFICE CODE                                        *         
* OVERLAY WILL DO ITS OWN FILTERING                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFTOFF  CLC   ONAKOFF,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON OFFICE CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
SRCHOFF  CLI   CSACT,A#LST                                                      
         BNE   SROFF10                                                          
         L     RF,FVADDR                                                        
         CLI   FHDA-FHD(RF),C'='                                                
         BNE   EXITOK                                                           
SROFF10  GOTO1 VACSRCHC,BOPARM,(3,FVADDR),ATWA,OFFUL,ACOM,(X'11',0)             
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A OFFICE CODE NAME FIELD                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFNDTA  LA    RF,OFFNTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OFFNTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFFN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE CODE NAME FIELD FROM THE KEY                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
DISOFFN  CLC   ONAKOFF,BCSPACES    TEST ALL OFFICE                              
         BE    EXITOK              YES - DON'T DISPLAY OFFICE CODE NAME         
*                                                                               
         MVC   IOKEY,BCSPACES                READ THE ACCOUNT RECORD            
         MVC   T.ACTKCPY,ONAKCPY             COMPANY                            
         MVC   T.ACTKUNT(L'OFFUL),OFFUL      UNIT/LEDGER                        
         MVC   T.ACTKACT(L'ONAKOFF),ONAKOFF  OFFICE CODE CODE                   
         DROP  T                                                                
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET OFFICE CODE NAME                         
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR START NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
STRDTA   LA    RF,STRTBL                                                        
         B     ITER                                                             
*                                                                               
STRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL START NUMBER                                          *         
***********************************************************************         
         SPACE 1                                                                
DISSTR   OC    SVONCEL,SVONCEL     ANY ORDER NUMBER ELEMENT                     
         BZ    EXITOK              NO - EXITOK                                  
         LA    RF,SVONCEL                                                       
         MVC   FVIFLD(L'ONCSTA#),ONCSTA#-ONCELD(RF)                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE START NUMBER                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSTR   MVC   BOWORK1(5),=5X'F0'                                               
         MVZ   BOWORK1(5),FVIFLD+1                                              
         CLC   BOWORK1(5),=5X'F0'  2-6 MUST BE NUMERIC                          
         BNE   EXITNV                                                           
         CLI   FVIFLD,C'0'         1ST CHAR MUST BE ALPHA-NUMERIC               
         BNL   VSTR04                                                           
         CLI   FVIFLD,C'A'                                                      
         BL    EXITNV                                                           
         CLI   FVIFLD,C'Z'                                                      
         BH    EXITNV                                                           
*                                                                               
VSTR04   LA    RF,SVONCEL                                                       
         CLC   ONCSTA#-ONCELD(L'ONCSTA#,RF),FVIFLD                              
         BE    VSTR06              RESET CURRENT IF NEW START NUMBER            
         MVC   ONCNXT#-ONCELD(L'ONCSTA#,RF),FVIFLD                              
*                                                                               
VSTR06   MVC   ONCSTA#-ONCELD(L'ONCSTA#,RF),FVIFLD                              
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR END NUMBER                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
ENDDTA   LA    RF,ENDTBL                                                        
         B     ITER                                                             
*                                                                               
ENDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEND)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEND)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL END NUMBER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISEND   OC    SVONCEL,SVONCEL     ANY ORDER NUMBER ELEMENT                     
         BZ    EXITOK              NO - EXITOK                                  
         LA    RF,SVONCEL                                                       
         MVC   FVIFLD(L'ONCEND#),ONCEND#-ONCELD(RF)                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE END NUMBER                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALEND   MVC   BOWORK1(5),=5X'F0'                                               
         MVZ   BOWORK1(5),FVIFLD+1                                              
         CLC   BOWORK1(5),=5X'F0'  2-6 MUST BE NUMERIC                          
         BNE   EXITNV                                                           
         CLI   FVIFLD,C'0'         1ST CHAR MUST BE ALPHA-NUMERIC               
         BNL   VEND04                                                           
         CLI   FVIFLD,C'A'                                                      
         BL    EXITNV                                                           
         CLI   FVIFLD,C'Z'                                                      
         BH    EXITNV                                                           
*                                                                               
VEND04   LA    R4,SVONCEL                                                       
         USING ONCELD,R4                                                        
         MVC   ONCEND#,FVIFLD                                                   
*                                                                               
         CLC   ONCSTA#,ONCEND#                                                  
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OTLOF)                                           
         B     EXITL               ORDER TO LESS THAN ORDER FROM                
         DROP  R4                                                               
*                                                                               
         BAS   RE,CHKDON           CHECK DUPLICATE ORDER NUMBER                 
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* OPTIONS OBJECT                                                      *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS DEFAULT HELP NUMBER IN 1ST BYTE. CHANGE IF REQUIRED.       *         
***********************************************************************         
         SPACE 1                                                                
OPT      LM    R0,R3,SVPARMS                                                    
         LA    RF,OPTTABL1                                                      
         B     ITER                                                             
*                                                                               
OPTTABL1 DC    AL1(OHLP),AL1(0,0,0),AL4(OPTHLP)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* OPTION HELP HOOK                                                    *         
***********************************************************************         
         SPACE 1                                                                
OPTHLP   CLI   CSACT,A#LST         LIST USES OPTIONS                            
         BE    EXITOK              HELP=DEFAULT                                 
         B     EXITL               OTHERWISE OPTIONS NOT USED.                  
         EJECT ,                                                                
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
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#ORDN         ORDER NUMBER CONTROL RECORD                  
         BE    *+12                                                             
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
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
THIS     USING ONARECD,R2                                                       
LAST     USING ONARECD,R3                                                       
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
X        USING ONARECD,IOKEY                                                    
FLST     MVC   X.ONAKEY,THIS.ONAKEY                                             
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     L     R1,=AL4(XOSQD+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
*                                                                               
NLST02   CLC   X.ONAKEY(ONAKSPR-ONARECD),THIS.ONAKEY                            
         BNE   EXITL               CHANGE COMPANY                               
*                                                                               
         CLI   CRECDEL,0           NO FILTER - DEFAULT IS DELETE=NO             
         BE    NLST04                                                           
         CLI   CRECDEL,YES         DELETE=YES                                   
         BE    NLST06                                                           
         CLI   CRECDEL,ONLY        DELETE=ONLY                                  
         BNE   NLST04                                                           
         TM    IOERR,IOEDEL        TEST IF RECORD IS DELETED                    
         BZ    NLST                NO - GET NEXT                                
         B     NLST06                                                           
*                                                                               
NLST04   TM    IOERR,IOEDEL        IT MUST BE DELETE=NO                         
         BO    NLST                                                             
*                                                                               
NLST06   MVC   THIS.ONAKEY(ACCKLEN),IOKEY                                       
         B     EXITOK                                                           
         DROP  THIS,LAST,X                                                      
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO CHECK DUPLICATE ORDER NUMBER                             *         
* NTRY - SVONCEL: ORDER NUMBER CONTROL ELEMENT                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING ONAPASD,IOKEY                                                    
X        USING ONCELD,SVONCEL                                                   
CHKDON   NTR1  ,                                                                
         XC    IOKEY,IOKEY         READ ORDER NUMBER ALLOC. PASSIVES            
         MVI   T.ONAPTYP,ONAPTYPQ                                               
         MVI   T.ONAPSUB,ONAPSUBQ                                               
         MVC   T.ONAPCPY,CUABIN                                                 
         MVC   SVPKEY,IOKEY        SAVE ORDER TYPE AND COMPANY                  
*                                                                               
         L     R1,=AL4(XOHID+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         BE    CKDON10                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BZ    CKDON04             RECORD DELETED - GET NEXT                    
         DC    H'0'                IO ERROR                                     
*                                                                               
CKDON04  CLC   T.ONAPKEY(L'SVPKEY),SVPKEY                                       
         BNE   CKDONOK             CHANGE RECORD TYPE/COMPANY                   
CKDON08  L     R1,=AL4(XOSQD+XOACCDIR+XIO2)                                     
         GOTO1 AIO                                                              
         BE    CKDON10                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BZ    CKDON04             RECORD DELETED - GET NEXT                    
         DC    H'0'                IO ERROR                                     
*                                                                               
CKDON10  CLC   T.ONAPKEY(L'SVPKEY),SVPKEY                                       
         BNE   CKDONOK             CHANGE RECORD TYPE/COMPANY                   
*                                                                               
         USING ONARECD,RF                                                       
         L     RF,AIOREC           ORDER NUMBER ALLOC. RECORD                   
         CLC   T.ONAPSAPP,ONAKAPP                                               
         BNE   CKDON14                                                          
         CLC   T.ONAPSCLS,ONAKCLS                                               
         BNE   CKDON14                                                          
         CLC   T.ONAPSOFF,ONAKOFF                                               
         BE    CKDON08             SAME RECORD - OK                             
         DROP  RF                                                               
*                                                                               
CKDON14  CLC   T.ONAPSTA#,X.ONCSTA#                                             
         BH    CKDON18                                                          
         CLC   T.ONAPEND#,X.ONCSTA# MUST BE LESS THAN START OREDER #            
         BL    CKDON08                                                          
         B     CKDONER              ERROR - DUPLICATE ORDER #                   
*                                                                               
CKDON18  CLC   T.ONAPSTA#,X.ONCEND# MUST BE GREATER END ORDER #                 
         BH    CKDON08                                                          
         B     CKDONER              ERROR - DUPLICATE ORDER #                   
*                                                                               
CKDONER  MVC   FVMSGNO,=AL2(AE$DUPON)                                           
         MVI   FVXTRA,C'-'                                                      
         MVC   FVXTRA+2(6),UC@EBUY                                              
         CLI   T.ONAPSAPP,ONAKAEQ                                               
         BE    CKDONER2                                                         
         MVC   FVXTRA+2(6),UC@PRSTO                                             
         CLI   T.ONAPSAPP,ONAKAPQ                                               
         BE    CKDONER2                                                         
         MVC   FVXTRA+2(6),UC@HOUSE                                             
*                                                                               
CKDONER2 MVI   FVXTRA+8,C'/'                                                    
         MVC   FVXTRA+9(3),UC@EXP                                               
         CLI   T.ONAPSCLS,ONAKCEQ                                               
         BE    CKDONER4                                                         
         MVC   FVXTRA+9(3),UC@PRD                                               
         CLI   T.ONAPSCLS,ONAKCPQ                                               
         BE    CKDONER4                                                         
         MVC   FVXTRA+9(3),UC@ARTST                                             
         CLI   T.ONAPSCLS,ONAKCAQ                                               
         BE    CKDONER4                                                         
         MVC   FVXTRA+9(3),UC@INTOR                                             
         CLI   T.ONAPSCLS,ONAKCIQ                                               
         BE    CKDONER4                                                         
         MVC   FVXTRA+9(3),BC@ALL                                               
*                                                                               
CKDONER4 CLC   T.ONAPSOFF,BCSPACES                                              
         BNH   EXITL                                                            
         MVI   FVXTRA+12,C'/'                                                   
         MVC   FVXTRA+13(L'ONAPSOFF),T.ONAPSOFF                                 
         B     EXITL                                                            
*                                                                               
CKDONOK  B     EXITOK                                                           
         DROP  T,X                                                              
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD KEY FOR ORDER NUMBER ALLOC. PASSIVES               *         
* NTRY -  AIOREC - CURRENT ORDER NUMBER ALLOC. RECORD                 *         
* EXIT -  IOKEY   - KEY FOR ONAPKEY                                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING ONAPASD,IOKEY                                                    
BLDKEY   NTR1  ,                                                                
         XC    IOKEY,IOKEY         READ ORDER NUMBER ALLOC. PASSIVES            
         GOTO1 AGETEL,BOPARM,('ONCELQ',AIOREC),0                                
         BE    *+6                                                              
         DC    H'0'                NO ONCEL!!!                                  
*                                                                               
         MVI   T.ONAPTYP,ONAPTYPQ                                               
         MVI   T.ONAPSUB,ONAPSUBQ                                               
         MVC   T.ONAPCPY,CUABIN                                                 
         MVC   T.ONAPSTA#,BOELEM+(ONCSTA#-ONCELD)                               
         MVC   T.ONAPEND#,BOELEM+(ONCEND#-ONCELD)                               
         B     EXITOK                                                           
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECKS FOR EXISTENCE OF ORDER CONTROL RECORD AND         *         
* ACCOMPANYING ONCELD ELEMENT. CREATES IF MISSING                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ORDRECD,IOKEY                                                    
CHKCTRL  NTR1  ,                                                                
         XC    IOKEY,IOKEY         BUILD KEY FOR ORDER CONTROL RECORD           
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUABIN                                                   
         MVC   ORDKORD,=C'000000'                                               
*                                                                               
         L     R1,=AL4(XIO2+XOACCDIR+XORDUP)  READ FOR UPDATE                   
         GOTO1 AIO                                                              
* GOING TO ASSUME THAT NO FOOL WOULD LET U DELETE THE CONTROL REC               
         BH    BLDCTRL - OCONTROL RECORD DOESN'T EXIST, ADD IT                  
         BE    *+6     - RECORD EXISTS - CHECK FOR ONCELD                       
         DC    H'0'    - I/O ERROR                                              
         L     R1,=AL4(XIO2+XOACCMST+XORDUP) READ MASTER REC                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                          RECORD NOT FOUND                   
         USING ORDRECD,R4                                                       
         L     R4,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('ONCELQ',ORDRECD),0  DOES ONCELD EXIST?           
         BE    EXITOK - EVERYTHING EXISTS                                       
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIO2,NEWONC ADD ONCELD             
         CLI   12(R1),0      WAS ADD SUCCESSFUL?                                
         BNE   EXITNOTN      NO                                                 
         LHI   R1,XOPUTREC+XOACCMST+XIO2   YES - SAVE REC                       
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         B     EXITNOTN  - COULDN'T SAVE                                        
         DROP  R4                                                               
*                                                                               
         USING ORDRECD,RF                                                       
BLDCTRL  L     RF,AIO2                                                          
         XC    ORDKEY(ORDRFST-ORDRECD),ORDKEY   BUILD ORDRECD RECORD            
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,CUABIN                                                   
         MVC   ORDKORD,=C'000000'                                               
*                              SET LENGTH                                       
         MVI   ORDRLEN+1,65   (56(REC) + 8(ONCELD) + 1(EOR))                    
*                                                                               
         MVC   ORDRFST(9),NEWONC  ADD ONCELD ELEMENT                            
*                                                                               
         LHI   R1,XOADDREC+XOACCMST+XIO2    WRITE RECORD TO FILE                
         GOTO1 AIO                                                              
         BE    EXITOK                 ADD SUCCESSFUL                            
         BH    EXITNOTN                                                         
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD PASSIVE POINTER                                      *         
* NTRY -  IOKEY  - KEY FOR ONAPKEY                                    *         
*      -  PASACT - PASADD OR PASDEL                                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING ONAPASD,IOKEY                                                    
ONAPAS   NTR1  ,                                                                
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)   READ FOR UPDATE                 
         GOTO1 AIO                                                              
         BE    ONAP10                                                           
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF                                                     
         BNO   *+10                                                             
         MVC   IOKEY,IOKEYSAV          RESTORE KEY IF NOT FOUND                 
                                                                                
ONAP10   NI    T.ONAPSTAT,FF-ONAPSDLT  SET DELETE OFF                           
         CLI   PASACT,PASADDQ          ADD PASSIVE                              
         BE    *+8                                                              
         OI    T.ONAPSTAT,ONAPSDLT     SET IT DELETED                           
*                                                                               
         L     R4,AIOREC                                                        
         USING ONARECD,R4                                                       
         MVC   T.ONAPSAPP,ONAKAPP                                               
         MVC   T.ONAPSCLS,ONAKCLS                                               
         MVC   T.ONAPSOFF,ONAKOFF                                               
         MVC   T.ONAPDA,GSRECDA                                                 
         DROP  R4                                                               
                                                                                
         LHI   R1,XOADD+XOACCDIR+XIO2  ADD PASSIVE                              
         TM    IOERR,IOERNF                                                     
         BO    *+8                                                              
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
*                                                                               
OFFUL    DC    C'2D'                                                            
NEWONC   DC    X'6608F0F0F0F0F0F100' ONCELD TO ADD TO ORDER CONTROL REC         
*                                                                               
DCLISTU  DS    0D                                                               
         DCDDL AC#EBUY,L'UC@EBUY,L                                              
         DCDDL AC#PRSTO,L'UC@PRSTO,L                                            
         DCDDL AC#HOUSE,L'UC@HOUSE,L                                            
         DCDDL AC#PRD,L'UC@PRD,L                                                
         DCDDL AC#EXP,L'UC@EXP,L                                                
         DCDDL AC#ARTST,L'UC@ARTST,L                                            
         DCDDL AC#INTOR,L'UC@INTOR,L                                            
DCLISTUX DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
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
PASACT   DS    CL1                                                              
PASADDQ  EQU   C'A'                                                             
PASDELQ  EQU   C'D'                                                             
*                                                                               
SVONCEL  DS    CL(ONCOANQ+1)       SAVE ORDER NUMBER CONTROL ELEMENT            
SVONSTA# DS    CL(L'ONCSTA#)                                                    
SVONEND# DS    CL(L'ONCEND#)                                                    
SVPKEY   DS    CL(ONAPSPR-ONAPASD)                                              
*                                                                               
DSLISTU  DS    0F                                                               
UC@EBUY  DS    CL8                 EBUYER                                       
UC@PRSTO DS    CL8                 PRESTO                                       
UC@HOUSE DS    CL8                 HOUSE                                        
UC@PRD   DS    CL10                PRODUCTION                                   
UC@EXP   DS    CL10                EXPENSE                                      
UC@ARTST DS    CL10                ARTIST                                       
UC@INTOR DS    CL10                INTERNAL                                     
*                                                                               
OVERWRKN EQU   *-OVERWRKD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACFIL49   08/04/06'                                      
         END                                                                    
