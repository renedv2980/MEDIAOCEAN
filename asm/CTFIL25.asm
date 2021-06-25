*          DATA SET CTFIL25    AT LEVEL 023 AS OF 09/25/17                      
*PHASE TA1325A                                                                  
TA1325   TITLE 'CTFIL25 - RADIO RECORD DISPLAY'                                 
FIL25    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL25*,R7,RR=RE                                              
         USING TWAD,RA                                                          
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
EXITL    CLI   *,255               SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL                                                            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       FILTER RETURN LOW                            
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       FILTER RETURN EQUAL                          
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       FILTER RETURN HIGH                           
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       FILTER RETURN NOT WANTED                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* TABLE  ITERATING ROUTINE - R1=EQUATED VERB                          *         
*                          - RF=A(TABLE)                              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** SET HIGH IF NOT WANT OVERRIDE                
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE                                      
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* OBJECT CONTROLLER                                                   *         
***********************************************************************         
         SPACE 1                                                                
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO                                                       
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     MVI   OKTOSHOW,C'N'                                                    
         MVI   GSSMCODE,C'A'                                                    
*                                                                               
         LA    R2,IOKEY                                                         
         XC    IOKEY,IOKEY                                                      
         USING CT5REC,R2                                                        
         MVI   CT5KTYP,CT5KTYPQ                                                 
         MVC   CT5KALPH,CUASEC                                                  
         ICM   R1,15,=AL4(XOHIGH+XOCONFIL+XIO1)                                 
         GOTOX ('XIO',AGROUTS)                                                  
*                                                                               
         L     R2,AIO1                                                          
         CLI   CT5KTYP,CT5KTYPQ                                                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   CT5KALPH,CUASEC                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         AHI   R2,CT5DATA-CT5REC                                                
         XR    RF,RF                                                            
         USING CTSYSD,R2                                                        
INIT02   CLI   CTSYSEL,0                                                        
         BE    EXITOK                                                           
         CLI   CTSYSEL,CTSYSELQ                                                 
         BNE   INIT04                                                           
         CLI   CTSYSNUM,2                                                       
         BNE   INIT04                                                           
         TM    CTSYSIND,CTSYSRAD+CTSYSMF                                        
         BZ    EXITOK                                                           
         MVI   OKTOSHOW,C'Y'                                                    
         MVI   GSSMCODE,0                                                       
         B     EXITOK                                                           
*                                                                               
INIT04   IC    RF,CTSYSLEN                                                      
         BXH   R2,RF,INIT02                                                     
         DC    H'0'                                                             
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                          *         
***********************************************************************         
         SPACE 1                                                                
KEY      LM    R0,R3,SVPARMS                                                    
         USING CT99RECD,R2                                                      
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS+12       EQUATED VERB                                 
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    CT99KEY,CT99KEY                                                  
         MVI   CT99KTYP,CT99KTYQ                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT FILTER                      *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    CT99KEY,CT99KEY                                                  
         MVI   CT99KTYP,CT99KTYQ                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
DATA     ICM   R1,15,SVPARMS+4     RE HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING CT99RECD,R2                                                      
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
         LM    R1,R3,SVPARMS+8     R1 HOLDS VERB                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
         BR    RF                                                               
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(0201),AL4(UIDDTA)        UNIQUE CODE                         
         DC    AL2(0202),AL4(CALLDTA)       CALL LETTERS                        
         DC    AL2(0203),AL4(BNDDTA)        BAND                                
         DC    AL2(0204),AL4(FRQDTA)        FREQUENCY                           
         DC    AL2(0205),AL4(LCYDTA)        LOCAL CITY                          
         DC    AL2(0206),AL4(STEDTA)        STATE                               
         DC    AL2(0207),AL4(FMTDTA)        FORMAT                              
         DC    AL2(0208),AL4(OWNDTA)        OWNER                               
         DC    AL2(0223),AL4(PARDTA)        PARENT                              
         DC    AL2(0209),AL4(RP1DTA)        REP1                                
         DC    AL2(0210),AL4(RP2DTA)        REP2                                
         DC    AL2(0211),AL4(AMCDTA)        AMCODE                              
         DC    AL2(0212),AL4(CH1DTA)        CALL HISTORY                        
         DC    AL2(0213),AL4(CH2DTA)        CALL HISTORY 2                      
         DC    AL2(0214),AL4(FH1DTA)        FREQ HISTORY                        
         DC    AL2(0215),AL4(FH2DTA)        FREQ HISTORY 2                      
         DC    AL2(0216),AL4(MH1DTA)        FMT HISTORY                         
         DC    AL2(0217),AL4(MH2DTA)        FMT HISTORY 2                       
         DC    AL2(0218),AL4(XH1DTA)        CITY HISTORY 1                      
         DC    AL2(0219),AL4(XH2DTA)        CITY HISTORY 2                      
         DC    AL2(0220),AL4(SH1DTA)        STATE HISTORY 1                     
         DC    AL2(0221),AL4(SH2DTA)        STATE HISTORY 2                     
         DC    AL2(0230),AL4(TA1DTA)        TAPE ADDRESS 1                      
         DC    AL2(0231),AL4(TA2DTA)             ADDRESS 2                      
         DC    AL2(0232),AL4(TCTDTA)             CITY                           
         DC    AL2(0233),AL4(TSTDTA)             STATE                          
         DC    AL2(0234),AL4(TZPDTA)             ZIP                            
         DC    AL2(0235),AL4(TCNDTA)             COUNTRY                        
         DC    AL2(0236),AL4(MA1DTA)        MAILING ADDRESS 1                   
         DC    AL2(0237),AL4(MA2DTA)                ADDRESS 2                   
         DC    AL2(0238),AL4(MCTDTA)                CITY                        
         DC    AL2(0239),AL4(MSTDTA)                STATE                       
         DC    AL2(0240),AL4(MZPDTA)                ZIP                         
         DC    AL2(0241),AL4(MCNDTA)                COUNTRY                     
         DC    AL2(0242),AL4(MINDTA)        MINORITY OWNERSHIP                  
         DC    AL2(0243),AL4(SRCDTA)        OWNER SOURCE                        
         DC    AL2(0244),AL4(MNFDTA)        MINORITY OWNRSHIP IS FEMALE         
         DC    AL2(0245),AL4(FCCDTA)        FCC MINORITY OWNERSHIP              
         DC    AL2(0246),AL4(FCFDTA)        FCC MINORITY IS FEMALE              
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL25    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OWNER SOURCE                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
SRCDTA   LA    RF,SRCETBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SRCETBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSRCE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSRCE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTSRCE)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTSRCE)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTSRCE)                               
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OWNER SOURCE                                                *         
***********************************************************************         
         SPACE 1                                                                
DISSRCE  DS    0H                   PROTECTED                                   
         MVC   FVIFLD(L'CT99KSRC),CT99KSRC                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OWNER SOURCE                                               *         
***********************************************************************         
         SPACE 1                                                                
VALSRCE  CLI   FVIFLD,CT99KOMS      C'S' - MSTREET                              
         BE    VSRCE10                                                          
         CLI   FVIFLD,CT99KOSD      C'R' - SDRS                                 
         BE    VSRCE10                                                          
         CLI   FVIFLD,CT99KOMF      C'F' - MEDIAFRAMEWORKS                      
         BE    VSRCE10                                                          
         CLI   FVIFLD,CT99KODS      C'D' - DS SPOT                              
         BNE   EXITNV                                                           
VSRCE10  MVC   CT99KSRC,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OWNER SOURCE FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTSRCE MVC   FVIFLD(L'CT99KSRC),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OWNER SOURCE FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTSRCE CLI   FVIFLD,CT99KOMS      C'S' - MSTREET                              
         BE    VFSRCE10                                                         
         CLI   FVIFLD,CT99KOSD      C'R' - SDRS                                 
         BE    VFSRCE10                                                         
         CLI   FVIFLD,CT99KOMF      C'F' - MEDIAFRAMEWORKS                      
         BE    VFSRCE10                                                         
         CLI   FVIFLD,CT99KODS      C'D' - DS SPOT                              
         BNE   EXITNV                                                           
VFSRCE10 MVC   FLTIFLD(L'CT99KSRC),FVIFLD                                       
         MVC   CT99KSRC,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR SOURCE                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTSRCE CLC   CT99KSRC,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR UNIQUE ID                                           *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
UIDDTA   LA    RF,UIDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
UIDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUID)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTUID)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTUID)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VUID)                                   
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTUID)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY UNIQUE ID                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISUID   MVC   FVIFLD(L'CT99KUID),CT99KUID                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE UNIQUE ID FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
VUID     TM    FVIIND,FVINUM                                                    
         BO    *+14                                                             
         MVC   CT99KUID,FVIFLD                                                  
         B     EXITOK                                                           
*                                                                               
         MVC   BCDUB,ZEROS                                                      
         LHI   RF,L'CT99KUID                                                    
         XR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         SR    RF,R1                                                            
         LA    RF,BCDUB(RF)                                                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FVIFLD                                                   
         MVC   CT99KUID,BCDUB                                                   
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY UNIQUE ID FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTUID  MVC   FVIFLD(L'CT99KUID),CT99KUID                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE UNIQUE ID FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTUID  TM    FVIIND,FVINUM                                                    
         BO    VFUID02                                                          
         MVC   CT99KUID,FVIFLD                                                  
         MVC   FLTIFLD(L'CT99KUID),FVIFLD                                       
         B     EXITOK                                                           
*                                                                               
VFUID02  MVC   BCDUB,ZEROS                                                      
         LHI   RF,L'CT99KUID                                                    
         XR    R1,R1                                                            
         IC    R1,FVILEN                                                        
         SR    RF,R1                                                            
         LA    RF,BCDUB(RF)                                                     
         BCTR  R1,0                                                             
         EX    R1,VFUMVC                                                        
         MVC   CT99KUID,BCDUB                                                   
         MVC   FLTIFLD(L'CT99KUID),BCDUB                                        
         B     EXITOK                                                           
*                                                                               
VFUMVC   MVC   0(0,RF),FVIFLD                                                   
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR UNIQUE ID                                          *         
***********************************************************************         
         SPACE 1                                                                
DOFTUID  CLC   CT99KUID,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CALL LETTERS                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
CALLDTA  LA    RF,CALLTBL          TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CALLTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCALL)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCALL)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTCALL)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTCALL)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTCALL)                               
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CALL LETTERS                                                *         
***********************************************************************         
         SPACE 1                                                                
DISCALL  NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRCLD,R3                                                         
         MVC   FVIFLD(L'CRCLCLL),CRCLCLL                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE CALL LETTERS                                               *         
***********************************************************************         
         SPACE 1                                                                
VALCALL  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         L     R3,12(R1)                                                        
         USING CRCLD,R3                                                         
         MVC   CRCLCLL,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CALL LETTERS FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTCALL GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRCLD,R3                                                         
         MVC   FVIFLD(L'CRCLCLL),FLTIFLD                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE UNIQUE ID FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VFLTCALL MVC   FLTIFLD(L'CRCLCLL),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DO FILTERING FOR UNIQUE ID                                          *         
***********************************************************************         
         SPACE 1                                                                
DOFTCALL GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   FLTXX                                                            
         L     R3,12(R1)                                                        
         USING CRCLD,R3                                                         
         CLC   CRCLCLL,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BAND                                                *         
***********************************************************************         
         SPACE 1                                                                
BNDDTA   LA    RF,BNDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
BNDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBND)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALBND)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY BAND                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISBND   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRCLD,R3                                                         
         MVC   FVIFLD(L'CRCLBND),CRCLBND                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE BAND                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALBND   CLI   FVIFLD,C'F'                                                      
         BE    VBND02                                                           
         CLI   FVIFLD,C'A'                                                      
         BE    VBND02                                                           
         MVC   FVXTRA,FVIFLD                                                    
         MVC   FVMSGNO,=AL2(444)                                                
         B     EXITL                                                            
*                                                                               
VBND02   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         L     R3,12(R1)                                                        
         USING CRCLD,R3                                                         
         MVC   CRCLBND,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 1                                                                
***********************************************************************         
* DATA OBJECT FOR FREQUENCY                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FRQDTA   LA    RF,FRQTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FRQTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRQ)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHFRQ)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFRQ)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FREQUENCY HEADING                                           *         
***********************************************************************         
         SPACE 1                                                                
DMHFRQ   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FREQUENCY                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISFRQ   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         MVC   FVIFLD(L'CRCLFRQ),CRCLFRQ                                        
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE FREQUENCY                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALFRQ   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         MVC   CRCLFRQ,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LOCAL CITY                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
LCYDTA   LA    RF,LCYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LCYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCY)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY LOCAL CITY                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISLCY   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         MVC   FVIFLD(L'CRCLCTY),CRCLCTY                                        
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE LOCAL CITY                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALLCY   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         MVC   CRCLCTY,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LOCAL STATE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
STEDTA   LA    RF,STETBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
STETBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTE)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTE)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY STATE                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSTE   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         MVC   FVIFLD(L'CRCLSTE),CRCLSTE                                        
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE STATE                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALSTE   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         MVC   CRCLSTE,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* DATA OBJECT FOR FORMAT                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
FMTDTA   LA    RF,FMTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FMTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFMT)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHFMT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFMT)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FORMAT                                                      *         
***********************************************************************         
         SPACE 1                                                                
DMHFMT   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FORMAT                                                      *         
*                                                                               
* ON ENTRY:    (R2)                A(RADIO RECORD)                              
***********************************************************************         
         SPACE 1                                                                
DISFMT   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   FVIFLD(L'CRFMFMT),CRFMFMT                                        
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         XC    WORK,WORK                                                        
         MVC   WORK(1),CT99KSRC-CT99KEY(R2)   GRAB THE SOURCE                   
         MVC   WORK+L'CT99KSRC(L'CRFMFMT),CRFMFMT                               
         GOTO1 (RF),BOPARM,(C'F',WORK),FVIFLD+9                                 
         CLI   BOPARM+4,0                                                       
         BNE   EXITOK                                                           
*                                                                               
         MVI   FVIFLD+8,C'<'                                                    
         LA    R1,FVIFLD+L'CFNAME+10                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'>'                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE FORMAT                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALFMT   L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         XC    WORK,WORK                                                        
         MVC   WORK(1),CT99KSRC-CT99KEY(R2)   GRAB THE SOURCE                   
         MVC   WORK+L'CT99KSRC(L'CRFMFMT),FVIFLD                                
         GOTO1 (RF),BOPARM,(C'F',WORK),FVIFLD+9                                 
         CLI   BOPARM+4,0                                                       
         BNE   EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   CRFMFMT,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OWNER                                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
OWNDTA   LA    RF,OWNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
OWNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOWN)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHOWN)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOWN)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OWNER HEADING                                               *         
***********************************************************************         
         SPACE 1                                                                
DMHOWN   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OWNER                                                       *         
***********************************************************************         
         SPACE 1                                                                
DISOWN   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(1),CT99KSRC-CT99KEY(R2)   GRAB THE SOURCE                   
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         CLI   CRCLBND,C'T'        IS THE BAND T OR L?                          
         BE    *+12                                                             
         CLI   CRCLBND,C'L'                                                     
         BNE   *+12                                                             
         MVI   WORK+L'CT99KSRC,C'T'  YES, THEN TV MEDIA                         
         B     *+8                                                              
         MVI   WORK+L'CT99KSRC,C'R'  ELSE RADIO MEDIA                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   WORK+L'CT99KSRC+L'CRCLBND(L'CRFMOWN),CRFMOWN                     
         MVC   FVIFLD+1(L'CRFMOWN),CRFMOWN                                      
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         GOTO1 (RF),BOPARM,(C'O',WORK),FVIFLD+9                                 
         CLI   BOPARM+4,0                                                       
         BNE   DISOWN50                                                         
*                                                                               
         MVI   FVIFLD+8,C'<'                                                    
         LA    R1,FVIFLD+L'CONAME+10                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'>'                                                       
*                                                                               
DISOWN50 MVC   FVIFLD(5),FVIFLD+1                                               
         MVI   FVIFLD+5,C' '                                                    
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE OWNER                                                      *         
***********************************************************************         
         SPACE 1                                                                
VALOWN   CLI   FVILEN,0                                                         
         BE    VOWN02                                                           
         CLC   FVIFLD(5),BCSPACES                                               
         BE    VOWN02                                                           
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         XC    WORK,WORK                                                        
         MVC   WORK(1),CT99KSRC-CT99KEY(R2)   GRAB THE SOURCE                   
*                                                                               
         LA    R1,CT99DATA-CT99KEY(R2)       R1 = A(1ST ELEMENT)                
         CLI   CRCLBND-CRCLD(R1),C'T'        IS THE BAND T OR L?                
         BE    *+12                                                             
         CLI   CRCLBND-CRCLD(R1),C'L'                                           
         BNE   *+12                                                             
         MVI   WORK+L'CT99KSRC,C'T'  YES, THEN TV MEDIA                         
         B     *+8                                                              
         MVI   WORK+L'CT99KSRC,C'R'  ELSE RADIO MEDIA                           
*                                                                               
         MVC   WORK+L'CT99KSRC+L'CRCLBND(L'CRFMOWN),FVIFLD                      
         GOTO1 (RF),BOPARM,(C'O',WORK),FVIFLD+9                                 
         CLI   BOPARM+4,0                                                       
         BNE   EXITNV                                                           
*                                                                               
VOWN02   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   CRFMOWN,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PARENT                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
PARDTA   LA    RF,PARTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PARTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPAR)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHPAR)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY PARENT HEADING                                              *         
***********************************************************************         
         SPACE 1                                                                
DMHPAR   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY PARENT                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISPAR   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCLELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRCLD,RF                                                         
         MVC   FVIFLD(L'CRCLPRNT),CRCLPRNT                                      
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         XC    WORK,WORK                                                        
         MVC   WORK(1),CT99KSRC-CT99KEY(R2)   GRAB THE SOURCE                   
         MVC   WORK+L'CT99KSRC(L'CRCLPRNT),CRCLPRNT                             
*                                                                               
         GOTO1 (RF),BOPARM,(C'P',WORK),FVIFLD+9                                 
         CLI   BOPARM+4,0                                                       
         BNE   EXITOK                                                           
*                                                                               
         MVI   FVIFLD+8,C'<'                                                    
         LA    R1,FVIFLD+L'CMMNAME+10                                           
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'>'                                                       
*                                                                               
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REP1                                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
RP1DTA   LA    RF,RP1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
RP1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRP1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHRP1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRP1)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY REP1 HEADING                                                *         
***********************************************************************         
         SPACE 1                                                                
DMHRP1   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY REP1                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRP1   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   FVIFLD(L'CRFMREP1),CRFMREP1                                      
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         GOTO1 (RF),BOPARM,(C'R',FVIFLD),FVIFLD+9                               
         CLI   BOPARM+4,0                                                       
         BNE   EXITOK                                                           
*                                                                               
         MVI   FVIFLD+8,C'<'                                                    
         LA    R1,FVIFLD+L'CREAME+10                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'>'                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 1                                                                
***********************************************************************         
* VALIDATE REP1                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALRP1   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   CRFMREP1,FVIFLD                                                  
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR REP2                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
RP2DTA   LA    RF,RP2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
RP2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRP2)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHRP2)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRP2)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY REP2 HEADING                                                *         
***********************************************************************         
         SPACE 1                                                                
DMHRP2   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY REP2                                                        *         
***********************************************************************         
         SPACE 1                                                                
DISRP2   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   FVIFLD(L'CRFMREP2),CRFMREP2                                      
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         GOTO1 (RF),BOPARM,(C'R',FVIFLD),FVIFLD+9                               
         CLI   BOPARM+4,0                                                       
         BNE   EXITOK                                                           
*                                                                               
         MVI   FVIFLD+8,C'<'                                                    
         LA    R1,FVIFLD+L'CREAME+10                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'>'                                                       
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE REP2                                                       *         
***********************************************************************         
         SPACE 1                                                                
VALRP2   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CRFMD,RF                                                         
         MVC   CRFMREP2,FVIFLD                                                  
         B     EXITOK                                                           
         DROP  RF                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AMCODE                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
AMCDTA   LA    RF,AMCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
AMCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAMC)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHAMC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAMC)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY AMCODE HEADING                                              *         
***********************************************************************         
         SPACE 1                                                                
DMHAMC   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY AMCODE                                                      *         
***********************************************************************         
         SPACE 1                                                                
DISAMC   NI    FVATRB,255-FHATPR                                                
         CLI   CSACT,A#CHA         PROTECT UNLESS CHANGE                        
         BE    *+8                                                              
         OI    FVATRB,FHATPR                                                    
*                                                                               
         CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRFMD,R3                                                         
         MVC   FVIFLD(L'CRFMAMC),CRFMAMC                                        
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         GOTO1 (RF),BOPARM,(C'M',FVIFLD),FVIFLD+9                               
         CLI   BOPARM+4,0                                                       
         BNE   EXITOK                                                           
*                                                                               
         MVI   FVIFLD+8,C'<'                                                    
         LA    R1,FVIFLD+L'CMNAME+10                                            
         CLI   0(R1),C' '                                                       
         BH    *+8                                                              
         BCT   R1,*-8                                                           
         MVI   1(R1),C'>'                                                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE AMCODE                                                     *         
***********************************************************************         
         SPACE 1                                                                
VALAMC   CLI   FVILEN,0                                                         
         BE    VAMC02                                                           
         CLC   FVIFLD(3),BCSPACES                                               
         BE    VAMC02                                                           
*                                                                               
         L     RF,ASYSFAC                                                       
         L     RF,VGETRAD-SYSFACD(RF)                                           
         GOTO1 (RF),BOPARM,(C'M',FVIFLD),FVIFLD+9                               
         CLI   BOPARM+4,0                                                       
         BNE   EXITNV                                                           
*                                                                               
VAMC02   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         L     R3,12(R1)                                                        
         USING CRFMD,R3                                                         
         MVC   CRFMAMC,FVIFLD                                                   
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CALL HISTORY 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
CH1DTA   LA    RF,CH1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CH1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCH1)                                 
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CALL HISTORY 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCH1   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRCHD,R3                                                         
         CLC   CRCHHST1,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'1='                                             
         MVC   FVIFLD+2(L'CRCHHST1),CRCHHST1                                    
*                                                                               
         OC    CRCHHDT1,CRCHHDT1                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRCHHDT1),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CALL HISTORY 2                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
CH2DTA   LA    RF,CH2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
CH2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCH2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CALL HISTORY 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DISCH2   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRCHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRCHD,R3                                                         
         CLC   CRCHHST2,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'2='                                             
         MVC   FVIFLD+2(L'CRCHHST2),CRCHHST2                                    
*                                                                               
         OC    CRCHHDT2,CRCHHDT2                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRCHHDT2),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FREQ HISTORY 1                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
FH1DTA   LA    RF,FH1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FH1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFH1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHFH1)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FH1 HEADING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DMHFH1   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FREQ HISTORY 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISFH1   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRFHD,R3                                                         
         CLC   CRFHHST1,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'1='                                             
         MVC   FVIFLD+2(L'CRFHHST1),CRFHHST1                                    
*                                                                               
         OC    CRFHHDT1,CRFHHDT1                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRFHHDT1),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FREQ HISTORY 2                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
FH2DTA   LA    RF,FH2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FH2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFH2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FREQ HISTORY 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DISFH2   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRFHD,R3                                                         
         CLC   CRFHHST2,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'2='                                             
         MVC   FVIFLD+2(L'CRFHHST2),CRFHHST2                                    
*                                                                               
         OC    CRFHHDT2,CRFHHDT2                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRFHHDT2),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FMT HISTORY 1                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MH1DTA   LA    RF,MH1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MH1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMH1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMH1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FMT HEADING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DMHMH1   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FMT  HISTORY 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DISMH1   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRMHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRMHD,R3                                                         
         CLC   CRMHHST1,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'1='                                             
         MVC   FVIFLD+2(L'CRMHHST1),CRMHHST1                                    
*                                                                               
         OC    CRMHHDT1,CRMHHDT1                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRMHHDT1),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FMT  HISTORY 2                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MH2DTA   LA    RF,MH2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MH2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMH2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FMT  HISTORY 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DISMH2   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRMHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     R3,12(R1)                                                        
         USING CRMHD,R3                                                         
         CLC   CRMHHST2,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'2='                                             
         MVC   FVIFLD+2(L'CRMHHST2),CRMHHST2                                    
*                                                                               
         OC    CRMHHDT2,CRMHHDT2                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRMHHDT2),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CITY HISTORY 1                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
XH1DTA   LA    RF,XH1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
XH1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISXH1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHXH1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CITY HEADING 1                                              *         
***********************************************************************         
         SPACE 1                                                                
DMHXH1   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CITY HISTORY 1                                              *         
***********************************************************************         
         SPACE 1                                                                
*&&DO                                                                           
DISXH1   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRZHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRZHD,R3                                                         
         CLC   CRZHHST1,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'1='                                             
         MVC   FVIFLD+2(L'CRZHHST1),CRZHHST1                                    
*                                                                               
         OC    CRZHHDT1,CRZHHDT1                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRZHHDT1),(13,FVIFLD+28)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
*                                                                               
DISXH1   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         XR    R3,R3                                                            
         XR    R4,R4                                                            
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRZHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   *+8                                                              
         L     R3,12(R1)                                                        
         USING CRZHD,R3                                                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRSHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   *+8                                                              
         L     R4,12(R1)                                                        
         USING CRSHD,R4                                                         
*                                                                               
         LTR   R3,R3               MAKE SURE SOMETHING TO DISPLAY               
         BNZ   *+10                                                             
         LTR   R4,R4                                                            
         BZ    EXITOK                                                           
*                                                                               
         LA    R5,FVIFLD+2                                                      
*                                                                               
         LTR   R3,R3               ANY OLD CITY?                                
         BZ    DXH104              NO                                           
         CLC   CRZHHST1,BCSPACES   MAKE SURE CITY IS TEXT                       
         BNH   DXH104                                                           
         MVC   FVIFLD+0(2),=CL2'1='                                             
         MVC   0(L'CRZHHST1,R5),CRZHHST1                                        
         AHI   R5,L'CRZHHST1-1                                                  
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         AHI   R5,1                                                             
*                                                                               
         LTR   R4,R4               SEPERATOR FOR STATE REQUIRED?                
         BZ    DXH104              NO                                           
         CLC   CRSHHST1,BCSPACES                                                
         BNH   DXH104                                                           
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
*                                                                               
DXH104   LTR   R4,R4               ANY OLD STATE?                               
         BZ    DXH106              NO                                           
         CLC   CRSHHST1,BCSPACES                                                
         BNH   DXH106                                                           
         MVC   FVIFLD+0(2),=CL2'1='                                             
         MVC   0(L'CRSHHST1,R5),CRSHHST1                                        
*                                                                               
DXH106   LTR   R3,R3                                                            
         BZ    DXH108                                                           
         OC    CRZHHDT1,CRZHHDT1                                                
         BZ    DXH108                                                           
         GOTO1 VDATCON,BOPARM,(3,CRZHHDT1),(13,FVIFLD+35)                       
         MVC   FVIFLD+0(2),=CL2'1='                                             
*                                                                               
DXH108   LA    R0,FVIFLD+35                                                     
         LTR   R4,R4                                                            
         BZ    EXITOK                                                           
         OC    CRSHHDT1,CRSHHDT1                                                
         BZ    EXITOK                                                           
*                                                                               
         LTR   R3,R3                                                            
         BZ    DXH110                                                           
         OC    CRZHHDT1,CRZHHDT1                                                
         BZ    DXH110                                                           
         CLC   CRZHHDT1,CRSHHDT1                                                
         BE    EXITOK                                                           
         LA    R0,FVIFLD+45                                                     
*                                                                               
DXH110   GOTO1 VDATCON,BOPARM,(3,CRSHHDT1),(13,(R0))                            
         MVC   FVIFLD+0(2),=CL2'1='                                             
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CITY HISTORY 2                                      *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
XH2DTA   LA    RF,XH2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
XH2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISXH2)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHXH2)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CITY HEADING 2                                              *         
***********************************************************************         
         SPACE 1                                                                
DMHXH2   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY CITY HISTORY 2                                              *         
***********************************************************************         
         SPACE 1                                                                
*&&DO                                                                           
DISXH2   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRZHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRZHD,R3                                                         
         CLC   CRZHHST2,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'2='                                             
         MVC   FVIFLD+2(L'CRZHHST2),CRZHHST2                                    
*                                                                               
         OC    CRZHHDT2,CRZHHDT2                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRZHHDT2),(13,FVIFLD+28)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
*&&                                                                             
DISXH2   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         XR    R3,R3                                                            
         XR    R4,R4                                                            
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRZHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   *+8                                                              
         L     R3,12(R1)                                                        
         USING CRZHD,R3                                                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRSHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   *+8                                                              
         L     R4,12(R1)                                                        
         USING CRSHD,R4                                                         
*                                                                               
         LTR   R3,R3               MAKE SURE SOMETHING TO DISPLAY               
         BNZ   *+10                                                             
         LTR   R4,R4                                                            
         BZ    EXITOK                                                           
*                                                                               
         LA    R5,FVIFLD+2                                                      
*                                                                               
         LTR   R3,R3               ANY OLD CITY?                                
         BZ    DXH204              NO                                           
         CLC   CRZHHST2,BCSPACES   MAKE SURE CITY IS TEXT                       
         BNH   DXH204                                                           
         MVC   FVIFLD+0(2),=CL2'2='                                             
         MVC   0(L'CRZHHST2,R5),CRZHHST2                                        
         AHI   R5,L'CRZHHST2-1                                                  
         CLI   0(R5),C' '                                                       
         BH    *+8                                                              
         BCT   R5,*-8                                                           
         AHI   R5,1                                                             
*                                                                               
         LTR   R4,R4               SEPERATOR FOR STATE REQUIRED?                
         BZ    DXH204              NO                                           
         CLC   CRSHHST2,BCSPACES                                                
         BNH   DXH204                                                           
         MVI   0(R5),C'/'                                                       
         AHI   R5,1                                                             
*                                                                               
DXH204   LTR   R4,R4               ANY OLD STATE?                               
         BZ    DXH206              NO                                           
         CLC   CRSHHST2,BCSPACES                                                
         BNH   DXH206                                                           
         MVC   FVIFLD+0(2),=CL2'2='                                             
         MVC   0(L'CRSHHST2,R5),CRSHHST2                                        
*                                                                               
DXH206   LTR   R3,R3                                                            
         BZ    DXH208                                                           
         OC    CRZHHDT2,CRZHHDT2                                                
         BZ    DXH208                                                           
         GOTO1 VDATCON,BOPARM,(3,CRZHHDT2),(13,FVIFLD+35)                       
         MVC   FVIFLD+0(2),=CL2'2='                                             
*                                                                               
DXH208   LA    R0,FVIFLD+35                                                     
         LTR   R4,R4                                                            
         BZ    EXITOK                                                           
         OC    CRSHHDT2,CRSHHDT2                                                
         BZ    EXITOK                                                           
*                                                                               
         LTR   R3,R3                                                            
         BZ    DXH210                                                           
         OC    CRZHHDT2,CRZHHDT2                                                
         BZ    DXH210                                                           
         CLC   CRZHHDT2,CRSHHDT2                                                
         BE    EXITOK                                                           
         LA    R0,FVIFLD+45                                                     
*                                                                               
DXH210   GOTO1 VDATCON,BOPARM,(3,CRSHHDT2),(13,(R0))                            
         MVC   FVIFLD+0(2),=CL2'2='                                             
         B     EXITOK                                                           
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATE HISTORY 1                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
SH1DTA   LA    RF,SH1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SH1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSH1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHSH1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY STATE HEADING 1                                             *         
***********************************************************************         
         SPACE 1                                                                
DMHSH1   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY STATE HISTORY 1                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSH1   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRSHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRSHD,R3                                                         
         CLC   CRSHHST1,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'1='                                             
         MVC   FVIFLD+2(L'CRSHHST1),CRSHHST1                                    
*                                                                               
         OC    CRSHHDT1,CRSHHDT1                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRSHHDT1),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATE HISTORY 2                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
SH2DTA   LA    RF,SH2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
SH2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSH2)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHSH2)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY STATE HEADING 2                                             *         
***********************************************************************         
         SPACE 1                                                                
DMHSH2   CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY STATE HISTORY 2                                             *         
***********************************************************************         
         SPACE 1                                                                
DISSH2   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRSHELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRSHD,R3                                                         
         CLC   CRSHHST2,BCSPACES                                                
         BNH   EXITOK                                                           
         MVC   FVIFLD+0(2),=CL2'2='                                             
         MVC   FVIFLD+2(L'CRSHHST2),CRSHHST2                                    
*                                                                               
         OC    CRSHHDT2,CRSHHDT2                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(3,CRSHHDT2),(13,FVIFLD+10)                       
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAPE ADDRESS 1                                      *         
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
TA1DTA   LA    RF,TA1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TA1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTA1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHTA1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TAPE ADDRESS HEADINGS, MAILING ADDRESS HEADINGS, AND                  
*    MINORITY HEADING                                                           
***********************************************************************         
DMHTA1   DS    0H                                                               
DMHTA2   DS    0H                                                               
DMHTCT   DS    0H                                                               
DMHTST   DS    0H                                                               
DMHTZP   DS    0H                                                               
DMHTCN   DS    0H                                                               
DMHMA1   DS    0H                                                               
DMHMA2   DS    0H                                                               
DMHMCT   DS    0H                                                               
DMHMST   DS    0H                                                               
DMHMZP   DS    0H                                                               
DMHMCN   DS    0H                                                               
DMHMIN   DS    0H                                                               
DMHMNF   DS    0H                                                               
DMHFCC   DS    0H                                                               
DMHFCF   DS    0H                                                               
         CLI   OKTOSHOW,C'Y'                                                    
         BE    EXITOK                                                           
         ICM   RF,15,SVPARMS6                                                   
         MVI   0(RF),X'FF'                                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TAPE ADDRESS 1                                              *         
***********************************************************************         
DISTA1   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'T',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADAD1),CMADAD1                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAPE ADDRESS 2                                                
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
TA2DTA   LA    RF,TA2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TA2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTA2)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHTA2)                                
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY TAPE ADDRESS 2                                                        
***********************************************************************         
DISTA2   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'T',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADAD2),CMADAD2                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAPE CITY                                                     
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
TCTDTA   LA    RF,TCTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TCTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCT)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHTCT)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TAPE CITY                                                             
***********************************************************************         
         SPACE 1                                                                
DISTCT   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'T',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADCITY),CMADCITY                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAPE STATE                                                    
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
TSTDTA   LA    RF,TSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTST)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHTST)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TAPE STATE                                                            
***********************************************************************         
         SPACE 1                                                                
DISTST   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'T',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADSTAT),CMADSTAT                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAPE ZIP                                                      
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
TZPDTA   LA    RF,TZPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TZPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTZP)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHTZP)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TAPE ZIP                                                              
***********************************************************************         
         SPACE 1                                                                
DISTZP   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'T',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADZIP),CMADZIP                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TAPE COUNTRY                                                  
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
TCNDTA   LA    RF,TCNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
TCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTCN)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHTCN)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY TAPE COUNTRY                                                          
***********************************************************************         
         SPACE 1                                                                
DISTCN   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'T',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADCTRY),CMADCTRY                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MAILING ADDRESS 1                                   *         
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MA1DTA   LA    RF,MA1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MA1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMA1)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMA1)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MAILING ADDRESS 1                                           *         
***********************************************************************         
         SPACE 1                                                                
DISMA1   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'M',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADAD1),CMADAD1                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MAILING ADDRESS 2                                   *         
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MA2DTA   LA    RF,MA2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MA2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMA2)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMA2)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MAILING ADDRESS 2                                           *         
***********************************************************************         
         SPACE 1                                                                
DISMA2   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'M',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADAD2),CMADAD2                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MAILING CITY                                        *         
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MCTDTA   LA    RF,MCTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MCTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMCT)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMCT)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MAILING CITY                                                *         
***********************************************************************         
         SPACE 1                                                                
DISMCT   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'M',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADCITY),CMADCITY                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MAILING STATE                                       *         
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MSTDTA   LA    RF,MSTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MSTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMST)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMST)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MAILING STATE                                               *         
***********************************************************************         
         SPACE 1                                                                
DISMST   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'M',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADSTAT),CMADSTAT                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MAILING ZIP                                         *         
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MZPDTA   LA    RF,MZPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MZPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMZP)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMZP)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MAILING ZIP                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISMZP   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'M',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADZIP),CMADZIP                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MAILING COUNTRY                                     *         
***********************************************************************         
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MCNDTA   LA    RF,MCNTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MCNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMCN)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMCN)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MAILING COUNTRY                                             *         
***********************************************************************         
         SPACE 1                                                                
DISMCN   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CMADRELQ',(R2)),=C'M',0          
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CMADRMD,R3                                                       
         MVC   FVIFLD(L'CMADCTRY),CMADCTRY                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MINORITY OWNERSHIP                                  *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MINDTA   LA    RF,MINTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MINTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMIN)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMIN)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MINORITY                                                    *         
***********************************************************************         
         SPACE 1                                                                
DISMIN   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRFMD,R3                                                         
         CLI   CRFMLN,CRFMLNQ      NEW LENGTH?                                  
         BNH   EXITOK                                                           
         MVC   FVIFLD(L'CRFMMIN),CRFMMIN                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MINORITY OWNERSHIP IS FEMALE?                       *         
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
MNFDTA   LA    RF,MNFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
MNFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISMNF)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHMNF)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY MINORITY OWNERSHIP IS FEMALE                                *         
***********************************************************************         
         SPACE 1                                                                
DISMNF   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
* LOOKING FOR CRFMELQ - X'02' ELEMENT                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRFMD,R3                                                         
         CLI   CRFMLN,CRFMLNQ      NEW LENGTH?                                  
         BNH   EXITOK                                                           
         MVC   FVIFLD(L'CRFMMINF),CRFMMINF                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FCC QUALIFIED MINORITY OWNERSHIP                              
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
FCCDTA   LA    RF,FCCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FCCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFCC)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHFCC)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FCC QUALIFIED MINORITY OWNERSHIP                                      
***********************************************************************         
         SPACE 1                                                                
DISFCC   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRFMD,R3                                                         
         CLI   CRFMLN,CRFMLNQ      NEW LENGTH?                                  
         BNH   EXITOK                                                           
         MVC   FVIFLD(L'CRFMQFCC),CRFMQFCC                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FCC QUALIFIED MINORITY OWNERSHIP IS FEMALE?                   
***********************************************************************         
         SPACE 1                                                                
         USING CT99RECD,R2         R2 HOLDS A(RECORD)                           
FCFDTA   LA    RF,FCFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FCFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFCF)                                 
         DC    AL1(DMHED),AL1(0,0,0),AL4(DMHFCF)                                
         DC    AL1(EOT)                                                         
         SPACE 1                                                                
***********************************************************************         
* DISPLAY FCC QUALIFIED OWNERSHIP IS FEMALE                                     
***********************************************************************         
         SPACE 1                                                                
DISFCF   CLI   OKTOSHOW,C'Y'       SUBSCRIBED?                                  
         BNE   EXITOK              NO                                           
* LOOKING FOR CRFMELQ - X'02' ELEMENT                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CRFMELQ',(R2)),0                 
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING CRFMD,R3                                                         
         CLI   CRFMLN,CRFMLNQ      NEW LENGTH?                                  
         BNH   EXITOK                                                           
         MVC   FVIFLD(L'CRFMQFCF),CRFMQFCF                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R2,SVPARMS                                                    
THIS     USING CT99RECD,R2                                                      
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LINIT),AL1(0,0,0),AL4(INITL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
INITL    MVC   LSNUMHED,=AL2(1)                                                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST FOR LIST PAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CT99KEY),THIS.CT99RECD                                   
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5                                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     MVC   IOKEY(L'CT99KEY),THIS.CT99RECD                                   
         XR    RF,RF                                                            
         IC    RF,IOKEY+L'CT99KEY-1                                             
         AHI   RF,1                                                             
         STC   RF,IOKEY+L'CT99KEY-1                                             
*                                                                               
         ICM   R1,15,=AL4(XOHI+XOCONFIL)                                        
         A     R1,SVPARMS5                                                      
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   L     RF,SVPARMS5         CONTROL FILE RECORDS ARE RETURNED IN         
         SRL   RF,4-2              THE I/O AREA                                 
         L     RF,AIO1-L'AIO1(RF)                                               
         CLC   IOKEY(CT99KSPR-CT99KEY),0(RF)  SHOULD BE A RADIO RECORD          
         BNE   EXITL                          FOR THE SAME SOURCE               
*&&DO                                                                           
         CLI   0(RF),CT99KTYQ                                                   
         BNE   EXITL                                                            
         CLI   1(RF),CT99KSRA                                                   
         BNE   EXITL                                                            
*&&                                                                             
         MVC   THIS.CT99RECD(L'CT99KEY),0(RF)                                   
         B     EXITOK                                                           
         DROP  THIS                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FFS      DC    16X'FF'                                                          
ZEROS    DC    8C'0'                                                            
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
OKTOSHOW DS    X                                                                
WORK     DS    XL64                                                             
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        CTGENRAD                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENRAD                                                       
         PRINT ON                                                               
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        CTMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        CTGENFILE                                                              
         PRINT OFF                                                              
       ++INCLUDE CTGENFILE                                                      
         PRINT ON                                                               
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023CTFIL25   09/25/17'                                      
         END                                                                    
