*          DATA SET CTFIL23    AT LEVEL 030 AS OF 08/22/00                      
*PHASE TA1323A                                                                  
FIL23    TITLE 'PHASE RECORDS'                                                  
FIL23    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL23*,R6,R7,RR=RE                                           
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
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
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
EXIT     L     R1,CALLR1                                                        
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       FILTER RETURN LOW                            
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       FILTER RETURN EQUAL                          
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       FILTER RETURN HIGH                           
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       FILTER RETURN NOT WANTED                     
         B     EXITOK                                                           
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
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
INIT     GOTO1 VDICTAT,BOPARM,C'LU',DCLIST,DSLISTU                              
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
*                                                                               
         ICM   RF,8,=C'R'          LOAD DAYVAL                                  
         ICM   RF,7,=XL3'000A03'                                                
         GOTOX VCOLY,BOPARM,0,(RF),0                                            
         CLI   4(R1),FF                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VDAYVAL,0(R1)                                                    
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
KEY      LM    R0,R3,SVPARMS                                                    
         USING CTPHRECD,R2                                                      
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
KEYFRST  L     R1,SVPARMS+12       EQUATED VERB                                 
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
KFKVAL   XC    CTPHPKEY,CTPHPKEY                                                
         MVI   CTPHID,CTPHIDQ                                                   
         MVI   CTPHSUBI,CTPHSUBQ                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    CTPHPKEY,CTPHPKEY                                                
         MVI   CTPHID,CTPHIDQ                                                   
         MVI   CTPHSUBI,CTPHSUBQ                                                
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
DATA     ICM   R1,15,SVPARMS+4     RE HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING CTPHRECD,R2                                                      
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
         LM    R1,R3,SVPARMS+8     R1 HOLDS VERB                                
         USING CTPHRECD,R2         R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(00400),AL4(PHSDTA)       PHASE NUMBER                        
         DC    AL2(00401),AL4(LNGDTA)       LANGUAGE                            
         DC    AL2(00402),AL4(DSCDTA)       DESCRIPTION                         
         DC    AL2(00403),AL4(LVLDTA)       LEVEL                               
         DC    AL2(00404),AL4(CORDTA)       CORE RESIDENT                       
         DC    AL2(00405),AL4(SNDDTA)       START NODE                          
         DC    AL2(00406),AL4(ENDDTA)       END NODE                            
         DC    AL2(00407),AL4(SCRDTA)       PHASE TYPE                          
         DC    AL2(00408),AL4(DMYDTA)       CORE=DUMMY                          
         DC    AL2(00409),AL4(SBYDTA)       SPARE BYTES                         
         DC    AL2(00410),AL4(OFFDTA)       OFFLINE ONLY                        
         DC    AL2(00411),AL4(RLDDTA)       RELOAD DATE                         
         DC    AL2(00412),AL4(NEDDTA)       ACCEPT ANY DATE                     
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL23    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS+8                                                     
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
DFDDIS   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CTPHSCEQ',(R2)),0                
         CLI   12(R1),0                                                         
         BNE   *+12                                                             
         L     RE,12(R1)                                                        
         ST    RE,ASYSEL                                                        
*                                                                               
         GOTO1 (RF),(R1),,('CTPHLCEQ',(R2)),0                                   
         CLI   12(R1),0                                                         
         BNE   *+12                                                             
         L     RE,12(R1)                                                        
         ST    RE,ALSTEL                                                        
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS+8                                                     
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PHASE NUMBER                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PHSDTA   LA    RF,PHSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PHSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPHS)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPHS)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPHS)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPHS)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTPHS)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PHASE NUMBER FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISPHS   OC    CTPHNAME(3),CTPHNAME                                             
         BZ    *+14                                                             
         MVC   FVIFLD(L'CTPHNAME),CTPHNAME                                      
         B     EXITOK                                                           
*                                                                               
         GOTO1 VHEXOUT,BOPARM,CTPHHEXN,BODUB1,L'CTPHHEXN,0                      
         MVI   BODUB1,C'T'                                                      
         MVC   FVIFLD(L'CTPHNAME),BODUB1                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PHASE NUMBER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALPHS   CLI   FVIFLD,C'T'         ONLINE PHASE?                                
         BE    *+14                                                             
         MVC   CTPHNAME,FVIFLD                                                  
         B     EXITOK                                                           
*                                                                               
         MVC   BODUB1,FVIFLD                                                    
         MVI   BODUB1,C'0'                                                      
         GOTO1 VHEXIN,BOPARM,BODUB1,CTPHHEXN,6,0                                
         ICM   RF,15,12(R1)                                                     
         BZ    EXITNV                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PHASE NUMBER FILTER FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLTPHS  OC    FLTIFLD(3),FLTIFLD                                               
         BZ    *+14                                                             
         MVC   FVIFLD(L'CTPHNAME),CTPHNAME                                      
         B     EXITOK                                                           
*                                                                               
         GOTO1 VHEXOUT,BOPARM,FLTIFLD,BODUB1,L'CTPHHEXN,0                       
         MVI   BODUB1,C'T'                                                      
         MVC   FVIFLD(L'CTPHNAME),BODUB1                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PHASE NUMBER FILTER FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
VFLTPHS  CLI   FVIFLD,C'T'         ONLINE PHASE?                                
         BE    VFPHS02                                                          
         MVC   CTPHNAME,FVIFLD                                                  
         MVC   FLTIFLD(L'CTPHNAME),FVIFLD                                       
         B     EXITOK                                                           
*                                                                               
VFPHS02  MVC   BODUB1,FVIFLD                                                    
         MVI   BODUB1,C'0'                                                      
         GOTO1 VHEXIN,BOPARM,BODUB1,CTPHHEXN,6,0                                
         ICM   RF,15,12(R1)                                                     
         BZ    EXITNV                                                           
         MVC   FLTIFLD(L'CTPHNAME),CTPHNAME                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR PHASE NUMBER                                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTPHS  CLC   CTPHNAME,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LANGUAGE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LNGDTA   LA    RF,LNGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LNGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNG)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLNG)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLNG)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLNG)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLNG)                                
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFFLTLNG)                              
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFLFLNG)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR MAINT SCREEN FOR LANGUAGE                           *         
***********************************************************************         
         SPACE 1                                                                
DFFLTLNG MVC   FVIFLD(3),=CL3'ENG'                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR LIST SCREEN FOR LANGUAGE                            *         
***********************************************************************         
         SPACE 1                                                                
DFLFLNG  MVC   FVIFLD(3),=CL3'ENG'                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LANGUAGE FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISLNG   LA    R3,LANGTAB                                                       
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         A     RF,BORELO                                                        
         LA    R3,6(R3)                                                         
         USING LANGTABD,R3                                                      
*                                                                               
DLNG02   CLC   CTPHLANG,LANGCODE                                                
         BE    *+10                                                             
         BXLE  R3,RE,DLNG02                                                     
         DC    H'0'                                                             
*                                                                               
         MVC   FVIFLD(L'LANGSHR),LANGSHR                                        
         CLI   LSHRN,C'Y'                                                       
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'LANGSHRN),LANGSHRN                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LANGUAGE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALLNG   LA    R3,LANGTAB                                                       
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         A     RF,BORELO                                                        
         LA    R3,6(R3)                                                         
         USING LANGTABD,R3                                                      
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
VLNG02   EX    R1,*+8                                                           
         BE    VLNG06                                                           
         CLC   FVIFLD(0),LANGSHR                                                
         EX    R1,*+8                                                           
         BE    VLNG04                                                           
         CLC   FVIFLD(0),LANGSHRN                                               
         BXLE  R3,RE,VLNG02                                                     
         B     EXITNV                                                           
*                                                                               
VLNG04   MVI   LSHRN,C'Y'                                                       
*                                                                               
VLNG06   MVC   CTPHLANG,LANGCODE   GOOD LANGUAGE CODE                           
         XI    CTPHLANG,X'FF'      FLIP THE BITS OF LANG CODE                   
*&&US*&& CLI   CTPHLANG,X'FE'      IN U.S., LANG EUK NOT ALLOWED                
*&&UK*&& CLI   CTPHLANG,X'FD'      IN U.K., LANG EUS NOT ALLOWED                
         BE    EXITNV                                                           
         XI    CTPHLANG,X'FF'      FLIP THE BITS OF LANG CODE AGAIN             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LANGUAGE FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTLNG  LA    R3,LANGTAB                                                       
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         A     RF,BORELO                                                        
         LA    R3,6(R3)                                                         
         USING LANGTABD,R3                                                      
*                                                                               
DFLNG02  CLC   LANGCODE,FLTIFLD                                                 
         BE    *+10                                                             
         BXLE  R3,RE,DFLNG02                                                    
         DC    H'0'                                                             
*                                                                               
         MVC   FVIFLD(L'LANGSHR),LANGSHR                                        
         CLI   LSHRN,C'Y'                                                       
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'LANGSHRN),LANGSHRN                                      
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LANGUAGE FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTLNG  LA    R3,LANGTAB                                                       
         LH    RE,0(R3)                                                         
         L     RF,2(R3)                                                         
         A     RF,BORELO                                                        
         LA    R3,6(R3)                                                         
         USING LANGTABD,R3                                                      
*                                                                               
         XR    R1,R1                                                            
         IC    R1,FVXLEN                                                        
VFLNG02  EX    R1,*+8                                                           
         BE    VFLNG06                                                          
         CLC   FVIFLD(0),LANGSHR                                                
         EX    R1,*+8                                                           
         BE    VFLNG04                                                          
         CLC   FVIFLD(0),LANGSHRN                                               
         BXLE  R3,RE,VFLNG02                                                    
         B     EXITNV                                                           
*                                                                               
VFLNG04  MVI   LSHRN,C'Y'                                                       
*                                                                               
VFLNG06  MVC   CTPHLANG,LANGCODE   GOOD LANGUAGE CODE                           
         XI    CTPHLANG,X'FF'      FLIP THE BITS OF LANG CODE                   
*&&US*&& CLI   CTPHLANG,X'FE'      IN U.S., LANG EUK NOT ALLOWED                
*&&UK*&& CLI   CTPHLANG,X'FD'      IN U.K., LANG EUS NOT ALLOWED                
         BE    EXITNV                                                           
         XI    CTPHLANG,X'FF'      FLIP THE BITS OF LANG CODE AGAIN             
         MVC   FLTIFLD(L'CTPHLANG),CTPHLANG                                     
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LANGUAGE                                           *         
***********************************************************************         
         SPACE 1                                                                
DOFTLNG  CLC   CTPHLANG,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LEVEL                                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LVLDTA   LA    RF,LVLTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LVLTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLVL)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLVL)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLVL)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLVL)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLVL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISLVL   CLI   CTPHLVL,0                                                        
         BE    EXITOK                                                           
         MVC   FVIFLD(L'CTPHLVL),CTPHLVL                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL FIELD                                                *         
***********************************************************************         
         SPACE 1                                                                
VALLVL   XC    CTPHLVL,CTPHLVL                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'A'                                                      
         BL    EXITNV                                                           
         CLI   FVIFLD,C'C'                                                      
         BH    EXITNV                                                           
         MVC   CTPHLVL,FVIFLD                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY LEVEL FILTER FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTLVL  CLI   FLTIFLD,0                                                        
         BE    EXITOK                                                           
         MVC   FVIFLD(L'CTPHLVL),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE LEVEL FILTER FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VFLTLVL  XC    CTPHLVL,CTPHLVL                                                  
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLI   FVIFLD,C'P'         PRODUCTION ONLY                              
         BNE   VFLVL02                                                          
         MVI   FLTIFLD,C'P'                                                     
         B     EXITOK                                                           
*                                                                               
VFLVL02  CLI   FVIFLD,C'A'                                                      
         BL    EXITNV                                                           
         CLI   FVIFLD,C'C'                                                      
         BH    EXITNV                                                           
         MVC   CTPHLVL,FVIFLD                                                   
         MVC   FLTIFLD(L'CTPHLVL),CTPHLVL                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LANGUAGE                                           *         
***********************************************************************         
         SPACE 1                                                                
DOFTLVL  CLI   FLTIFLD,C'P'        SPECIAL PRODUCTION FILTER                    
         BNE   DOFLVL02            NO                                           
         CLI   CTPHLVL,0                                                        
         BE    FLTXE                                                            
         B     FLTXX                                                            
*                                                                               
DOFLVL02 CLC   CTPHLVL,FLTIFLD     REGULAR A,B,C                                
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DESCRIPTION                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSCDTA   LA    RF,DSCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DSCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DESCRIPTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISDSC   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('CTPHDCEQ',(R2)),0                
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
         L     RF,12(R1)                                                        
         USING CTPHDSCD,RF                                                      
         XR    R1,R1                                                            
         IC    R1,CTPHDLEN                                                      
         AHI   R1,-(CTPHDOVQ+1)                                                 
         BM    EXITOK                                                           
         EX    R1,*+4                                                           
         MVC   FVIFLD(0),CTPHDDSC                                               
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DESCRIPTION                                                *         
***********************************************************************         
         SPACE 1                                                                
VALDSC   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('CTPHDCEQ',(R2)),0                
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         XC    BOELEM,BOELEM       BUILD DESCRIPTION ELEMENT                    
TMP      USING CTPHDSCD,BOELEM                                                  
         MVI   TMP.CTPHDCDE,CTPHDCEQ                                            
         XR    RF,RF                                                            
         IC    RF,FVILEN                                                        
         AHI   RF,CTPHDOVQ                                                      
         STC   RF,TMP.CTPHDLEN                                                  
         IC    RF,FVXLEN                                                        
         EX    RF,*+4                                                           
         MVC   TMP.CTPHDDSC(0),FVIFLD                                           
         DROP  TMP                                                              
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),(R2),BOELEM,0                      
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PHASE CORE RESIDENT                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
CORDTA   LA    RF,CORTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CORTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CORE RESIDENT FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISCOR   ICM   R3,15,ASYSEL        SYSTEM ELEMENT?                              
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
*                                                                               
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    CTPHSFL1,CTPHSCRQ                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CORE RESIDENT FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
VALCOR   LTR   R3,R3               SYSTEM ELEMENT?                              
         BZ    DIE                                                              
         NI    CTPHSFL1,FF-(CTPHSCRQ)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VCOR02   EX    RE,*+8              YES                                          
         BE    VCOR04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VCOR04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VCOR06                                                           
*                                                                               
VCOR04   OI    CTPHSFL1,CTPHSCRQ                                                
         B     EXITOK                                                           
*                                                                               
VCOR06   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR START NODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SNDDTA   LA    RF,SNDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SNDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSND)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSND)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY START NODE FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSND   ICM   R3,15,ASYSEL        SYSTEM ELEMENT                               
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
*                                                                               
         ICM   RF,8,CTPHSNDE       OUTPUT START NODE                            
         SRL   RF,32-4                                                          
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  FVIFLD(1),BODUB1                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE START NODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALSND   MVI   SNDVAL,0            DEFAULT START NODE IS ZERO                   
         CLI   FVILEN,0                                                         
         BNE   VSND02                                                           
*                                                                               
         OC    CTPHNAME(3),CTPHNAME                                             
         BZ    EXITOK              OFFLINE DEFAULT IS ZERO                      
         CLI   CTPHHEXN,0                                                       
         BE    EXITOK              SYSTEM ZERO DEFAULT IS ZERO                  
         CLI   CTPHHEXN+2,X'C0'                                                 
         BNL   EXITOK              SCREEN DEFAULT IS ZERO                       
*                                                                               
         MVI   SNDVAL,1            PROGRAM DEFAULT IS 1                         
         CLI   CTPHHEXN+2,0        UNLESS IT IS THE CONTROLLER                  
         BNE   EXITOK                                                           
         MVI   SNDVAL,0            WHEN IT IS ZERO AGAIN                        
         B     EXITOK                                                           
*                                                                               
VSND02   ICM   RF,15,BCFULL                                                     
         CHI   RF,16                                                            
         BNL   EXITNV                                                           
         STC   RF,SNDVAL                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR END NODE                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
ENDDTA   LA    RF,ENDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ENDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEND)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEND)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY END NODE FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISEND   ICM   R3,15,ASYSEL        SYSTEM ELEMENT                               
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
*                                                                               
         ICM   RF,8,CTPHSNDE       OUTPUT END NODE                              
         SLL   RF,4                                                             
         SRL   RF,32-4                                                          
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  FVIFLD(1),BODUB1                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE END NODE FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALEND   MVI   ENDVAL,0            DEFAULT END NODE IS ZERO                     
         CLI   FVILEN,0                                                         
         BNE   VEND02                                                           
*                                                                               
         OC    CTPHNAME(3),CTPHNAME                                             
         BZ    VEND04              OFFLINE DEFAULT IS ZERO                      
         CLI   CTPHHEXN,0                                                       
         BE    VEND04              SYSTEM ZERO DEFAULT IS ZERO                  
         CLI   CTPHHEXN+2,X'C0'                                                 
         BNL   VEND04              SCREEN DEFAULT IS ZERO                       
*                                                                               
         CLI   CTPHHEXN+2,0        PROGRAM DEFAULTS TO ZERO                     
         BNE   VEND04              UNLESS IT IS THE CONTROLLER                  
         MVI   ENDVAL,1            WHEN IT IS ONE                               
         B     VEND04                                                           
*                                                                               
VEND02   ICM   RF,15,BCFULL                                                     
         CHI   RF,16                                                            
         BNL   EXITNV                                                           
         STC   RF,ENDVAL                                                        
         B     EXITOK                                                           
*                                                                               
VEND04   XR    RF,RF                                                            
         ICM   RF,1,ENDVAL                                                      
         SLL   RF,32-4                                                          
         XR    RE,RE                                                            
         ICM   RE,1,SNDVAL                                                      
         SRDL  RE,32-4                                                          
         STC   RF,CTPHSNDE                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROGRAM OR SCREEN                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SCRDTA   LA    RF,SCRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SCRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCR)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY PROGRAM OR SCREEN FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISSCR   ICM   R3,15,ASYSEL        SYSTEM ELEMENT?                              
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
*                                                                               
         MVC   FVIFLD(L'LC@PROG),LC@PROG                                        
         TM    CTPHSFL1,CTPHSSCQ                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@CSCR),LC@CSCR                                        
         TM    CTPHSFL1,CTPHSCRQ+CTPHSCSQ                                       
         BO    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@SCRN),LC@SCRN                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE PROGRAM OR SCREEN FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VALSCR   LTR   R3,R3               SYSTEM ELEMENT?                              
         BZ    DIE                                                              
         NI    CTPHSFL1,FF-(CTPHSCRQ+CTPHSCSQ)                                  
         CLI   FVILEN,0                                                         
         BE    VSCR12                                                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
         EX    RE,*+8              PROGRAM                                      
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@PROG                                                
         EX    RE,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@PROG                                                
*                                                                               
VSCR02   EX    RE,*+8              SCREEN                                       
         BE    VSCR04                                                           
         CLC   FVIFLD(0),UE@SCRN                                                
         EX    RE,*+8                                                           
         BE    VSCR04                                                           
         CLC   FVIFLD(0),LC@SCRN                                                
         B     VSCR06                                                           
*                                                                               
VSCR04   OI    CTPHSFL1,CTPHSSCQ                                                
         B     EXITOK                                                           
*                                                                               
VSCR06   EX    RE,*+8              CORE RESIDENT SCREEN                         
         BE    VSCR08                                                           
         CLC   FVIFLD(0),UE@CSCR                                                
         EX    RE,*+8                                                           
         BE    VSCR08                                                           
         CLC   FVIFLD(0),LC@CSCR                                                
         B     VSCR10                                                           
*                                                                               
VSCR08   OI    CTPHSFL1,CTPHSSCQ+CTPHSCSQ                                       
         B     EXITOK                                                           
*                                                                               
VSCR10   B     EXITNV              NOTHING ELSE IS VALID                        
*                                                                               
VSCR12   OC    CTPHNAME(3),CTPHNAME                                             
         BZ    EXITOK              OFFLINE DEFAULT IS PROGRAM                   
         CLI   CTPHHEXN,0                                                       
         BE    EXITOK              IF 0 SYSTEM THEN PHASE                       
         CLI   CTPHHEXN+2,X'C0'    IS IT A SCREEN?                              
         BL    EXITOK                                                           
         OI    CTPHSFL1,CTPHSSCQ                                                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CORE=DUMMY                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DMYDTA   LA    RF,DMYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DMYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDMY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDMY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CORE DUMMY FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISDMY   ICM   R3,15,ASYSEL        SYSTEM ELEMENT?                              
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
*                                                                               
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    CTPHSFL1,CTPHSDMQ                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CORE DUMMY FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VALDMY   LTR   R3,R3               SYSTEM ELEMENT?                              
         BZ    DIE                                                              
         NI    CTPHSFL1,FF-(CTPHSDMQ)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VDMY02   EX    RE,*+8              YES                                          
         BE    VDMY04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VDMY04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VDMY06                                                           
*                                                                               
VDMY04   OI    CTPHSFL1,CTPHSDMQ                                                
         B     EXITOK                                                           
*                                                                               
VDMY06   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SPARE BYTES                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
SBYDTA   LA    RF,SBYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SBYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSBY)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSBY)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SPARE BYTES FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISSBY   ICM   R3,15,ASYSEL        SYSTEM ELEMENT?                              
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
         CURED (4,CTPHSSPR),(10,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SPARE BYTES FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALSBY   LTR   R3,R3               SYSTEM ELEMENT?                              
         BZ    DIE                                                              
         CP    BCDUB,=P'2147483647'                                             
         BH    EXITNV                                                           
         MVC   CTPHSSPR,BCFULL                                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFLINE ONLY                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
OFFDTA   LA    RF,OFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
OFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFF)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY OFFLINE ONLY FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   ICM   R3,15,ASYSEL        SYSTEM ELEMENT?                              
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
*                                                                               
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         TM    CTPHSFL1,CTPHSOFQ                                                
         BZ    EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'LC@YES),LC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE OFFLINE ONLY FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   LTR   R3,R3               SYSTEM ELEMENT?                              
         BZ    DIE                                                              
         NI    CTPHSFL1,FF-(CTPHSOFQ)                                           
         CLI   FVILEN,0                                                         
         BE    EXITOK              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VOFF02   EX    RE,*+8              YES                                          
         BE    VOFF04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VOFF04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     VOFF06                                                           
*                                                                               
VOFF04   OI    CTPHSFL1,CTPHSOFQ                                                
         B     EXITOK                                                           
*                                                                               
VOFF06   B     EXITNV              NOTHING ELSE IS VALID                        
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RELOAD DATE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RLDDTA   LA    RF,RLDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
RLDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRLD)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALRLD)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RELOAD DATE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISRLD   ICM   R3,15,ASYSEL        SYSTEM ELEMENT?                              
         BZ    EXITOK                                                           
         USING CTPHSYSD,R3                                                      
*                                                                               
         OC    CTPHSRDT,CTPHSRDT                                                
         BZ    EXITOK                                                           
         GOTO1 VDATCON,BODMCB,(8,CTPHSRDT),(11,FVIFLD)                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RELOAD DATE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALRLD   LTR   R3,R3               SYSTEM ELEMENT?                              
         BZ    DIE                                                              
         MVC   ARLDTE,FVADDR                                                    
         BRAS  RE,VALIDTE                                                       
         BNE   EXITNV                                                           
*                                                                               
         LA    R6,PERVALBK         CONVERT DATE TO JULIAN FOR STORAGE           
         USING PERVALD,R6                                                       
         GOTO1 VDATCON,BODMCB,(2,PVALCSTA),(19,RLDATE)                          
         MVC   FVIFLD(8),PVALCPER  RECOPY NEW VALID DATE                        
*??      OI    FILTRFLG,X'10'      TURN ON THE FILTER FLAG                      
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ACCPET ANY DATE                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
NEDDTA   LA    RF,NEDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
NEDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISNED)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNED)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY ACCEPT ANY DATE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISNED   CLI   ANYDATE,C'Y'                                                     
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'LC@NO),LC@NO                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE ACCEPT ANY DATE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VALNED   MVI   ANYDATE,C'N'                                                     
         CLI   FVILEN,0                                                         
         BE    VNED06              DEFAULT IS NO                                
         XR    RE,RE                                                            
         IC    RE,FVXLEN                                                        
*                                                                               
         EX    RE,*+8              NO                                           
         BE    VNED06                                                           
         CLC   FVIFLD(0),UE@NO                                                  
         EX    RE,*+8              NO                                           
         BE    VNED06                                                           
         CLC   FVIFLD(0),LC@NO                                                  
*                                                                               
VNED02   EX    RE,*+8              YES                                          
         BE    VNED04                                                           
         CLC   FVIFLD(0),UE@YES                                                 
         EX    RE,*+8              YES                                          
         BE    VNED04                                                           
         CLC   FVIFLD(0),LC@YES                                                 
         B     EXITNV                                                           
*                                                                               
VNED04   MVI   ANYDATE,C'Y'                                                     
         MVC   ARLDTE,FVADDR                                                    
         B     EXITOK                                                           
*                                                                               
VNED06   GOTO1 VDATCON,BODMCB,(5,0),(2,DATEBIN)                                 
         CLI   ANYDATE,C'Y'                                                     
         BE    ???                                                              
         CLC   DATEBIN,??????                                                   
         EJECT                                                                  
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
LIST     LM    R0,R2,SVPARMS                                                    
THIS     USING CTPHRECD,R2                                                      
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST PAGE                                                 *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'CTPHPKEY),THIS.CTPHPKEY                                  
         ICM   R1,15,=AL4(XIO11+XOCONFIL+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               FUCK UP ON THE READ HIGH                     
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     MVC   IOKEY(L'CTPHPKEY),THIS.CTPHPKEY                                  
         ICM   R1,15,=AL4(XIO11+XOSEQ+XOCONFIL)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   L     RF,SVPARMS5         CONTROL FILE RECORDS ARE RETURNED IN         
         SRL   RF,4-2              THE I/O AREA                                 
         L     RF,AIO1-L'AIO1(RF)                                               
         CLC   0(CTPHNAME-CTPHRECD,RF),THIS.CTPHRECD                            
         BNE   EXITL                                                            
*                                                                               
         MVC   THIS.CTPHRECD(L'CTPHPKEY),0(RF)                                  
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE A DATE                                                     *         
* NTRY: A(FIELD) VIFLD             PERVAL OUTPUT AREA                 *         
* ON EXIT:     PERVALBK            PERVAL OUTPUT AREA                 *         
***********************************************************************         
         SPACE 1                                                                
VALIDTE  NTR1  ,                                                                
         TM    FVIIND,FVIALF       VALID ALPHA?                                 
         BZ    VDTE02              NO                                           
         CLI   FVILEN,3            GREATER THAN 3 LETTERS?                      
         BH    VDTE06              THEN PERVAL IT                               
*                                                                               
         LA    R3,FVIFLD                                                        
         ICM   R3,8,FVILEN                                                      
         GOTO1 VDAYVAL,BODMCB,(R3),BODUB1,BODUB1+1                              
         CLI   BODUB1,0            VALID ALPHA DAY?                             
         BE    VDTE06              MAYBE PERVAL CAN HANDLE IT                   
*                                                                               
         MVC   DATEBIN(1),BODUB1+1 GET NUMBER EQUIVALENT OF DAY                 
         NI    DATEBIN,X'0F'       REMOVE THE DUPLICATE                         
         GOTO1 VDATCON,BODMCB,(5,0),(0,TODAY)                                   
         GOTO1 VGETDAY,BODMCB,TODAY,BODUB1                                      
*                                                                               
         MVC   DATEBIN+1(1),BODMCB GET NUMBER EQUIV. OF DAY FOR TODAY           
         XR    R1,R1                                                            
         IC    R1,DATEBIN                                                       
         CLC   DATEBIN(1),DATEBIN+1                                             
         BH    *+8                                                              
         LA    R1,7(R1)            ADD A WEEK AHEAD                             
*                                                                               
         MVI   DATEBIN,0                                                        
         SH    R1,DATEBIN          DIFFERENCE IN # OF DAYS                      
         MVC   FVIFLD(4),=C'T(1)'                                               
         CVD   R1,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  FVIFLD+2(1),BODUB1                                               
         MVI   FVILEN,4                                                         
         B     VDTE06              LET PERVAL HANDLE IT FROM HERE               
*                                                                               
VDTE02   TM    FVIIND,FVINUM       VALID NUMERIC?                               
         BZ    VDTE06              NO                                           
         CLI   5(R2),2             1 OR 2 DIGIT NUMERIC?                        
         BH    VDTE06              NO, GO THROUGH PERVAL                        
*                                                                               
         GOTO1 VDATCON,BODMCB,(5,0),(3,BODUB1)                                  
         MVC   BODUB1+3(3),BODUB1  MAKE A COPY OF TODAYS DATE                   
*                                                                               
         L     R0,BCFULL                                                        
         STC   R0,BODUB1+2         STORE THE DAY AWAY                           
         CLM   R0,1,BODUB1+5       DAY GREATER?                                 
         BH    VDTE04              YES, SAME MONTH                              
*                                                                               
         IC    R0,BODUB1+1         GET MONTH(TODAY)                             
         AHI   R0,1                NEXT MONTH                                   
         STC   R0,BODUB1+1         STORE IT                                     
*                                                                               
         CLI   BODUB1+1,12         MONTH(TODAY) DECEMBER?                       
         BNH   VDTE04              NO                                           
*                                                                               
         IC    R0,BODUB1           GET YEAR(TODAY)                              
         AHI   R0,1                NEXT YEAR                                    
         STC   R0,BODUB1           STORE IT                                     
         MVI   BODUB1+1,1          MONTH IS JANUARY                             
*                                                                               
VDTE04   GOTO1 VDATCON,BODMCB,(3,BODUB1),(11,FVIFLD)                            
         MVI   FVILEN,8            CONVERT DATE AND SET NEW LENGTH              
*                                                                               
VDTE06   LA    R3,FVIFLD           PREPARE FIRST PARAMETER FOR PERVAL           
         OI    FVILEN,X'40'        VALIDATE DD.MM                               
         ICM   R3,8,FVILEN         LENGTH OF INPUT                              
         NI    R3,255-X'40'                                                     
*                                                                               
         LA    R4,PERVALBK         ADDR OF OUTPUT AREA                          
         ICM   R4,8,=X'40'         SINGLE DATE ONLY IS VALID                    
         GOTO1 VPERVAL,BODMCB,(R3),(R4)                                         
         TM    BODMCB+4,X'03'      DID WE GET VALID INPUT?                      
         BNZ   EXITL               NO                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFS      DC    16X'FF'                                                          
       ++INCLUDE FALANGTAB                                                      
*                                                                               
DCLIST   DCDDL CT#YES,4,L                                                       
         DCDDL GE#NO,4,L                                                        
         DCDDL CT#PROG,8,L                                                      
         DCDDL CT#SCRN,8,L                                                      
         DCDDL CT#CSCRN,8,L                                                     
DCLISTX  DC    X'00'                                                            
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
*                                                                               
VDAYVAL  DS    V                   V(DAYVAL)                                    
ASYSEL   DS    A                   A(CTPHSYSD)                                  
ALSTEL   DS    A                   A(CTPHLSTD)                                  
ARLDTE   DS    A                   A(RELOAD DATE FIELD)                         
*                                                                               
DATEBIN  DS    H                                                                
*                                                                               
ANYDATE  DS    X                   ACCEPT ANY DATE FIELD SET                    
LSHRN    DS    X                                                                
SNDVAL   DS    X                   START NODE VALUE                             
ENDVAL   DS    X                   END NODE VALUE                               
RLDATE   DS    XL3                                                              
TODAY    DS    CL6                 TODAYS DATE IN YYMMDD FORMAT                 
*                                                                               
PERVALBK DS    XL(L'PVALOUTB)                                                   
*                                                                               
DSLISTU  DS    0D                                                               
UE@YES   DS    XL4                                                              
UE@NO    DS    XL4                                                              
UE@PROG  DS    XL8                                                              
UE@SCRN  DS    XL8                                                              
UE@CSCR  DS    XL8                                                              
*                                                                               
DSLISTL  DS    0D                                                               
LC@YES   DS    XL4                                                              
LC@NO    DS    XL4                                                              
LC@PROG  DS    XL8                                                              
LC@SCRN  DS    XL8                                                              
LC@CSCR  DS    XL8                                                              
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
*        FALANG                                                                 
         PRINT OFF                                                              
       ++INCLUDE FALANG                                                         
         PRINT ON                                                               
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        GEGENASS                                                               
         PRINT OFF                                                              
       ++INCLUDE CTGENPHASE                                                     
         PRINT ON                                                               
*        CTMSGEQUS                                                              
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030CTFIL23   08/22/00'                                      
         END                                                                    
