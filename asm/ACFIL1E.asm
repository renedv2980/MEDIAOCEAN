*          DATA SET ACFIL1E    AT LEVEL 005 AS OF 09/23/13                      
*&&      SET   NOP=N                                                            
*PHASE T6231EA                                                                  
         TITLE 'LIST RECORD OBJECT VERSION'                                     
         SPACE 2                                                                
FIL1E    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL1E**,R7,RR=RE                                              
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
         LH    R6,=Y(TWUSER-TWAD)                                               
         AR    R6,RA                                                            
         USING TWUSER,R6                                                        
                                                                                
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
EXITL    DS    0H                                                               
         XC    FVADDR,FVADDR                                                    
         CLI   *,FF                SET CC LOW                                   
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
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
         USING TYPTABD,R3                                                       
         LA    R3,TYPTAB           TRANSLATE TABLE                              
         CLI   TYPNAME,X'40'       ALREADY DONE                                 
         BH    INITX                                                            
INIT10   GOTO1 VDICTAT,BOPARM,C'SU  ',TYPNAME,0                                 
         LA    R3,TYPTABQ(R3)                                                   
         CLI   0(R3),0                                                          
         BNE   INIT10                                                           
         DROP  R3                                                               
INITX    B     EXITOK                                                           
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
         USING LSTRECD,R2                                                       
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
KFKVAL   MVC   LSTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   LSTKTYP,LSTKTYPQ    1D RECORD                                    
         MVC   LSTKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   LSTKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   LSTKTYP,LSTKTYPQ    1D RECORD                                    
         MVC   LSTKCPY,CUABIN      CONNECTED ID                                 
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
         USING ACTRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RFDIS),AL1(0,0,0),AL4(RECFDIS)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD FILTER FIELDS                                                  
***********************************************************************         
         SPACE 1                                                                
RECFDIS  DS    0H                                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT                                        *         
***********************************************************************         
         SPACE 1                                                                
RECFRST  L     R1,SVPARMS4                                                      
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RFTABL   DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RCPY),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RREN),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELTE,RESTORE,COPY AND RENAME        *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    TM    BIT2,NOACCESS       USER HAS LEDGER ACCESS TO THIS REC?          
         BZ    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA(L'SVXTRA),SVXTRA                                          
         MVC   BOCURSOR,ALSTCODE   POINT TO FIRST LINE                          
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
         USING LSTRECD,R2                                                       
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
         USING LSTRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(LST#CODE),AL4(LCODE)    LIST CODE                            
*        DC    AL2(LST#EDTE),AL4(EDATE)    LIST EXPIRY DATE                     
         DC    AL2(LST#TYPE),AL4(LTYP)     LIST TYPE                            
         DC    AL2(LST#UL),AL4(LSTUL)      1ST UL ON LIST RECORD                
         DC    AL2(LST#DATA1),AL4(LDATA1)  1ST LINE OF LIST DATA                
         DC    AL2(LST#DATA2),AL4(LDATA2)  2ND LINE OF LIST DATA                
         DC    AL2(LST#DATA3),AL4(LDATA2)  3RD LINE OF LIST DATA                
         DC    AL2(LST#DATA4),AL4(LDATA2)  4TH LINE OF LIST DATA                
         DC    AL2(LST#DATA5),AL4(LDATA2)  5TH LINE OF LIST DATA                
         DC    AL2(LST#DATA6),AL4(LDATA2)  6TH LINE OF LIST DATA                
         DC    AL2(LST#DATA7),AL4(LDATA2)  7TH LINE OF LIST DATA                
         DC    AL2(LST#DATA8),AL4(LDATA2)  8TH LINE OF LIST DATA                
         DC    AL2(LST#DATA9),AL4(LDATA2)  9TH LINE OF LIST DATA                
         DC    AL2(LST#DATAA),AL4(LDATA2)  10TH LINE OF LIST DATA               
         DC    AL2(LST#DATAB),AL4(LDATA2)  11TH LINE OF LIST DATA               
         DC    AL2(LST#DATAC),AL4(LDATA2)  12TH LINE OF LIST DATA               
         DC    AL2(LST#DATAD),AL4(LDATA2)  13TH LINE OF LIST DATA               
         DC    AL2(LST#DATAE),AL4(LDATA2)  14TH LINE OF LIST DATA               
         SPACE 2                                                                
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL1E    CSECT                                                                  
*                                                                               
TYPTAB   DS    0CL8                                                             
         DCDD  AC#MEDC,8                                                        
         DCDD  AC#WC,8                                                          
         DCDD  AC#LGRC,8                                                        
         DC    X'00'                                                            
*                                                                               
TYPTABD  DSECT                                                                  
TYPNAME  DS    CL8                                                              
TYPTABQ  EQU   *-TYPTABD                                                        
*                                                                               
FIL1E    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         GOTO1 AADDRST,LSTRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST CODE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LCODE    LA    RF,LCODETBL                                                      
         B     ITER                                                             
*                                                                               
LCODETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISLCDE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLCDE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLCDE)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTLCDE)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTLCDE)                               
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISLCDE)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISLCDE)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY A LIST CODE                                                           
***********************************************************************         
         SPACE 1                                                                
DISLCDE  MVC   FVIFLD(L'LSTKLST),LSTKLST                                        
         MVC   ALSTCODE,FVADDR                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LIST CODE                                                        
***********************************************************************         
         SPACE 1                                                                
VALLCDE  NI    BIT2,X'FF'-NOACCESS                                              
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,LSTKLST,FVIFLD   MOVE IN LIST CODE                            
         OC    LSTKLST,BCSPACES                                                 
         MVC   ALSTCODE,FVADDR                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A LIST CODE FILTER FIELD                                              
***********************************************************************         
         SPACE 1                                                                
DFLTLCDE MVC   FVIFLD,BCSPACES                                                  
         MVC   FVIFLD(L'LSTKLST),FLTIFLD                                        
         MVC   LSTKLST,FLTIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A LIST CODE FILTER FIELD                                             
***********************************************************************         
         SPACE 1                                                                
T        USING LSTRECD,RF                                                       
VFLTLCDE MVC   FLTIFLD,BCSPACES                                                 
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN LIST CODE TO FILTER FIELD            
         EXMVC RF,LSTKLST,FVIFLD                                                
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON LIST CODE                                                     
***********************************************************************         
         SPACE 1                                                                
DOFTLCDE CLC   LSTKLST,BCSPACES  IS THERE A CODE TO COMPARE ON?                 
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   LSTKLST,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
*&&DO                                                                           
***********************************************************************         
* DATA OBJECT FOR EXPIRING DATE                                       *         
* NOT SUPPORTED IN NEWFILE                                                      
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
EDATE    LA    RF,EDATTBL                                                       
         B     ITER                                                             
*                                                                               
EDATTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISEDAT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEDAT)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTDAT)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTDAT)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTDAT)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE EXPIRY DATE                                                       
***********************************************************************         
         SPACE 1                                                                
DISEDAT  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LITELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         USING LITELD,R4                                                        
         L     R4,12(R1)                                                        
         GOTO1 VDATCON,BOPARM,(1,LITEDAT),(8,FVIFLD)                            
         MVC   LTYPE,LITTYPE       SAVE TYPE OF LIST.                           
         CLC   LITEDAT,=X'FFFFFF' NEW FOR YR 2000                               
         BNE   EXITOK              NOT PERMANENT.                               
         MVC   FVIFLD(L'AC@PERM),AC@PERM                                        
         B     EXITOK                                                           
         SPACE 2                                                                
         DROP  R4                                                               
***********************************************************************         
* VALIDATE THE EXPIRY DATE                                                      
***********************************************************************         
         SPACE 1                                                                
VALEDAT  XC    SVTYPE,SVTYPE                                                    
         CLI   CSACT,A#ADD         FOR ACTION ADD NO ELEM TO GET                
         BE    VALED05                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LITELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
         L     R3,12(R1)                                                        
         USING LITELD,R3                                                        
         MVC   SVTYPE,LITTYPE      SAVE THE TYPE                                
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LITELQ',LSTRECD),0               
*                                                                               
         USING LITELD,R3                                                        
VALED05  LA    R3,BOELEM           BUILD 1E ELEM W/O THE LIST TYPE              
         XC    BOELEM,BOELEM       WILL FILL IT IN WHEN VALIDATE CODES          
         MVI   LITEL,LITELQ                                                     
         MVI   LITLN,LITLNQ                                                     
         MVC   LITTYPE,SVTYPE                                                   
*                                                                               
         XC    LITEDAT,LITEDAT                                                  
         ZIC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXCLC RF,FVIFLD,AC@TEMP                                                
         BE    VALED10                                                          
         EXCLC RF,FVIFLD,AC@PERM                                                
         BE    VALED20                                                          
         B     VALED30                                                          
*                                                                               
VALED10  GOTO1 VDATCON,BOPARM,(5,0),(0,SVTODAY)                                 
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 VGETDAY,BOPARM,SVTODAY,BOWORK1                                   
         LA    R4,3                DEFAULT TO KEEP FOR THREE DAYS               
         CLI   0(R1),3             IF THURS OR FRIDAY THEN KEEP FOR 5           
         BL    *+8                                                              
         LA    R4,5                                                             
         GOTO1 VADDAY,BOPARM,SVTODAY,BOWORK1,(R4)                               
         B     VALED50                                                          
*                                                                               
VALED20  MVC   LITEDAT,=X'FFFFFF'                                               
         B     VALED100                                                         
*                                                                               
VALED30  GOTO1 VDATVAL,BOPARM,(0,FVIFLD),BOWORK1                                
         OC    BOPARM,BOPARM                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFINVDT) INVALID DATE                              
         B     EXITL                                                            
         CLC   BOWORK1(6),SVTODAY                                               
         BH    VALED50                                                          
         MVC   FVMSGNO,=AL2(FVFINVDT)                                           
         B     EXITL                                                            
*                                                                               
VALED50  GOTO1 VDATCON,BOPARM,(0,BOWORK1),(1,LITEDAT)                           
*                                                                               
VALED100 DS    0H                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),LSTRECD,BOELEM,0                   
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
*                                                                               
***********************************************************************         
* DISPLAY AN EXPIRY DATE FILTER FIELD                                           
***********************************************************************         
         SPACE 1                                                                
DFLTDAT  CLC   FLTIFLD(3),=X'FFFFFF'                                            
         BNE   *+14                                                             
         MVC   FVIFLD(L'AC@PERM),AC@PERM                                        
         B     EXITOK                                                           
         GOTO1 VDATCON,BOPARM,(1,FLTIFLD),(8,FVIFLD)                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EXPIRY DATE FILTER                                                   
***********************************************************************         
         SPACE 1                                                                
VFLTDAT  MVC   FLTIFLD,BCSPACES                                                 
         CLI   FVILEN,0            ANY FILTER?                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(L'AC@PERM),AC@PERM     PERMANENT?                         
         BNE   VFLTD10                                                          
         MVC   FLTIFLD(3),=X'FFFFFF'                                            
         B     EXITOK                                                           
*                                                                               
VFLTD10  GOTO1 VDATVAL,BOPARM,(0,FVIFLD),BCWORK                                 
         OC    BOPARM,BOPARM                                                    
         BNZ   *+14                                                             
         MVC   FVMSGNO,=AL2(FVFINVDT)    INVALID DATE                           
         B     EXITL                                                            
*                                                                               
         GOTO1 VDATCON,BOPARM,BCWORK,(1,FLTIFLD)                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON EXPIRY DATE                                                   
***********************************************************************         
         SPACE 1                                                                
DOFLTDAT GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LITELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   FLTXX               NOT VALID                                    
*                                                                               
         L     RF,12(R1)                                                        
         USING LITELD,RF                                                        
         CLC   FLTIFLD(3),=X'FFFFFF'                                            
         BNE   DODAT10                                                          
         CLC   LITEDAT,=X'FFFFFF'                                               
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
*                                                                               
DODAT10  CLC   FLTIFLD(L'LITEDAT),LITEDAT                                       
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  RF                                                               
         EJECT                                                                  
*&&                                                                             
***********************************************************************         
* DATA OBJECT FOR LIST TYPE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LTYP     LA    RF,LTYPTBL                                                       
         B     ITER                                                             
*                                                                               
LTYPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISTYP)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDTYP)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVTYP)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTTYP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LIST TYPE                                                         
***********************************************************************         
         SPACE 1                                                                
DISTYP   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LITELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         USING LITELD,R4                                                        
         L     R4,12(R1)                                                        
         CLI   LITTYPE,LITTACT     IF ACCOUNT DON'T LOOK THROUGH TABLE          
         BE    DISTY25                                                          
         USING TYPTABD,RF                                                       
         LA    RF,TYPTAB           TABLE OF DIFFERENT TYPES                     
DISTY10  CLI   0(RF),0                                                          
         BE    DISTY15                                                          
         CLC   0(1,RF),LITTYPE       MATCH ON FIRST LETTER                      
         BE    DISTY20                                                          
         LA    RF,8(RF)            BUMP TO NEXT ENTRY                           
         B     DISTY10                                                          
*                                                                               
DISTY15  MVC   FVIFLD(L'AC@UNKWN),AC@UNKWN UNKNOWN TYPE                         
         B     EXITOK                                                           
*                                                                               
DISTY20  MVC   FVIFLD(TYPTABQ),TYPNAME                                          
         B     EXITOK                                                           
         DROP  RF                                                               
*                                                                               
DISTY25  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LIDELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         MVC   FVIFLD(L'AC@ACC),AC@ACC DEFAULT IS "ACCOUNT"                     
         USING LIDELD,R4                                                        
         L     R4,12(R1)                                                        
         MVC   IOKEY,BCSPACES        SET TO READ NEW LEDGER.                    
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(2),LIDDAUNT                                              
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,AIO2                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING ACLELD,R3                                                        
DISTY30  CLI   0(R3),0                                                          
         BE    EXITOK                                                           
         CLI   0(R3),ACLELQ                                                     
         BE    DISTY35                                                          
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DISTY30                                                          
*                                                                               
DISTY35  LA    RF,ACLVDESC         POINT TO FIRST LEVEL DESCRIPTION             
         ZIC   R1,LIDDALVL         ACCOUNT LEVEL                                
         BCTR  R1,0                                                             
         LTR   R1,R1               IF ZERO THAN DOING THE FIRST LEVEL           
         BZ    *+8                                                              
         MH    R1,=Y(L'ACLVALS)    MULTIPLY FOR LENGTH OF EA LEVEL              
         AR    RF,R1               AND BUMP TO THAT LEVEL                       
         MVC   FVIFLD(9),0(RF)     MOVE IN NAME (9 IS MAX)                      
         B     EXITOK                                                           
         SPACE 2                                                                
         DROP  R4                                                               
***********************************************************************         
* DISPLAY THE LIST TYPE AS A FILTER                                             
***********************************************************************         
         SPACE 1                                                                
DFDTYP   CLI   FLTIFLD,0                                                        
         BE    EXITOK              NO FILTER TO DISPLAY                         
         MVC   FVIFLD(9),FLTIFLD   9 IS MAX                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LIST TYPE AS A FILTER                                            
***********************************************************************         
         SPACE 1                                                                
DFVTYP   MVC   FLTIFLD,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         ZIC   R1,FVXLEN                                                        
         EXMVC R1,FLTIFLD,FVIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LIST TYPE                                                    
***********************************************************************         
         SPACE 1                                                                
DOFLTTYP CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTER ON                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LITELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         USING LITELD,R4                                                        
         L     R4,12(R1)                                                        
         CLC   FLTIFLD(L'AC@MEDC),AC@MEDC     TYPE=MEDIA                        
         BE    DOTYF02                                                          
         CLC   FLTIFLD(L'AC@WC),AC@WC       TYPE=WORKCODE                       
         BE    DOTYF04                                                          
         CLC   FLTIFLD(L'AC@LGRC),AC@LGRC     TYPE=LEDGER                       
         BE    DOTYF06                                                          
         B     DOTYF08             MUST BE AN ACCOUNT TYPE                      
*                                                                               
DOTYF02  CLI   LITTYPE,LITTMED     CHECK FOR MEDIA                              
         BE    FLTXE                                                            
         BNE   FLTXL                                                            
*                                                                               
DOTYF04  CLI   LITTYPE,LITTWRK     CHECK FOR WORKCODE                           
         BE    FLTXE                                                            
         BNE   FLTXL                                                            
*                                                                               
DOTYF06  CLI   LITTYPE,LITTLDG     CHECK FOR LEDGER                             
         BE    FLTXE                                                            
         BNE   FLTXL                                                            
*                                                                               
DOTYF08  CLI   LITTYPE,LITTACT     IF NOT ACCOUNT                               
         BNE   FLTXL               THEN SKIP THIS LIST RECORD                   
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LIDELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITNV                                                           
*                                                                               
         USING LIDELD,R4                                                        
         L     R4,12(R1)                                                        
         MVC   BOWORK2(L'IOKEY),IOKEY      SAVE KEY                             
         MVC   IOKEY,BCSPACES        SET TO READ NEW LEDGER.                    
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(2),LIDDAUNT                                              
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITOK                                                           
*                                                                               
         MVC   IOKEY,BOWORK2       RESTORE LIST KEY                             
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                 RESTORE READ SEQUENCE                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,AIO2                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING ACLELD,R3                                                        
DOTYF30  CLI   0(R3),0                                                          
         BE    EXITOK                                                           
         CLI   0(R3),ACLELQ                                                     
         BE    DOTYF35                                                          
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DOTYF30                                                          
*                                                                               
DOTYF35  LA    RF,ACLVDESC         POINT TO FIRST LEVEL DESCRIPTION             
         ZIC   R1,LIDDALVL         ACCOUNT LEVEL                                
         BCTR  R1,0                                                             
         LTR   R1,R1               IF ZERO THAN DOING THE FIRST LEVEL           
         BZ    *+8                                                              
         MH    R1,=Y(L'ACLVALS)    MULTIPLY FOR LENGTH OF EA LEVEL              
         AR    RF,R1               AND BUMP TO THAT LEVEL                       
         MVC   BOWORK1(L'ACLVDESC),FLTIFLD                                      
         OC    BOWORK1(L'ACLVDESC),BCSPACES                                     
         CLC   BOWORK1(L'ACLVDESC),0(RF)                                        
         BE    FLTXE                                                            
         BNE   FLTXL                                                            
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LISTING THE 1ST UNIT AND LEDGER FOUND ON THE RECORD *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LSTUL    LA    RF,LULTBL                                                        
         B     ITER                                                             
*                                                                               
LULTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLUL)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDLUL)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVLUL)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTLUL)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LIST UNIT AND LEDGER                                              
***********************************************************************         
         SPACE 1                                                                
DISLUL   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LIDELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         USING LIDELD,R4                                                        
         L     R4,12(R1)                                                        
         MVC   FVIFLD(L'LIDDAUNT+L'LIDDALDG),LIDDAUNT                           
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LIST UNIT AND LEDGER AS A FILTER                                  
***********************************************************************         
         SPACE 1                                                                
DFDLUL   CLI   FLTIFLD,0                                                        
         BE    EXITOK              NO FILTER TO DISPLAY                         
         MVC   FVIFLD(L'LIDDAUNT+L'LIDDALDG),FLTIFLD                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE LIST UNIT AND LEDGER AS A FILTER                                 
***********************************************************************         
         SPACE 1                                                                
DFVLUL   MVC   FLTIFLD,BCSPACES                                                 
         CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
*                                                                               
         CLC   =C'ME',FVIFLD       ENTER 'ME' FOR BILLING SOURCES               
         BE    DFVLUL10            OR A VALID U/L                               
*                                                                               
         USING ACTRECD,R4                                                       
         LA    R4,IOKEY                                                         
         MVC   IOKEY,BCSPACES      VALIDATE THE U/L ENTERED                     
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),FVIFLD                              
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INULA)                                           
         B     EXITL                                                            
*                                                                               
DFVLUL10 ZIC   R1,FVXLEN                                                        
         EXMVC R1,FLTIFLD,FVIFLD                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LIST UNIT AND LEDGER                                         
***********************************************************************         
         SPACE 1                                                                
DOFLTLUL CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTER ON                         
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LIDELQ',LSTRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         USING LIDELD,R4                                                        
         L     R4,12(R1)                                                        
         CLC   FLTIFLD(L'LIDDAUNT+L'LIDDALDG),LIDDAUNT                          
         BE    FLTXE                                                            
         B     FLTXL                                                            
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST DATA (1ST LINE ONLY)                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LDATA1   LA    RF,LDATA1TB                                                      
         B     ITER                                                             
*                                                                               
LDATA1TB DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDTA)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDTA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LIST DATA                                                         
***********************************************************************         
         SPACE 1                                                                
DISLDTA  MVI   LFSTEL,1                                                         
         MVI   COUNT,0                                                          
         NI    BIT2,FF-NOACCESS                                                 
*        LA    R4,SVLINE1H                                                      
*        LR    RE,R4               CLEAR SAVED LINES                            
*        LA    RF,SVLINQ                                                        
*        XCEF                                                                   
*                                                                               
         LA    RE,SVLINQ2          NUMBER OF ENTRIES                            
         LA    RF,SVLINE1H         RF POINTS TO FIRST ENTRY TO CLEAR            
DISL02   MVC   8(LINELNQ,RF),BCSPACES  CLEAR THE SAVED LINE                     
         LA    RF,DATALNQ(RF)      BUMP TO NEXT SAVED ENTRY                     
         BCT   RE,DISL02                                                        
*                                                                               
         LR    RF,R2               R3 POINTS TO RECORD                          
         LA    R3,LSTRECD+(LSTRFST-LSTRECD) POINT TO FIRST ELEM                 
DISL05   CLI   0(R3),0                                                          
         BE    DISLEND                                                          
         CLI   0(R3),LITELQ        1E                                           
         BE    DISL09                                                           
         CLI   0(R3),LIDELQ        1F                                           
         BE    DISL10                                                           
DISL07   ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     DISL05                                                           
*                                                                               
         USING LITELD,R3                                                        
DISL09   MVC   LTYPE,LITTYPE       SAVE LTYPE HERE TOO FOR BECAUSE              
         B     DISL07              DISPLAYING THE LIST DATA ON THE LIST         
         DROP  R3                  SCRN COMES BEFORE EXPIRY DTE ROUTINE         
*                                                                               
         USING LIDELD,R3                                                        
DISL10   ZIC   R1,COUNT                                                         
         LA    R1,1(R1)                                                         
         STC   R1,COUNT                                                         
*        CLI   COUNT,4                                                          
*        BNE   *+6                                                              
*        DC    H'0'                                                             
         CLI   LFSTEL,0                                                         
         BE    DISL20                                                           
         MVI   LFSTEL,0                                                         
         MVC   BOWORK1,BCSPACES                                                 
         XC    DISPERR,DISPERR     SET ERROR STORAGE.                           
         LA    R1,SVLINE1H         GET A(LINE IN USE).                          
         ST    R1,AHEADER                                                       
         LA    R1,SVLINE1          GET A(THIS DISPLAY POSITION).                
         ST    R1,AFIELD                                                        
         LA    R1,L'SVLINE1        GET L'THIS LINE                              
         STC   R1,FLDLEN                                                        
*                                                                               
DISL20   ZIC   R1,LIDLN            GET L'THIS ELEMENT.                          
         SH    R1,=H'7'                                                         
         STC   R1,ELEMLEN                                                       
         LA    R1,LIDDATA          GET A(DATA ITEM).                            
         ST    R1,LADATA                                                        
         CLI   LTYPE,C'L'                                                       
         BNE   DISL30              IT'S AN ACCT,ME, OR W/C LIST                 
         MVI   ITEMLEN,1           SET ITEM LNTH TO L'U/L-1. IE 1.              
         B     DISL50                                                           
*                                                                               
DISL30   ZIC   R1,ELEMLEN                                                       
         SH    R1,=H'3'                                                         
         STC   R1,ELEMLEN                                                       
         LA    R1,LIDDACCS                                                      
         ST    R1,LADATA                                                        
         MVC   LUNIT(3),LIDDAUNT   SAVE NEW UNIT/LEDGER/LEVEL.                  
         MVC   BOWORK1(3),=C'UL='     SET TO DISPLAY UNIT/LEDGER.               
         MVC   BOWORK1+3(2),LUNIT                                               
         MVI   ITEMLEN,5           DUMMY LENGTH FOR UL=AA.                      
         BAS   RE,ITEMOUT          DISPLAY THEM.                                
*                                                                               
DISL40   MVC   BOWORK1(4),=C'LVL='    SET TO DISPLAY LEVEL.                     
         MVC   BOWORK1+4(1),LLEVEL                                              
         OI    BOWORK1+4,X'F0'        EBCDICIZE THE LEVEL NUMBER.               
         MVC   BODUB1+1(1),ITEMLEN    SAVE REAL ITEM LENGTH.                    
         MVI   ITEMLEN,5           DUMMY ITEM LENGTH FOR LVL=N.                 
         BAS   RE,ITEMOUT          DISPLAY IT.                                  
         ZIC   R1,LIDITLN                                                       
         BCTR  R1,0                                                             
         STC   R1,ITEMLEN                                                       
*        CLI   ERROR,NOLVLLDG      WAS THERE A LEVEL ERROR.                     
*        BNE   *+8                 NO.                                          
*        BAS   RE,SETERROR         YES, HOLD IT FOR LATER.                      
*                                                                               
DISL50   ZIC   R1,ITEMLEN          R1 = L'DATA ITEM.                            
         L     RE,LADATA           RE = A(DATA ITEM).                           
         EX    R1,GETITEM                                                       
         LA    R1,1(R1)                                                         
         LA    RE,0(R1,RE)         RE = A(NEXT DATA ITEM).                      
         ST    RE,LADATA                                                        
         ZIC   RE,ELEMLEN                                                       
         SR    RE,R1                                                            
         STC   RE,ELEMLEN          RE = REMAINING L'ELEMENT.                    
*                                                                               
         CLI   LTYPE,C'L'                                                       
         BE    DISL60                                                           
         BAS   RE,GETACC                                                        
         BE    DISL70                                                           
         OI    BIT2,NOACCESS                                                    
         MVC   SVXTRA,FVXTRA                                                    
         CLI   CSACT,A#LST         FOR ACTION LIST                              
         BNE   EXITL                                                            
         MVC   FVXTRA,BCSPACES     DON'T MOVE IN ACCOUNT CODE TO                
         MVC   SVXTRA,BCSPACES     EXTRA MESSAGE AREA                           
         B     EXITL                                                            
*        BNE   EXITL                                                            
*        BAS   RE,SETERROR                                                      
*        MVC   FVXTRA,BCSPACES                                                  
*        B     DISL70                                                           
*                                                                               
DISL60   MVC   LUNIT(2),BOWORK1                                                 
         MVI   LLEVEL,1            DUMMY LEVEL FOR LEDGER LISTS.                
         BAS   RE,ITEMOUT                                                       
         BAS   RE,GETUNIT                                                       
*        BE    *+8                 UNIT IS OK.                                  
*        BAS   RE,SETERROR                                                      
         BAS   RE,GETLEDG                                                       
*        BNE   EXITL               LEDGER IS OK.                                
*        BAS   RE,SETERROR                                                      
         B     DISL80                                                           
*                                                                               
DISL70   BAS   RE,ITEMOUT                                                       
DISL80   CLI   ELEMLEN,0                                                        
         BNE   DISL50              ELEMENT CONTINUES.                           
         B     DISL07              ELEMENT ENDS, GET NEXT.                      
*                                                                               
DISLEND  CLC   SVLINE1,BCSPACES                                                 
         BNE   *+14                                                             
         CLI   CSACT,A#LST         MUST HAVE SOMETHING ON THE 1ST LINE          
         BE    EXITOK              BUT DON'T CARE IF LISTING THE RECORD         
         DC    H'0'                                                             
         MVC   FVIFLD(L'SVLINE1),SVLINE1                                        
         OC    FVIFLD(L'SVLINE1),BCSPACES                                       
         LA    R1,L'SVLINE1                                                     
         STC   R1,FVILEN           SET LENGTH                                   
         CLI   CSACT,A#LST         FOR ACTION LIST                              
         BNE   EXITOK                                                           
         MVC   FVXTRA,BCSPACES     CLEAR THE XTRA MESSAGE                       
         B     EXITOK                                                           
         EJECT                                                                  
*ISLEND  LA    R2,LOGLNAMH                                                      
*        OC    DISPERR,DISPERR                                                  
*        BZ    EXIT2               NOT HOLDING ANY ERRORS.                      
*        L     R2,DISPERR          GET A(ERROR LYN).                            
*        MVC   ERROR,DISPERN       GET ERROR MESSAGE NUMBER.                    
*        B     EXIT2                                                            
*                                                                               
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
GETITEM  MVC   BOWORK1(0),0(RE)                                                 
DISPITEM MVC   0(0,RF),BOWORK1                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
***********************************************************************         
* MOVE THE DATA ITEM TO THE SAVED LINE                                          
***********************************************************************         
         SPACE 1                                                                
ITEMOUT  NTR1                                                                   
         ZIC   R1,ITEMLEN                                                       
         LA    RE,BOWORK1(R1)       RE = A(LAST BYTE OF MAX LNTH ITEM).         
         LA    R1,1(R1)            R1 = MAX REAL L'ITEM                         
         CLI   0(RE),C' '          FIND L'ITEM LESS TRAILING SPACES.            
         BNE   *+10                                                             
         BCTR  RE,0                                                             
         BCT   R1,*-10                                                          
         LTR   R1,R1                                                            
         BP    *+6                                                              
         DC    H'0'                ITEM WAS ONLY SPACES.                        
*                                                                               
         STC   R1,BODUB1           SAVE REAL L'INPUT ITEM.                      
         LA    R1,1(R1)            R1 = L'ITEM + PRECEDING COMMA.               
         CLM   R1,1,FLDLEN                                                      
         BH    IOUT100             ITEM + COMMA TOO LONG FOR LYN.               
         L     RF,AFIELD           ITEM + COMMA FITS ON LYN.                    
         L     RE,AHEADER          FOR FIRST ITEM IN LYN,                       
         LA    RE,8(RE)            DON'T INSERT PRECEDING COMMA.                
         CR    RF,RE                                                            
         BE    IOUT120                                                          
         MVI   0(RF),C','          SET THE COMMA.                               
         LA    RF,1(RF)                                                         
         ST    RF,AFIELD           SET DISPLACEMENT PAST COMMA.                 
         B     IOUT120                                                          
*                                                                               
IOUT100  BAS   RE,NEXTLYN          FIND A(START OF NEXT LINE, ETC.).            
         ZICM  RE,AHEADER,4                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    R1,L'SVLINE1        GET L'THIS LINE                              
         STC   R1,FLDLEN                                                        
*                                                                               
IOUT120  L     RF,AFIELD           RF = A(SLOT FOR ITEM).                       
         ZIC   R1,BODUB1           R1 = SAVED L'ITEM.                           
         BCTR  R1,0                                                             
         EX    R1,DISPITEM         MVC   0(0,RF),WORK                           
         LA    R1,1(R1)            R1 = REAL L'ITEM                             
         LA    RF,0(R1,RF)                                                      
         ST    RF,AFIELD           SET A(NEXT SLOT IN LYN).                     
         ZIC   RF,FLDLEN                                                        
         SR    RF,R1                                                            
         BCTR  RF,0                RF V REMAINING L'LYN.                        
         STC   RF,FLDLEN                                                        
         MVC   BOWORK1,BCSPACES                                                 
         B     EXITOK                                                           
*                                                                               
*ETERROR OC    DISPERR,DISPERR                                                  
*        BNZR  RE                  HOLD ONLY FIRST ERROR ON DISPLAY.            
*        L     R2,AHEADER                                                       
*        ST    R2,DISPERR          SAVE A(ERROR LYN).                           
*        MVC   DISPERN,ERROR       SAVE ERROR MESSAGE NUMBER.                   
*        BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* BUMP TO THE NEXT SAVED LINE                                                   
***********************************************************************         
         SPACE 1                                                                
NEXTLYN  NTR1                                                                   
         L     RE,AHEADER          RE = A(PRESENT LINE)                         
         LA    RF,SVLINEEH         POINT TO THE LAST LINE                       
         CR    RE,RF               IF = THEN NO MORE ROOM                       
         BL    *+6                                                              
         DC    H'0'                                                             
*        LA    RF,L'SVLINE1+8      RF = L'LINE +8(HEADER)                       
*        AR    RE,RF               RE = A(PUTATIVE NEXT LINE).                  
         LA    RE,DATALNQ(RE)      BUMP PAST HEADER, LINE AND ADDR              
         ST    RE,AHEADER                                                       
         LA    RF,L'SVLINE1        R1 = L'LINE                                  
         STC   RF,FLDLEN                                                        
         LA    RE,8(RE)            RE = A(1ST DATA IN LYN).                     
         ST    RE,AFIELD                                                        
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR LIST DATA (ALL LINES EXCEPT THE 1ST)                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LDATA2   LA    RF,LDATA2TB                                                      
         B     ITER                                                             
*                                                                               
LDATA2TB DC    AL1(DDIS),AL1(0,0,0),AL4(DISLDTA2)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLDTA)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE LIST DATA (ALL LINES BUT 1ST LINE)                                
***********************************************************************         
         SPACE 1                                                                
DISLDTA2 L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(LST#DATA2)  LINE 2                                     
         BNE   DISB10                                                           
         CLC   SVLINE2,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE2),SVLINE2                                        
         OC    FVIFLD(L'SVLINE2),BCSPACES                                       
         LHI   R1,L'SVLINE2                                                     
         STC   R1,FVILEN           SET LENGTH                                   
         B     EXITOK                                                           
*                                                                               
DISB10   CLM   R1,3,=AL2(LST#DATA3)   LINE 3                                    
         BNE   DISB15                                                           
         CLC   SVLINE3,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE3),SVLINE3                                        
         OC    FVIFLD(L'SVLINE3),BCSPACES                                       
         LHI   R1,L'SVLINE3                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB15   CLM   R1,3,=AL2(LST#DATA4)    LINE 4                                   
         BNE   DISB20                                                           
         CLC   SVLINE4,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE4),SVLINE4                                        
         OC    FVIFLD(L'SVLINE4),BCSPACES                                       
         LHI   R1,L'SVLINE4                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB20   CLM   R1,3,=AL2(LST#DATA5)    LINE 5                                   
         BNE   DISB25                                                           
         CLC   SVLINE5,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE5),SVLINE5                                        
         OC    FVIFLD(L'SVLINE5),BCSPACES                                       
         LHI   R1,L'SVLINE5                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB25   CLM   R1,3,=AL2(LST#DATA6)    LINE 6                                   
         BNE   DISB30                                                           
         CLC   SVLINE6,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE6),SVLINE6                                        
         OC    FVIFLD(L'SVLINE6),BCSPACES                                       
         LHI   R1,L'SVLINE6                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB30   CLM   R1,3,=AL2(LST#DATA7)    LINE 7                                   
         BNE   DISB35                                                           
         CLC   SVLINE7,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE7),SVLINE7                                        
         OC    FVIFLD(L'SVLINE7),BCSPACES                                       
         LHI   R1,L'SVLINE7                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB35   CLM   R1,3,=AL2(LST#DATA8)    LINE 8                                   
         BNE   DISB40                                                           
         CLC   SVLINE8,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE8),SVLINE8                                        
         OC    FVIFLD(L'SVLINE8),BCSPACES                                       
         LHI   R1,L'SVLINE8                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB40   CLM   R1,3,=AL2(LST#DATA9)    LINE 9                                   
         BNE   DISB45                                                           
         CLC   SVLINE9,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINE9),SVLINE9                                        
         OC    FVIFLD(L'SVLINE9),BCSPACES                                       
         LHI   R1,L'SVLINE9                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB45   CLM   R1,3,=AL2(LST#DATAA)    LINE 10                                  
         BNE   DISB50                                                           
         CLC   SVLINEA,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINEA),SVLINEA                                        
         OC    FVIFLD(L'SVLINEA),BCSPACES                                       
         LHI   R1,L'SVLINEA                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB50   CLM   R1,3,=AL2(LST#DATAB)    LINE 11                                  
         BNE   DISB55                                                           
         CLC   SVLINEB,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINEB),SVLINEB                                        
         OC    FVIFLD(L'SVLINEB),BCSPACES                                       
         LHI   R1,L'SVLINEB                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB55   CLM   R1,3,=AL2(LST#DATAC)    LINE 12                                  
         BNE   DISB60                                                           
         CLC   SVLINEC,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINEC),SVLINEC                                        
         OC    FVIFLD(L'SVLINEC),BCSPACES                                       
         LHI   R1,L'SVLINEC                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB60   CLM   R1,3,=AL2(LST#DATAD)    LINE 13                                  
         BNE   DISB65                                                           
         CLC   SVLINED,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINED),SVLINED                                        
         OC    FVIFLD(L'SVLINED),BCSPACES                                       
         LHI   R1,L'SVLINED                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
*                                                                               
DISB65   CLM   R1,3,=AL2(LST#DATAE)    LINE 14                                  
         BNE   EXITOK                                                           
         CLC   SVLINEE,BCSPACES                                                 
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVLINEE),SVLINEE                                        
         OC    FVIFLD(L'SVLINEE),BCSPACES                                       
         LHI   R1,L'SVLINEE                                                     
         STC   R1,FVILEN                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE LIST DATA                                                        
***********************************************************************         
         SPACE 1                                                                
VALLDTA  NI    TYPEBIT,FF-(ULTYPE+LVLTYPE+CODETYPE)                             
         TM    BIT2,NOACCESS       USER HAS LEDGER ACCESS TO THIS REC?          
         BZ    VALLD01                                                          
         MVC   FVMSGNO,=AL2(AE$SECLK)                                           
         MVC   FVXTRA(L'SVXTRA),SVXTRA                                          
         B     EXITL                                                            
*                                 DELETE 1E ELEMENT TO REBUILD                  
VALLD01  L     R1,SVPARMS2                                                      
         CLM   R1,3,=AL2(LST#DATA1)  SAVE LINES 1-13                            
         BNE   VALLD05                                                          
         CLI   FVILEN,0                                                         
         BE    EXITNO              MUST ENTER SOMETHING ON 1ST LINE             
         MVC   SVLINE1,BCSPACES                                                 
         MVC   SVLINE1H,FVIHDR                                                  
         MVC   SVLINE1,FVIFLD                                                   
         MVC   SVADDR1,FVADDR                                                   
*                                 DELETE 1E ELEMENT IF EXISTS                   
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LITELQ',LSTRECD),0               
         USING LITELD,R3                                                        
         LA    R3,BOELEM           BUILD 1E ELEM FIRST                          
         XC    BOELEM,BOELEM                                                    
         MVI   LITEL,LITELQ                                                     
         MVI   LITLN,LITLNQ                                                     
* NO LONGER SUPPORT EXPIRY DATE BUT CHECKS STILL LOOKS AT IT SO ALWAYS          
         MVC   LITEDAT,=X'FFFFFF'   MOVE IN FF'S (FOR PERMANENT)                
         MVI   LITTYPE,C'L'                                                     
         CLC   FVIFLD(3),=C'UL='                                                
         BNE   VALLD02             MUST BE LEDGER TYPE                          
         MVI   LITTYPE,C'M'                                                     
         CLC   FVIFLD+3(2),=C'ME'                                               
         BE    VALLD02             MUST BE MEDIA TYPE                           
         MVI   LITTYPE,C'W'                                                     
         CLC   FVIFLD+6(5),=C'LVL=0'                                            
         BE    VALLD02             MUST BE WORK-CODE TYPE                       
         MVI   LITTYPE,C'A'       ALL OTHERS ACCOUNT TYPE                       
VALLD02  MVC   LTYPE,LITTYPE      SAVE TYPE                                     
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),LSTRECD,BOELEM,0                   
         B     EXITOK                                                           
         DROP  R3                                                               
*                                                                               
VALLD05  CLM   R1,3,=AL2(LST#DATA2)                                             
         BNE   VALLD05A                                                         
         MVC   SVLINE2,BCSPACES                                                 
         MVC   SVLINE2,FVIFLD                                                   
         MVC   SVLINE2H,FVIHDR                                                  
         MVC   SVADDR2,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05A CLM   R1,3,=AL2(LST#DATA3)                                             
         BNE   VALLD05B                                                         
         MVC   SVLINE3,BCSPACES                                                 
         MVC   SVLINE3,FVIFLD                                                   
         MVC   SVLINE3H,FVIHDR                                                  
         MVC   SVADDR3,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05B CLM   R1,3,=AL2(LST#DATA4)                                             
         BNE   VALLD05C                                                         
         MVC   SVLINE4,BCSPACES                                                 
         MVC   SVLINE4,FVIFLD                                                   
         MVC   SVLINE4H,FVIHDR                                                  
         MVC   SVADDR4,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05C CLM   R1,3,=AL2(LST#DATA5)                                             
         BNE   VALLD05D                                                         
         MVC   SVLINE5,BCSPACES                                                 
         MVC   SVLINE5,FVIFLD                                                   
         MVC   SVLINE5H,FVIHDR                                                  
         MVC   SVADDR5,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05D CLM   R1,3,=AL2(LST#DATA6)                                             
         BNE   VALLD05E                                                         
         MVC   SVLINE6,BCSPACES                                                 
         MVC   SVLINE6,FVIFLD                                                   
         MVC   SVLINE6H,FVIHDR                                                  
         MVC   SVADDR6,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05E CLM   R1,3,=AL2(LST#DATA7)                                             
         BNE   VALLD05F                                                         
         MVC   SVLINE7,BCSPACES                                                 
         MVC   SVLINE7,FVIFLD                                                   
         MVC   SVLINE7H,FVIHDR                                                  
         MVC   SVADDR7,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05F CLM   R1,3,=AL2(LST#DATA8)                                             
         BNE   VALLD05G                                                         
         MVC   SVLINE8,BCSPACES                                                 
         MVC   SVLINE8,FVIFLD                                                   
         MVC   SVLINE8H,FVIHDR                                                  
         MVC   SVADDR8,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05G CLM   R1,3,=AL2(LST#DATA9)                                             
         BNE   VALLD05H                                                         
         MVC   SVLINE9,BCSPACES                                                 
         MVC   SVLINE9,FVIFLD                                                   
         MVC   SVLINE9H,FVIHDR                                                  
         MVC   SVADDR9,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05H CLM   R1,3,=AL2(LST#DATAA)                                             
         BNE   VALLD05I                                                         
         MVC   SVLINEA,BCSPACES                                                 
         MVC   SVLINEA,FVIFLD                                                   
         MVC   SVLINEAH,FVIHDR                                                  
         MVC   SVADDRA,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05I CLM   R1,3,=AL2(LST#DATAB)                                             
         BNE   VALLD05J                                                         
         MVC   SVLINEB,BCSPACES                                                 
         MVC   SVLINEB,FVIFLD                                                   
         MVC   SVLINEBH,FVIHDR                                                  
         MVC   SVADDRB,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALLD05J CLM   R1,3,=AL2(LST#DATAC)                                             
         BNE   VALL05K                                                          
         MVC   SVLINEC,BCSPACES                                                 
         MVC   SVLINEC,FVIFLD                                                   
         MVC   SVLINECH,FVIHDR                                                  
         MVC   SVADDRC,FVADDR                                                   
         B     EXITOK                                                           
*                                                                               
VALL05K  CLM   R1,3,=AL2(LST#DATAD)                                             
         BNE   VALLD20                                                          
         MVC   SVLINED,BCSPACES                                                 
         MVC   SVLINED,FVIFLD                                                   
         MVC   SVLINEDH,FVIHDR                                                  
         MVC   SVADDRD,FVADDR                                                   
*        ST    R2,SVREG            SAVE THE ADDRESS OF THIS LINE                
         B     EXITOK                                                           
*                                                                               
         USING LIDELD,R3                                                        
VALLD20  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',LSTRECD),0               
         LA    R5,SVLINE1H         POINT TO THE FIRST LINE                      
         ST    R5,AHEADER                                                       
         CLI   5(R5),0             ANY INPUT?                                   
         BNE   *+14                                                             
         MVC   BOCURSOR,SVADDR1                                                 
         B     EXITNO                                                           
*                                                                               
         LA    R3,BOELEM           NOW START BUILDING THE 1F                    
         MVI   BOELEM,0                                                         
         BAS   RE,PUTDEL           SET UP ELEMENT                               
         MVC   LUNIT(3),BCSPACES                                                
         MVI   GOODEL,C'N'                                                      
         CLI   5(R5),0                                                          
         BNE   *+14                                                             
         MVC   BOCURSOR,SVADDR1                                                 
         B     EXITNO                                                           
*                                                                               
         USING SCANBLKD,R4                                                      
TOP      LA    R4,SCANBLK                                                       
         LR    RE,R4               CLEAR SCANBLK                                
         LA    RF,SCANMAX*SCBLKLQ                                               
         XCEF                                                                   
         GOTO1 VSCANNER,BOPARM,(R5),(40,SCANBLK)                                
         CLI   BOPARM+4,0                                                       
         BNE   TOP10                                                            
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITNV                                                           
         SPACE 1                                                                
TOP10    MVC   LINCOUNT,BOPARM+4     SAVE # OF ITEMS IN SCANBLK LINE            
         BAS   RE,CHKFORM          CHECK FOR CORRECT FORMAT                     
         BE    TOP20                                                            
         MVC   FVMSGNO,=AL2(AE$IAMOT) GIVE MSG OF CORRECT FORM                  
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOP20    ZIC   R3,LINCOUNT         NUMBER OF ITEMS IN SCANBLK                   
         LA    R4,SCANBLK                                                       
TOP30    CLI   SC2NDLEN,0                                                       
         BE    TOPDATA             MUST BE DATA ONLY                            
         CLI   SC1STLEN,2                                                       
         BNE   TOPLVL              MUST BE LVL=                                 
         CLC   SC1STFLD(2),=C'UL'                                               
         BE    TOPUL                                                            
         MVC   FVMSGNO,=AL2(AE$ENTUL)                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
         SPACE 1                                                                
TOPUL    NI    TYPEBIT,FF-ULTYPE                                                
         CHI   R3,1                IS THIS THE LAST ENTRY?                      
         BNE   *+8                                                              
         OI    TYPEBIT,ULTYPE                                                   
         CLI   SC2NDLEN,2                                                       
         BE    TOPU10                                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITNV                                                           
TOPU10   CLI   GOODEL,C'Y'         DO WE ALREADY HAVE AN ELEMENT                
         BNE   TOPU20                                                           
         BAS   RE,PUTDEL                                                        
         MVC   LUNIT(3),BCSPACES                                                
         MVI   GOODEL,C'N'                                                      
TOPU20   MVC   LUNIT(2),SC2NDFLD                                                
         BAS   RE,GETUNIT                                                       
         BE    TOPU30                                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPU30   BAS   RE,GETLEDG                                                       
         BE    TOPU40                                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPU40   L     RF,LADATA                                                        
         MVC   0(2,RF),LUNIT                                                    
         LA    RF,2(RF)                                                         
         ST    RF,LADATA                                                        
         CLC   LUNIT(2),=C'ME'                                                  
         BE    TOPMEDIA            MEDIA LIST                                   
         B     TOPX                                                             
         SPACE 1                                                                
TOPLVL   DS    0H                                                               
         NI    TYPEBIT,FF-LVLTYPE                                               
         CHI   R3,1                IS THIS THE LAST ENTRY?                      
         BNE   *+8                                                              
         OI    TYPEBIT,LVLTYPE                                                  
         CLC   LUNIT(2),BCSPACES                                                
         BNE   TOPLV10                                                          
         MVC   FVMSGNO,=AL2(AE$ENTUL)  PLS ENTER UL=                            
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPLV10  CLI   SC1STLEN,3                                                       
         BNE   TOPLV50                                                          
         CLC   SC1STFLD(3),=C'LVL'                                              
         BNE   TOPLV50                                                          
         CLI   SC2NDLEN,1            LEVEL=1 CHAR                               
         BNE   TOPLV60                                                          
         CLI   SC2NDNUM+3,4          LEVEL CAN'T BE HIGHER THAN 4               
         BH    TOPLV60                                                          
         MVC   LLEVEL,SC2NDNUM+3                                                
         CLC   LUNIT(2),=C'ME'                                                  
         BNE   TOPLV20                                                          
         CLI   LLEVEL,1                                                         
         BNE   TOPLV60                                                          
         B     TOPLV40                                                          
TOPLV20  MVI   ITEMLEN,1           WORKCODE LEN-1                               
         CLI   LLEVEL,0                                                         
         BNE   TOPLV30                                                          
         CLI   LTYPE,C'W'          WORK-CODE                                    
         BNE   BADTYPE                                                          
         B     TOPLV40                                                          
TOPLV30  CLI   LTYPE,C'A'          ACCOUNT                                      
         BNE   BADTYPE                                                          
         BAS   RE,GETLVL           AND ACCOUNT LEN.                             
         BE    TOPLV40                                                          
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPLV40  L     RF,LADATA                                                        
         MVC   0(1,RF),LLEVEL                                                   
         LA    RF,1(RF)                                                         
         ST    RF,LADATA                                                        
         B     TOPX                                                             
*                                                                               
TOPLV50  MVC   FVMSGNO,=AL2(AE$ELACT)                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPLV60  MVC   FVMSGNO,=AL2(AE$NOLEV)                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
*                                                                               
         SPACE 1                                                                
TOPLEDG  DS    0H                  LEDGER                                       
         CLI   LTYPE,C'L'                                                       
         BNE   BADTYPE                                                          
         CLC   LUNIT(3),BCSPACES                                                
         BE    TOPLE10                                                          
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITNV                                                           
TOPLE10  MVI   ITEMLEN,1                                                        
         B     TOPD20                                                           
TOPLE20  MVC   LUNIT(2),BOWORK1                                                 
         BAS   RE,GETUNIT                                                       
         BE    TOPLE30                                                          
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPLE30  BAS   RE,GETLEDG                                                       
         BE    TOPLE40                                                          
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPLE40  MVC   LUNIT(2),BCSPACES                                                
         B     TOPD70                                                           
         SPACE 1                                                                
TOPMEDIA DS    0H                  MEDIA                                        
         CLI   LTYPE,C'M'                                                       
         BNE   BADTYPE                                                          
         MVI   ITEMLEN,11                                                       
         B     TOPX                                                             
         SPACE 1                                                                
TOPDATA  DS    0H                                                               
         NI    TYPEBIT,FF-CODETYPE                                              
         CHI   R3,1                IS THIS THE LAST ENTRY?                      
         BNE   *+8                                                              
         OI    TYPEBIT,CODETYPE                                                 
         CLI   LUNIT,C' '                                                       
         BNE   TOPD05                                                           
         CLI   SC1STLEN,2          IF A LEDGER LIST MAKE SURE THEY              
         BE    TOPLEDG             ENTER BOTH A UNIT AND A LEDGER               
         MVC   FVMSGNO,=AL2(AE$IVUNL)                                           
         LR    RF,R5                                                            
         LA    RF,LINHLNQ+1(RF)                                                 
         MVC   FVADDR,0(RF)                                                     
         B     EXITL                                                            
TOPD05   CLI   LLEDGER,C' '                                                     
         BNE   TOPD10                                                           
         MVC   FVMSGNO,=AL2(AE$ENTUL)   PLS ENTER UNIT/LEDGER                   
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPD10   CLI   LLEVEL,C' '                                                      
         BNE   TOPD20                                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITNV                                                           
         SPACE 1                                                                
TOPD20   MVC   BOWORK1,BCSPACES                                                 
         ZIC   R1,ITEMLEN                                                       
         ZIC   RF,SC1STLEN                                                      
         BCTR  RF,0                                                             
         CR    RF,R1                                                            
         BNH   TOPD30                                                           
         MVC   FVMSGNO,=AL2(AE$ANLNG)  ACCOUNT NAME TOO LONG                    
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPD30   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   BOWORK1(0),SC1STFLD                                              
         CLC   BOWORK1,BCSPACES                                                 
         BE    TOPX                IGNORE - GO TO NEXT                          
         BAS   RE,GETACC                                                        
         BE    TOPD40                                                           
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPD40   BAS   RE,CHKLIST          CHECK FOR DUPLICATES                         
         CLI   FOUND,C'Y'                                                       
         BNE   TOPD50                                                           
         MVC   FVMSGNO,=AL2(AE$DUPAC)                                           
         MVC   FVXTRA(12),BOWORK1                                               
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
TOPD50   CLI   FOUND,C'L'          ELEMENT TOO LARGE                            
         BNE   TOPD60                                                           
         MVC   BASMSG(L'TOOLARGE),TOOLARGE                                      
         MVC   BASMSG+L'TOOLARGE(12),BOWORK1                                    
         MVC   BASMSG+20(11),FVIFLD                                             
         CLC   BASMSG+20(3),=C'UL='                                             
         BE    *+10                                                             
         MVC   BASMSG,TOOLARG2                                                  
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
         SPACE 1                                                                
TOPD60   CLI   LTYPE,C'L'                                                       
         BE    TOPLE20                                                          
TOPD70   ZIC   R1,ITEMLEN                                                       
         L     RF,LADATA                                                        
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SC1STFLD    MOVE DATA TO ELEMENT                         
         LA    RF,1(R1,RF)                                                      
         ST    RF,LADATA                                                        
         MVI   GOODEL,C'Y'                                                      
         B     TOPX                                                             
         SPACE 1                                                                
TOPX     DS    0H                                                               
         LA    R4,32(R4)           NEXT ITEM IN SCAN BLOCK                      
         BCT   R3,TOP30                                                         
*        L     R1,SVPARMS2                                                      
*        CLM   R1,3,=AL2(LST#DATAD) ARE WE ON THE 2ND TO LAST LINE?             
         LA    R1,FVIHDR                                                        
         CR    R5,R1               IF THIS = THEN WE ARE DONE                   
         BE    TOPEND                                                           
         LA    R1,SVLINEDH         ARE WE ON THE 2ND TO LAST LINE?              
         CR    R5,R1                                                            
         BNE   TOPX10                                                           
         LA    R5,FVIHDR            YES THAN CAN JUST POINT TO LAST LN          
         B     *+8                                                              
TOPX10   A     R5,=A(DATALNQ)      NEXT LINE ON SCREEN                          
*        ZIC   R1,0(R2)                                                         
*        LA    R2,0(R1,R2)         NEXT LINE ON SCREEN                          
         ST    R5,AHEADER                                                       
         CLI   5(R5),0             IF NOTHING THERE                             
         BE    TOPEND              FINISH UP                                    
         B     TOP                                                              
         SPACE 1                                                                
TOPEND   DS    0H                                                               
         CLI   GOODEL,C'Y'                                                      
         BE    *+14                                                             
         MVC   BOCURSOR,SVADDR1                                                 
         B     EXITNO                                                           
         BAS   RE,PUTDEL                                                        
         B     VALLDX                                                           
*                                                                               
BADTYPE  MVC   FVMSGNO,=AL2(AE$INCLS) INCOMPATIBLE LIST TYPE                    
         LR    RF,R5               POINT TO SAVED LINE HEADER                   
         LA    RF,LINHLNQ+1(RF)    BUMP PAST HEADER AND LINE                    
         MVC   FVADDR,0(RF)        AND MOVE IN SAVED ADDR                       
         B     EXITL                                                            
*                                                                               
VALLDX   B     EXITOK                                                           
         SPACE 2                                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* ROUTINE TO CHECK FOR DUPLICATE ENTRIES                                        
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 2                                                                
CHKLIST  NTR1                                                                   
         MVI   FOUND,C'Y'                                                       
         LA    RF,ENTLIST                                                       
         ZIC   R1,ITEMLEN          ITEM LENGTH-1                                
CHK2     CLI   0(RF),0             END OF LIST                                  
         BE    CHK4                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),BOWORK1                                                  
         BE    EXITOK              ALREADY ON LIST                              
         LA    RF,1(R1,RF)         BUMP TO NEXT ITEM ON LIST                    
         B     CHK2                                                             
CHK4     LA    RE,ENTLIST+230      INSURE ELEMENT DOESN'T GET TOO BIG           
         CR    RE,RF                                                            
         BH    *+12                                                             
         MVI   FOUND,C'L'                                                       
         B     EXITOK                                                           
         EX    R1,*+8              NOT FOUND, SO ADD IT                         
         B     *+10                                                             
         MVC   0(0,RF),BOWORK1                                                  
         MVI   FOUND,C'N'                                                       
         B     EXITOK                                                           
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* CORRECT FORMAT IS UL=XX,LVL=N,ACCOUNT                                         
* BUMP THROUGH SCANNER LINE AND CHECK FOR:                                      
* 1) TWO U/L'S WITHOUT DATA IN BETWEEN                                          
* 2) TWO LVL='S WITHOUT DATA IN BETWEEN                                         
* 3) UL=XX,LVL=N THEN ANOTHER UL= WITHOUT ANY ACCOUNTS                          
* 4) LVL=X WITHOUT UL=XX                                                        
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
*                                                                               
         USING SCANBLKD,R4                                                      
CHKFORM  NTR1                                                                   
*                                                                               
         NI    BIT,FF-LVLUSE                                                    
         NI    BIT,FF-ULUSE                                                     
         OI    BIT2,FIRST                                                       
*                                                                               
         LA    R4,SCANBLK                                                       
         ZIC   R3,BOPARM+4           R3=# OF ITEMS IN SCANBLK                   
CHK20    CLI   SC1STLEN,3            'LVL'                                      
         BNE   CHK22                                                            
         CLC   SC1STFLD(3),=C'LVL'                                              
         BNE   CHK30                                                            
         TM    BIT,ULUSE           MUST HAVE UL FIRST                           
         BO    CHK21                                                            
         TM    BIT2,FIRST           IF LINE SPLIT AT LVL THAN OKAY              
         BZ    EXITL                BUT NEED TO MAKE SURE THEY ENTERED          
         LA    R1,SVLINE1           UL= ON PREVIOUS LINE, BUT SKIP              
         L     RF,AHEADER           CHECK IF WE ARE ON THE FIRST LINE           
         CR    RF,R1                                                            
         BE    *+12                                                             
         TM    TYPEBIT,ULTYPE      DID THEY ENTER UL= ON PREVIOUS LINE?         
         BZ    EXITL                                                            
CHK21    TM    BIT,LVLUSE                                                       
         BO    EXITL                                                            
         OI    BIT,LVLUSE          SET BIT THAT LEVEL IS THERE                  
         NI    BIT,FF-ULUSE                                                     
         B     CHKNXT                                                           
*                                                                               
CHK22    CLI   SC1STLEN,2            'UL'                                       
         BNE   CHK30                                                            
         CLC   SC1STFLD(2),=C'UL'                                               
         BNE   CHK30                                                            
         TM    BIT,ULUSE                                                        
         BO    EXITL                                                            
         TM    BIT,LVLUSE          PREVIOUSLY USED LVL=                         
         BO    EXITL                                                            
         OI    BIT,ULUSE           SET BIT THAT UL= IS IN USE                   
         NI    BIT,FF-LVLUSE                                                    
         B     CHKNXT                                                           
*                                                                               
CHK30    NI    BIT,FF-LVLUSE                                                    
CHK32    NI    BIT,FF-ULUSE                                                     
*                                                                               
CHKNXT   NI    BIT2,FF-FIRST     SHUT OFF FIRST TIME FOR LINE                   
         LA    R4,32(R4)           BUMP TO NEXT ITEM IN SCAN BLOCK              
         BCT   R3,CHK20                                                         
*                                                                               
CHKYES   B     EXITOK                                                           
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* GET LEVEL                                                                     
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
GETLVL   NTR1                                                                   
         CLC   LUNIT(2),=C'ME'     DON'T IF MEDIA LIST                          
         BE    EXITOK                                                           
         MVI   ITEMLEN,0                                                        
*                                                                               
         MVC   IOKEY,BCSPACES      READ LEDGER RECORD                           
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(2),LUNIT                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING ACTRECD,R3                                                       
         L     R3,AIO2                                                          
         LA    R3,ACTRFST-ACTRECD(R3)                                           
         USING ACLELD,R3                                                        
GTL02    CLI   0(R3),0                                                          
         BE    GTL050                                                           
         CLI   0(R3),ACLELQ                                                     
         BE    GTL040                                                           
         ZIC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     GTL02                                                            
*                                                                               
GTL040   LA    RF,ACLVLEN          RF = A(1ST LEVEL INFO).                      
         ZIC   R1,LLEVEL           R1 = NO. OF SPECIFIED ACCT LEVEL.            
         BCTR  R1,0                SET TO INDEX INTO HIER EL BY LEVEL.          
         SLL   R1,4                ENTRIES ARE 16 BYTES LONG.                   
         AR    RF,R1               RF = A(INFO FOR SPECIFIED LEVEL).            
         ZICM  R1,0(RF)            R1 = L'ACC'T FOR LEVEL.                      
         BNZ   GTL060              LEVEL IS OK.                                 
         MVC   FVMSGNO,=AL2(AE$NOLEV)                                           
         B     EXITL                                                            
*                                                                               
GTL050   MVC   FVMSGNO,=AL2(AE$ENTUL)                                           
         B     EXITL                                                            
*                                                                               
GTL060   BCTR  R1,0                ITEMLEN = L'ACC'T FOR LEVEL LESS 1.          
         STC   R1,ITEMLEN                                                       
         B     EXITOK                                                           
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* GET UNIT                                                                      
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
GETUNIT  NTR1                                                                   
         CLC   LUNIT(2),=C'ME'                                                  
         BE    EXITOK                                                           
         MVC   IOKEY,BCSPACES                                                   
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(1),LUNIT                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INUNT)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'AC@UNIT),AC@UNIT                                        
         MVC   FVXTRA+L'AC@UNIT+1(L'LUNIT),LUNIT                                
         B     EXITL                                                            
         SPACE 3                                                                
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* GET LEDGER                                                                    
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
GETLEDG  NTR1                                                                   
         CLC   LUNIT(2),=C'ME'                                                  
         BE    EXITOK                                                           
         MVC   IOKEY,BCSPACES        SET TO READ NEW LEDGER.                    
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(2),LUNIT                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INLDG)                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVC   FVXTRA(L'AC@LGRC),AC@LGRC                                        
         MVC   FVXTRA+L'AC@LGRC+1(L'LLEVEL),LUNIT+1                             
         B     EXITL                                                            
         EJECT                                                                  
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
* GET ACCOUNT                                                                   
*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*         
         SPACE 1                                                                
GETACC   NTR1                                                                   
         CLI   LTYPE,C'L'          DON'T IF LEDGER OR                           
         BE    EXITOK                                                           
         CLI   LTYPE,C'M'          MEDIA LISTS                                  
         BE    EXITOK                                                           
         MVC   IOKEY,BCSPACES                                                   
         CLI   LTYPE,C'W'                                                       
         BE    GETAC5              MUST BE WORK-CODES                           
         MVC   IOKEY(1),CUABIN                                                  
         MVC   IOKEY+1(2),LUNIT                                                 
         ZIC   R1,ITEMLEN                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   IOKEY+3(0),BOWORK1    ACCT CODE.                                 
         B     GETAC15                                                          
*                                                                               
GETAC5   DS    0H                                                               
         CLC   BOWORK1(2),=C'98'      DON'T BOTHER IF PLANNING                  
         BE    EXITOK                                                           
         CLC   BOWORK1(2),=C'99'      OR BILLING W/C'S                          
         BE    EXITOK                                                           
         CLC   BOWORK1(2),=C'**'      OR ORDERS                                 
         BE    EXITOK                                                           
         MVI   IOKEY,X'0A'           FIND WORK-CODE RECORD                      
         MVC   IOKEY+1(1),CUABIN                                                
         MVC   IOKEY+2(2),LUNIT                                                 
         MVC   IOKEY+4(2),BOWORK1      W-C MUST BE TWO CHARS.                   
         MVC   BASMSG,BCSPACES                                                  
         MVC   BASMSG(L'INCDEMSG),INCDEMSG                                      
         MVC   BASMSG+19(2),BOWORK1                                             
*                                                                               
GETAC10  LHI   R1,XOREAD+XOACCDIR+XIO2   READ WORK-CODE RECORD MYSELF           
         GOTO1 AIO                                                              
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$INACC)                                           
         MVC   FVXTRA,BCSPACES                                                  
         ZIC   R1,ITEMLEN                                                       
         BCTR  R1,0                                                             
         EXMVC R1,FVXTRA,BOWORK1                                                
         B     EXITL                                                            
*                                                                               
GETAC15  GOTO1 AGETACT,0              GET ACCOUNT/TEST SECURITY                 
         BNE   EXITL                                                            
         B     EXITOK                                                           
***********************************************************************         
* SET UP ELEMENT AND ADD IT (MAYBE)                                             
***********************************************************************         
         SPACE 1                                                                
         USING LIDELD,R3                                                        
PUTDEL   NTR1                                                                   
         CLI   BOELEM,0                                                         
         BE    PTD040              1ST ENTRY, SET UP ONLY.                      
         LA    R3,BOELEM                                                        
         L     RF,LADATA           RF = A(1ST UNUSED ELEMENT BYTE).             
         LA    RE,BOELEM           RE = A(1ST BYTE OF ELEMENT).                 
         SR    RF,RE               RF = L'ELEMENT                               
         STC   RF,LIDLN                                                         
         ZIC   R1,ITEMLEN                                                       
         LA    R1,1(R1)                                                         
         STC   R1,LIDITLN                                                       
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),LSTRECD,BOELEM,0                   
PTD040   LA    R3,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   LIDEL,LIDELQ                                                     
         LA    RE,LIDDATA                                                       
         ST    RE,LADATA           RE = A(1ST AVAILABLE DATA SLOT.              
         MVI   LFSTEL,1            SET NEW ELEMENT MARKER.                      
         XC    ENTLIST,ENTLIST     CLEAR ENTRY LIST FOR NEW ELEMENT             
         MVI   ELEMLEN,248         'L' TYPE DATA LENGTH = 248.                  
         CLI   LTYPE,C'L'                                                       
         BE    EXITOK              IT'S A LEDGER LIST.                          
         MVI   ELEMLEN,245         DATA LENGTH FOR OTHERS=245.                  
         B     EXITOK                                                           
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
THIS     USING LSTRECD,R2                                                       
LAST     USING LSTRECD,R3                                                       
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(LDEFCLM),AL1(0,0,0),AL4(DEFCLM)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET UP DEFAULT COLUMN LIST BASED ON RECORD TYPE AND SECURITY ACCESS           
* ONE DEF_CLM REC ALLOWS USER TO OPEN UP COLUMNS FOR CHANGE AND ANOTHER         
* DEF_CLM RECORD DOES NOT.                                                      
***********************************************************************         
         SPACE 1                                                                
DEFCLM   DS    0H                                                               
*&&DO                                                                           
         MVI   GSSMCODE,0                                                       
         MVI   BCWORK,3            3=ACTION CHANGE                              
         LA    R3,BCWORK                                                        
         MVC   SBYTE,CSREC         RECORD NUMBER                                
         ICM   R3,8,SBYTE                                                       
         GOTO1 VSECRET,BODMCB,('SECPRACT',ASECBLK),(R3)                         
         CLI   BODMCB,SECPYES      ACCESS TO ACTION CHANGE?                     
         BE    DEFCLMX             YES- DEF_CLM GETS NO CODE                    
         MVI   GSSMCODE,C'A'       NO- DEF_LCM GETS CODE 'A'                    
*&&                                                                             
*                                                                               
DEFCLMX  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'LSTKEY),THIS.LSTRECD                                     
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
NLST02   CLC   IOKEY(LSTKLST-LSTRECD),THIS.LSTRECD                              
*        BNE   EXITL               NO MORE FOR THIS COMPANY                     
         BE    *+8                                                              
         B     EXITL                                                            
         MVC   THIS.LSTKEY(L'LSTKEY+L'LSTKSTA+L'LSTKDA),IOKEY                   
         B     EXITOK                                                           
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
TOOLARGE DC    C'TO CONTINUE, ENTER ''UL=UL,LVL=L'' BEFORE ENTRY = '            
TOOLARG2 DC    CL60'**ERROR** MAXIMUM RECORD SIZE EXCEEDED'                     
INCDEMSG DC    C'**ERROR** WORKCODE XX IS INVALID'                              
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SCANMAX  EQU   40                  40 MAX ENTRIES PER LINE                      
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#PERM,9,L         PERMANENT                                    
         DCDDL AC#TMP,9,L          TEMPORARY                                    
         DCDDL AC#UNIT,4,L         UNIT                                         
         DCDDL AC#LEVEL,5,L        LEVEL                                        
         DCDDL AC#UNKWN,7,L        UNKNOWN                                      
         DCDDL AC#ACC,7,L          ACCOUNT                                      
         DCDDL AC#MEDC,5,L         MEDIA                                        
         DCDDL AC#WC,8,L           WORKCODE                                     
         DCDDL AC#LGRC,6,L         LEDGER                                       
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
*                                                                               
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*ACFILWORK                                                                      
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
ALSTCODE DS    A                   A(LIST CODE FIELD)                           
SVLINE1H DS    CL8                                                              
SVLINE1  DS    CL67                                                             
LINELNQ  EQU   *-SVLINE1           LINELNQ=LEN OF SAVED LINE                    
LINHLNQ  EQU   *-SVLINE1H          LINHLNQ=LEN OF SAVED LINE + HEADER           
SVADDR1  DS    A                                                                
DATALNQ  EQU   *-SVLINE1H          DATALNQ=L'SAVED LINE+HEADR+ADDR              
SVLINE2H DS    CL8                                                              
SVLINE2  DS    CL67                                                             
SVADDR2  DS    A                                                                
SVLINE3H DS    CL8                                                              
SVLINE3  DS    CL67                                                             
SVADDR3  DS    A                                                                
SVLINE4H DS    CL8                                                              
SVLINE4  DS    CL67                                                             
SVADDR4  DS    A                                                                
SVLINE5H DS    CL8                                                              
SVLINE5  DS    CL67                                                             
SVADDR5  DS    A                                                                
SVLINE6H DS    CL8                                                              
SVLINE6  DS    CL67                                                             
SVADDR6  DS    A                                                                
SVLINE7H DS    CL8                                                              
SVLINE7  DS    CL67                                                             
SVADDR7  DS    A                                                                
SVLINE8H DS    CL8                                                              
SVLINE8  DS    CL67                                                             
SVADDR8  DS    A                                                                
SVLINE9H DS    CL8                                                              
SVLINE9  DS    CL67                                                             
SVADDR9  DS    A                                                                
SVLINEAH DS    CL8                                                              
SVLINEA  DS    CL67                                                             
SVADDRA  DS    A                                                                
SVLINEBH DS    CL8                                                              
SVLINEB  DS    CL67                                                             
SVADDRB  DS    A                                                                
SVLINECH DS    CL8                                                              
SVLINEC  DS    CL67                                                             
SVADDRC  DS    A                                                                
SVLINEDH DS    CL8                                                              
SVLINED  DS    CL67                                                             
SVADDRD  DS    A                                                                
SVLINEEH DS    CL8                                                              
SVLINEE  DS    CL67                                                             
SVADDRE  DS    A                                                                
SVLINQ   EQU   *-SVLINE1H                                                       
SVLINQ2  EQU   (*-SVLINE1H)/DATALNQ                                             
SVXTRA   DS    CL16                SAVED FVXTRA FIELD FOR MESSAGE               
BIT2     DS    XL1                                                              
FIRST    EQU   X'80'               FIRST TIME CHECKING THIS LINE                
NOACCESS EQU   X'40'               LEDGER ACCESS FAILED                         
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
SVADDR   DS    A                   SAVED ADDRESS                                
SAVRB    DS    F                                                                
DISPERR  DS    A                                                                
SVELADDR DS    A                   A(THIS ELEMENT)                              
         SPACE 1                                                                
SVACLEL  DS    CL100               SAVE ACCOUNT LENGTHS ELEMENT                 
SCANBLK  DS    (SCANMAX)CL(SCBLKLQ)                                             
ENTLIST  DS    CL256               LIST OF CODES (FOR 1 LINE AT A TIME)         
*                                                                               
                                                                                
SBYTE    DS    XL1                 SECURITY BYTE                                
SVTODAY  DS    CL6                 TODAYS DATE(EBCDIC)                          
SVEDATE  DS    XL3                 SAVED EXPIRY DATE                            
SVELDISP DS    XL4                 DISPLACEMENT TO THIS ELEMENT                 
SVTYPE   DS    CL1                                                              
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
LFSTEL   DS    X                                                                
COUNT    DS    X                                                                
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
NEWELEM  EQU   X'01'               PROCESSING A DIFFERENT (NEW) ELEMENT         
*                                                                               
TYPEBIT  DS    XL1                 TYPE OF DATA ENTERED                         
ULTYPE   EQU   X'80'               UL= ENTERED                                  
LVLTYPE  EQU   X'40'               LVL= ENTERED                                 
CODETYPE EQU   X'20'               CODE ENTERED (ACC, WORKCODE, ETC.)           
*                                                                               
DSLIST   DS    0D                                                               
AC@PERM  DS    CL9                                                              
AC@TEMP  DS    CL9                                                              
AC@UNIT  DS    CL4                                                              
AC@LVL   DS    CL5                                                              
AC@UNKWN DS    CL7                                                              
AC@ACC   DS    CL7                                                              
AC@MEDC  DS    CL5                                                              
AC@WC    DS    CL8                                                              
AC@LGRC  DS    CL6                                                              
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005ACFIL1E   09/23/13'                                      
         END                                                                    
