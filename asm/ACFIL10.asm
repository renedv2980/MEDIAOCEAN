*          DATA SET ACFIL10    AT LEVEL 008 AS OF 12/04/19                      
*&&      SET   NOP=N                                                            
*PHASE T62310C,*                                                                
         TITLE 'OFFICE/OFFICE LIST OBJECT VERSON'                               
***********************************************************************         
*  ID  LVL   DATE    TICKET            COMMENTS                       *         
* ---- --- ------- ------------ --------------------------------------*         
* JSAY 007 06MAR19 <SPEC-33021> CHANGED PROGRAM TO ALLOW UP TO 240    *         
*                               OFFICES.                              *         
* ABID 008 05DEC19 <DSRD-23587> FIX OLIST PASSIVE POINTER ISSUE       *         
*                                                                     *         
***********************************************************************         
         SPACE 2                                                                
FIL10    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL10**,R7,RR=RE                                              
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
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLIST                             
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
         USING OFFRECD,R2                                                       
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
KFKVAL   MVC   OFFKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   OFFKTYP,OFFKTYPQ    01 RECORD                                    
         MVC   OFFKCPY,CUABIN      CONNECTED ID                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  MVC   OFFKEY,BCSPACES   INITIALIZE KEY OF RECORD                       
         MVI   OFFKTYP,OFFKTYPQ    01 RECORD                                    
         MVC   OFFKCPY,CUABIN      CONNECTED ID                                 
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
         USING OFFRECD,R2                                                       
         LA    RF,TABLREC                                                       
         B     ITER                                                             
*                                                                               
TABLREC  DC    AL1(RFIRST),AL1(0,0,0),AL4(EXITOK)                               
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                        *          
***********************************************************************         
         SPACE 1                                                                
RECLAST  L     R1,SVPARMS4                                                      
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)   RADD IF CALLED FOR             
         DC    AL1(RCPY),AL1(0,0,0),AL4(RLCPY)   ADD OLIST POINTERS             
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLADD)   COPY ACT(WAS CAUSING           
         DC    AL1(EOT)                          DUPL. SEARCH POINTERS)         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR ADD/COPY/WRITE                                                  
***********************************************************************         
         SPACE 2                                                                
RLADD    TM    OFFRSTAT,OFFSLIST                                                
         BZ    EXITOK                                                           
*                                                                               
         GOTO1 ADDPAS,BOPARM,OFFRECD    ADD OLIST POINTERS                      
*                                                                               
         BAS   RE,ADDACTPT         ADD ACTIVITY POINTER                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - COPY / RESTORE                                  
***********************************************************************         
         SPACE 2                                                                
RLCPY    TM    OFFRSTAT,OFFSLIST                                                
         BZ    EXITOK                                                           
*                                                                               
         GOTO1 ADDPAS,BOPARM,OFFRECD    ADD OLIST POINTERS                      
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING OFFRECD,R2                                                       
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
         USING OFFRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(OFF#OFFC),AL4(OFFCDE)   OFFICE CODE                          
         DC    AL2(OFF#SNAME),AL4(SHNAME)  SHORT OFFICE NAME                    
         DC    AL2(OFF#OFFDTA),AL4(OFFDTA) OFFICES IN THE LIST                  
         DC    AL2(OFF#OFFNME),AL4(OFFNMEL) OFFICE NAMES IN THE LIST            
         DC    AL2(OFF#NUM),AL4(NUMLISTS)  # OF LISTS THIS OFFICE IS IN         
         DC    AL2(OFF#NUM2),AL4(NUMOFFS)  # OF OFFICES IN LIST                 
         DC    AL2(OFF#TYPE),AL4(OTYPE)    TYPE - OFFICE OR OFFICE LIST         
         DC    AL2(OFF#FLIST),AL4(FLIST)   WHICH LISTS OFFICE XX IS IN          
         SPACE 2                                                                
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL10    CSECT                                                                  
*                                                                               
FIL10    CSECT                                                                  
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
DFDVAL   B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE CODE                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFCDE   LA    RF,OCODETBL                                                      
         B     ITER                                                             
*                                                                               
OCODETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISOCDE)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOCDE)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTOCDE)                              
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTOCDE)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTOCDE)                               
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* DISPLAY AN OFFICE CODE                                                        
***********************************************************************         
         SPACE 1                                                                
DISOCDE  MVC   FVIFLD(L'OFFKOFF),OFFKOFF                                        
         CLC   OFFKOFF,=C'AQ'                                                   
         BNE   *+8                                                              
         B     *+4                                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE OFFICE CODE                                                      
***********************************************************************         
         SPACE 1                                                                
VALOCDE  CLI   CSACT,A#ADD        ACTION ADD                                    
         BE    *+12                                                             
         CLI   CSACT,A#CPY        OR COPY                                       
         BNE   VALOCD10                                                         
         USING OFFALD,R1          CHECK FOR MAX # OF OFFICES OF 255             
         L     R1,AOFFBLK         BY LOOKING AT THE FIRST TWO BYTES             
         LA    RF,OFFAWORK        OF OFFAWORK                                   
         LH    RE,0(RF)                                                         
         CHI   RE,255                                                           
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$MAXOF)                                           
         B     EXITL                                                            
*                                                                               
         BAS   RE,VALPHA          VALIDATE FOR SPECIAL CHARACTERS               
         BE    VALOCD10                                                         
         MVC   FVMSGNO,=AL2(AE$INOFF) INVALID OFFICE/OFFICE LIST                
         B     EXITL                                                            
*                                                                               
* LIMITED OFFICE ACCESS CHECK - DOES NOT WORK - GIVES A LIST OF VALID           
* OFFICES BUT DOES NOT INCLUDE THE OFFICE LIST IF YOUR SIGN ON ID IS            
* LINKED TO AN OFFICE LIST (E.G.  DDS2Y)                                        
VALOCD10 DS    0H                                                               
*        L     R1,AOFFBLK          TEST OFFICE SECURITY                         
*        USING OFFALD,R1                                                        
*        MVI   OFFAACT,OFFAREQ     VALIDATE REQUESTED OFFICE                    
*        LA    RE,OFFAWORK                                                      
*        ST    RE,OFFAREQL         A(REQUESTED OFFICE LIST OUTPUT AREA)         
*        MVC   OFFAOFFC,FVIFLD     OFFICE TO VALIDATE                           
*        GOTO1 VOFFAL                                                           
*        BE    VALOCD20                                                         
*        MVC   FVMSGNO,=AL2(AE$OFFID)   INVALID OFFICE FOR THIS ID              
*        B     EXITL                                                            
*                                                                               
VALOCD20 LLC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,OFFKOFF,FVIFLD   MOVE IN OFFICE CODE                          
         EXMVC RF,SVCODE,FVIFLD    SAVE THE OFFICE CODE                         
         OC    OFFKOFF,BCSPACES                                                 
         MVC   AOFFICE,FVADDR      SAVE A(OFFICE FIELD)                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN OFFICE CODE FILTER FIELD                                           
***********************************************************************         
         SPACE 1                                                                
DFLTOCDE MVC   FVIFLD(L'OFFKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN OFFICE CODE FILTER FIELD                                          
***********************************************************************         
         SPACE 1                                                                
T        USING OFFRECD,RF                                                       
VFLTOCDE CLI   FVILEN,2                                                         
         CLI   FVILEN,2                                                         
         BNE   EXITNV                                                           
         LLC   RF,FVXLEN           INPUT DATA LENGTH-1                          
         EXMVC RF,FLTIFLD,FVIFLD   MOVE IN OFFICE CODE TO FILTER FIELD          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON OFFICE CODE                                                   
***********************************************************************         
         SPACE 1                                                                
DOFTOCDE CLC   OFFKOFF,BCSPACES  IS THERE A CODE TO COMPARE ON?                 
         BNH   FLTXX               NO - WE DON`T WANT IT THEN                   
*                                                                               
         CLC   OFFKOFF,FLTIFLD                                                  
         BE    FLTXE                                                            
         BL    FLTXL                                                            
         BH    FLTXH                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER ELEMENTS                                 
* R1 POINTS TO RECORD                                                           
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R5                                                       
ADDACTEL NTR1                                                                   
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    EXITOK              NO                                           
         L     R2,0(R1)                                                         
         L     R5,AIO3                                                          
         XC    RAPBLK(RAPBLKL),RAPBLK                                           
         MVI   RAPACTN,RAPAELEM    BUILD RAP PTR ELEM                           
         MVC   RAPCPY,CUABIN                                                    
         MVI   RAPRTYP,RAPKROFL                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOM                                                     
         ST    R2,RAPAREC         ADDRESS OF RECORD                             
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO GENERATE ACTIVITY POINTER                                *         
***********************************************************************         
         SPACE 1                                                                
         USING RAPPERD,R5                                                       
ADDACTPT NTR1                                                                   
         TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    EXITOK              NO                                           
         L     R5,AIO3                                                          
         MVI   RAPACTN,RAPAPTR     BUILD RAP PTR RECORD                         
         MVC   RAPCPY,CUABIN                                                    
         MVI   RAPRTYP,RAPKROFL                                                 
         MVI   RAPEMU,C'N'                                                      
         MVC   RAPACOM,ACOM                                                     
         ST    R2,RAPAREC         ADDRESS OF RECORD                             
         CLI   CSACT,A#CPY         FOR ACTN COPY CLEAR OUT THE OLD PTR          
         BNE   *+10                OR ELSE WON'T BUILD THE ACTIVITY             
         XC    RAPOLDP,RAPOLDP     REC                                          
         GOTO1 VRAPPER,RAPBLK                                                   
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE THAT OFFICE CODE IS NOT A SPECIAL CHARACTER                *         
***********************************************************************         
         SPACE 1                                                                
VALPHA   NTR1                                                                   
         LA    R0,2                VALIDATE TWO CHARACTERS                      
         LA    R1,FVIFLD                                                        
*                                                                               
VALPH2   CLI   0(R1),C'0'                                                       
         BL    *+16                                                             
         CLI   0(R1),C'9'                                                       
         BH    EXITNV                                                           
         B     VALPH4                                                           
         CLI   0(R1),C'A'                                                       
         BL    EXITNV                                                           
         CLI   0(R1),C'I'                                                       
         BNH   VALPH4                                                           
         CLI   0(R1),C'J'                                                       
         BL    EXITNV                                                           
         CLI   0(R1),C'R'                                                       
         BNH   VALPH4                                                           
         CLI   0(R1),C'S'                                                       
         BL    EXITNV                                                           
         CLI   0(R1),C'Z'                                                       
         BH    EXITNV                                                           
*                                                                               
VALPH4   LA    R1,1(R1)                                                         
         BCT   R0,VALPH2                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SHORT OFFICE NAME                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SHNAME   LA    RF,SNAMTBL                                                       
         B     ITER                                                             
*                                                                               
SNAMTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISSNAM)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSNAM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE SHORT OFFICE NAME                                                 
***********************************************************************         
         SPACE 1                                                                
DISSNAM  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SNMELQ',OFFRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK              NO SHORT NAME                                
*                                                                               
         USING SNMELD,R4                                                        
         L     R4,12(R1)                                                        
         MVC   FVIFLD(L'SNMNAME),SNMNAME                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE SHORT OFFICE NAME                                                
***********************************************************************         
         SPACE 1                                                                
VALSNAM  CLI   FVIFLD,C' '         1ST CHAR CAN'T BE A SPACE                    
         BE    EXITNV                                                           
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('SNMELQ',OFFRECD),0               
*                                                                               
         USING SNMELD,R4                                                        
         LA    R4,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   SNMEL,SNMELQ        X'70' ELEMENT                                
         MVC   SNMNAME,BCSPACES                                                 
         LLC   R1,FVXLEN                                                        
         EXMVC R1,SNMNAME,FVIFLD                                                
         MVI   SNMLN,SNMLNQ                                                     
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),OFFRECD,BOELEM,0                   
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICES IN LIST                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFDTA   LA    RF,OFFDTBL                                                       
         USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
OFFDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISOFFD)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOFFD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE OFFICES                                                           
***********************************************************************         
         SPACE 1                                                                
DISOFFD  OC    TLKOFFC,TLKOFFC     IS THIS AN OFFICE LIST REC?                  
         BZ    EXITOK              YES NOTHING TO SHOW                          
         CLC   TLKOFFC,EFFS        IS THIS THE DUMMY ENTRY?                     
         BE    EXITOK              DON'T SHOW                                   
         MVC   FVIFLD(L'TLKOFFC),TLKOFFC                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE OFFICE AND PUT OFFICE TO TSAR                                    
***********************************************************************         
         SPACE 1                                                                
VALOFFD  DS    0H                                                               
         CLI   FVILEN,0            USER DELETING THIS ENTRY?                    
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   TELL CONTROLLER                              
         B     EXITOK                                                           
*                                                                               
VALOFD02 CLI   LISTTYPE,C'N'       IF ENTRY HERE AND TYPE IF OFFICE             
         BE    EXITNV             THAN GIVE INVALID INPUT MESSAGE               
         CLI   CSACT,A#ADD         ACTION ADD?                                  
         BE    VALOFD10            OKAY                                         
         TM    BIT,NOTLIST         IF TRYING TO CHANGE FROM OFFICE TO           
         BZ    VALOFD10            OFFICE LIST - FIRST CHECK BECAUSE            
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('OFIELQ',AIOREC),0                
         CLI   12(R1),0            CANNOT MAKE IT AN OLIST IF THE OFF           
         BE    *+6                 IS ALREADY PART OF AN OFFICE LIST(!)         
         DC    H'0'                                                             
*                                                                               
         USING OFIELD,R4                                                        
         L     R4,12(R1)                                                        
         OC    OFIINC,OFIINC       THIS HOLD THE # OF LISTS THIS OFFICE         
         BZ    VALOFD05            IS INCLUDED IN                               
         MVC   FVADDR,AOFFICE                                                   
         MVC   FVMSGNO,=AL2(AE$OLCOL)                                           
         B     EXITL                                                            
*                                                                               
         USING ACTRECD,R3                                                       
VALOFD05 LA    R3,IOKEY            ALSO IF THERE IS A 2D ACCOUNT FOR            
         MVC   IOKEY,BCSPACES      THIS OFFICE THAN DON'T ALLOW TO MAKE         
         MVC   ACTKCPY,CUABIN      IT INTO AN OFFICE LIST                       
         MVC   ACTKUNT(L'ACTKUNT+L'ACTKLDG),=C'2D'                              
         MVC   ACTKACT(L'SVCODE),SVCODE  MOVE IN THE OFFICE                     
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   VALOFD10            OKAY TO CONTINUE                             
         MVC   FVMSGNO,=AL2(AE$OLNO) OFFICE LIST NOT ALLOWED                    
         B     EXITL                                                            
         DROP  R3                                                               
*                                                                               
VALOFD10 OC    TLKOFFC,TLKOFFC     IS THERE ALREADY AN OFFICE?                  
         BNZ   VALOFD20            YES SO DON'T CHECK FOR MAX                   
         LH    RE,LSLST#X          LAST LIST REC #                              
         LH    RF,LSLST#1             FIRST LIST REC #                          
         SR    RE,RF                                                            
         LA    RE,1(RE)            BUMP UP RE FOR REAL NUMBER                   
         CHI   RE,MAXOFF           240 OFFICES MAX                              
         BL    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$OFFMX)                                           
         B     EXITL                                                            
VALOFD20 MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         BAS   RE,VALOFF           MAKE SURE OFFICE EXITS                       
         BE    VALOFD30                                                         
         CLC   FVMSGNO,=AL2(AE$NOLST)  ALREADY SET ERROR MSG?                   
         BE    EXITL               YES SO USE IT                                
         CLC   FVMSGNO,=AL2(AE$OFFID)  ALREADY SET ERROR MSG?                   
         BE    EXITL               YES SO USE IT                                
         MVC   FVMSGNO,=AL2(AE$IVOFF)                                           
         B     EXITL                                                            
VALOFD30 CLC   SVCODE,FVIFLD       MAKE SURE THE OFFICE CODE IS                 
         BE    EXITNV              NOT A MEMBER OF IT'S OWN LIST                
         OI    BIT,OFFLIST         SET BIT THAT THERE IS AT LEAST 1 OFF         
         MVC   TLKOFFC(L'OFLNTRY),FVIFLD  MOVE INTO TSAR REC                    
         OI    BIT,OFFCSET                                                      
         BAS   RE,GETSNAM          GET OFFICE NAME AND MOVE TO TSAR             
*                                                                               
VALOFDX  DS    0H                                                               
*&&DO                                                                           
         L     R3,ATYPADDR         POINT TO SAVED TYPE FIELD                    
         USING FHD,R3                                                           
         MVC   FHDA(L'AC@LIST),AC@LIST    MOVE IN LIST INFO                     
         MVI   FHIL,L'AC@LIST                                                   
         CLI   LISTTYPE,C'Y'              IS THIS TRULY A LIST?                 
         BE    *+14                                                             
         MVC   FHDA(L'AC@OFFC),AC@OFFC    NO SO MOVE IN OFFICE INFO             
         MVI   FHIL,L'AC@OFFC                                                   
         OI    FHOI,FHOITR                                                      
*&&                                                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* GET THE OFFICE NAME FROM THE SHORT OFFICE ELEMENT & MOVE TO TSAR              
***********************************************************************         
*                                                                               
T        USING OFFRECD,R5                                                       
GETSNAM  NTR1                                                                   
         LA    R5,IOKEY            READ OFFICE REC TO GET NAME                  
         MVC   T.OFFKEY,BCSPACES                                                
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKCPY,CUABIN                                                 
         MVC   T.OFFKOFF,TLKOFFC                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
                                                                                
         L     R5,AIO2                                                          
         LA    R4,T.OFFRECD+(T.OFFRFST-T.OFFRECD)                               
GETS10   CLI   0(R4),0                                                          
         BE    EXITOK                                                           
         CLI   0(R4),X'70'         SHORT NAME ELEMENT                           
         BE    GETS15                                                           
         LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     GETS10                                                           
*                                                                               
         USING SNMELD,R4                                                        
GETS15   MVC   TLKNAME,SNMNAME                                                  
         OC    TLKNAME,BCSPACES                                                 
         B     EXITOK                                                           
         DROP  R4,T                                                             
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THAT THE OFFICE EXISTS                                               
***********************************************************************         
         SPACE 1                                                                
T        USING OFFRECD,R5                                                       
VALOFF   NTR1                                                                   
* LIMITED OFFICE ACCESS CHECK - DOES NOT WORK - GIVES A LIST OF VALID           
* OFFICES BUT DOES NOT INCLUDE THE OFFICE LIST IF YOUR SIGN ON ID IS            
* LINKED TO AN OFFICE LIST (E.G.  DDS2Y)                                        
*        L     R1,AOFFBLK          TEST OFFICE SECURITY                         
*        USING OFFALD,R1                                                        
*        MVI   OFFAACT,OFFAREQ     VALIDATE REQUESTED OFFICE                    
*        LA    RE,OFFAWORK                                                      
*        ST    RE,OFFAREQL         A(REQUESTED OFFICE LIST OUTPUT AREA)         
*        MVC   OFFAOFFC,FVIFLD     OFFICE TO VALIDATE                           
*        GOTO1 VOFFAL                                                           
*        BE    VALOF10                                                          
*        MVC   FVMSGNO,=AL2(AE$OFFID)   INVALID OFFICE FOR THIS ID              
*        B     EXITL                                                            
*                                                                               
VALOF10  LA    R5,IOKEY                                                         
         MVC   T.OFFKEY,BCSPACES                                                
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKCPY,CUABIN                                                 
         CLI   FVILEN,2            MUST BE 2 BYTE OFFICES                       
         BNE   EXITL                                                            
         MVC   T.OFFKOFF,FVIFLD                                                 
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         L     R5,AIO2                                                          
         TM    T.OFFRSTAT,OFFSLIST   IS THIS AN OFFICE LIST?                    
         BZ    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$NOLST)  THEN CAN'T ADD                           
         B     EXITL                                                            
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* CHECK FOR ADDITIONS (MUST UPDATE THE OFFICE RECORDS)                          
***********************************************************************         
*                                                                               
         USING OFLELD,R3                                                        
CHKADDS  NTR1                                                                   
         LA    R0,BLDOFFC          ANYTHING IN TABLE OF OFFICE CODES?           
         LA    R1,L'BLDOFFC                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE                                                            
         BE    EXITOK              NO MUST BE DELETING ALL CODES                
*                                                                               
         LA    R0,OFFTAB           IF NOTHING IS IN THE SAVED OFFC TBL          
         LA    R1,L'OFFTAB         THEN MUST BE ADDING ALL                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE                                                            
         BE    CHKA50                                                           
*                                                                               
         LA    R3,BLDOFFC          R3 POINTS TO 1ST OFFICE CODE IN              
         LLC   R1,SVLEN            THE ELEMENT WORK AREA                        
         LA    R4,OFFTAB                                                        
         ST    R4,SVADDR           STORE OFFTAB ADDRESS                         
         LLC   R5,NUMOFFSV         R5 # NO OF OFFICES IN OFFTAB                 
CHKA10   CLC   0(L'OFLNTRY,R3),0(R4)                                            
         BE    CHKA20                                                           
         LA    R4,L'OFLNTRY(R4)    BUMP UP SAVED AREA                           
         BCT   R5,CHKA10                                                        
         MVC   SVOFF,0(R3)         SAVE OFFICE CODE FOR UPDATE                  
         OI    BIT,ADDONE                                                       
         BAS   RE,UPDOFF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
CHKA20   LA    R3,L'OFLNTRY(R3)    BUMP TO NEXT ENTRY                           
         L     R4,SVADDR           RESET R4 TO PNT TO BEGINNING OF BLK          
         LLC   R5,NUMOFFSV         RESET 5R TO # OF CODES IN BLOCK              
         BCT   R1,CHKA10           AND CHECK NEXT ENTRY                         
         B     EXITOK                                                           
*                                                                               
CHKA50   LA    R4,BLDOFFC          ADDING ALL ENTRIES                           
         LLC   R5,SVLEN                                                         
CHKA60   MVC   SVOFF,0(R4)                                                      
         OI    BIT,ADDONE                                                       
         BAS   RE,UPDOFF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,L'OFLNTRY(R4)    BUMP TO NEXT ENTRY                           
         BCT   R5,CHKA60                                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* CHECK FOR DELETIONS (MUST UPDATE THE OFFICE RECORDS)                          
***********************************************************************         
*                                                                               
CHKDELS  NTR1                                                                   
         LA    R0,OFFTAB           ANYTHING FROM D2 ELEMENT                     
         LA    R1,L'OFFTAB         NO-MUST BE NEW                               
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE                                                            
         BE    EXITOK                                                           
*                                                                               
         LA    R0,BLDOFFC          IF NOTHING IN ELEM BLK                       
         LA    R1,L'BLDOFFC        THEN MUST BE DELETING ALL                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE                                                            
         BE    CHKD50                                                           
*                                                                               
         LA    R3,BLDOFFC          R3 POINTS TO 1ST OFFICE CODE IN              
         ST    R3,SVADDR           THE ELEMENT WORK AREA                        
         LLC   R1,SVLEN                                                         
         STC   R1,NUMOFFSV                                                      
         LA    R4,OFFTAB                                                        
         LLC   R5,NUMOFSV1         R5=# OF OFFICES IN SAVED OFFICE BLK          
CHKD10   CLC   0(L'OFLNTRY,R4),0(R3)                                            
         BE    CHKD20                                                           
         LA    R3,L'OFLNTRY(R3)    BUMP UP ELEMENT                              
         BCT   R1,CHKD10                                                        
         MVC   SVOFF,0(R4)         SAVE OFFICE CODE TO UPDATE                   
         NI    BIT,X'FF'-ADDONE                                                 
         BAS   RE,UPDOFF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
CHKD20   LA    R4,L'OFLNTRY(R4)   BUMP TO NEXT ENTRY                            
         L     R3,SVADDR           RESET R3 TO PNT TO BEGINNING OF ELEM         
         LLC   R1,NUMOFFSV         RESET R1 TO # OF CODES IN ELEM               
         BCT   R5,CHKD10           AND CHECK NEXT ENTRY                         
         B     EXITOK                                                           
*                                                                               
CHKD50   LA    R4,OFFTAB           DELETING ALL ENTRIES                         
         LLC   R5,NUMOFSV1         R5 #NO OF OFFICES                            
CHKD60   NI    BIT,X'FF'-ADDONE                                                 
         MVC   SVOFF,0(R4)                                                      
         BAS   RE,UPDOFF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R4,L'OFLNTRY(R4)    BUMP TO NEXT ENTRY                           
         BCT   R5,CHKD60                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* CHECK HOW MANY OFFICE RECORDS EXIST ON THE FILE                               
***********************************************************************         
         SPACE 1                                                                
T        USING OFFRECD,R5                                                       
CHKLIMIT NTR1                                                                   
         ZAP   RECCOUNT,=P'0'                                                   
         LA    R5,IOKEY                                                         
         MVC   T.OFFKEY,BCSPACES                                                
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKCPY,CUABIN                                                 
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   IOKEY(OFFKOFF-OFFKEY),IOKEYSAV    SAME COMPANY?                  
         BNE   CHKLM30                                                          
         B     CHKLM20                                                          
CHKLM10  LHI   R1,XOSEQ+XOACCDIR+XIO2                                           
         GOTO1 AIO                                                              
         CLC   IOKEY(OFFKOFF-OFFKEY),IOKEYSAV                                   
         BNE   CHKLM30                                                          
CHKLM20  AP    RECCOUNT,=P'1'                                                   
         B     CHKLM10                                                          
*                                                                               
CHKLM30  CP    RECCOUNT,=P'255'                                                 
         BL    CHKLME                                                           
*                                                                               
CHKLML   CLI   *,FF                                                             
         B     CHKLMX                                                           
CHKLME   CR    RB,RB                                                            
CHKLMX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* UPDATE THE OFFICE RECORD                                                      
***********************************************************************         
         SPACE 1                                                                
T        USING OFFRECD,R5                                                       
UPDOFF   NTR1                                                                   
         LA    R5,IOKEY                                                         
         MVC   T.OFFKEY,BCSPACES                                                
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKCPY,CUABIN                                                 
         MVC   T.OFFKOFF,SVOFF                                                  
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)   READ FOR UPDATE                  
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2) GETREC FOR UPDATE                
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         L     R5,AIO2                                                          
         LA    R3,T.OFFRECD+(T.OFFRFST-T.OFFRECD)                               
UPD05    CLI   0(R3),0                                                          
         BE    EXITL                                                            
         CLI   0(R3),X'D1'                                                      
         BE    UPD10                                                            
         LLC   R1,1(R3)                                                         
         AR    R3,R1                                                            
         B     UPD05                                                            
*                                                                               
         USING OFIELD,R3                                                        
UPD10    ICM   R1,3,OFIINC           # OF LISTS THIS OFFICE IS IN               
         TM    BIT,ADDONE          ADDING ONE TO THE NUMBER OF LISTS?           
         BZ    *+12                                                             
         AHI   R1,1                YES ADD ONE                                  
         B     *+6                                                              
         BCTR  R1,0                NO DECREMENT BY ONE                          
         STCM  R1,3,OFIINC                                                      
         LHI   R1,XOPUT+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         LHI   R1,XOWRITE+XOACCDIR+XIO2                                         
         GOTO1 AIO                                                              
         BNE   EXITL                                                            
         B     EXITOK                                                           
         DROP  R3,T                                                             
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE NAMES IN LIST                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OFFNMEL  LA    RF,OFFNMTBL                                                      
T        USING TLSTD,R2                                                         
         B     ITER                                                             
*                                                                               
OFFNMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISONAM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE OFFICE LIST MEMBER NAME FROM TSAR                                 
***********************************************************************         
         SPACE 1                                                                
DISONAM  OC    TLKNAME,TLKNAME     IS THIS AN OFFICE LIST REC?                  
         BZ    EXITOK              NO THEN NOTHING TO DISPLAY                   
         CLC   TLKNAME,EFFS        IS THIS THE DUMMY ENTRY?                     
         BE    EXITOK              DON'T SHOW                                   
         MVC   FVIFLD(L'T.TLKNAME),T.TLKNAME                                    
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NUMBER OF LISTS THIS OFFICE IS IN                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NUMLISTS LA    RF,NUMLSTTB                                                      
         B     ITER                                                             
*                                                                               
NUMLSTTB DC    AL1(DDIS),AL1(0,0,0),AL4(DISNLST)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE NUMBER OF LISTS THIS OFFICE IS IN ON THE LIST SCREEN              
***********************************************************************         
         SPACE 1                                                                
T        USING OFFKSTA,GSRECSTA                                                 
         USING OFFRECD,R2                                                       
DISNLST  CLI   CSACT,A#LST         FOR ACTION LIST ONLY                         
         BNE   EXITOK                                                           
         TM    T.OFFKSTAT,OFFSLIST OFFICE LIST?   THIS ROUTINE IS               
         BZ    DNLST10                          ONLY FOR OFFICES                
         MVI   FVIFLD,C'.'                                                      
         B     EXITOK                                                           
         DROP  T                                                                
*                                                                               
DNLST10  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('OFIELQ',OFFRECD),0               
         CLI   12(R1),0                                                         
         BE    *+14                                                             
         CLI   CSACT,A#LST        FOR ACTION LIST DON'T BOTHER                  
         BE    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
         USING OFIELD,R4                                                        
         L     R4,12(R1)                                                        
         EDIT  (2,OFIINC),(5,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK,        +        
               WRK=BOWORK1,DUB=BODUB1                                           
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR NUMBER OF OFFICES IN LIST                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
NUMOFFS  LA    RF,NUMOFFTB                                                      
         B     ITER                                                             
*                                                                               
NUMOFFTB DC    AL1(DDIS),AL1(0,0,0),AL4(DISNOFF)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE NUMBER OF OFFICES IN LIST                                         
***********************************************************************         
         SPACE 1                                                                
T        USING OFFKSTA,GSRECSTA                                                 
         USING OFFRECD,R2                                                       
DISNOFF  MVI   TOTOFF,ZERO         INIT TOTAL NO OF OFFICES                     
         CLI   CSACT,A#LST         FOR ACTION LIST ONLY                         
         BNE   EXITOK                                                           
         TM    T.OFFKSTAT,OFFSLIST OFFICE LIST?   THIS ROUTINE IS               
         BO    DNOFF10                          FOR OFFICE LISTS ONLY           
         MVI   FVIFLD,C'.'                                                      
         B     EXITOK                                                           
         DROP  T                                                                
*                                                                               
DNOFF10  DS    0H                                                               
         USING OFLELD,R4                                                        
         LA    R4,OFFRFST                                                       
DNOFF12  CLI   OFLEL,ZERO          END OF REC?                                  
         BE    DNOFF18             YES, CONTINUE                                
         CLI   OFLEL,OFLELQ        NO, OFLEL FOUND?                             
         BE    DNOFF16             NO, CHECK FOR NEXT                           
*                                                                               
DNOFF14  LLC   R1,OFLLN            CHECK NEXT ELEMENT                           
         AR    R4,R1                                                            
         J     DNOFF12                                                          
*                                                                               
DNOFF16  LLC   R3,OFLLN            LENGTH OF ELEMENT                            
         SHI   R3,OFLLN1Q          MINUS OVERHEAD                               
         SRA   R3,1                DIVIDE BY 2 TO GET # OF OFFICES              
         LLC   R1,TOTOFF           ACCUMULATE TOTAL NO OF OFFICES FROM          
         AR    R3,R1               ALL THE OFLEL ELEMENTS                       
         STC   R3,TOTOFF                                                        
         J     DNOFF14                                                          
*                                                                               
DNOFF18  LLC   R3,TOTOFF           DISPLAY TOT NO OF OFFICES FROM ALL           
         EDIT  (R3),(3,FVIFLD),0,ALIGN=LEFT,ZERO=NOBLANK,WRK=BOWORK1,  +        
               DUB=BODUB1          OFLEL ELEMENTS                               
         B     EXITOK                                                           
         DROP  R4                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TYPE OF OFFICE                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
OTYPE    LA    RF,OTYPTABL                                                      
         B     ITER                                                             
*                                                                               
OTYPTABL DC    AL1(DDIS),AL1(0,0,0),AL4(DISOTYP)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOTYP)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDTYP)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVTYP)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTTYP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE TYPE OF OFFICE                                                    
***********************************************************************         
         SPACE 1                                                                
T        USING OFFKSTA,GSRECSTA                                                 
DISOTYP  MVC   ATYPADDR,FVADDR    SAVE ADDR OF THIS FIELD                       
*                                 TO DISPLAY AFTER PROCESS LIST PART            
         TM    T.OFFKSTAT,OFFSLIST                                              
         BO    *+14                                                             
         MVC   FVIFLD(L'AC@OFFC),AC@OFFC   DISPLAY OFFICE                       
         B     EXITOK                                                           
         MVC   FVIFLD(L'AC@LIST),AC@LIST  DISPLAY LIST                          
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE TYPE OF OFFICE                                                   
***********************************************************************         
         SPACE 1                                                                
T        USING OFFKSTA,GSRECSTA                                                 
VALOTYP  MVI   LISTTYPE,C'N'             DEFAULT TO OFFICE                      
         LLC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,AC@OFFC         OFFICE                                 
         BNE   VALOTY10                                                         
         BAS   RE,CHKLIST                IF OFFICE THERE SHOULD BE NO           
         BL    EXITOK                    OFFICES IN LIST                        
         SR    R1,R1                                                            
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA            R1 POINTS TO FIRST OFFICE FIELD               
         ST    R1,BOCURSOR                                                      
         B     EXITNV                                                           
VALOTY10 EXCLC R1,FVIFLD,AC@LIST         LIST                                   
         BNE   EXITNV                                                           
         MVI   LISTTYPE,C'Y'                                                    
         OI    T.OFFKSTAT,OFFSLIST   TURN BIT ON IN STATUS OF KEY               
         CLI   CSACT,A#ADD           IF LIST MAKE SURE AT LEAST ONE             
         BE    EXITOK                OFFICE HAS BEEN ENTERED BUT DON'T          
         BAS   RE,CHKLIST            BOTHER WHEN ADDING (UPDLAST WILL           
         BE    EXITOK                HANDLE IT.)                                
         SR    R1,R1                                                            
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA            R1 POINTS TO FIRST OFFICE FIELD               
         ST    R1,BOCURSOR                                                      
         B     EXITNO                                                           
         SPACE  2                                                               
***********************************************************************         
* DISPLAY THE OFFICE TYPE AS A FILTER                                           
***********************************************************************         
         SPACE 1                                                                
DFDTYP   CLI   FLTIFLD,0                                                        
         BE    EXITOK              NO FILTER TO DISPLAY                         
         MVC   FVIFLD(6),FLTIFLD   9 IS MAX                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE OFFICE TYPE AS A FILTER                                          
***********************************************************************         
         SPACE 1                                                                
DFVTYP   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         LLC   R1,FVXLEN                                                        
         EXCLC R1,FVIFLD,AC@OFFC   OFFICE?                                      
         BNE   *+14                                                             
         MVC   FLTIFLD(L'AC@OFFC),AC@OFFC                                       
         B     EXITOK                                                           
DFVTYP5  EXCLC R1,FVIFLD,AC@LIST   LIST?                                        
         BNE   EXITNV                                                           
         MVC   FLTIFLD(L'AC@LIST),AC@LIST                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR LIST TYPE                                                    
***********************************************************************         
         SPACE 1                                                                
T        USING OFFKSTA,GSRECSTA                                                 
DOFLTTYP CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTER ON                         
         LLC   R1,FVXLEN                                                        
         EXCLC R1,FLTIFLD,AC@OFFC  OFFICE?                                      
         BNE   *+16                                                             
         TM    T.OFFKSTAT,OFFSLIST                                              
         BZ    FLTXE                                                            
         B     FLTXL                                                            
         EXCLC R1,FLTIFLD,AC@LIST  LIST?                                        
         BNE   FLTXL                                                            
         TM    T.OFFKSTAT,OFFSLIST                                              
         BO    FLTXE                                                            
         B     FLTXL                                                            
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FILTER TO SEE WHICH OFFICE LISTS INCLUDE A CERTAIN  *         
* OFFICE ENTERED                                                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FLIST    LA    RF,FLISTTBL                                                      
         B     ITER                                                             
*                                                                               
FLISTTBL DC    AL1(DFDIS),AL1(0,0,0),AL4(DFDFLI)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(DFVFLI)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLTFLI)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY THE 'WHICH OFFICE LISTS CONTAIN OFFICE XX' FILTER                     
***********************************************************************         
         SPACE 1                                                                
DFDFLI   CLI   FLTIFLD,0                                                        
         BE    EXITOK              NO FILTER TO DISPLAY                         
         MVC   FVIFLD(L'OFFKOFF),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE THE 'WHICH OFFICE LISTS CONTAIN OFFICE XX' FILTER                    
***********************************************************************         
         SPACE 1                                                                
DFVFLI   CLI   FVILEN,0                                                         
         BE    EXITOK              NO FILTER                                    
         MVC   FLTIFLD(L'OFFKOFF),FVIFLD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR 'WHICH OFFICE LISTS CONTAIN OFFICE XX' FILTER                
***********************************************************************         
         SPACE 1                                                                
T        USING OFFKSTA,GSRECSTA                                                 
DOFLTFLI CLI   FLTIFLD,0                                                        
         BE    EXITOK              NOTHING TO FILTER ON                         
         TM    T.OFFKSTAT,OFFSLIST   MUST BE AN OFFICE LIST                     
         BZ    FLTXL                                                            
*                                                                               
         LA    R4,OFFRECD+(OFFRFST-OFFRECD)                                     
         USING OFLELD,R4                                                        
DOFLI10  CLI   0(R4),0                                                          
         BE    FLTXL               OFFICE NOT IN THIS LIST                      
         CLI   0(R4),OFLELQ        D2 ELEMENT                                   
         BE    DOFLI30                                                          
DOFLI20  LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     DOFLI10                                                          
*                                                                               
DOFLI30  LLC   RF,OFLLN            LENGTH OF ELEMENT                            
         SHI   RF,OFLLN1Q          MINUS OVERHEAD                               
         SRA   RF,1                DIVIDE BY 2 TO GET # OF OFFICES              
         LA    RE,OFLNTRY          POINT TO FIRST OFFICE IN LIST                
DOFLI40  CLC   0(L'OFFKOFF,RE),FLTIFLD   MATCH ON OFFICE WANTED?                
         BE    FLTXE                                                            
         LA    RE,L'OFFKOFF(RE)    BUMP TO NEXT OFFICE IN LIST                  
         BCT   RF,DOFLI40                                                       
         B     DOFLI20             CHECK FOR MORE ELEMENTS                      
*                                                                               
         DROP  R4                                                               
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
         USING OFFRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING OFFRECD,R2                                                       
LAST     USING OFFRECD,R3                                                       
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
LISTABL  DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
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
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY(L'LSTKEY),THIS.OFFRECD                                     
         LHI   R1,XOHIGH+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         BNE   EXITL               MESS UP ON THE READ HIGH                     
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
NLST02   CLC   IOKEY(OFFKOFF-OFFRECD),THIS.OFFRECD                              
         BNE   EXITL                                                            
*                                                                               
* LIMITED OFFICE ACCESS CHECK - DOES NOT WORK - GIVES A LIST OF VALID           
* OFFICES BUT DOES NOT INCLUDE THE OFFICE LIST IF YOUR SIGN ON ID IS            
* LINKED TO AN OFFICE LIST (E.G.  DDS2Y)                                        
*        L     R1,AOFFBLK          TEST OFFICE SECURITY                         
*        USING OFFALD,R1                                                        
*        MVI   OFFAACT,OFFAREQ     VALIDATE REQUESTED OFFICE                    
*        LA    RE,OFFAWORK                                                      
*        ST    RE,OFFAREQL         A(REQUESTED OFFICE LIST OUTPUT AREA)         
*        MVC   OFFAOFFC,IOKEY+(OFFKOFF-OFFRECD)   OFFICE TO VALIDATE            
*        GOTO1 VOFFAL                                                           
*        BNE   NLST                                                             
*                                                                               
         LHI   R1,XOREAD+XOACCDIR+XIO1                                          
         GOTO1 AIO                                                              
         MVC   THIS.OFFKEY(L'OFFKEY+L'OFFKSTA+L'OFFKDA),IOKEY                   
         B     EXITOK                                                           
*                                                                               
         SPACE 2                                                                
         LTORG                                                                  
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
FTFLST1  LA    RF,OFFRFST-OFFRECD                                               
         STH   RF,RECDSP           SAVE DISPL TO FIRST ELEMENT                  
         NI    BIT,X'FF'-NOTLIST                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1                                                    *         
***********************************************************************         
         SPACE 1                                                                
FLST1    MVI   NUMOFF,ZERO                                                      
         LA    R0,SAVOFLST         CLEAR SAVOFLST                               
         LA    R1,L'SAVOFLST                                                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    RF,SAVOFLST         POINT TO AVAILABLE SPACES IN                 
         ST    RF,ASVOFLST         SAVOFLST                                     
*                                                                               
         LH    RF,RECDSP           CURRENT DISPLACEMENT INTO RECORD             
         A     RF,AIOREC           A(RECORD)                                    
         C     RF,AIOREC           MAKE SURE ELEMDSP INITIALISED                
         BH    *+8                                                              
         LA    RF,OFFRFST(RF)      IT IS NOW.                                   
         XR    RE,RE                                                            
         ST    RF,SAVERF                                                        
*                                                                               
         USING OFLELD,RF                                                        
FML02    CLI   OFLEL,0             RECORD END?                                  
         BNE   FML02A                                                           
         CLI   NUMOFF,ZERO                                                      
         JNZ   FML06                                                            
         MVI   NUMOFF,ZERO         MOVE 00'S INTO NUMOFF SO WE ARE DONE         
         OI    BIT,NOTLIST         SET BIT THAT THIS IS NOT A LIST              
         B     EXITOK                                                           
FML02A   CLI   OFLEL,OFLELQ        D2 ELEMENT?                                  
         BE    FML04               NO                                           
FML02B   LLC   R1,OFLLN                                                         
         AR    RF,R1                                                            
         B     FML02                                                            
                                                                                
FML04    LR    RE,RF                                                            
         S     RE,AIOREC                                                        
         STH   RE,RECDSP           SAVE DISPL TO ELEMENT                        
*                                                                               
T        USING OFLELD,RE                                                        
         LLC   RE,OFLLN            FIND # OF OFFICES IN ELEMENT                 
         SHI   RE,OFLLN1Q                                                       
*                                                                               
         BCTR  RE,0               STORE OFFICE CODES FROM OFLEL INTO            
         L     R1,ASVOFLST        SAVOFLST                                      
         EXMVC RE,0(R1),OFLNTRY                                                 
         AHI   RE,1                                                             
         AR    R1,RE              POINT TO NEXT AVAILABLE SPACE IN              
         ST    R1,ASVOFLST        SAVOFLST                                      
*                                                                               
         SRA   RE,1               NO. OF. OFFICES STORED IN SAVOFLST            
         LLC   R1,NUMOFF          ACCUMULATE NO. OF OFFICES STORED IN           
         AR    RE,R1              SAVOFLST AND SAVE IT IN NUMOFF                
         STC   RE,NUMOFF                                                        
         J     FML02B                                                           
*                                                                               
FML06    LA    RF,SAVOFLST        RESTORE THE POINTER TO FIRST OFFICE           
         ST    RF,ASVOFLST        CODE IN SAVOFLST                              
         MVC   NUMOFFSV,NUMOFF    STORE NO. OF OFFICES                          
         MVC   NUMOFSV1,NUMOFF                                                  
         B     EXITOK             EXIT                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1                                                     *         
***********************************************************************         
         SPACE 1                                                                
NLST1    CLI   NUMOFF,0           NO OFFICE CODES?                              
         BE    EXITL              YES, EXIT                                     
         L     RF,ASVOFLST        POINT TO NEXT AVAILABLE OFFICE CODE           
         AHI   RF,2               IN SAVOFLST                                   
         ST    RF,ASVOFLST                                                      
         B     EXITOK             EXIT                                          
         SPACE 2                                                                
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
         L     RF,ASVOFLST         POINT TO NEXT AVAILABLE OFFICE CODE          
*                                                                               
         CLI   NUMOFF,ZERO         IF RECDSP IS EFFS                            
         BNE   TSARF05             THEN CREATE A DUMMY TSAR ENTRY               
         MVC   TLKOFFC,EFFS        SO WILL GET CALLED WITH VALIDATE             
         MVC   TLKNAME,EFFS        ROUTINES                                     
         OI    LSLTIND1,LSLTIFVL   FORCE VAL OF LIST LINES                      
         B     EXITOK                                                           
*                                                                               
TSARF05  MVC   TLKOFFC,0(RF)       MOVE OFFICE TO TSAR                          
*                                                                               
T        USING OFFRECD,R5                                                       
         LA    R5,IOKEY            READ OFFICE REC TO GET NAME                  
         MVC   T.OFFKEY,BCSPACES                                                
         MVI   T.OFFKTYP,OFFKTYPQ                                               
         MVC   T.OFFKCPY,CUABIN                                                 
         MVC   T.OFFKOFF,TLKOFFC                                                
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
         LHI   R1,XOGET+XOACCMST+XIO2                                           
         GOTO1 AIO                                                              
         BNE   EXITNV                                                           
                                                                                
         L     R5,AIO2                                                          
         LA    R4,T.OFFRECD+(T.OFFRFST-T.OFFRECD)                               
TSARF10  CLI   0(R4),0                                                          
         BE    TSARF16                                                          
         CLI   0(R4),X'70'         SHORT NAME ELEMENT                           
         BE    TSARF15                                                          
         LLC   R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     TSARF10                                                          
                                                                                
         USING SNMELD,R4                                                        
TSARF15  MVC   TLKNAME,SNMNAME                                                  
         OC    TLKNAME,BCSPACES                                                 
*                                                                               
TSARF16  LLC   R1,NUMOFF           DECREMENT # OF OFFICES TO PROCESS            
         BCTR  R1,0                                                             
         STC   R1,NUMOFF                                                        
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 LA    R0,OFFTAB           CLEAR SAVED OFFICES TABLE                    
         LA    R1,L'OFFTAB                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LA    R0,BLDOFFC          CLEAR TABLE TO BUILD OFFICES                 
         LA    R1,L'BLDOFFC                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE A            
         BNE   UPDFRST2            A MAIN ACTION OF CHANGE                      
*                                                                               
         TM    OFFRSTAT,OFFSLIST                                                
         BZ    UPDFRST2                                                         
*                                                                               
         GOTO1 DELPAS,BOPARM,OFFRECD    DELETE OLIST POINTERS                   
*                                                                               
UPDFRST2 MVI   SVLEN,0             CLEAR DISPL INTO TABLE                       
         XC    SVOFIINC,SVOFIINC   CLEAR # OF LISTS THIS OFFICE IS IN           
         CLI   CSACT,A#ADD         ACTION ADD?                                  
         BE    EXITOK              THEN NO ELEMS TO FIND                        
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('OFIELQ',OFFRECD),0               
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING OFIELD,R4                                                        
         L     R4,12(R1)                                                        
         MVC   SVOFIINC,OFIINC     SAVE # OF LISTS THIS OFFICE IS IN            
         MVC   BCBYTE1,OFISTAT     SAVE OFFICE STATUS BYTE                      
         GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('OFIELQ',OFFRECD),0               
         TM    BCBYTE1,X'40'       OFFICE LIST?                                 
         BZ    EXITOK              THEN NO D2 ELEMENT TO GET                    
*                                                                               
         LA    R0,OFFTAB           BUILD OFFICE TABLE FROM SAVED OFFICE         
         LA    R1,L'OFFTAB                                                      
         LA    RE,SAVOFLST                                                      
         LA    RF,L'SAVOFLST                                                    
         MVCL  R0,RE                                                            
*                                                                               
         LA    R5,OFFRFST          FIRST ELEMENT OF OFFRECD                     
*                                                                               
         USING OFLELD,R5                                                        
UPDFRST3 CLI   OFLEL,0             RECORD END?                                  
         BE    UPDFRST5            YES, CONTINUE                                
         CLI   OFLEL,OFLELQ        NO, OFLEL FOUND?                             
         BNE   UPDFRST4            NO, GET NEXT ELEMENT                         
         MVI   OFLEL,X'FF'         YES, MARK OFLEL TO DELETE                    
*                                                                               
UPDFRST4 LLC   R1,OFLLN            GET NEXT ELEMENT                             
         AR    R5,R1                                                            
         J     UPDFRST3                                                         
*                                                                               
UPDFRST5 GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',AIOREC),0,0                 
         B     EXITOK              DELETE ALL OFLEL ELEMENTS MARKED             
         DROP  R4,R5                                                            
         SPACE 2                                                                
***********************************************************************         
* UPDATE DIRECTORY FROM TSAR RECORD 1                                 *         
* P3 = A (DIRECTORY RECORD)                                           *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDDIR1  B     EXITOK              EXIT                                         
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1                                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING OFLELD,R2                                                        
         USING TLSTD,R3                                                         
UPDREC1  LM    R2,R3,SVPARMS3                                                   
         LA    R4,BLDOFFC          R4 POINTS TO ELEM WORK AREA                  
         LLC   R1,SVLEN                                                         
         SLA   R1,1                                                             
         AR    R4,R1               R3 POINTS TO NEXT OPEN SPOT                  
         CLC   TLKOFFC,EFFS        IF THIS IS THE DUMMY ENTRY                   
         BNE   *+8                 THEN DON'T ADD TO ELEM                       
         B     EXITOK                                                           
         MVC   0(2,R4),TLKOFFC     MOVE IN OFFICE TO BLDOFFC                    
         LLC   R1,SVLEN                                                         
         AHI   R1,1                UPDATE THE COUNT                             
         STC   R1,SVLEN                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING OFFRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDLAST1 LM    R2,R3,SVPARMS3                                                   
*                                                                               
*&&DO                                                                           
* WANT TO FIND A BETTER WAY TO DO THIS INSTEAD OF READING ALL THE               
* RECORDS MYSELF                                                                
         CLI   CSACT,A#ADD         ACTION ADD?                                  
         BNE   UPDL02                                                           
         BAS   RE,CHKLIMIT         CAN ONLY HAVE 255 OFFICES                    
         BE    UPDL02                                                           
         MVC   FVMSGNO,=AL2(AE$MAXRC)                                           
         B     EXITL                                                            
*&&                                                                             
*                                                                               
UPDL02   BAS   RE,CHKADDS          MUST UPD OFFICE REC W/ NEW OFFICES           
         BAS   RE,CHKDELS          MUST UPD OFFICE REC W/ DEL OFFICES           
*                                                                               
         LA    R0,BLDOFFC          NOT AN OFFICE LIST?                          
         LA    R1,L'BLDOFFC        THEN DON'T ADD DE ELEMENT                    
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE                                                            
         BNE   UPDL05                                                           
*                                                                               
         CLI   LISTTYPE,C'Y'       IF IT'S DEFINED AS A LIST THAN MUST          
         BNE   UPDL10              ENTER AT LEAST ONE OFFICE                    
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA                                                          
         ST    R1,BOCURSOR                                                      
         B     EXITNO                                                           
*                                                                               
UPDL05   LA    R5,BLDOFFC          POINT TO NEXT AVAILABLE OFFICE CODE          
         ST    R5,ABLDOFFC         IN BLDOFFC                                   
         MVI   SEQNUM,0            INIT SEQNUM TO ZERO                          
                                                                                
*                                                                               
         USING OFLELD,R4           BUILD OFLEL ELEMENT                          
UPDL06   CLI   SVLEN,0             OFFICE CODES LEFT?                           
         BE    UPDL10              NO,CONTINUE                                  
         LA    R4,BOELEM           YES, BUILD OFLEL ELEMENT                     
         XC    BOELEM(OFLLN1Q),BOELEM                                           
         MVI   OFLEL,OFLELQ                                                     
         LLC   R1,SEQNUM           GET SEQ NUM                                  
         AHI   R1,1                                                             
         STC   R1,SEQNUM                                                        
         MVC   OFLSEQ,SEQNUM       SET OFLEL ELEMENT SEQUENCE NUMBER            
         LLC   R1,SVLEN            GET TOT NO. OFF STORED IN BLDOFFC            
         SLA   R1,1                R1 # NO OF BYTES IN BLDOFFC                  
         CHI   R1,MAXOFF           IF MORE THAN 240 (120 OFFICES)               
         BNH   *+8                 YES                                          
         LA    R1,MAXOFF           STORE FIRST 120 OFFICES IN OFLEL             
         BCTR  R1,0                                                             
         EXMVC R1,OFLNTRY,0(R5)                                                 
         LA    R1,1(R1)            RESET TOT NO OF BYTES                        
         AR    R5,R1               POINT TO NEXT AVAILABLE OFFICE CODE          
         ST    R5,ABLDOFFC         IN BLDOFFC                                   
         LA    RE,OFLLN1Q          GET OVERHEAD                                 
         AR    RE,R1               ADD TOT NO OFF BYTES STORED IN OFLEL         
         STC   RE,OFLLN                                                         
         LLC   RE,SVLEN            CALCULATE NO. OF OFFICES TO BE               
         SRA   R1,1                STORED IN NEXT OFLEL AND STORE IT IN         
         SR    RE,R1               SVLEN AND                                    
         STC   RE,SVLEN            ADD ELLEMENT TO OFFRECD                      
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,BOELEM,0                    
         J     UPDL06              CHECK FOR NEXT OFLEL ELEMENT                 
*                                                                               
         USING OFIELD,R4                                                        
UPDL10   LA    R4,BOELEM                                                        
         XC    BOELEM(OFILNQ),BOELEM                                            
         MVI   OFIEL,OFIELQ                                                     
         MVI   OFILN,OFILNQ                                                     
*                                                                               
         LA    R0,BLDOFFC                                                       
         LA    R1,L'BLDOFFC                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE               NO OFFICES IN LIST?                          
         BE    *+8                 THEN NOT AN OFFICE LIST                      
         OI    OFISTAT,OFISLIST                                                 
         MVC   OFIINC,SVOFIINC                                                  
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,BOELEM,0                    
*                                                                               
T        USING OFFKSTA,GSRECSTA                                                 
         NI    T.OFFKSTAT,X'FF'-OFFSLIST                                        
         LA    R0,BLDOFFC                                                       
         LA    R1,L'BLDOFFC                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE               NO OFFICES IN LIST?                          
         BE    *+8                 THEN NOT AN OFFICE LIST                      
         OI    T.OFFKSTAT,OFFSLIST   TURN BIT ON IN STATUS OF KEY               
*                                                                               
UPDL20   TM    BCCPYST6,CPYSRAPP   TEST FOR RECORD ACTIVITY POINTERS            
         BZ    EXITOK              NO                                           
*                                                                               
         LA    R0,BLDOFFC                                                       
         LA    R1,L'BLDOFFC                                                     
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         CLCL  R0,RE               IF NOT AN OFFICE LIST REC                    
         BE    EXITOK              THEN NO POINTERS TO ADD                      
*                                                                               
         GOTO1 ADDACTEL,AIOREC     CAN'T ADD PTRS IN RECFRST MODE B/C           
         B     EXITOK              DON'T KNOW IF OFFICE OR OFFICE LIST          
         SPACE 2                   YET.                                         
         DROP  T                                                                
         EJECT                                                                  
***********************************************************************         
* MAKE SURE FOR OFFICE LISTS THAT AT LEAST ONE OFFICE HAS BEEN ENTERED*         
* ON EXIT - EQUAL MEANS AN OFFICE FOUND                                         
*           LOW MEANS NO OFFICE FOUND ON FIRST PAGE OF SCREEN                   
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R1                                                           
CHKLIST  NTR1  BASE=*,LABEL=*                                                   
         SR    R1,R1                                                            
         LH    R1,LS1STLIN                                                      
         A     R1,ATWA            R1 POINTS TO FIRST OFFICE FIELD               
         LA    RF,MAXOPAGE        RF CONTAINS MAX NUMBER OF OFFICES             
CHKLI10  OC    FHDA(L'OFFKOFF),FHDA   ANY OFFICE?                               
         BNZ   CHKLIE                 YES SO EQUAL                              
         LLC   RE,FHLN            BUMP PAST THIS OFFICE FIELD                   
         AR    R1,RE                                                            
         LLC   RE,FHLN            AND BUMP PAST THE NAME FIELD TO GET           
         AR    R1,RE              TO NEXT OFFICE FIELD                          
         BCT   RF,CHKLI10                                                       
*                                                                               
CHKLIL   CLI   *,FF                                                             
         B     CHKLIX                                                           
CHKLIE   CR    RB,RB                                                            
CHKLIX   XIT1                                                                   
***********************************************************************         
* UPDATE OFFICE LIST PASSIVES                                         *         
*                                                                     *         
* NTRY - P1  = OFFICE RECORD                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING OFFRECD,R2                                                       
ADDPAS   NTR1  ,                                                                
         L     R2,0(R1)                                                         
*                                                                               
         MVC   IOKEY,OFFKEY        GET IODA FOR PADDLE                          
         LHI   R1,XOREAD+XOACCDIR+XIO2 READ OFFICE RECORD FOR DISKADDRS         
         GOTO1 AIO                                                              
         JE    ADDPAS10                                                         
         DC    H'0'                                                             
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
ADDPAS10 XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'A',(R2)),(C'A',T.CPTRBLK),IODA,0,ACOM          
*                                                                               
ADDPASX  XIT1                                                                   
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* DELETE OFFICE LIST PASSIVES                                         *         
*                                                                     *         
* NTRY - P1  = PID LIST ELEMENT                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING OFFRECD,R2                                                       
DELPAS   NTR1  ,                                                                
*                                                                               
         L     R2,0(R1)            GET RECORD ADDRESS                           
T        USING CPTRBLK,CPTRWRK                                                  
         XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'D',(R2)),(C'K',T.CPTRBLK),0,0,ACOM             
*                                                                               
DELPASX  XIT1                                                                   
*                                                                               
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
EFFS     DC    12X'FF'                                                          
ZERO     EQU   X'00'                                                            
*                                                                               
DCLIST   DS    0D                                                               
         DCDDL AC#OFFC,6,L                                                      
         DCDDL AC#LIST,4,L                                                      
DCLISTX  DC    X'00'                                                            
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
*ACFILWORK                                                                      
*DDSCANBLKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
       ++INCLUDE DDSCANBLKD                                                     
RAPPERD  DSECT                                                                  
       ++INCLUDE ACRAPPERD                                                      
         PRINT ON                                                               
***********************************************************************         
* DSECT TO COVER SAVED STORAGE @ TWUSER                                         
***********************************************************************         
         SPACE                                                                  
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
SAVEVALS EQU   *                                                                
AOFFICE  DS    A                   A(OFFICE FIELD)                              
ATYPADDR DS    A                   A(TYPE FIELD)                                
ADDR     DS    A                                                                
RECCOUNT DS    PL4                                                              
STDISP   DS    H                   START DISPLACEMENT INTO TABLE                
RECDSP   DS    H                                                                
ELEMDSP  DS    H                                                                
OFFTAB   DS    CL(480)             SAVED OFFICE CODE ELEM TABLE                 
BLDOFFC  DS    CL480               BUILD ELEMENT WORK AREA                      
*                                                                               
SVOFF    DS    CL2                                                              
SVCODE   DS    CL2                 SAVED OFFICE CODE                            
SVOFIINC DS    XL2                 SAVED # OF LISTS THIS OFFICE IS IN           
SVLEN    DS    XL1                                                              
NUMOFF   DS    XL1                 NUMBER OF OFFICES IN THIS LIST               
NUMOFFSV DS    XL1                                                              
NUMOFSV1 DS    XL1                                                              
LISTTYPE DS    CL1                                                              
TOTOFF   DS    XL1                                                              
SEQNUM   DS    XL1                                                              
SAVERF   DS    F                                                                
ABLDOFFC DS    A                                                                
ASVOFLST DS    A                                                                
SAVOFLST DS    CL480                                                            
*                                                                               
BIT      DS    XL1                                                              
FIRSTEL  EQU   X'80'               FIRST D2 ELEMENT FOUND                       
ADDONE   EQU   X'40'               ADD ONE TO OFFIINC IN OFFICE REC             
SWAPPED  EQU   X'20'               SWITCHED 2 OFFICE CODES FOR SORTING          
NOTLIST  EQU   X'10'               THIS REC IS NOT AN OFFICE LIST REC           
OFFLIST  EQU   X'08'               THIS REC IS AN OFFICE LIST                   
OFFCSET  EQU   X'02'               AT LEAST 1 OFFICE WAS ADDED TO LIST          
*                                                                               
DSLIST   DS    0D                  KEEP HERE IN THIS STORAGE                    
AC@OFFC  DS    CL6                 OFFICE                                       
AC@LIST  DS    CL4                 LIST                                         
*                                                                               
MAXOFF   EQU   240                 240 OFFICE MAX IN LIST                       
MAXOPAGE EQU   52                  MAX # OF OFFICE ON ONE PAGE                  
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
SVADDR   DS    A                   SAVED ADDRESS                                
SAVRB    DS    F                                                                
CPTRWRK  DS    XL128                                                            
         SPACE 2                                                                
         SPACE 1                                                                
OVERWRKN EQU   *-OVERWRKD                                                       
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKOFFC  DS    XL2                                                              
         ORG   TLUSER                                                           
TLKNAME  DS    XL12                                                             
TLLNQ    EQU   *-TLSTD                                                          
         EJECT ,                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'008ACFIL10   12/04/19'                                      
         END                                                                    
