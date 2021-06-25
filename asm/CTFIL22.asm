*          DATA SET CTFIL22    AT LEVEL 013 AS OF 08/22/00                      
*&&      SET   NOP=N                                                            
*PHASE TA1322A                                                                  
FIL22    TITLE 'DARE ASSIST RECORDS'                                            
FIL22    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,CTFIL22*,R6,R7,RR=RE                                           
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
INIT     B     EXITOK                                                           
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
         USING DASRECD,R2                                                       
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
KFKVAL   XC    DASKEY,DASKEY                                                    
         MVI   DASKTYP,DASKTYPQ    SET ASSIST RECORD                            
         MVC   DASKCPY,CUAALF                                                   
         GOTO1 VGETFACT,BOPARM,(X'80',DASKINIT),F#TSYM                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    DASKEY,DASKEY                                                    
         MVI   DASKTYP,DASKTYPQ    SET ASSIST RECORD                            
         MVC   DASKCPY,CUAALF                                                   
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
         USING DASRECD,R2                                                       
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
         USING DASRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(00200),AL4(INIDTA)       INITIALS                            
         DC    AL2(00201),AL4(IN1DTA)                                           
         DC    AL2(00202),AL4(IN2DTA)                                           
         DC    AL2(00203),AL4(IN3DTA)                                           
         DC    AL2(00204),AL4(IN4DTA)                                           
         DC    AL2(00205),AL4(IN5DTA)                                           
         DC    AL2(00206),AL4(IN6DTA)                                           
         DC    AL2(00207),AL4(IN7DTA)                                           
         DC    AL2(00208),AL4(IN8DTA)                                           
         DC    AL2(00209),AL4(IN9DTA)                                           
         DC    AL2(00210),AL4(INADTA)                                           
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL22    CSECT                                                                  
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
DFDDIS   XC    INITBLK,INITBLK     CLEAR BUILD BLOCK FOR INITIALS               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('DASELQ',DASRECD),0               
         CLI   12(R1),0                                                         
         BNE   EXITOK                                                           
*                                                                               
         L     R3,12(R1)                                                        
         USING DASELD,R3                                                        
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,DASLN                                                       
         AHI   RF,-(DASINITF-DASELD+1)                                          
         BM    EXITOK                                                           
*                                                                               
         EX    RF,*+4                                                           
         MVC   INITBLK(0),DASINITF                                              
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   XC    INITBLK,INITBLK     CLEAR BUILD BLOCK FOR INITIALS               
         B     EXITOK                                                           
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
DLDVAL   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('DASELQ',DASRECD),0               
*                                                                               
         XC    BOELEM,BOELEM                                                    
TMP      USING DASELD,BOELEM                                                    
         MVI   TMP.DASEL,DASELQ                                                 
*                                                                               
         BAS   RE,SORTIT                                                        
*                                                                               
         LA    RF,SORTBLK                                                       
         LA    R1,TMP.DASINITF                                                  
         LA    R0,DASINIT#                                                      
         XR    RE,RE                                                            
*                                                                               
DLDV02   CLC   0(DASINITL,RF),BCSPACES                                          
         BNH   DLDV04                                                           
*                                                                               
         MVC   0(DASINITL,R1),0(RF)                                             
         AHI   R1,DASINITL                                                      
         AHI   RE,DASINITL                                                      
*                                                                               
DLDV04   AHI   RF,DASINITL                                                      
         BCT   R0,DLDV02                                                        
*                                                                               
         AHI   RE,DASINITF-DASELD                                               
         STC   RE,TMP.DASLN                                                     
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),DASRECD,TMP.DASELD                 
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO SORT OUTPUT BLOCK                                        *         
***********************************************************************         
         SPACE 1                                                                
SORTIT   NTR1  ,                                                                
         LA    RF,INITBLK                                                       
         LA    R0,INITBLKL                                                      
*                                                                               
         CLI   0(RF),C' '          SET ALL FUNNIES TO SPACES                    
         BH    *+8                                                              
         MVI   0(RF),C' '                                                       
         BCT   R0,*-12                                                          
*                                                                               
         XC    SORTBLK,SORTBLK     CLEAR SORT BLOCK                             
         LA    RF,INITBLK                                                       
         LA    R1,SORTBLK                                                       
         LA    R0,DASINIT#                                                      
         XR    RE,RE                                                            
*                                                                               
SORT02   CLC   0(DASINITL,RF),BCSPACES                                          
         BE    SORT04              EMPTY SLOT                                   
*                                                                               
         MVC   0(DASINITL,R1),0(RF)                                             
         AHI   R1,DASINITL         MOVE INTO SORT BLOCK AND UP COUNTER          
         LA    RE,1(RE)                                                         
*                                                                               
SORT04   AHI   RF,DASINITL                                                      
         BCT   R0,SORT02                                                        
*                                                                               
         LTR   RF,RE               ANY DATA TO SORT?                            
         BZ    EXITOK              NO                                           
*                                                                               
         LR    R0,RE                                                            
         LA    R1,SORTBLK          SORT BLOCK ASCENDING                         
*                                                                               
SORT06   LR    RE,RF                                                            
         AHI   RE,-1                                                            
         BNP   SORT14                                                           
         LA    R4,DASINITL(R1)                                                  
*                                                                               
SORT08   CLC   0(DASINITL,R4),BCSPACES                                          
         BNH   SORT12                                                           
         CLC   0(DASINITL,R1),0(R4)                                             
         BL    SORT12              ALREADY SORTED                               
         BH    SORT10                                                           
*                                                                               
         AHI   R0,-1               REMOVE DUPLICATE                             
         MVC   0(DASINITL,R4),FFS                                               
         B     SORT12              ALREADY SORTED                               
*                                                                               
SORT10   XC    0(DASINITL,R1),0(R4)                                             
         XC    0(DASINITL,R4),0(R1)                                             
         XC    0(DASINITL,R1),0(R4)                                             
*                                                                               
SORT12   AHI   R4,DASINITL                                                      
         BCT   RE,SORT08                                                        
*                                                                               
         AHI   R1,DASINITL                                                      
         BCT   RF,SORT06                                                        
*                                                                               
SORT14   LA    RF,DASINIT#         REMOVE ANY DUPLICATES                        
         LA    R1,SORTBLK                                                       
*                                                                               
SORT16   CLC   0(DASINITL,R1),BCSPACES                                          
         BH    *+10                                                             
         MVC   0(DASINITL,R1),BCSPACES                                          
*                                                                               
         CLC   0(DASINITL,R1),FFS                                               
         BNE   *+10                                                             
         MVC   0(DASINITL,R1),BCSPACES                                          
*                                                                               
         AHI   R1,DASINITL                                                      
         BCT   RF,SORT16                                                        
         B     EXITOK                                                           
***********************************************************************         
* DATA OBJECT FOR INITIALS                                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
INIDTA   LA    RF,INITBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
INITBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISINI)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DISDFLT)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALINI)                                 
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTINI)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTINI)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTINI)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISDFLT  GOTO1 VGETFACT,BOPARM,(X'80',FVIFLD),F#TSYM                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
DISINI   MVC   FVIFLD(L'DASKINIT),DASKINIT                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
VALINI   MVC   DASKINIT,FVIFLD                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS FILTER FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DFLTINI  MVC   FVIFLD(L'DASKINIT),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS FILTER FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
VFLTINI  MVC   DASKINIT,FVIFLD                                                  
         MVC   FLTIFLD(L'DASKINIT),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR SYSTEM                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFTINI  CLC   DASKINIT,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 1                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN1DTA   LA    RF,IN1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 1                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN1   LA    RF,1                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 1                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN1   LA    RF,1                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 2                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN2DTA   LA    RF,IN2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 2                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN2   LA    RF,2                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 2                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN2   LA    RF,2                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 3                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN3DTA   LA    RF,IN3TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN3TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN3)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN3)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 3                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN3   LA    RF,3                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 3                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN3   LA    RF,3                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 4                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN4DTA   LA    RF,IN4TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN4TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN4)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN4)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 4                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN4   LA    RF,4                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 4                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN4   LA    RF,4                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 5                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN5DTA   LA    RF,IN5TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN5TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN5)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN5)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 5                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN5   LA    RF,5                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 5                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN5   LA    RF,5                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 6                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN6DTA   LA    RF,IN6TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN6TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN6)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN6)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 6                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN6   LA    RF,6                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 1                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN6   LA    RF,6                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 7                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN7DTA   LA    RF,IN7TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN7TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN7)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN7)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 7                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN7   LA    RF,7                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 7                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN7   LA    RF,7                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 8                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN8DTA   LA    RF,IN8TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN8TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN8)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN8)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 8                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN8   LA    RF,8                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 8                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN8   LA    RF,8                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 9                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
IN9DTA   LA    RF,IN9TBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
IN9TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISIN9)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALIN9)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 9                                            *         
***********************************************************************         
         SPACE 1                                                                
DISIN9   LA    RF,9                INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 9                                           *         
***********************************************************************         
         SPACE 1                                                                
VALIN9   LA    RF,9                                                             
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INITIALS VALUE 10                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
INADTA   LA    RF,INATBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
INATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISINA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALINA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY INITIALS VALUE 10                                           *         
***********************************************************************         
         SPACE 1                                                                
DISINA   LA    RF,10               INITIALS VALUE                               
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   FVIFLD(DASINITL),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE INITIALS VALUE 10                                          *         
***********************************************************************         
         SPACE 1                                                                
VALINA   LA    RF,10                                                            
         BCTR  RF,0                                                             
         MHI   RF,DASINITL                                                      
         LA    RF,INITBLK(RF)                                                   
         MVC   0(DASINITL,RF),FVIFLD                                            
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
         DROP  R2                                                               
         SPACE 1                                                                
LIST     LM    R0,R2,SVPARMS                                                    
THIS     USING DASRECD,R2                                                       
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
FLST     MVC   IOKEY(L'DASKEY),THIS.DASRECD                                     
         ICM   R1,15,=AL4(XIO11+XOGENDIR+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               FUCK UP ON THE READ HIGH                     
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     ICM   R1,15,=AL4(XIO11+XOSEQ+XOGENDIR)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   CLC   IOKEY(DASKSPR-DASRECD),THIS.DASRECD                              
         BNE   EXITL                                                            
*                                                                               
         MVC   THIS.DASKEY(DASKLEN),IOKEY   WE WANT THIS KEY HERE...            
         B     EXITOK                                                           
         DROP  THIS                                                             
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
FFS      DC    16X'FF'                                                          
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
INITBLK  DS    XL((DASINITL)*(DASINIT#))                                        
INITBLKL EQU   *-INITBLK                                                        
SORTBLK  DS    XL((DASINITL)*(DASINIT#))                                        
         EJECT                                                                  
***********************************************************************         
* OTHER DSECTS                                                        *         
***********************************************************************         
         SPACE 1                                                                
*        CTFILWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE CTFILWORK                                                      
         PRINT ON                                                               
*        GEGENASS                                                               
         PRINT OFF                                                              
       ++INCLUDE GEGENASS                                                       
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
**PAN#1  DC    CL21'013CTFIL22   08/22/00'                                      
         END                                                                    
