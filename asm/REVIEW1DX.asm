*          DATA SET REVIEW1DX  AT LEVEL 004 AS OF 08/31/00                      
*          DATA SET REVIEW1DX  AT LEVEL 003 AS OF 12/12/96                      
*PHASE T8171DA                                                                  
VIEW1D   TITLE 'KD REPORT FOR VIEWER'                                           
VIEW1D   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REVIEW1D,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         LH    R6,=Y(TWSAVE-TWAD)                                               
         A     R6,ATWA                                                          
         USING MYSAVED,R6                                                       
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
ROUTS    B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
***********************************************************************         
* TABLE  ITERATING ROUTINE - R1=EQUATED VERB                          *         
*                          - RF=A(TABLE)                              *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
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
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                        *         
***********************************************************************         
         SPACE 1                                                                
PFKEY    LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKTABL                                                       
         B     ITER                                                             
*                                                                               
PFKTABL  DC    AL1(PFREC),AL1(0,0,0),AL4(PPFREC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* RECORD NAME FOR PFKEY                                               *         
***********************************************************************         
         SPACE 1                                                                
PPFREC   OI    SVPARMS3,X'80'      SUPPRESS RECORD NAME                         
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
KEYFRST  L     R1,SVPARMS4         EQUATED VERB                                 
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
KFTABL   DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
KFKFVAL  MVC   REKDRPT,=C'KD'                                                   
         MVC   REKDREP,CUAALF                                                   
         B     EXITOK                                                           
         DROP  R2                                                               
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
DATA     ICM   R1,15,SVPARMS2      RE HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING FSRRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
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
         USING FSRRECD,R2          R2 HOLDS A(RECORD)                           
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00001),AL4(GRPDTA)       GROUP                               
         DC    AL2(00002),AL4(STADTA)       STATION                             
         DC    AL2(00003),AL4(OFFDTA)       OFFICE                              
         DC    AL2(00004),AL4(QTRDTA)       QUARTER                             
         DC    AL2(00005),AL4(RNKDTA)       MAX RANK                            
         DC    AL2(00006),AL4(OTHDTA)       OTHER - DOES NOTHING!!              
         DC    AL2(00007),AL4(LINDTA)       DISPLAY LINE                        
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
VIEW1D   CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR GROUP                                               *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
GRPDTA   LA    RF,GRPTBL                                                        
         B     ITER                                                             
*                                                                               
GRPTBL   DC    AL1(DFDIS),AL1(0,0,0),AL4(DISGRP)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALGRP)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFGRP)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETGRP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET FILTER FIELD                                                    *         
***********************************************************************         
         SPACE 1                                                                
SETGRP   B     FLTXX                                                            
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A GROUP FILTER                                              *         
***********************************************************************         
         SPACE 1                                                                
DISGRP   MVC   FVIFLD(L'REKDGRP),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A GROUP FILTER                                             *         
***********************************************************************         
         SPACE 1                                                                
VALGRP   GOTOX AVALGRP                                                          
         BNE   EXITL               ERROR VALIDATING GROUP                       
         MVC   REKDGRP,BOWORK1                                                  
         MVC   FLTIFLD(L'REKDGRP),BOWORK1                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO GROUP FILTERING                                                  *         
***********************************************************************         
         SPACE 1                                                                
DOFGRP   CLC   REKDGRP,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATION                                             *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
STADTA   LA    RF,STATBL                                                        
         B     ITER                                                             
*                                                                               
STATBL   DC    AL1(DFDIS),AL1(0,0,0),AL4(DISSTA)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALSTA)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFSTA)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETSTA)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET FILTER FIELD PROTECTION                                         *         
***********************************************************************         
         SPACE 1                                                                
SETSTA   B     FLTXX                                                            
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A STATION FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISSTA   GOTOX ADISSTN,BOPARM,FLTIFLD                                           
         MVC   FVIFLD(7),BOWORK1                                                
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A STATION FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALSTA   GOTOX AVALSTN                                                          
         BL    EXITL                                                            
         MVC   REKDSTN,BOWORK1                                                  
         MVC   FLTIFLD(L'REKDSTN),BOWORK1                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO STATION FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFSTA   CLC   REKDSTN,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OFFICE                                              *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
OFFDTA   LA    RF,OFFTBL                                                        
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DFDIS),AL1(0,0,0),AL4(DISOFF)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFF)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETOFF)                                 
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFLTOFF)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET FIELD PROTECTION                                                *         
***********************************************************************         
         SPACE 1                                                                
SETOFF   B     FLTXX                                                            
         SPACE 1                                                                
***********************************************************************         
* DISPLAY OFFICE DEFAULT                                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTOFF  CLC   =C'O=',TWAACCS      OFFICE ACCESS LIMITATION                     
         BNE   EXITOK                                                           
         TM    TWAAUTH,X'80'       OVERRIDE AUTHORISATION                       
         BO    EXITOK                                                           
         MVC   FVIFLD(2),TWAACCS+2 SET DEFAULT                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A OFFICE FILTER                                             *         
***********************************************************************         
         SPACE 1                                                                
DISOFF   GOTOX ADISOFF                                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A OFFICE FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
VALOFF   GOTOX AVALOFF                                                          
         BNE   EXITL                                                            
         MVC   REKDOFF,BOWORK1                                                  
         MVC   FLTIFLD(L'REKDOFF),BOWORK1                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO OFFICE FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFOFF   CLC   REKDOFF,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR QUARTER                                             *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
QTRDTA   LA    RF,QTRTBL                                                        
         B     ITER                                                             
*                                                                               
QTRTBL   DC    AL1(DFDIS),AL1(0,0,0),AL4(DISQTR)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALQTR)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFQTR)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETQTR)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET FIELD PROTECTION                                                *         
***********************************************************************         
         SPACE 1                                                                
SETQTR   B     FLTXX                                                            
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A QUARTER FILTER                                            *         
***********************************************************************         
         SPACE 1                                                                
DISQTR   LA    RF,QTRTAB                                                        
*                                                                               
DQTR02   CLI   0(RF),0                                                          
         BE    EXITOK                                                           
         CLC   1(2,RF),FLTIFLD                                                  
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     DQTR02                                                           
*                                                                               
         MVC   FVIFLD(1),0(RF)                                                  
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A QUARTER FILTER                                           *         
***********************************************************************         
         SPACE 1                                                                
VALQTR   CLI   FVILEN,3            ALL OR A SINGLE NUMBER                       
         BNE   VQTR02                                                           
         CLC   =C'ALL',FVIFLD                                                   
         BNE   EXITNV                                                           
         MVC   FLTIFLD(2),=XL2'0D01'                                            
         B     EXITOK                                                           
*                                                                               
VQTR02   CLI   FVILEN,1            QUARTER MODIFIER?                            
         BNE   EXITNV              NO                                           
         LA    RF,QTRTAB                                                        
*                                                                               
VQTR04   CLI   0(RF),0                                                          
         BE    EXITNV                                                           
         CLC   FVIFLD(1),0(RF)                                                  
         BE    *+12                                                             
         LA    RF,3(RF)                                                         
         B     VQTR04                                                           
*                                                                               
         MVC   FLTIFLD(2),1(RF)                                                 
         B     EXITOK                                                           
*                                                                               
QTRTAB   DC    C'1',XL2'0301'                                                   
         DC    C'2',XL2'0601'                                                   
         DC    C'3',XL2'0901'                                                   
         DC    C'4',XL2'0C01'                                                   
         DC    X'00'                                                            
         SPACE 2                                                                
***********************************************************************         
* DO QUARTER FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFQTR   OC    REKDTOT,REKDTOT                                                  
         BZ    FLTXX               NOT A TOTAL LINE                             
*                                                                               
         CLC   REKDPRD+1(1),FLTIFLD                                             
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR MAX RANK                                            *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
RNKDTA   LA    RF,RNKTBL                                                        
         B     ITER                                                             
*                                                                               
RNKTBL   DC    AL1(DFDIS),AL1(0,0,0),AL4(DISRNK)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALRNK)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFRNK)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETRNK)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET FIELD PROTECTION                                                *         
***********************************************************************         
         SPACE 1                                                                
SETRNK   B     FLTXX                                                            
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A RANK FILTER                                               *         
***********************************************************************         
         SPACE 1                                                                
DISRNK   ICM   RF,15,FLTIFLD                                                    
         CURED (RF),(4,FVIFLD),0,DMCB=BOPARM,ALIGN=LEFT                         
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A RANK FILTER                                              *         
***********************************************************************         
         SPACE 1                                                                
VALRNK   TM    FVIIND,FVINUM       MUST BE NUMERIC                              
         BZ    EXITNOTN                                                         
         MVC   FLTIFLD(4),BCFULL                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO RANK FILTERING                                                   *         
***********************************************************************         
         SPACE 1                                                                
DOFRNK   CLC   REKDRNK,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OTHER FIELD                                         *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
OTHDTA   LA    RF,OTHTBL                                                        
         B     ITER                                                             
*                                                                               
OTHTBL   DC    AL1(DSET),AL1(0,0,0),AL4(SETOFF)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET OTHER PROTECTION                                                *         
***********************************************************************         
         SPACE 1                                                                
SETOTH   B     FLTXX                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLAY LINE                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LINDTA   LA    RF,LINTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
LINTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLIN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD LINE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING REKDKEY,R2                                                       
DISLIN   LH    R4,LS1STKEY         SET CURSOR OVERRIDE TO FIRST KEY             
         A     R4,ATWA             FIELD                                        
         ST    R4,BOCURSOR                                                      
         LA    R4,FVIFLD           FIRST LINE IS FILTER DETAILS                 
*                                                                               
         BAS   RE,GETDATE                                                       
         BNE   DLIN02                                                           
*                                                                               
         MVC   0(L'PERIOD,R4),PERIOD                                            
         LA    R4,L'PERIOD(R4)                                                  
*                                                                               
         PACK  BODUB1,SVFROMM      PERIOD FROM                                  
         CVB   RF,BODUB1                                                        
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,MONTHS(RF)                                                    
         MVC   0(3,R4),0(RF)                                                    
         MVI   3(R4),C'/'                                                       
         MVC   4(2,R4),SVFROMY                                                  
         MVI   6(R4),C'-'                                                       
         LA    R4,7(R4)                                                         
*                                                                               
         PACK  BODUB1,SVTOM        PERIOD TO                                    
         CVB   RF,BODUB1                                                        
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,MONTHS(RF)                                                    
         MVC   0(3,R4),0(RF)                                                    
         MVI   3(R4),C'/'                                                       
         MVC   4(2,R4),SVTOY                                                    
         MVI   6(R4),C','                                                       
         LA    R4,7(R4)                                                         
*                                                                               
         MVC   0(L'RUNON,R4),RUNON                                              
         LA    R4,L'RUNON(R4)                                                   
         MVC   0(L'SVWHEND,R4),SVWHEND                                          
         LA    R4,L'SVWHEND+1(R4)                                               
         PACK  BODUB1,SVWHENM      RUN DATE                                     
         CVB   RF,BODUB1                                                        
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,MONTHS(RF)                                                    
         MVC   0(3,R4),0(RF)                                                    
         MVI   3(R4),C'/'                                                       
         MVC   4(2,R4),SVWHENY                                                  
         MVI   7(R4),C','                                                       
         LA    R4,7(R4)                                                         
*                              *** SET 'GROUP=XX' OR 'GROUP=ALL'                
DLIN02   MVC   0(L'GROUP,R4),GROUP                                              
         MVI   L'GROUP(R4),C'='                                                 
         LA    R4,L'GROUP+1(R4)                                                 
         MVC   0(L'REKDGRP,R4),REKDGRP                                          
         CLC   REKDGRP,BCSPACES   IS THIS FOR ALL GROUPS?                       
         BH    DLIN04             NO                                            
         MVC   0(L'ALL,R4),ALL                                                  
         LA    R4,L'ALL(R4)                                                     
         B     *+8                                                              
DLIN04   LA    R4,L'REKDGRP(R4)                                                 
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                               ** STATION FILTER                               
         MVC   0(L'STATION,R4),STATION                                          
         MVI   L'STATION(R4),C'='                                               
         LA    R4,L'STATION+1(R4)                                               
         GOTOX ADISSTN,BOPARM,REKDSTN                                           
         MVC   0(7,R4),BOWORK1                                                  
         LA    R4,7(R4)                                                         
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                               ** OFFICE FILTER                                
         MVC   0(L'OFFICE,R4),OFFICE                                            
         MVI   L'OFFICE(R4),C'='                                                
         LA    R4,L'OFFICE+1(R4)                                                
         MVC   0(L'REKDOFF,R4),REKDOFF                                          
         CLC   REKDOFF,BCEFFS     IS THIS FOR ALL OFFICES?                      
         BNE   DLIN06             NO                                            
         MVC   0(L'ALL,R4),ALL                                                  
         LA    R4,L'ALL(R4)                                                     
         B     *+8                                                              
DLIN06   LA    R4,L'REKDOFF(R4)                                                 
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                                                                               
         LH    R5,SVLINPAG         NUMBER OF LINES PER PAGE                     
         SH    R5,=H'1'            CONTROLLER HANDLES FIRST LINE                
         MVC   SVOFF,REKDOFF       SET OFFICE CODE                              
*                                                                               
         LH    R3,LSCURLIN         FIRST FIELD ON FIRST LINE                    
         AH    R3,LSLINLEN         NEXT LINE ON SCREEN                          
         A     R3,ATWA                                                          
*                                                                               
         MVC   IOKEY,REKDKEY       SET CURRENT RECORD INTO IO2                  
         ICM   R1,15,=AL4(XIO2+XOREPVEW+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BE    DLIN14              FIRST LINE DISPLAY CODE HERE                 
         DC    H'0'                READ HIGH PROBLEM                            
*                                                                               
DLIN08   L     RE,AIO2                                                          
         LH    RF,=Y(2000)         LENGTH OF AN IO AREA                         
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         ICM   R1,15,=AL4(XIO2+XOSEQ+XOREPVEW)                                  
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DLINX                                                            
*                                                                               
         L     R4,AIO2                                                          
REC      USING REKDKEY,R4                                                       
*                                                                               
         MVC   LSLASKEY,0(R4)      SAVE THIS KEY                                
         MVC   IOKEY,0(R4)         SET KEY IN IOKEY                             
         CLC   REKDRPT,REC.REKDRPT SAME REPORT?                                 
         BNE   DLINX               NO                                           
         CLC   REKDREP,REC.REKDREP SAME REP?                                    
         BNE   DLINX               NO                                           
         CLC   SVOFF,REC.REKDOFF   OFFICE PROVIDES CONTROL BREAK                
         BNE   DLINX               BREAK                                        
*                                                                               
         GOTOX AGEN,BOPARM,OFILT,FDOD,IOKEY                                     
         BL    DLIN08              FAILED DIRECTORY FILTERING                   
         GOTOX AGEN,BOPARM,OFILT,FDOR,(R4)                                      
         BL    DLIN08              FAILED RECORD FILTERING                      
*                                                                               
         AH    R3,LSLINLEN         NEXT LINE ON SCREEN                          
FLD      USING FHD,R3                                                           
L1       USING LINE1D,FLD.FHDA                                                  
*                                                                               
         L     RF,AIO1             SEE IF ADV/AGENCY CHANGED                    
         CLC   REKDADAG-REKDKEY(L'REKDADAG,RF),REC.REKDADAG                     
         BNE   DLIN10              YES                                          
*                                                                               
         CLI   FRSTLINE,C'1'       FIRST LINE ONLY DISPLAYED?                   
         BNE   DLIN20              NO                                           
         GOTOX DOAGY,BOPARM,AIO1                                                
         MVC   L1.L2AGY,BOWORK1                                                 
         MVI   FRSTLINE,C'2'       SET LINE 2 OF SERIES                         
         B     DLIN20                                                           
*                                                                               
DLIN10   CLI   FRSTLINE,C'1'       FIRST LINE ONLY DISPLAYED?                   
         BNE   DLIN14              NO                                           
         OI    FLD.FHOI,FHOITR                                                  
         L     R4,AIO1             FIRST LINE DETAILS IN AIO1                   
         CLC   REC.REKDADV,BCEFFS  ALL/ALL?                                     
         BNE   *+14                NO                                           
         CLC   REC.REKDAGY,BCEFFS  DON'T SHOW SECOND 'ALL'                      
         BE    DLIN12                                                           
         GOTOX DOAGY,BOPARM,AIO1                                                
         MVC   L1.L2AGY,BOWORK1                                                 
         AH    R3,LSLINLEN         NEXT LINE ON SCREEN                          
*                                                                               
DLIN12   MVI   FRSTLINE,C'2'       SET DISPLAYED LINE 2 INFORMATION             
         BCT   R5,DLIN14                                                        
         B     EXITOK              FINISHED FOR THIS SCREEN                     
*                                                                               
DLIN14   CH    R5,=H'2'            MUST BE 2 LINES FOR NEXT DISPLAY             
         BL    EXITOK                                                           
*                                                                               
         L     R4,AIO1                                                          
         LR    R0,R4                                                            
         L     RE,AIO2                                                          
         LH    R1,=Y(2000)         LENGTH OF AN IO AREA                         
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY FIRST LINE INTO AIO1                    
*                                                                               
*                               ** RANK                                         
         CLC   REC.REKDAGY,BCEFFS                                               
         BNE   DLIN16                                                           
         CLC   REC.REKDADV,BCEFFS                                               
         BNE   DLIN16                                                           
         CLC   REC.REKDPRD+1(1),BCEFFS                                          
         BNE   DLIN16                                                           
         B     DLIN18                                                           
*                                                                               
DLIN16   XR    RF,RF                                                            
         ICM   RF,15,REC.REKDRNK                                                
         CURED (RF),(4,L1.L1RANK),0,DMCB=BOPARM,ALIGN=LEFT                      
*                                                                               
*                               ** ADVERTISER                                   
DLIN18   GOTOX DOADV,BOPARM,AIO1                                                
         MVC   L1.L1ADV,BOWORK1                                                 
*                                                                               
         MVI   FRSTLINE,C'1'       SET DISPLAYED LINE 1                         
*                                                                               
DLIN20   GOTOX DOLINE,BOPARM,FLD.FHDA,(R4)                                      
         OI    FLD.FHOI,FHOITR                                                  
         BCT   R5,DLIN08                                                        
         B     EXITOK                                                           
*                                                                               
DLINX    CLI   FRSTLINE,C'1'       FIRST LINE OF PAIR DISPLAYED?                
         BNE   EXITOK              NO                                           
         AH    R3,LSLINLEN                                                      
         OI    FLD.FHOI,FHOITR                                                  
         L     R4,AIO1                                                          
         CLC   REC.REKDAGY,BCEFFS  DON'T SHOW SECOND 'ALL'                      
         BE    EXITOK                                                           
         GOTOX DOAGY,BOPARM,AIO1                                                
         MVC   L1.L2AGY,BOWORK1    SET AGENCY                                   
         B     EXITOK                                                           
         DROP  FLD,R2,REC                                                       
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO EXTRACT RUN DATES FOR THIS REPORT                        *         
***********************************************************************         
         SPACE 1                                                                
GETDATE  NTR1  ,                                                                
         XC    IOKEY,IOKEY                                                      
X        USING REKDKEY,IOKEY                                                    
         L     RF,AIOREC                                                        
         USING REKDKEY,RF                                                       
         MVC   X.REKDRPT,=AL2(REKDRPTQ)                                         
         MVC   X.REKDREP,CUAALF                                                 
         ICM   R1,15,=AL4(XIO3+XOREPVEW+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
         DROP  X                                                                
*                                                                               
         L     RF,AIO3                                                          
         CLC   =C'DATES=',REKDSTN                                               
         BNE   EXITL                                                            
         MVC   SVFROM,REKDSTN+6                                                 
         MVC   SVTO,REKDSTN+10                                                  
*                                                                               
         LA    R1,REKDFRST                                                      
         MVC   SVWHEN,2(R1)                                                     
         CLC   SVWHEN,BCSPACES                                                  
         BH    EXITOK                                                           
*                                                                               
         MVC   SVWHEND,ASEDAT+3                                                 
         MVC   SVWHENM,ASEDAT                                                   
         MVC   SVWHENY,ASEDAT+6                                                 
         B     EXITOK                                                           
         DROP  RF                                                               
                                                                                
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SHORT ADVERTISER INTO BOWORK1                    *         
*                                                                     *         
* NTRY P1: A(RECORD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
DOADV    NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING REKDKEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
         CLC   REKDADV,BCEFFS                                                   
         BNE   *+14                                                             
         MVC   BOWORK1(L'ALL),ALL                                               
         B     EXITOK                                                           
*                                                                               
X        USING RADVRECD,IOKEY                                                   
         MVI   X.RADVKTYP,X'08'                                                 
         MVC   X.RADVKADV,REKDADV                                               
         MVC   X.RADVKREP,CUAALF                                                
         L     R1,=AL4(XOREAD+XOREPDIR+XIO4)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK              RECORD NOT FOUND                             
         DROP  X                                                                
*                                                                               
         L     R1,=AL4(XOGET+XOREPFIL+XIO4)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         L     RF,AIO4                                                          
         USING RADVRECD,RF                                                      
         MVC   BOWORK1(L'RADVNAME),RADVNAME                                     
         B     EXITOK                                                           
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY SHORT AGENCY NAME INTO BOWORK1                   *         
*                                                                     *         
* NTRY P1: A(RECORD)                                                  *         
***********************************************************************         
         SPACE 1                                                                
DOAGY    NTR1  ,                                                                
         L     R2,0(R1)                                                         
         USING REKDKEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
         CLC   REKDAGY,BCEFFS                                                   
         BNE   *+14                                                             
         MVC   BOWORK1(L'ALL),ALL                                               
         B     EXITOK                                                           
*                                                                               
X        USING RAGYRECD,IOKEY                                                   
         MVI   X.RAGYKTYP,X'0A'                                                 
         MVC   X.RAGYKAGY,REKDAGY                                               
         MVC   X.RAGYKAOF,REKDAGY+L'RAGYKAGY                                    
         MVC   X.RAGYKREP,CUAALF                                                
         L     R1,=AL4(XOREAD+XOREPDIR+XIO4)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITOK              RECORD NOT FOUND                             
         DROP  X                                                                
*                                                                               
         L     R1,=AL4(XOGET+XOREPFIL+XIO4)                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    *+6                                                              
         DC    H'0'                DISK ERROR                                   
*                                                                               
         L     RF,AIO4                                                          
         USING RAGYRECD,RF                                                      
         MVC   BOWORK1(L'RAGYNAM1),RAGYNAM1                                     
         B     EXITOK                                                           
         DROP  R2,RF                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY RECORD INFORMATION TO A FIELD                    *         
*                                                                     *         
* NTRY P1: 78 CHARACTER FIELD TO FILL (COVERED BY LINE1D)             *         
*      P2: A(RECORD)                                                  *         
***********************************************************************         
         USING LINE1D,R3                                                        
         USING REKDKEY,R4                                                       
DOLINE   NTR1                                                                   
         LM    R3,R4,0(R1)                                                      
*                               ** PERIOD IS YYMM BINARY                        
         MVI   L1PED,C'*'                                                       
         MVC   L1PED(L'TOTAL),TOTAL                                             
         CLI   REKDPRD+1,X'0D'      IF MM IS 'OD' THIS IS A YRLY TOTAL          
         BE    DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         MVC   L1PED(L'ALL),ALL                                                 
         CLI   REKDPRD+1,FF         IF MM IS 'FF' PUT OUT 'ALL'                 
         BE    DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         MVC   L1PED(L'QTRTOT),QTRTOT                                           
         MVI   L1PED+2,C'1'                                                     
         CLI   REKDPRD+1,3                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'2'                                                     
         CLI   REKDPRD+1,6                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'3'                                                     
         CLI   REKDPRD+1,9                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'4'                                                     
*                                                                               
DOL00    OC    REKDTOT,REKDTOT      IF SET THIS IS SUB-TOTAL LINE               
         BNZ   DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         XR    RF,RF                                                            
         IC    RF,REKDPRD+1                                                     
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,MONTHS(RF)                                                    
         MVC   L1PED(3),0(RF)                                                   
         MVI   L1PED+3,C'/'                                                     
         XR    RF,RF                                                            
         IC    RF,REKDPRD                                                       
         CURED (RF),(2,L1PED+4),0,DMCB=BOPARM,ALIGN=LEFT                        
*                                                                               
DOL01    LA    RF,REKDFRST                                                      
         XR    RE,RE                                                            
*                                                                               
DOL02    CLI   0(RF),0                                                          
         BE    EXITOK                                                           
         CLI   0(RF),RDPBIELQ      PRIOR BILLING                                
         BE    DOL06                                                            
         CLI   0(RF),RDCBIELQ      CURRENT BILLING                              
         BE    DOL08                                                            
         CLI   0(RF),RDPSHELQ      PRIOR SHARE                                  
         BE    DOL10                                                            
         CLI   0(RF),RDCSHELQ      CURRENT SHARE                                
         BE    DOL12                                                            
*                                                                               
DOL04    ICM   RE,1,1(RF)                                                       
         LA    RF,0(RE,RF)                                                      
         B     DOL02                                                            
*                                                                               
         USING RDPBIEL,RF                                                       
DOL06    IC    RE,RDPBILN                                                       
         SH    RE,=Y(RDPBIDTA-RDPBIEL)                                          
         BNP   DOL04                                                            
         CLM   RE,1,=AL1(L'L1PRIFIN)                                            
         BNH   *+14                                                             
         MVC   L1PRIFIN,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1PRIFIN                                                    
         SR    R1,RE                                                            
         LA    R1,L1PRIFIN(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),RDPBIDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING RDCBIEL,RF                                                       
DOL08    IC    RE,RDCBILN                                                       
         SH    RE,=Y(RDCBIDTA-RDCBIEL)                                          
         BNP   DOL04                                                            
         CLM   RE,1,=AL1(L'L1CURFIN)                                            
         BNH   *+14                                                             
         MVC   L1CURFIN,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1CURFIN                                                    
         SR    R1,RE                                                            
         LA    R1,L1CURFIN(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),RDCBIDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING RDPSHEL,RF                                                       
DOL10    IC    RE,RDPSHLN                                                       
         SH    RE,=Y(RDPSHDTA-RDPSHEL)                                          
         BNP   DOL04                                                            
         CLM   RE,1,=AL1(L'L1PRISHR)                                            
         BNH   *+14                                                             
         MVC   L1PRISHR,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1PRISHR                                                    
         SR    R1,RE                                                            
         LA    R1,L1PRISHR(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),RDPSHDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING RDCSHEL,RF                                                       
DOL12    IC    RE,RDCSHLN                                                       
         SH    RE,=Y(RDCSHDTA-RDCSHEL)                                          
         BNP   DOL04                                                            
         CLM   RE,1,=AL1(L'L1CURSHR)                                            
         BNH   *+14                                                             
         MVC   L1CURSHR,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1CURSHR                                                    
         SR    R1,RE                                                            
         LA    R1,L1CURSHR(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),RDCSHDTA                                                 
         B     DOL04                                                            
         DROP  R3,R4,RF                                                         
         SPACE 2                                                                
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
NSSTABL  DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (BACK)                   *         
***********************************************************************         
         SPACE 1                                                                
XITOUT   OI    SNINDS1,SNIUSECR                                                 
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
LIST     LM    R0,R3,SVPARMS                                                    
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LSCRFRST),AL1(0,0,0),AL4(SCRFST)                             
         DC    AL1(LSETHEAD),AL1(0,0,0),AL4(SETHED)                             
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(BLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(BLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ILST     MVI   LSSUBLEN,0          NO SUB-ACTION FIELD                          
         OI    LSSTAT2,LSS1HEAD    SETTING MY OWN HEADLINES                     
         MVC   LSNUMHED,=AL2(1)    ONE HEADLINE FIELD                           
         XC    SVLINPAG,SVLINPAG   RESET THIS                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET LIST HEADLINE                                                   *         
***********************************************************************         
         SPACE 1                                                                
SETHED   ICM   RF,15,SVPARMS3                                                   
         USING FHD,RF                                                           
         USING LINE1D,FHDA                                                      
         XR    R1,R1                                                            
         IC    R1,FHLN                                                          
         SH    R1,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         AH    R1,=Y(FHDAD)                                                     
         EX    R1,*+4                                                           
         MVC   FHDA(0),BCSPACES    CLEAR THE CRAP THAT'S ALREADY THERE          
*                                                                               
         MVC   L1RANK,RANK         SET THE HEADLINE FIELDS                      
         MVC   L1ADV,ADVAGY                                                     
         MVC   L1PED,PERIOD                                                     
         MVC   L1PRIFIN,PRIFIN                                                  
         MVC   L1CURFIN,CURFIN                                                  
         MVC   L1PRISHR,PRISHR                                                  
         MVC   L1CURSHR,CURSHR                                                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFST   OC    SVLINPAG,SVLINPAG   REAL NUMBER OF LIST LINES ON SCREEN          
         BNZ   *+10                ALREADY SET                                  
         MVC   SVLINPAG,LSLINPAG   SAVE NUMBER OF LIST LINES ON SCREEN          
*                                                                               
         MVC   LSLINPAG,=AL2(1) ** FOOL CONTROLLER INTO THINKING THERE          
*                                  IS ONLY 1 LINE ON THE PAGE                   
*                                  (SO ONLY 1 TSAR RECORD PER PAGE)             
*                                                                               
         LH    R1,LS1STLIN         CLEAR ALL THE LIST LINES ON SCREEN           
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
         XR    RE,RE                                                            
         LH    RF,LSLINLEN         LENGTH OF A LINE                             
         MH    RF,SVLINPAG         NUMBER OF LINES ON A PAGE                    
         LA    RF,FHD(RF)                                                       
         BCTR  RF,0                RF=A(END OF LIST PORTION-1)                  
*                                                                               
SCRF02   IC    RE,FHLN             SET INDEX IN RE                              
         OI    FHII,FHIIVA         SET FIELD VALIDATED                          
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         NI    FHAT,FF-FHATHI      TURN OFF HIGHLIGHT                           
         LR    R3,RE                                                            
         SH    R3,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BO    *+8                                                              
         LA    R3,FHDAD(R3)                                                     
         EX    R3,*+4                                                           
         MVC   FHDA(0),BCSPACES    SPACE FILL TEXT PORTION                      
*                                                                               
         BXLE  R1,RE,SCRF02        REPEAT FOR ALL FIELDS                        
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST -  ONE RECORD PER PAGE                                   *         
***********************************************************************         
         SPACE 1                                                                
BLST     MVC   IOKEY,0(R2)         RESTORE READ SEQUENCE                        
         ICM   R1,15,=AL4(XIO11+XOREPVEW+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH PROBLEM                            
*                                                                               
         CLC   LSINIKEY,0(R2)      FIRST TIME IN?                               
         BE    BLST02              YES                                          
*                                                                               
BLST01   ICM   R1,15,=AL4(XIO11+XOSEQ+XOREPVEW)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
BLST02   L     RF,AIOREC           I/S RETURNS INTO RECORD BUFFER               
         CLC   0(REKDGRP-REKDKEY,RF),0(R2)                                      
         BNE   EXITL               MAKE SURE SAME REPORT/REP                    
*                                                                               
         CLC   =C'DATES=',REKDSTN-REKDKEY(RF)                                   
         BE    BLST01                                                           
*                                                                               
         MVC   0(L'IOKEY,R2),0(RF) WE WANT THIS KEY                             
         B     EXITOK                                                           
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
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
MEDTAB   DC    CL2'TV'                                                          
         DC    CL2'AM'                                                          
         DC    CL2'FM'                                                          
         DC    CL2'CM'                                                          
         DC    XL1'00'                                                          
         SPACE 1                                                                
CORETAB  DC    CL8'CORETAB'                                                     
OFFICE   DC    CL6'Office'                                                      
STATION  DC    CL7'Station'                                                     
GROUP    DC    CL5'Group'                                                       
ALL      DC    CL3'All'                                                         
TOTAL    DC    CL7'*Total*'                                                     
QTRTOT   DC    CL7'*Q? Tot'                                                     
RANK     DC    CL(L'L1RANK)'Rank'                                               
ADVAGY   DC    CL(L'L1ADV)'Advertiser/Agency'                                   
PRIFIN   DC    CL(L'L1PRIFIN)'PriFin'                                           
CURFIN   DC    CL(L'L1CURFIN)'CurFin'                                           
PRISHR   DC    CL(L'L1PRISHR)'PriShr'                                           
CURSHR   DC    CL(L'L1CURSHR)'CurShr'                                           
PERIOD   DC    CL(L'L1PED)'Period'                                              
RUNON    DC    CL6'Run On'                                                      
TOOBIG   DC    CL12'*OVERFLOW***'                                               
MONTHS   DC    C'JanFebMarAprMayJunJulAugSepOctNovDec'                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
MYSAVED  DSECT                                                                  
SVLINPAG DS    H                                                                
SVOFF    DS    XL2                                                              
SVKEY    DS    XL(L'IOKEY)                                                      
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
FRSTLINE DS    X                                                                
*                                                                               
SVDATES  DS    0XL14                                                            
SVFROM   DS    0XL4                                                             
SVFROMY  DS    XL2                                                              
SVFROMM  DS    XL2                                                              
SVTO     DS    0XL4                                                             
SVTOY    DS    XL2                                                              
SVTOM    DS    XL2                                                              
SVWHEN   DS    0XL6                                                             
SVWHENY  DS    XL2                                                              
SVWHENM  DS    XL2                                                              
SVWHEND  DS    XL2                                                              
*                                                                               
LINE1D   DSECT                                                                  
L1RANK   DS    CL4                                                              
         DS    C                                                                
L1ADV    DS    CL19                                                             
         ORG   L1ADV                                                            
         DS    C                                                                
L2AGY    DS    CL18                                                             
         DS    C                                                                
L1PED    DS    CL7                                                              
         DS    C                                                                
L1PRIFIN DS    CL10                                                             
         DS    C                                                                
L1PRISHR DS    CL6                                                              
         DS    C                                                                
L1CURFIN DS    CL10                                                             
         DS    C                                                                
L1CURSHR DS    CL6                                                              
         SPACE 2                                                                
*        REVIEWWRK                                                              
         PRINT OFF                                                              
       ++INCLUDE REVIEWWRK                                                      
         PRINT ON                                                               
*        DDSCANBLKD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004REVIEW1DX 08/31/00'                                      
         END                                                                    
