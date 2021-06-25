*          DATA SET REVIEW16   AT LEVEL 007 AS OF 09/28/05                      
*          DATA SET REVIEW16   AT LEVEL 005 AS OF 11/19/99                      
*PHASE T81716A                                                                  
VIEW16   TITLE 'K6 REPORT FOR VIEWER'                                           
VIEW16   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REVIEW16,R7,RR=RE                                              
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
         USING REK6KEY,R2                                                       
KFKFVAL  MVC   REK6RPT,=C'K6'                                                   
         MVC   REK6REP,CUAALF                                                   
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
VIEW16   CSECT                                                                  
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
         USING REK6KEY,R2                                                       
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
DISGRP   MVC   FVIFLD(L'REK6GRP),FLTIFLD                                        
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A GROUP FILTER                                             *         
***********************************************************************         
         SPACE 1                                                                
VALGRP   GOTOX AVALGRP                                                          
         BNE   EXITL                                                            
         MVC   REK6GRP,BOWORK1                                                  
         MVC   FLTIFLD(L'REK6GRP),BOWORK1                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO GROUP FILTERING                                                  *         
***********************************************************************         
         SPACE 1                                                                
DOFGRP   CLC   REK6GRP,FLTIFLD                                                  
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
*          DATA SET REVIEW13   AT LEVEL 009 AS OF 10/08/96                      
***********************************************************************         
* DATA OBJECT FOR OFFICE                                              *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REK6KEY,R2                                                       
OFFDTA   LA    RF,OFFTBL                                                        
         B     ITER                                                             
*                                                                               
OFFTBL   DC    AL1(DFDIS),AL1(0,0,0),AL4(DISOFF)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALOFF)                                
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFLTOFF)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFOFF)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETOFF)                                 
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
         MVC   REK6OFF,BOWORK1                                                  
         MVC   FLTIFLD(L'REK6OFF),BOWORK1                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO OFFICE FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFOFF   CLC   REK6OFF,FLTIFLD                                                  
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
         USING REK6KEY,R2                                                       
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
DOFQTR   OC    REK6TOT,REK6TOT                                                  
         BZ    FLTXX               NOT A TOTAL LINE                             
*                                                                               
         CLC   REK6PRD+1(1),FLTIFLD                                             
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
         USING REK6KEY,R2                                                       
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
         CURED (RF),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK,ALIGN=LEFT            
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
DOFRNK   CLC   REK6RNK,FLTIFLD                                                  
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
         USING REK6KEY,R2                                                       
OTHDTA   LA    RF,OTHTBL                                                        
         B     ITER                                                             
*                                                                               
OTHTBL   DC    AL1(DSET),AL1(0,0,0),AL4(SETOTH)                                 
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
         USING REK6KEY,R2                                                       
DISLIN   LH    R1,LSROWREP                                                      
         CHI   R1,1                                                             
         BH    DLIN00                                                           
*                                                                               
         LH    R4,LS1STKEY         SET CURSOR OVERRIDE TO FIRST KEY             
         A     R4,ATWA             FIELD                                        
         ST    R4,BOCURSOR                                                      
*                                                                               
         LH    RF,LSCURLIN                                                      
         A     RF,ATWA                                                          
         OI    FHAT-FHD(RF),FHATHI HIGHLIGHT FIRST LINE                         
*                                                                               
         LH    R4,LS1STKEY         SET CURSOR OVERRIDE TO FIRST KEY             
         A     R4,ATWA             FIELD                                        
         ST    R4,BOCURSOR                                                      
         LA    R4,FVIFLD           FIRST LINE IS FILTER DETAILS                 
         USING LINE1D,R4                                                        
         MVC   L1RANK,RANK         SET THE HEADLINE FIELDS                      
         MVC   L1ADV,ADVAGY                                                     
         MVC   L1PED,PERIOD                                                     
         MVC   L1PRIFIN,PRIFIN                                                  
         MVC   L1CURFIN,CURFIN                                                  
         MVC   L1PRISHR,PRISHR                                                  
         MVC   L1CURSHR,CURSHR                                                  
         MVC   L1PCT,PCT                                                        
         B     EXITOK                                                           
         DROP  R4                                                               
*                                                                               
DLIN00   LH    R1,LSROWREP                                                      
         CHI   R1,2                                                             
         BH    DLIN02                                                           
*                                                                               
         MVC   IOKEY,REK6KEY       SET CURRENT RECORD INTO IO2                  
         ICM   R1,15,=AL4(XIO2+XOREPVEW+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BE    DLIN08              FIRST LINE DISPLAY CODE HERE                 
         DC    H'0'                READ HIGH PROBLEM                            
*                                                                               
DLIN02   L     R4,AIO2                                                          
REC      USING REK6KEY,R4                                                       
         MVC   LSLASKEY,0(R4)      SAVE THIS KEY                                
         MVC   IOKEY,0(R4)                                                      
*                                                                               
DLIN03   L     RE,AIO2                                                          
         LH    RF,=Y(2000)         LENGTH OF AN IO AREA                         
         XR    R1,R1                                                            
         XR    R0,R0                                                            
         MVCL  RE,R0               CLEAR IO AREA                                
*                                                                               
         GOTOX ANXTKEY,FILTTAB                                                  
         ICM   R1,15,=AL4(XIO2+XOHIGH+XOREPVEW)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DLINX                                                            
*                                                                               
         MVC   IOKEY,0(R4)         SET KEY IN IOKEY                             
         CLC   REK6DSCT(REK6RNK-REK6DSCT),REC.REK6DSCT                          
         BNE   DLINX               CONTROL BREAK                                
*                                                                               
         GOTOX AGEN,BOPARM,OFILT,FDOD,IOKEY                                     
         BL    DLIN03              FAILED DIRECTORY FILTERING                   
         GOTOX AGEN,BOPARM,OFILT,FDOR,(R4)                                      
         BL    DLIN03              FAILED RECORD FILTERING                      
*                                                                               
L1       USING LINE1D,FVIFLD                                                    
*                                                                               
         L     RF,AIO1             SEE IF ADV/AGENCY CHANGED                    
         CLC   REK6ADAG-REK6KEY(L'REK6ADAG,RF),REC.REK6ADAG                     
         BNE   DLIN04              YES                                          
*                                                                               
         CLI   FRSTLINE,C'1'       FIRST LINE ONLY DISPLAYED?                   
         BNE   DLIN12              NO                                           
         GOTOX DOAGY,BOPARM,AIO1                                                
         MVC   L1.L2AGY,BOWORK1                                                 
         MVI   FRSTLINE,C'2'       SET LINE 2 OF SERIES                         
         B     DLIN12                                                           
*                                                                               
DLIN04   CLI   FRSTLINE,C'1'       FIRST LINE ONLY DISPLAYED?                   
         BNE   DLIN08              NO                                           
         L     R4,AIO1             FIRST LINE DETAILS IN AIO1                   
         CLC   REC.REK6ADV,BCEFFS  ALL/ALL?                                     
         BNE   *+14                NO                                           
         CLC   REC.REK6AGY,BCEFFS  DON'T SHOW SECOND 'ALL'                      
         BE    DLIN06                                                           
         GOTOX DOAGY,BOPARM,AIO1                                                
         MVC   L1.L2AGY,BOWORK1                                                 
         AH    R3,LSLINLEN         NEXT LINE ON SCREEN                          
*                                                                               
DLIN06   MVI   FRSTLINE,C'2'       SET DISPLAYED LINE 2 INFORMATION             
*                                                                               
DLIN08   L     R0,AIO1                                                          
         L     RE,AIO2                                                          
         LH    R1,=Y(2000)         LENGTH OF AN IO AREA                         
         LR    RF,R1                                                            
         MVCL  R0,RE               COPY FIRST LINE INTO AIO1                    
*                                                                               
         L     R4,AIO1                                                          
         CLC   REC.REK6AGY,BCEFFS  DONT SHOW RANK IF 'ALL'                      
         BNE   *+14                                                             
         CLC   REC.REK6ADV,BCEFFS                                               
         BE    DLIN10                                                           
         XR    RF,RF                                                            
         ICM   RF,15,REC.REK6RNK                                                
         CURED (RF),(4,L1.L1RANK),0,DMCB=BOPARM,ZERO=NOBLANK,ALIGN=LEFT         
*                                                                               
DLIN10   GOTOX DOADV,BOPARM,AIO1   ADVERTISER                                   
         MVC   L1.L1ADV,BOWORK1                                                 
         MVI   FRSTLINE,C'1'       SET DISPLAYED LINE 1                         
*                                                                               
DLIN12   GOTOX DOLINE,BOPARM,FVIFLD,(R4)                                        
         L     R4,AIO2                                                          
         MVC   LSLASKEY,0(R4)      SAVE THIS KEY                                
         B     EXITOK                                                           
*                                                                               
DLINX    CLI   FRSTLINE,C'1'       FIRST LINE OF PAIR DISPLAYED?                
         BNE   EXITOK              NO                                           
         AH    R3,LSLINLEN                                                      
         L     R4,AIO1                                                          
         CLC   REC.REK6AGY,BCEFFS  DON'T SHOW SECOND 'ALL'                      
         BE    EXITOK                                                           
         GOTOX DOAGY,BOPARM,AIO1                                                
         MVC   L1.L2AGY,BOWORK1    SET AGENCY                                   
         B     EXITOK                                                           
         DROP  R2,REC                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET DATES FOR THIS REPORT                                *         
***********************************************************************         
         SPACE 1                                                                
GETDATE  NTR1  ,                                                                
         XC    IOKEY,IOKEY                                                      
X        USING REK6KEY,IOKEY                                                    
         L     RF,AIOREC                                                        
         USING REK6KEY,RF                                                       
         MVC   X.REK6RPT,=AL2(REK6RPTQ)                                         
         MVC   X.REK6REP,CUAALF                                                 
         ICM   R1,15,=AL4(XIO3+XOREPVEW+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
         DROP  X                                                                
*                                                                               
         L     RF,AIO3                                                          
         CLC   =C'DATES=',REK6OFF                                               
         BNE   EXITL                                                            
         MVC   SVFROM,REK6OFF+6                                                 
         MVC   SVTO,REK6OFF+10                                                  
*                                                                               
         LA    R1,REK6FRST                                                      
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
         USING REK6KEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
         CLC   REK6ADV,BCEFFS                                                   
         BNE   *+14                                                             
         MVC   BOWORK1(L'ALL),ALL                                               
         B     EXITOK                                                           
*                                                                               
X        USING RADVRECD,IOKEY                                                   
         MVI   X.RADVKTYP,X'08'                                                 
         MVC   X.RADVKADV,REK6ADV                                               
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
         USING REK6KEY,R2                                                       
         XC    IOKEY,IOKEY                                                      
         MVC   BOWORK1,BCSPACES                                                 
*                                                                               
         CLC   REK6AGY,BCEFFS                                                   
         BNE   *+14                                                             
         MVC   BOWORK1(L'ALL),ALL                                               
         B     EXITOK                                                           
*                                                                               
X        USING RAGYRECD,IOKEY                                                   
         MVI   X.RAGYKTYP,X'0A'                                                 
         MVC   X.RAGYKAGY,REK6AGY                                               
         MVC   X.RAGYKAOF,REK6AGY+L'RAGYKAGY                                    
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
         USING REK6KEY,R4                                                       
DOLINE   NTR1                                                                   
         LM    R3,R4,0(R1)                                                      
*                               ** PERIOD IS YYMM BINARY                        
         MVI   L1PED,C'*'                                                       
         MVC   L1PED(L'TOTAL),TOTAL                                             
         CLI   REK6PRD+1,X'0D'      IF MM IS 'OD' THIS IS A YRLY TOTAL          
         BE    DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         MVC   L1PED(L'ALL),ALL                                                 
         CLI   REK6PRD+1,FF         IF MM IS 'FF' PUT OUT 'ALL'                 
         BE    DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         MVC   L1PED(L'QTRTOT),QTRTOT                                           
         MVI   L1PED+2,C'1'                                                     
         CLI   REK6PRD+1,3                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'2'                                                     
         CLI   REK6PRD+1,6                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'3'                                                     
         CLI   REK6PRD+1,9                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'4'                                                     
*                                                                               
DOL00    OC    REK6TOT,REK6TOT      IF SET THIS IS SUB-TOTAL LINE               
         BNZ   DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         XR    RF,RF                                                            
         IC    RF,REK6PRD+1                                                     
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,MONTHS(RF)                                                    
         MVC   L1PED(3),0(RF)                                                   
         MVI   L1PED+3,C'/'                                                     
         XR    RF,RF                                                            
         IC    RF,REK6PRD                                                       
         CHI   RF,100                                                           
         BL    *+8                                                              
         AHI   RF,-100                                                          
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  L1PED+4(2),BODUB1                                                
*                                                                               
DOL01    LA    RF,REK6FRST                                                      
         XR    RE,RE                                                            
*                                                                               
DOL02    CLI   0(RF),0                                                          
         BE    EXITOK                                                           
         CLI   0(RF),R4PBIELQ      PRIOR BILLING                                
         BE    DOL06                                                            
         CLI   0(RF),R4CBIELQ      CURRENT BILLING                              
         BE    DOL08                                                            
         CLI   0(RF),R4PSHELQ      PRIOR SHARE                                  
         BE    DOL10                                                            
         CLI   0(RF),R4CSHELQ      CURRENT SHARE                                
         BE    DOL12                                                            
         CLI   0(RF),R4PCTELQ      PERCENTAGE +/-                               
         BE    DOL14                                                            
*                                                                               
DOL04    ICM   RE,1,1(RF)                                                       
         LA    RF,0(RE,RF)                                                      
         B     DOL02                                                            
*                                                                               
         USING R4PBIEL,RF                                                       
DOL06    IC    RE,R4PBILN                                                       
         SH    RE,=Y(R4PBIDTA-R4PBIEL)                                          
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
         MVC   0(0,R1),R4PBIDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING R4CBIEL,RF                                                       
DOL08    IC    RE,R4CBILN                                                       
         SH    RE,=Y(R4CBIDTA-R4CBIEL)                                          
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
         MVC   0(0,R1),R4CBIDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING R4PSHEL,RF                                                       
DOL10    IC    RE,R4PSHLN                                                       
         SH    RE,=Y(R4PSHDTA-R4PSHEL)                                          
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
         MVC   0(0,R1),R4PSHDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING R4CSHEL,RF                                                       
DOL12    IC    RE,R4CSHLN                                                       
         SH    RE,=Y(R4CSHDTA-R4CSHEL)                                          
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
         MVC   0(0,R1),R4CSHDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING R4PCTEL,RF                                                       
DOL14    IC    RE,R4PCTLN                                                       
         SH    RE,=Y(R4PCTDTA-R4PCTEL)                                          
         BNP   DOL04                                                            
         CLM   RE,1,=AL1(L'L1PCT)                                               
         BNH   *+14                                                             
         MVC   L1PCT,TOOBIG                                                     
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1PCT                                                       
         SR    R1,RE                                                            
         LA    R1,L1PCT(R1)                                                     
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),R4PCTDTA                                                 
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
         DC    AL1(LSCRLAST),AL1(0,0,0),AL4(SCRLST)                             
         DC    AL1(LSETHEAD),AL1(0,0,0),AL4(SETHED)                             
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ILST     MVI   LSSUBLEN,0          NO SUB-ACTION FIELD                          
         OI    LSSTAT2,LSS1HEAD    SETTING MY OWN HEADLINES                     
         OI    LSSTAT3,LS3RFIX                                                  
         MVC   LSROWLIN,=AL2(16)                                                
         MVC   LSNUMHED,=AL2(1)    ONE HEADLINE FIELD                           
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
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR SCREEN                                                    *         
***********************************************************************         
         SPACE 1                                                                
SCRFST   LH    R1,LS1STLIN         CLEAR ALL THE LIST LINES ON SCREEN           
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
         XR    RE,RE                                                            
         LH    RF,LSLINLEN         LENGTH OF A LINE                             
         MH    RF,LSLINPAG         NUMBER OF LINES ON A PAGE                    
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
* LAST FOR SCREEN                                                     *         
***********************************************************************         
         SPACE 1                                                                
SCRLST   LH    R1,LSHEDDSP         REDISPLAY HEADLINE FIELD                     
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
         IC    RE,FHLN             SET INDEX IN RE                              
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
         DROP  R1                                                               
*                                                                               
         L     R2,AIO1                                                          
         USING REK6KEY,R2                                                       
         TM    LSLTIND1,LSLTISOL   STARTED LIST BUILD?                          
         BZ    EXITOK              NO                                           
*                                                                               
         LA    R4,FVIFLD                                                        
         MVC   FVIFLD,BCSPACES                                                  
         BAS   RE,GETDATE                                                       
         BNE   SCRL02                                                           
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
         PACK  BODUB1,SVWHENM      RUN DATE                                     
         CVB   RF,BODUB1                                                        
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,MONTHS(RF)                                                    
         MVC   0(3,R4),0(RF)                                                    
         MVC   3(L'SVWHEND,R4),SVWHEND                                          
         LA    R4,L'SVWHEND+3(R4)                                               
         MVI   0(R4),C'/'                                                       
         MVC   1(2,R4),SVWHENY                                                  
         MVI   3(R4),C','                                                       
         LA    R4,4(R4)                                                         
*                               ** GROUP FILTER                                 
SCRL02   MVC   0(L'GROUP,R4),GROUP                                              
         MVI   L'GROUP(R4),C'='                                                 
         LA    R4,L'GROUP+1(R4)                                                 
         MVC   0(L'REK6GRP,R4),REK6GRP                                          
         CLC   REK6GRP,BCSPACES   IS THIS FOR ALL GROUPS?                       
         BH    SCRL04             NO                                            
         MVC   0(L'ALL,R4),ALL                                                  
         LA    R4,L'ALL(R4)                                                     
         B     *+8                                                              
SCRL04   LA    R4,L'REK6GRP(R4)                                                 
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                               ** OFFICE FILTER                                
         MVC   0(L'OFFICE,R4),OFFICE                                            
         MVI   L'OFFICE(R4),C'='                                                
         LA    R4,L'OFFICE+1(R4)                                                
         MVC   0(L'REK6OFF,R4),REK6OFF                                          
         CLC   REK6OFF,BCEFFS     IS THIS FOR ALL OFFICES?                      
         BNE   *+10               NO                                            
         MVC   0(L'ALL,R4),ALL                                                  
*                                                                               
         LH    R1,LSHEDDSP         REDISPLAY HEADLINE FIELD                     
         A     R1,ATWA                                                          
         USING FHD,R1                                                           
         XR    RE,RE                                                            
         IC    RE,FHLN             SET INDEX IN RE                              
         OI    FHII,FHIIVA         SET FIELD VALIDATED                          
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         NI    FHAT,FF-FHATHI      TURN OFF HIGHLIGHT                           
         LR    R3,RE                                                            
         SH    R3,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BO    *+8                                                              
         LA    R3,FHDAD(R3)                                                     
         EX    R3,*+4                                                           
         MVC   FHDA(0),FVIFLD      SET NEW TEXT LINE                            
         MVC   FVIFLD,BCSPACES                                                  
         B     EXITOK                                                           
         DROP  R1,R2                                                            
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST -  ONE RECORD PER PAGE                                   *         
***********************************************************************         
         SPACE 1                                                                
FLST     MVC   IOKEY,0(R2)         RESTORE READ SEQUENCE                        
         ICM   R1,15,=AL4(XIO11+XOREPVEW+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               READ HIGH PROBLEM                            
*                                                                               
         TM    LSLTIND1,LSLTISOL   LIST STARTED?                                
         BZ    NLST02              NO                                           
         B     NLST                                                             
         SPACE 2                                                                
***********************************************************************         
* BUILD LIST -  ONE RECORD PER PAGE                                   *         
***********************************************************************         
         SPACE 1                                                                
NLST     L     RF,AIOREC                                                        
         MVC   IOKEY,0(RF)                                                      
*                                                                               
         GOTOX ANXTKEY,FILTTAB                                                  
         ICM   R1,15,=AL4(XIO11+XOREPVEW+XOHIGH)                                
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL               END OF FILE                                  
*                                                                               
NLST02   L     RF,AIOREC           I/S RETURNS INTO RECORD BUFFER               
         CLC   0(REK6GRP-REK6KEY,RF),LSINIKEY                                   
         BNE   EXITL               MAKE SURE SAME REPORT/REP                    
*                                                                               
         CLC   =C'DATES=',REK6OFF-REK6KEY(RF)                                   
         BE    NLST                                                             
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
FILTTAB  DC    AL2(00001),AL1(REK6GRP-REK6DSCT,L'REK6GRP)                       
         DC    AL2(00003),AL1(REK6OFF-REK6DSCT,L'REK6OFF)                       
         DC    AL2(00005),AL1(REK6RNK-REK6DSCT,L'REK6RNK)                       
         DC    AL2(EOT)                                                         
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
PRIFIN   DC    CL(L'L1PRIFIN)'    PriFin'                                       
CURFIN   DC    CL(L'L1CURFIN)'    CurBlg'                                       
PRISHR   DC    CL(L'L1PRISHR)'PriShr'                                           
CURSHR   DC    CL(L'L1CURSHR)'CurShr'                                           
PERIOD   DC    CL(L'L1PED)'Period'                                              
PCT      DC    CL(L'L1PCT)'Pct+/-'                                              
RUNON    DC    C'Run On '                                                       
TOOBIG   DC    CL10'*OVERFLOW***'                                               
MONTHS   DC    C'JanFebMarAprMayJunJulAugSepOctNovDec'                          
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
MYSAVED  DSECT                                                                  
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
         DS    C                                                                
L1PCT    DS    CL6                                                              
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
**PAN#1  DC    CL21'007REVIEW16  09/28/05'                                      
         END                                                                    
