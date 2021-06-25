*          DATA SET REVIEW23   AT LEVEL 015 AS OF 09/27/05                      
*          DATA SET REVIEW23   AT LEVEL 013 AS OF 11/19/99                      
*PHASE T81723A                                                                  
VIEW23   TITLE 'W3/6/9 REPORTS FOR VIEWER'                                      
VIEW23   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REVIEW23,R7,RR=RE                                              
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
         USING REW3KEY,R2                                                       
KFKFVAL  MVC   REW3RPT,=C'W3'                                                   
         MVC   REW3REP,CUAALF                                                   
         MVC   REW3GRP,=AL2(1)                                                  
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
         DC    AL2(00007),AL4(LINDTA)       DISPLAY LINE                        
         DC    AL2(00008),AL4(TEMDTA)       TEAM                                
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
VIEW23   CSECT                                                                  
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
         USING REW3KEY,R2                                                       
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
DISGRP   MVC   FVIFLD(L'REW3GRSB),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A GROUP FILTER                                             *         
***********************************************************************         
         SPACE 1                                                                
VALGRP   GOTOX AVALGRP                                                          
         BNE   EXITL                                                            
         MVC   REW3GRSB,BOWORK1                                                 
         MVC   FLTIFLD(L'REW3GRSB),BOWORK1                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO GROUP FILTERING                                                  *         
***********************************************************************         
         SPACE 1                                                                
DOFGRP   CLC   REW3GRSB,FLTIFLD                                                 
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TEAM                                                *         
*                                                                     *         
* P1 HOLDS EQUATED DATA IDENTIFIER                                    *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* P4 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         USING REW3KEY,R2                                                       
TEMDTA   LA    RF,TEMTBL                                                        
         B     ITER                                                             
*                                                                               
TEMTBL   DC    AL1(DFDIS),AL1(0,0,0),AL4(DISTEM)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALTEM)                                
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTEM)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETTEM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET FILTER FIELD PROTECTION                                         *         
***********************************************************************         
         SPACE 1                                                                
SETTEM   B     FLTXX                                                            
         SPACE 1                                                                
***********************************************************************         
* DISPLAY A TEAM FILTER                                               *         
***********************************************************************         
         SPACE 1                                                                
DISTEM   CLC   FLTIFLD(L'REW3TEAM),BCEFFS                                       
         BNE   *+14                                                             
         MVC   FVIFLD(3),=C'ALL'                                                
         B     EXITOK                                                           
*                                                                               
         MVC   FVIFLD(L'REW3TEAM),REW3TEAM                                      
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* VALIDATE A TEAM FILTER                                              *         
***********************************************************************         
         SPACE 1                                                                
VALTEM   CLI   FVILEN,3                                                         
         BNE   VTEM02                                                           
         CLC   =C'ALL',FVIFLD                                                   
         BNE   VTEMN                                                            
         MVC   FLTIFLD(L'REW3TEAM),BCEFFS                                       
         B     EXITOK                                                           
*                                                                               
VTEM02   CLI   FVILEN,L'REW3TEAM                                                
         BNE   VTEMN                                                            
*                                                                               
X        USING RTEMRECD,IOKEY                                                   
         XC    IOKEY,IOKEY                                                      
         MVI   X.RTEMKTYP,5                                                     
         MVC   X.RTEMKREP,CUAALF                                                
         MVC   X.RTEMKTEM,FVIFLD                                                
         L     R1,=AL4(XOREAD+XOREPDIR+XIO1)                                    
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   VTEMN               RECORD NOT FOUND                             
         MVC   FLTIFLD(L'RTEMKTEM),FVIFLD                                       
         B     EXITOK                                                           
         DROP  X                                                                
*                                                                               
VTEMN    MVC   FVMSGNO,=AL2(RR#ITEAM)                                           
         B     EXITL                                                            
         SPACE 1                                                                
***********************************************************************         
* DO STATION FILTERING                                                *         
***********************************************************************         
         SPACE 1                                                                
DOFTEM   CLC   REW3TEAM,FLTIFLD                                                 
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
         USING REW3KEY,R2                                                       
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
         SPACE 2                                                                
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
         MVC   REW3OFF,BOWORK1                                                  
         MVC   FLTIFLD(L'REW3OFF),BOWORK1                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO OFFICE FILTERING                                                 *         
***********************************************************************         
         SPACE 1                                                                
DOFOFF   CLC   REW3OFF,FLTIFLD                                                  
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
         USING REW3KEY,R2                                                       
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
DOFQTR   OC    REW3TOT,REW3TOT                                                  
         BZ    FLTXX               NOT A TOTAL LINE                             
*                                                                               
         CLC   REW3PRD+1(1),FLTIFLD                                             
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
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
         USING REW3KEY,R2                                                       
DISLIN   LH    R1,LSROWREP                                                      
         CHI   R1,1                                                             
         BH    DLIN01                                                           
*                                                                               
         LH    RF,LSCURLIN                                                      
         A     RF,ATWA                                                          
         OI    FHAT-FHD(RF),FHATHI HIGHLIGHT FIRST LINE                         
*                                                                               
         LH    R4,LS1STKEY         SET CURSOR OVERRIDE TO FIRST KEY             
         A     R4,ATWA             FIELD                                        
         ST    R4,BOCURSOR                                                      
*                                                                               
         LA    R4,FVIFLD           FIRST LINE IS FILTER DETAILS                 
         OC    SVCOLS,SVCOLS                                                    
         BNZ   DLIN00                                                           
*                                                                               
         USING LINE1D,R4                                                        
         MVC   L1PED,PERIOD                                                     
         MVC   L1CURBKD,CURBKD                                                  
         MVC   L1PRIBKD,PRIBKD                                                  
         MVC   L1CURBIL,CURBIL                                                  
         MVC   L1PRIBIL,PRIBIL                                                  
         MVC   L1CURPCT,CURPCT                                                  
         MVC   L1PRIACT,PRIACT                                                  
         MVC   L1ACTPCT,ACTPCT                                                  
         MVC   L1CURUNC,CURUNC                                                  
         MVC   L1CURDRC,CURDRC                                                  
         B     DLINX                                                            
         DROP  R4                                                               
*                                                                               
         USING LINE2D,R4                                                        
DLIN00   MVC   L2PED,PERIOD                                                     
         MVC   L2CURBIL,CURBIL                                                  
         MVC   L2PRIBIL,PRIBIL                                                  
         MVC   L2CURPCT,CURPCT                                                  
         MVC   L2PRIACT,PRIACT                                                  
         MVC   L2ACTPCT,ACTPCT                                                  
         MVC   L2CURUNC,CURUNC                                                  
         MVC   L2CURDRC,CURDRC                                                  
         MVC   L2CURBDG,CURBDG                                                  
         MVC   L2BDGPCT,BDGPCT                                                  
         B     DLINX                                                            
         DROP  R4                                                               
*                                                                               
DLIN01   LH    R1,LSROWREP                                                      
         CHI   R1,2                                                             
         BH    DLIN02                                                           
*                                                                               
         L     R0,AIO1             CLEAR IO AREA                                
         LH    R1,=Y(2000)                                                      
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVC   IOKEY,REW3KEY       SET CURRENT RECORD INTO IO1                  
         ICM   R1,15,=AL4(XIO1+XOREPVEW+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BE    DLIN04              FIRST LINE DISPLAY CODE HERE                 
         DC    H'0'                READ HIGH PROBLEM                            
*                                                                               
DLIN02   L     R4,AIO1                                                          
REC      USING REW3KEY,R4                                                       
         MVC   IOKEY,0(R4)         SET KEY IN IOKEY                             
         MVC   LSLASKEY,0(R4)      SAVE THIS KEY                                
*                                                                               
DLIN03   L     R0,AIO1             CLEAR IO AREA                                
         LH    R1,=Y(2000)         NO '00' ON END OF RECORD SO MUST             
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTOX ANXTKEY,FILTTAB                                                  
         ICM   R1,15,=AL4(XIO1+XOREPVEW+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   DLINX                                                            
*                                                                               
         MVC   IOKEY,0(R4)         SET KEY IN IOKEY                             
         CLC   REW3KEY(REW3PRD-REW3KEY),REC.REW3KEY                             
         BNE   DLINX               CONTROL BREAK                                
*                                                                               
         GOTOX AGEN,BOPARM,OFILT,FDOD,IOKEY                                     
         BL    DLIN03              FAILED DIRECTORY FILTERING                   
         GOTOX AGEN,BOPARM,OFILT,FDOR,(R4)                                      
         BL    DLIN03              FAILED RECORD FILTERING                      
*                                                                               
L1       USING LINE1D,FVIFLD                                                    
*                                                                               
DLIN04   L     R4,AIO1                                                          
*                                                                               
         GOTOX DOLINE,BOPARM,FVIFLD,(R4)                                        
         MVC   LSLASKEY,0(R4)      SAVE THIS KEY                                
*                                                                               
DLINX    B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD DATES FROM HEADER RECORD                           *         
***********************************************************************         
         SPACE 1                                                                
GETDATE  NTR1  ,                                                                
         XC    IOKEY,IOKEY                                                      
X        USING REW3KEY,IOKEY                                                    
         L     RF,AIOREC                                                        
         USING REW3KEY,RF                                                       
         MVC   X.REW3RPT,=C'W3'                                                 
         MVC   X.REW3REP,CUAALF                                                 
         ICM   R1,15,=AL4(XIO3+XOREPVEW+XOHIGH)                                 
         GOTOX ('XIO',AGROUTS)                                                  
         BNE   EXITL                                                            
         DROP  X                                                                
*                                                                               
         L     RF,AIO3                                                          
         CLC   =C'DATES=',REW3GRSB                                              
         BNE   EXITL                                                            
         MVC   SVFROM,REW3GRSB+6                                                
         MVC   SVTO,REW3GRSB+10                                                 
*                                                                               
         LA    R1,REW3FRST                                                      
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
* ROUTINE TO DISPLAY RECORD INFORMATION TO A FIELD                    *         
*                                                                     *         
* NTRY P1: 78 CHARACTER FIELD TO FILL (COVERED BY LINE1D)             *         
*      P2: A(RECORD)                                                  *         
***********************************************************************         
         USING REW3KEY,R4                                                       
DOLINE   NTR1                                                                   
         LM    R3,R4,0(R1)                                                      
         USING LINE1D,R3                                                        
*                               ** PERIOD IS YYMM BINARY                        
         MVI   L1PED,C'*'                                                       
         MVC   L1PED(L'TOTAL),TOTAL                                             
         CLI   REW3PRD+1,X'0D'      IF MM IS 'OD' THIS IS A YRLY TOTAL          
         BE    DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         MVC   L1PED(L'ALL),ALL                                                 
         CLI   REW3PRD+1,FF         IF MM IS 'FF' PUT OUT 'ALL'                 
         BE    DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         MVC   L1PED(L'QTRTOT),QTRTOT                                           
         MVI   L1PED+2,C'1'                                                     
         CLI   REW3PRD+1,3                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'2'                                                     
         CLI   REW3PRD+1,6                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'3'                                                     
         CLI   REW3PRD+1,9                                                      
         BNH   DOL00                                                            
         MVI   L1PED+2,C'4'                                                     
*                                                                               
DOL00    OC    REW3TOT,REW3TOT                                                  
         BNZ   DOL01                                                            
*                                                                               
         MVC   L1PED,BCSPACES                                                   
         XR    RF,RF                                                            
         IC    RF,REW3PRD+1                                                     
         BCTR  RF,0                                                             
         MH    RF,=H'3'                                                         
         LA    RF,MONTHS(RF)                                                    
         MVC   L1PED(3),0(RF)                                                   
         MVI   L1PED+3,C'/'                                                     
         XR    RF,RF                                                            
         IC    RF,REW3PRD                                                       
         CHI   RF,100                                                           
         BL    *+8                                                              
         AHI   RF,-100                                                          
         CVD   RF,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
         UNPK  L1PED+4(2),BODUB1                                                
*                                                                               
DOL01    LA    RF,REW3FRST                                                      
         XR    RE,RE                                                            
*                                                                               
DOL02    CLI   0(RF),0                                                          
         BE    EXITOK                                                           
         CLI   0(RF),W1CBKELQ      CURRENT BOOKING                              
         BE    DOL06                                                            
         CLI   0(RF),W1PBKELQ      PRIOR BOOKING                                
         BE    DOL10                                                            
         CLI   0(RF),W1CBLELQ      CURRENT BILLING                              
         BE    DOL14                                                            
         CLI   0(RF),W1PBLELQ      PRIOR BILLING                                
         BE    DOL18                                                            
         CLI   0(RF),W1CPCELQ      CURRENT PERCENT                              
         BE    DOL22                                                            
         CLI   0(RF),W1PACELQ      PRIOR ACTIVITY                               
         BE    DOL26                                                            
         CLI   0(RF),W1PPCELQ      ACT PERCENT                                  
         BE    DOL30                                                            
         CLI   0(RF),W1CUCELQ      CURRENT UNCONFIRMED                          
         BE    DOL34                                                            
         CLI   0(RF),W1CDRELQ      CURRENT DIRECT                               
         BE    DOL38                                                            
         CLI   0(RF),W1CBGELQ      CURRENT BUDGET                               
         BE    DOL42                                                            
         CLI   0(RF),W1BPCELQ      BUDGET PERCENT                               
         BE    DOL46                                                            
*                                                                               
DOL04    ICM   RE,1,1(RF)                                                       
         LA    RF,0(RE,RF)                                                      
         B     DOL02                                                            
*                                                                               
         USING W1CBKIDS,RF         CURRENT BOOKING ELEMENT                      
DOL06    IC    RE,W1CBKLN                                                       
         SH    RE,=Y(W1CBKDTA-W1CBKEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL08                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1CURBKD)                                            
         BNH   *+14                                                             
         MVC   L1CURBKD,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1CURBKD                                                    
         SR    R1,RE                                                            
         LA    R1,L1CURBKD(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CBKDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL08    B     DOL04                                                            
*                                                                               
         USING W1PBKIDS,RF         PRIOR BOOKING ELEMENT                        
DOL10    IC    RE,W1PBKLN                                                       
         SH    RE,=Y(W1PBKDTA-W1PBKEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL12                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1PRIBKD)                                            
         BNH   *+14                                                             
         MVC   L1PRIBKD,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1PRIBKD                                                    
         SR    R1,RE                                                            
         LA    R1,L1PRIBKD(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1PBKDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL12    B     DOL04                                                            
*                                                                               
         USING W1CBLDS,RF          CURRENT BILLING ELEMENT                      
DOL14    IC    RE,W1CBLLN                                                       
         SH    RE,=Y(W1CBLDTA-W1CBLEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL16                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1CURBIL)                                            
         BNH   *+14                                                             
         MVC   L1CURBIL,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1CURBIL                                                    
         SR    R1,RE                                                            
         LA    R1,L1CURBIL(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CBLDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL16    CLM   RE,1,=AL1(L'L2CURBIL)                                            
         BNH   *+14                                                             
         MVC   L2CURBIL,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2CURBIL                                                    
         SR    R1,RE                                                            
         LA    R1,L2CURBIL(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CBLDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1PBLDS,RF          PRIOR BILLING ELEMENT                        
DOL18    IC    RE,W1PBLLN                                                       
         SH    RE,=Y(W1PBLDTA-W1PBLEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL20                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1PRIBIL)                                            
         BNH   *+14                                                             
         MVC   L1PRIBIL,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1PRIBIL                                                    
         SR    R1,RE                                                            
         LA    R1,L1PRIBIL(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1PBLDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL20    CLM   RE,1,=AL1(L'L2PRIBIL)                                            
         BNH   *+14                                                             
         MVC   L2PRIBIL,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2PRIBIL                                                    
         SR    R1,RE                                                            
         LA    R1,L2PRIBIL(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1PBLDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1CPCDS,RF          CURR PCT                                     
DOL22    IC    RE,W1CPCLN                                                       
         SH    RE,=Y(W1CPCDTA-W1CPCEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL24                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1CURPCT)                                            
         BNH   *+14                                                             
         MVC   L1CURPCT,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1CURPCT                                                    
         SR    R1,RE                                                            
         LA    R1,L1CURPCT(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CPCDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL24    CLM   RE,1,=AL1(L'L2CURPCT)                                            
         BNH   *+14                                                             
         MVC   L2CURPCT,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2CURPCT                                                    
         SR    R1,RE                                                            
         LA    R1,L2CURPCT(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CPCDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1PACDS,RF          PRI ACT                                      
DOL26    IC    RE,W1PACLN                                                       
         SH    RE,=Y(W1PACDTA-W1PACEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL28                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1PRIACT)                                            
         BNH   *+14                                                             
         MVC   L1PRIACT,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1PRIACT                                                    
         SR    R1,RE                                                            
         LA    R1,L1PRIACT(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1PACDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL28    CLM   RE,1,=AL1(L'L2PRIACT)                                            
         BNH   *+14                                                             
         MVC   L2PRIACT,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2PRIACT                                                    
         SR    R1,RE                                                            
         LA    R1,L2PRIACT(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1PACDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1PPCDS,RF          ACT PCT                                      
DOL30    IC    RE,W1PPCLN                                                       
         SH    RE,=Y(W1PPCDTA-W1PPCEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL32                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1ACTPCT)                                            
         BNH   *+14                                                             
         MVC   L1ACTPCT,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1ACTPCT                                                    
         SR    R1,RE                                                            
         LA    R1,L1ACTPCT(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1PPCDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL32    CLM   RE,1,=AL1(L'L2ACTPCT)                                            
         BNH   *+14                                                             
         MVC   L2ACTPCT,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2ACTPCT                                                    
         SR    R1,RE                                                            
         LA    R1,L2ACTPCT(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1PPCDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1CUCDS,RF          CURR UNCF                                    
DOL34    IC    RE,W1CUCLN                                                       
         SH    RE,=Y(W1CUCDTA-W1CUCEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL36                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1CURUNC)                                            
         BNH   *+14                                                             
         MVC   L1CURUNC,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1CURUNC                                                    
         SR    R1,RE                                                            
         LA    R1,L1CURUNC(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CUCDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL36    CLM   RE,1,=AL1(L'L2CURUNC)                                            
         BNH   *+14                                                             
         MVC   L2CURUNC,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2CURUNC                                                    
         SR    R1,RE                                                            
         LA    R1,L2CURUNC(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CUCDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1CDRDS,RF          CURR DRCT                                    
DOL38    IC    RE,W1CDRLN                                                       
         SH    RE,=Y(W1CDRDTA-W1CDREL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL40                                                            
*                                                                               
         USING LINE1D,R3                                                        
         CLM   RE,1,=AL1(L'L1CURDRC)                                            
         BNH   *+14                                                             
         MVC   L1CURDRC,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L1CURDRC                                                    
         SR    R1,RE                                                            
         LA    R1,L1CURDRC(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CDRDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL40    CLM   RE,1,=AL1(L'L2CURDRC)                                            
         BNH   *+14                                                             
         MVC   L2CURDRC,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2CURDRC                                                    
         SR    R1,RE                                                            
         LA    R1,L2CURDRC(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CDRDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1CBGDS,RF          CURR BDGT                                    
DOL42    IC    RE,W1CBGLN                                                       
         SH    RE,=Y(W1CBGDTA-W1CBGEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BNZ   DOL44                                                            
         B     DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
DOL44    CLM   RE,1,=AL1(L'L2CURBDG)                                            
         BNH   *+14                                                             
         MVC   L2CURBDG,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2CURBDG                                                    
         SR    R1,RE                                                            
         LA    R1,L2CURBDG(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1CBGDTA                                                 
         B     DOL04                                                            
*                                                                               
         USING W1BPCDS,RF          BDGT PCT                                     
DOL46    IC    RE,W1BPCLN                                                       
         SH    RE,=Y(W1BPCDTA-W1BPCEL)                                          
         BNP   DOL04                                                            
*                                                                               
         OC    SVCOLS,SVCOLS       WHICH COLUMN DISPLAY?                        
         BZ    DOL04                                                            
*                                                                               
         USING LINE2D,R3                                                        
         CLM   RE,1,=AL1(L'L2BDGPCT)                                            
         BNH   *+14                                                             
         MVC   L2BDGPCT,TOOBIG                                                  
         B     DOL04                                                            
*                                                                               
         LA    R1,L'L2BDGPCT                                                    
         SR    R1,RE                                                            
         LA    R1,L2BDGPCT(R1)                                                  
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),W1BPCDTA                                                 
         B     DOL04                                                            
*                                                                               
         DROP  R3,R4,RF                                                         
         EJECT                                                                  
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
*                                                                               
         CLI   BCPFKEY,1           TOGGLE DISPLAY                               
         BNE   EXITOK                                                           
         XI    SVCOLS,FF                                                        
         OI    LSSTAT3,LS3NOSCR                                                 
         OI    LSSCIND1,LSSCIINP                                                
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
         L     R2,AIOREC                                                        
         USING REW3KEY,R2                                                       
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
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         LA    R4,2(R4)                                                         
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
         MVC   0(L'REW3GRSB,R4),REW3GRSB                                        
         CLC   REW3GRSB,BCEFFS    IS THIS FOR ALL GROUPS?                       
         BNE   SCRL04             NO                                            
         MVC   0(L'ALL,R4),ALL                                                  
         LA    R4,L'ALL(R4)                                                     
         B     *+8                                                              
SCRL04   LA    R4,L'REW3GRSB(R4)                                                
         MVI   0(R4),C','                                                       
         LA    R4,1(R4)                                                         
*                               ** OFFICE FILTER                                
         MVC   0(L'OFFICE,R4),OFFICE                                            
         MVI   L'OFFICE(R4),C'='                                                
         LA    R4,L'OFFICE+1(R4)                                                
         MVC   0(L'REW3OFF,R4),REW3OFF                                          
         CLC   REW3OFF,BCEFFS     IS THIS FOR ALL OFFICES?                      
         BNE   *+10               NO                                            
         MVC   0(L'ALL,R4),ALL                                                  
         LA    R4,L'ALL(R4)                                                     
         CLI   0(R4),C' '                                                       
         BH    *+8                                                              
         BCT   R4,*-8                                                           
         MVI   1(R4),C','                                                       
         LA    R4,2(R4)                                                         
*                               ** TEAM FILTER                                  
         MVC   0(L'TEAM,R4),TEAM                                                
         MVI   L'TEAM(R4),C'='                                                  
         LA    R4,L'TEAM+1(R4)                                                  
         MVC   0(L'REW3TEAM,R4),REW3TEAM                                        
         CLC   REW3TEAM,BCEFFS    IS THIS FOR ALL TEAMS?                        
         BNE   SCRL06             NO                                            
         MVC   0(L'ALL,R4),ALL                                                  
         LA    R4,L'ALL(R4)                                                     
         B     *+8                                                              
SCRL06   LA    R4,L'REW3TEAM(R4)                                                
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
         CLC   0(REW3PAD-REW3KEY,RF),LSINIKEY                                   
         BNE   EXITL               MAKE SURE SAME REPORT/REP                    
*                                                                               
         CLC   =C'DATES=',REW3OFF-REW3KEY(RF)                                   
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
FILTTAB  DC    AL2(00001),AL1(REW3GRSB-REW3DSCT,L'REW3GRSB)                     
         DC    AL2(00003),AL1(REW3OFF-REW3DSCT,L'REW3OFF)                       
         DC    AL2(00008),AL1(REW3TEAM-REW3DSCT,L'REW3TEAM)                     
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
CORETAB  DC    CL8'CORETAB'                                                     
OFFICE   DC    CL6'Office'                                                      
STATION  DC    CL7'Station'                                                     
TEAM     DC    CL4'Team'                                                        
GROUP    DC    CL5'Group'                                                       
GRSB     DC    CL7'Grp/Sub'                                                     
ALL      DC    CL3'All'                                                         
TOTAL    DC    CL9'*Total*'                                                     
QTRTOT   DC    CL9'*Q? Total'                                                   
PERIOD   DC    CL(L'L1PED)'Period'                                              
RUNON    DC    C'Run On '                                                       
TOOBIG   DC    CL10'*OVERFLOW***'                                               
MONTHS   DC    C'JanFebMarAprMayJunJulAugSepOctNovDec'                          
         SPACE 1                                                                
CURBKD   DC    CL(L'L1CURBKD)'CurBkd'                                           
PRIBKD   DC    CL(L'L1PRIBKD)'PriBkd'                                           
CURBIL   DC    CL(L'L1CURBIL)' CurBlg'                                          
PRIBIL   DC    CL(L'L1PRIBIL)' PriBlg'                                          
CURPCT   DC    CL(L'L1CURPCT)'CurPct'                                           
PRIACT   DC    CL(L'L1PRIACT)' PriAct'                                          
ACTPCT   DC    CL(L'L1ACTPCT)'ActPct'                                           
CURUNC   DC    CL(L'L1CURUNC)'CurUnc'                                           
CURDRC   DC    CL(L'L1CURDRC)'CurDrc'                                           
CURBDG   DC    CL(L'L2CURBDG)'CurBdg'                                           
BDGPCT   DC    CL(L'L2BDGPCT)'BdgPct'                                           
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
MYSAVED  DSECT                                                                  
SVKEY    DS    XL(L'IOKEY)                                                      
SVCOLS   DS    X                                                                
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
L1PED    DS    CL9                                                              
         DS    C                                                                
L1CURBKD DS    CL6                                                              
         DS    C                                                                
L1PRIBKD DS    CL6                                                              
         DS    C                                                                
L1CURBIL DS    CL7                                                              
         DS    C                                                                
L1PRIBIL DS    CL7                                                              
         DS    C                                                                
L1CURPCT DS    CL6                                                              
         DS    C                                                                
L1PRIACT DS    CL7                                                              
         DS    C                                                                
L1ACTPCT DS    CL6                                                              
         DS    C                                                                
L1CURUNC DS    CL6                                                              
         DS    C                                                                
L1CURDRC DS    CL6                                                              
         SPACE 2                                                                
LINE2D   DSECT                                                                  
L2PED    DS    CL9                                                              
         DS    C                                                                
L2CURBIL DS    CL7                                                              
         DS    C                                                                
L2PRIBIL DS    CL7                                                              
         DS    C                                                                
L2CURPCT DS    CL6                                                              
         DS    C                                                                
L2PRIACT DS    CL7                                                              
         DS    C                                                                
L2ACTPCT DS    CL6                                                              
         DS    C                                                                
L2CURUNC DS    CL6                                                              
         DS    C                                                                
L2CURDRC DS    CL6                                                              
         DS    C                                                                
L2CURBDG DS    CL6                                                              
         DS    C                                                                
L2BDGPCT DS    CL6                                                              
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
**PAN#1  DC    CL21'015REVIEW23  09/27/05'                                      
         END                                                                    
