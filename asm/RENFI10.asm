*          DATA SET RENFI10    AT LEVEL 172 AS OF 08/31/00                      
*          DATA SET RENFI10    AT LEVEL 171 AS OF 07/06/98                      
*&&      SET   NOP=N                                                            
*PHASE T83110A                                                                  
*INCLUDE DECODE                                                                 
NFI10    TITLE 'FILE MAINTENANCE OVERLAY FOR SPT SYSTEM'                        
NFI10    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,RENFI10*,RA,R7,RR=RE                                           
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LH    R6,=Y(TWUSER-TWAD)                                               
         A     R6,ATWA                                                          
         USING SAVED,R6                                                         
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         L     RE,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(RE)                                          
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
ROUTSN   EQU   (*-ROUTS)/5                                                      
*                                                                               
GSFRP    USING FRPELD,GSFRPEL                                                   
         EJECT                                                                  
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)              *         
*                          - EXPECTS R1 TO HOLD VERB                  *         
***********************************************************************         
         SPACE 1                                                                
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
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
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUB)                                 
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITOK)                                
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTOX VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTOX (RF),(R1),C'LL  ',,DSLISTL                                       
*                                                                               
         L     RF,=V(DECODE)       RELOCATE DECODE                              
         A     RF,BORELO                                                        
         ST    RF,ADECODE                                                       
*                                                                               
         LH    RF,GSDIRDSP         DISPLACEMENT TO DIRECTORY DETAILS            
         A     RF,AXFILTAB                                                      
         ST    RF,RTDSPDIR         RECORD DIRECTORY DETAILS                     
         LH    RF,GSFILDSP         DISPLACEMENT TO DIRECTORY DETAILS            
         A     RF,AXFILTAB                                                      
         ST    RF,RTDSPREC         RECORD FILE DETAILS                          
*                                                                               
         CLI   CSACT,A#ADD                                                      
         BNE   *+8                                                              
         OI    GSINDSL1,GSIXKEY                                                 
*                                                                               
         OI    GCINDS3,GCINOACT    LEAVE ACTIVITY ALONE                         
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
KEY      LM    R0,R2,SVPARMS                                                    
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR KEY OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
KEYFRST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KFKDIS)      DISPLAY                    
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KFKDIS   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  GOTOX =A(VALOPT),RR=BORELO                                             
         BL    EXITL                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
KLKVAL   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  LA    R0,L'LSINIKEY                                                    
         GOTOX ADECODE,BOPARM,((R0),KEYBLK),(0,(R2)),0                          
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         MVC   BOCURSOR,AKEY1                                                   
         B     EXITNV                                                           
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
DATA     ICM   R1,15,SVPARMS2      R1=DATA IDENTIFIER                           
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS4                                                      
         USING FCRRECD,R2                                                       
         XR    R1,R1                                                            
         IC    R1,SVPARMS3         GET GLOBAL VERB                              
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DATA02   LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA05   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA06                                                           
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA05                                                           
*                                                                               
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FCRRECD,R2          R2 HOLDS A(RECORD)                           
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
KNOWTAB  DC    AL2(00085),AL4(LN1DTA)    KEY LINE 1                             
         DC    AL2(00086),AL4(LN2DTA)    KEY LINE 2                             
         DC    AL2(00097),AL4(DSPDTA)    DISPLACEMENT                           
         DC    AL2(00100),AL4(HEXDTA)    LIST HEX INFORMATION                   
         DC    AL2(00150),AL4(DECDTA)    LIST DECIMAL INFORMATION               
*                                                                               
         DC    AL2(00084),AL4(DSKDTA)    DISK ADDRESS                           
         DC    AL2(00103),AL4(DSKDTA)    DISK ADDRESS (COPY KEY)                
         DC    AL2(00003),AL4(LENDTA)    RECORD LENGTH                          
         DC    AL2(00101),AL4(DSP1DTA)   MAINT DISPLACEMENT                     
         DC    AL2(00102),AL4(VEWDTA)    RECORD VIEW TYPE                       
         DC    AL2(00098),AL4(HEX1DTA)   MAINT HEX INFORMATION                  
         DC    AL2(00099),AL4(DEC1DTA)   MAINT DECIMAL INFORMATION              
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
NFI10    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR DISPLAY OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DFDDIS   CLI   FLAG,FF             ONLY ONCE PER TRANSACTION                    
         BE    EXITOK                                                           
         CLI   GSSMPAGE,0          ONLY FOR REAL LIST                           
         BNE   EXITOK                                                           
         TM    GCINDS2,GCIXITS     ONLY FOR XITSES                              
         BZ    EXITOK                                                           
*                                                                               
         MVI   FLAG,FF                                                          
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         L     RF,RTDSPDIR                                                      
         XR    R1,R1               DIRECTORY RECORD LENGTH                      
         IC    R1,NFIKEYL-NFITABD(RF)                                           
         LA    R4,TLFIL(R1)        R4=CONTROL ON TSAR RECORD                    
         LA    R1,2(R1)                                                         
         A     R1,AIOREC           R1=CONTROL ON FILE RECORD                    
         XR    RE,RE                                                            
         IC    RE,NFICTLL-NFITABD(RF)                                           
         BCTR  RE,0                                                             
*                                                                               
         EX    RE,*+4                                                           
         MVC   0(0,R4),0(R1)       SET NEW CONTROL BYTES                        
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR KEY LINE 1                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LN1DTA   LA    RF,LN1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LN1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN1)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETLN1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN1)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFLN1)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLN1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET PROTECTION FOR NESTED FIELDS                                    *         
***********************************************************************         
         SPACE 1                                                                
SETLN1   B     FLTXX                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY LINE 1                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISLN1   MVC   FVIFLD(K_LEN),KEYBLK                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE KEY LINE 1                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALLN1   CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         CLI   FVIFLD,C'?'         KEY BUILD REQUESTED                          
         BNE   VLN102                                                           
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,R#KEY                                                     
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNIPARMS                                               
         LA    R1,BOPARM                                                        
         LA    RE,OSES                                                          
         ST    RE,0(R1)                                                         
         LA    RE,SNTR                                                          
         ST    RE,4(R1)                                                         
         L     RF,AGEN                                                          
         L     RA,ATWA                                                          
         BASR  RE,RF                                                            
         DC    H'0'                                                             
*                                                                               
VLN102   MVC   AKEY1,FVADDR                                                     
         MVC   KEYBLK(K_LEN),FVIFLD                                             
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* VALIDATE KEY LINE 1 AS A FILTER                                     *         
***********************************************************************         
         SPACE 1                                                                
VALFLN1  MVC   AKEY1,FVADDR                                                     
         MVC   FLTIFLD(K_LEN),FVIFLD                                            
         CLI   FVIFLD,C'?'         KEY BUILD REQUESTED                          
         BNE   VFLN102                                                          
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVI   N.SREC,R#KEY                                                     
         MVI   N.SACT,A#LST                                                     
         OI    N.SNINDS1,SNIPARMS                                               
         LA    R1,BOPARM                                                        
         LA    RE,OSES                                                          
         ST    RE,0(R1)                                                         
         LA    RE,SNTR                                                          
         ST    RE,4(R1)                                                         
         L     RF,AGEN                                                          
         L     RA,ATWA                                                          
         BASR  RE,RF                                                            
         DC    H'0'                                                             
*                                                                               
VFLN102  CLC   KEYBLK(K_LEN),FVIFLD                                             
         BE    *+8                                                              
         OI    LSSCIND1,LSSCIFLT                                                
         MVC   KEYBLK(K_LEN),FVIFLD                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO KEY LINE 1 FILTERING                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFLN1   B     FLTXE                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR KEY LINE 2                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LN2DTA   LA    RF,LN2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LN2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN2)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETLN2)                                 
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFLN2)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLN2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET PROTECTION FOR NESTED FIELDS                                    *         
***********************************************************************         
         SPACE 1                                                                
SETLN2   B     FLTXX                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY LINE 2                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISLN2   MVC   FVIFLD(K_LEN),KEYBLK+K_LEN                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE KEY LINE 2                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALLN2   MVC   AKEY2,FVADDR        SAVE A(SECOND LINE)                          
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         MVC   KEYBLK+K_LEN(K_LEN),FVIFLD                                       
         MVI   KEYBLK+(K_LEN*2),C' '                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE KEY LINE 2 AS A FILTER                                     *         
***********************************************************************         
         SPACE 1                                                                
VALFLN2  MVC   AKEY2,FVADDR        SAVE A(SECOND LINE)                          
         MVC   FLTIFLD(K_LEN),FVIFLD                                            
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
*                                                                               
         CLC   KEYBLK+K_LEN(K_LEN),FVIFLD                                       
         BE    *+8                                                              
         OI    LSSCIND1,LSSCIFLT                                                
         MVC   KEYBLK+K_LEN(K_LEN),FVIFLD                                       
         MVI   KEYBLK+(K_LEN*2),C' '                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO KEY LINE 2 FILTERING                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFLN2   B     FLTXE                                                            
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR HEXADECIMAL DATA ON LIST SCREEN                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
HEXDTA   LA    RF,HEXTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HEXTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHEX)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY HEXADECIMAL DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISHEX   LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         MH    R0,=Y(LISWIDTH)                                                  
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         LA    R1,LISWIDTH(R1)     DISP TO END OF THIS LINE                     
         CR    R1,RF               WHOLE LINE REQUIRED?                         
         BH    *+12                NO                                           
         LA    RF,LISWIDTH         SET FULL WIDTH TO MOVE OUT                   
         B     DHEX02                                                           
*                                                                               
         SR    RF,R0                                                            
         BM    EXITOK                                                           
*                                                                               
DHEX02   GOTOX VHEXOUT,BOPARM,(R3),FVIFLD,(RF),0                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLACEMENT                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSPDTA   LA    RF,DSPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISPLACEMENT                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISDSP   LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         BNZ   *+8                                                              
         MVI   FVIFLD+4,C'-'       INDICATES FIRST LINE OF DIRECTORY            
*                                                                               
         MH    R0,=Y(LISWIDTH)     DISPLACEMENT FROM                            
         CURED (R0),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         OC    FVIFLD(4),=CL4'0000'                                             
*                                                                               
         LH    R0,LSROWREP                                                      
         MH    R0,=Y(LISWIDTH)     MAX DISPLACEMENT REACHED                     
         XR    R1,R1                                                            
         IC    R1,TLLEN            LENGTH OF DATA                               
                                                                                
         CR    R1,R0               MORE DATA AFTER THIS?                        
         BH    *+6                 YES                                          
         LR    R0,R1               SET FULL WIDTH TO MOVE OUT                   
         SH    R0,=H'1'                                                         
         CURED (R0),(4,FVIFLD+5),0,DMCB=BOPARM,ZERO=NOBLANK                     
         OC    FVIFLD+5(4),=CL4'0000'                                           
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DECIMAL INFORMATION                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DECDTA   LA    RF,DECTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DECTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDEC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DECIMAL INFORMATION                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISDEC   LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         MH    R0,=Y(LISWIDTH)                                                  
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         LA    R1,LISWIDTH(R1)     DISP TO END OF MOVE BLOCK                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         CR    R1,RF               CAN WE MOVE IT ALL IN?                       
         BH    DDEC02              NO                                           
         MVC   FVIFLD(LISWIDTH),0(R3)                                           
         TR    FVIFLD,TRTABL                                                    
         B     EXITOK                                                           
*                                                                               
DDEC02   SR    RF,R0               LENGTH WE CAN MOVE IN                        
         BNP   EXITOK                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),0(R3)                                                  
         TR    FVIFLD,TRTABL                                                    
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DECIMAL INFORMATION                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
VALDEC   LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         MH    R0,=Y(LISWIDTH)                                                  
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R1               MOVE FROM TO                                 
*                                                                               
         LA    R1,LISWIDTH(R1)     DISP TO END OF MOVE BLOCK                    
*                                                                               
         LA    RE,LISWIDTH         NUMBER OF CHARACTERS TO MOVE IN              
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         CR    R1,RF               CAN WE MOVE IT ALL IN?                       
         BNH   *+8                 YES                                          
         SR    RF,R0                                                            
         LR    RE,RF               LENGTH THAT CAN BE MOVED IN                  
         LA    RF,FVIFLD                                                        
*                                                                               
VDEC06   CLI   0(RF),C'?'          SPECIAL CHARACTER?                           
         BE    *+10                YES - IGNORE                                 
         MVC   0(1,R3),0(RF)                                                    
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,VDEC06                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISK ADDRESS                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSKDTA   LA    RF,DSKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSK)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISK ADDRESS                                                *         
***********************************************************************         
         SPACE 1                                                                
DISDSK   GOTOX VHEXOUT,BOPARM,GSRECDA,FVIFLD,L'GSRECDA,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DISK ADDRESS                                               *         
***********************************************************************         
         SPACE 1                                                                
VALDSK   MVC   ADSK,FVADDR                                                      
         CLI   FVILEN,0                                                         
         BNE   VDSK08                                                           
         CLI   CSACT,A#ADD                                                      
         BNE   EXITNO                                                           
         TM    ADDFLAG,ADD1ST                                                   
         BO    VDSK04                                                           
*                                                                               
         XC    GSRECKEY,GSRECKEY                                                
         XC    GSRECSTA,GSRECSTA                                                
         XC    GSRECDA,GSRECDA                                                  
*                                                                               
VDSK02   GOTO1 AGEN,BOPARM,ORECH,RGET,0                                         
         NI    BCINDS1,255-(BCINREC+BCINACT)                                    
         GOTO1 AGEN,BOPARM,ORECH,RDIS,AIOREC                                    
         OI    BCINDS1,BCINREC+BCINACT                                          
         MVC   FVADDR,ADSK                                                      
         MVC   FVIFLD,BCSPACES                                                  
*                                                                               
         OI    ADDFLAG,ADD1ST                                                   
         MVC   FVMSGNO,=AL2(FVFEKEY)                                            
         XR    RF,RF                                                            
         ICM   RF,3,LS1STINP                                                    
         BZ    EXITL                                                            
         A     RF,ATWA                                                          
         ST    RF,BOCURSOR                                                      
         B     EXITL                                                            
*                                                                               
VDSK04   GOTO1 AGEN,BOPARM,ORECH,RVAL,AIOREC                                    
         GOTO1 AGEN,BOPARM,OLIST,LMNTUPD,0                                      
*                                                                               
         L     RF,RTDSPDIR                                                      
         L     R1,AIOREC                                                        
         XR    RE,RE                                                            
         IC    RE,NFIKEYL-NFITABD(RF)                                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECKEY(0),0(R1)                                                
*                                                                               
VDSK06   NI    ADDFLAG,255-ADD1ST                                               
         MVC   FVADDR,ADSK                                                      
         LA    R0,L'FVIFLD                                                      
         L     R1,FVADDR           RESTORE THIS INTO FVIHDR,ETC                 
         GOTOX ('FLDVAL',AGROUTS),(R1)                                          
         B     EXITOK                                                           
*                                                                               
VDSK08   NI    ADDFLAG,255-ADD1ST                                               
         CLI   SESNL,1                                                          
         BNH   *+8                                                              
         OI    FVATRB,FVAPROT                                                   
         CLI   FVILEN,8            DISK ADDRESS MUST BE LENGTH 8                
         BNE   EXITNV                                                           
*                                                                               
         TM    FVIIND,FVIHEX       DISK ADDRESSES MUST BE HEX                   
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INHEX)                                           
         B     EXITL                                                            
*                                                                               
         GOTOX VHEXIN,BOPARM,FVIFLD,DSKADDR,8,0                                 
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   GSRECDA,DSKADDR                                                  
*                                                                               
         L     RF,RTDSPREC                                                      
         XR    R1,R1                                                            
         IC    R1,NFINUM-NFITABD(RF)                                            
         SLL   R1,8                                                             
         A     R1,=A(XIO11+XOGET)                                               
         MVC   IODAOVER,DSKADDR                                                 
*                                                                               
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    VDSK10                                                           
         TM    IOERR,IOEDEL        DELETED ARE ALLOWED                          
         BO    VDSK10                                                           
         MVC   FVMSGNO,=AL2(EG$INVDA)                                           
         B     EXITL                                                            
*                                                                               
VDSK10   L     R1,AIOREC                                                        
         L     RF,RTDSPDIR                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL-NFITABD(RF)                                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   IOKEY(0),0(R1)      COPY KEY INTO IOKEY                          
*                                                                               
VDSK12   L     RF,RTDSPDIR         NOW TRY TO READ THE THING                    
         XR    R1,R1                                                            
         IC    R1,NFINUM-NFITABD(RF)                                            
         SLL   R1,8                                                             
         A     R1,=A(XORDD)                                                     
         GOTOX ('XIO',AGROUTS)                                                  
                                                                                
         CLI   IOERR,0                                                          
         BE    VDSK14                                                           
         TM    IOERR,IOEDEL                                                     
         BO    VDSK14                                                           
         DC    H'0'                                                             
*                                                                               
VDSK14   MVC   0(L'IOKEY,R2),IOKEY                                              
         MVC   GCLASKEY,GSRECKEY   STOP LEVEL RESET                             
*                                                                               
         TM    GSINDSL1,GSIRCOPY   COPY RECORD NEEDING VALIDATION?              
         BZ    EXITOK              NO                                           
*                                                                               
         GOTO1 AGEN,BOPARM,ORECH,RVAL,AIOREC                                    
         GOTO1 AGEN,BOPARM,OLIST,LMNTUPD,0                                      
*                                                                               
         L     RF,RTDSPDIR                                                      
         L     R1,AIOREC                                                        
         XR    RE,RE                                                            
         IC    RE,NFIKEYL-NFITABD(RF)                                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   GSRECKEY(0),0(R1)                                                
*                                                                               
         MVC   FVADDR,ADSK                                                      
         LA    R0,L'FVIFLD                                                      
         L     R1,FVADDR           RESTORE THIS INTO FVIHDR,ETC                 
         GOTOX ('FLDVAL',AGROUTS),(R1)                                          
         B     EXITOK                                                           
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR VIEW TYPE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
VEWDTA   LA    RF,VEWTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
VEWTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVEW)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVEW)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETVEW)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET RECORD VIEW PROTECTION                                          *         
***********************************************************************         
         SPACE 1                                                                
SETVEW   B     FLTXX                                                            
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD VIEW TYPE                                            *         
***********************************************************************         
         SPACE 1                                                                
DISVEW   MVC   AVIEW,FVADDR                                                     
         MVC   FVIFLD(L'FL@RECD),FL@RECD                                        
         CLI   FVIEW,FVIEWR                                                     
         BE    EXITOK                                                           
         MVC   FVIFLD(L'FL@ELEM),FL@ELEM                                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE RECORD VIEW TYPE                                           *         
***********************************************************************         
         SPACE 1                                                                
VALVEW   MVC   AVIEW,FVADDR                                                     
         MVC   BOBYTE1,FVIEW       SAVE OLD VALUE                               
         CLI   FVILEN,0                                                         
         BNE   VVEW02                                                           
         MVI   FVIEW,FVIEWE        DEFAULT IS ELEMENT                           
         B     VVEW10                                                           
*                                                                               
VVEW02   XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    VVEW04                                                           
         CLC   FVIFLD(0),FU@RECD                                                
         EX    RF,*+8                                                           
         BE    VVEW04                                                           
         CLC   FVIFLD(0),FL@RECD                                                
         B     VVEW06                                                           
*                                                                               
VVEW04   MVI   FVIEW,FVIEWR                                                     
         B     VVEW10                                                           
*                                                                               
VVEW06   EX    RF,*+8                                                           
         BE    VVEW08                                                           
         CLC   FVIFLD(0),FU@ELEM                                                
         EX    RF,*+8                                                           
         BE    VVEW08                                                           
         CLC   FVIFLD(0),FL@ELEM                                                
         B     EXITNV                                                           
*                                                                               
VVEW08   MVI   FVIEW,FVIEWE                                                     
         B     VVEW10                                                           
*                                                                               
VVEW10   CLC   FVIEW,BOBYTE1       VIEW TYPE CHANGED?                           
         BE    EXITOK              NO                                           
         XC    CCOUNT,CCOUNT                                                    
         NI    LSLTIND1,FF-(LSLTIBLD)                                           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR RECORD LENGTH                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
LENDTA   LA    RF,LENTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LENTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECORD LENGTH                                               *         
***********************************************************************         
         SPACE 1                                                                
DISLEN   L     RF,RTDSPREC                                                      
         XR    RE,RE                                                            
         ICM   RE,1,NFIKEYL-NFITABD(RF)                                         
         AR    RE,R2               RE = RECORD LENGTH                           
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,3,0(RE)                                                       
         CURED (RF),(6,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISPLACEMENT ON MAINTENANCE SCREEN                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSP1DTA  LA    RF,DSP1TBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSP1TBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSP1)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISPLACEMENT                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISDSP1  OC    TLUSTAT,TLUSTAT     PARTIAL ACTION FOR THIS LINE?                
         BZ    *+8                 NO                                           
         OI    FVATRB,FVAHIGH                                                   
         OC    TLUSTAT2,TLUSTAT2   NEW LINE?                                    
         BZ    *+8                 NO                                           
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         CLI   TLTYP,TTKEY         PART OF RECORD KEY?                          
         BNE   DDSP102             NO                                           
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         BNZ   *+8                                                              
         MVI   FVIFLD+4,C'*'       INDICATES FIRST LINE OF KEY                  
*                                                                               
         MH    R0,=Y(DISWIDTH)     DISPLACEMENT FROM                            
         CURED (R0),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         OC    FVIFLD(4),=CL4'0000'                                             
*                                                                               
         LH    R0,LSROWREP                                                      
         MH    R0,=Y(DISWIDTH)     MAX DISPLACEMENT REACHED                     
         XR    R1,R1                                                            
         IC    R1,TLLEN            LENGTH OF DATA                               
                                                                                
         CR    R1,R0               MORE DATA AFTER THIS?                        
         BH    *+6                 YES                                          
         LR    R0,R1               SET FULL WIDTH TO MOVE OUT                   
         SH    R0,=H'1'                                                         
         CURED (R0),(4,FVIFLD+5),0,DMCB=BOPARM,ZERO=NOBLANK                     
         OC    FVIFLD+5(4),=CL4'0000'                                           
         B     EXITOK                                                           
*                                                                               
DDSP102  CLI   FVIEW,FVIEWR        RECORD TYPE VIEW?                            
         BNE   DDSP104             NO                                           
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,TLKSNUM                                                     
         SH    R3,=Y(2)            FIRST TSAR RECORD IS KEY                     
         MH    R3,=Y(DISWIDTH)     R3=DISPLACEMENT FROM                         
*                                                                               
         L     RF,RTDSPREC                                                      
         XR    R0,R0                                                            
         IC    R0,NFIDISP-NFITABD(RF)                                           
         AR    R3,R0               ADD LENGTH OF KEY                            
*                                                                               
         CURED (R3),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         OC    FVIFLD(4),=CL4'0000'                                             
*                                                                               
         AH    R3,=Y(DISWIDTH)     DISPLACEMENT TO                              
*                                                                               
         L     RF,RTDSPREC                                                      
         XR    RE,RE                                                            
         IC    RE,NFIKEYL-NFITABD(RF)                                           
         A     RE,AIOREC                                                        
         XR    RF,RF                                                            
         ICM   RF,3,0(RE)          PICK UP RECORD LENGTH                        
         CR    R3,RF                                                            
         BNH   *+6                                                              
         LR    R3,RF                                                            
*                                                                               
         SH    R3,=H'1'                                                         
         CURED (R3),(4,FVIFLD+5),0,DMCB=BOPARM,ZERO=NOBLANK                     
         OC    FVIFLD+5(4),=CL4'0000'                                           
         B     EXITOK                                                           
*                                                                               
DDSP104  LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         BNZ   *+8                                                              
         MVI   FVIFLD+4,C'-'       INDICATES FIRST LINE OF ELEMENT              
*                                                                               
         MH    R0,=Y(DISWIDTH)     DISPLACEMENT FROM                            
         CURED (R0),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         OC    FVIFLD(4),=CL4'0000'                                             
*                                                                               
         LH    R0,LSROWREP                                                      
         MH    R0,=Y(DISWIDTH)     MAX DISPLACEMENT REACHED                     
         XR    R1,R1                                                            
         IC    R1,TLLEN            LENGTH OF DATA                               
                                                                                
         CR    R1,R0               MORE DATA AFTER THIS?                        
         BH    *+6                 YES                                          
         LR    R0,R1               SET FULL WIDTH TO MOVE OUT                   
         SH    R0,=H'1'                                                         
         CURED (R0),(4,FVIFLD+5),0,DMCB=BOPARM,ZERO=NOBLANK                     
         OC    FVIFLD+5(4),=CL4'0000'                                           
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR HEXADECIMAL DATA ON MAINTENANCE SCREEN              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
HEX1DTA  LA    RF,HEX1TBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HEX1TBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISHEX1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHEX1)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY HEXADECIMAL DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISHEX1  OC    TLUSTAT,TLUSTAT     HIGHLIGHT UNRESOLVED ACTIONS                 
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
         OC    TLUSTAT2,TLUSTAT2   HIGHLIGHT INSERTED LINES                     
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         CLI   TLTYP,TTKEY         ALSO HIGHLIGHT KEY                           
         BNE   *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         MH    R0,=Y(DISWIDTH)                                                  
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         LA    R1,DISWIDTH(R1)     DISP TO END OF THIS LINE                     
         CR    R1,RF               WHOLE LINE REQUIRED?                         
         BH    *+12                NO                                           
         LA    RF,DISWIDTH         SET FULL WIDTH TO MOVE OUT                   
         B     DHEX104                                                          
*                                                                               
         SR    RF,R0                                                            
         BM    EXITOK                                                           
*                                                                               
DHEX104  GOTOX VHEXOUT,BOPARM,(R3),FVIFLD,(RF),0                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE HEXADECIMAL DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
VALHEX1  TM    FVIIND,FVIHEX       VALID HEX INPUT?                             
         BO    VHX108              YES                                          
         XR    R0,R0                                                            
         IC    R0,FVILEN           FIND FIRST NON-HEX FIELD                     
         XR    RF,RF                                                            
         LA    R1,FVIFLD                                                        
*                                                                               
VHX102   CLI   0(R1),C'A'                                                       
         BL    VHX106                                                           
         CLI   0(R1),C'F'                                                       
         BNH   VHX104                                                           
*                                                                               
         CLI   0(R1),C'0'                                                       
         BL    VHX106                                                           
         CLI   0(R1),C'9'                                                       
         BH    VHX106                                                           
*                                                                               
VHX104   LA    R1,1(R1)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VHX102                                                        
         DC    H'0'                                                             
*                                                                               
VHX106   MVC   FVMSGNO,=AL2(GE$INHEX)                                           
         STC   RF,FVERRNDX                                                      
         B     EXITL                                                            
*                                                                               
VHX108   CLI   TLTYP,TTKEY         PART OF RECORD KEY?                          
         BNE   VHX124              NO                                           
         CLI   CSACT,A#CPY                                                      
         BE    VHX124                                                           
         CLI   CSACT,A#ADD                                                      
         BE    VHX124                                                           
         XC    BOWORK1,BOWORK1                                                  
         LH    R1,LSROWREP         REPEAT COUNT                                 
         MH    R1,=Y(DISWIDTH)     DISP TO END OF THIS LINE                     
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         CR    R1,RF               WHOLE LINE WILL FIT?                         
         BH    *+12                NO                                           
         LA    RF,DISWIDTH         SET FULL WIDTH TO MOVE IN                    
         B     VHX110                                                           
*                                                                               
         SH    R1,=Y(DISWIDTH)     R1=DISP TO START OF LINE                     
         SR    RF,R1                                                            
         BP    *+6                                                              
         DC    H'0'                ODD LENGTH                                   
*                                                                               
VHX110   STH   RF,BOHALF1          SAVE LENGTH MOVED                            
         SLL   RF,1                HEX SOURCE IS TWICE AS LONG                  
         GOTOX VHEXIN,BOPARM,FVIFLD,BOWORK1,(RF),0                              
*                                                                               
         LH    R1,LSROWREP         REPEAT COUNT                                 
         BCTR  R1,0                                                             
         MH    R1,=Y(DISWIDTH)     DISP TO START OF THIS LINE                   
         LA    R3,TLFIL                                                         
         AR    R3,R1               R3=MOVE INTO POINT                           
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R0,R0                                                            
         IC    R0,NFIKEYL-NFITABD(RF)                                           
         CR    R1,R0                                                            
         BH    VHX116              START IS PAST END OF KEY                     
         SR    R0,R1                                                            
         BZ    VHX116              START IS AT END OF KEY                       
*                                                                               
         LA    RF,BOWORK1                                                       
VHX112   CLC   0(1,RF),0(R3)       COPMPARE KEY BYTES WITH SCREEN               
         BNE   VHX114                                                           
         LA    RF,1(RF)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,VHX112                                                        
         B     VHX116              KEY IS UNCHANGED                             
*                                                                               
VHX114   MVC   FVMSGNO,=AL2(GE$CCKEY)                                           
         LA    RE,BOWORK1                                                       
         SR    RF,RE                                                            
         SLL   RF,1                2X FOR DISPLAY                               
         STC   RF,FVERRNDX                                                      
         B     EXITL                                                            
*                                                                               
VHX116   L     RF,RTDSPDIR                                                      
         XR    RE,RE               KEY LENGTH                                   
         IC    RE,NFIKEYL-NFITABD(RF)                                           
         LA    RE,2(RE)            +2 FOR LENGTH IN RECORD                      
         XR    R0,R0               CONTROL LENGTH                               
         IC    R0,NFICTLL-NFITABD(RF)                                           
         AR    RE,R0               RE=DISP TO DISK ADDRESS                      
*                                                                               
         LH    R0,LSROWREP                                                      
         BCTR  R0,0                                                             
         MH    R0,=Y(DISWIDTH)                                                  
         AH    R0,BOHALF1          R1=DISP TO LAST CHARACTER MOVED              
*                                                                               
         CR    RE,R0                                                            
         BH    VHX122              END BEFORE DISK ADDRESS                      
         SR    R0,RE                                                            
         BZ    VHX122              END AT DISK ADDRESS                          
*                                                                               
         LH    RF,BOHALF1                                                       
         SR    RF,R0                                                            
         LA    RF,BOWORK1(RF)      RF=START OF PORTION IN D/A                   
         LA    R3,TLFIL                                                         
         AR    R3,RE               R3=START OF D/A ON TSAR RECORD               
*                                                                               
VHX118   CLC   0(1,RF),0(R3)       COPMPARE KEY BYTES WITH SCREEN               
         BNE   VHX120                                                           
         LA    RF,1(RF)                                                         
         LA    R3,1(R3)                                                         
         BCT   R0,VHX118                                                        
         B     VHX122              KEY IS UNCHANGED                             
*                                                                               
VHX120   MVC   FVMSGNO,=AL2(GE$CCKEY)                                           
         LA    RE,BOWORK1                                                       
         SR    RF,RE                                                            
         SLL   RF,1                2X FOR DISPLAY                               
         STC   RF,FVERRNDX                                                      
         B     EXITL                                                            
*                                                                               
VHX122   LH    R1,LSROWREP         REPEAT COUNT                                 
         BCTR  R1,0                                                             
         MH    R1,=Y(DISWIDTH)     DISP TO START OF THIS LINE                   
         LA    R3,TLFIL                                                         
         AR    R3,R1               R3=MOVE INTO POINT                           
         LH    R1,BOWORK1                                                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     EXITOK                                                           
         MVC   0(0,R3),BOWORK1                                                  
*                                                                               
VHX124   LH    R0,LSROWREP         REPEAT COUNT                                 
         SH    R0,=H'1'                                                         
         BNZ   VHX126                                                           
         TM    TLUSTAT,TLUSINS     INSERTING AT THIS RECORD?                    
         BZ    VHX126                                                           
         GOTOX VHEXIN,BOPARM,FVIFLD,TLINS,4,0                                   
         B     EXITOK                                                           
*                                                                               
VHX126   MH    R0,=Y(DISWIDTH)                                                  
         LR    R1,R0               DISP TO START OF THIS BLOCK                  
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM TO                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         LA    R1,DISWIDTH(R1)     DISP TO END OF THIS LINE                     
         CR    R1,RF               WHOLE LINE REQUIRED?                         
         BH    *+12                NO                                           
         LA    RF,DISWIDTH         SET FULL WIDTH TO MOVE IN                    
         B     VHX128                                                           
*                                                                               
         SR    RF,R0                                                            
         BP    *+6                                                              
         DC    H'0'                ODD LENGTH                                   
*                                                                               
VHX128   SLL   RF,1                HEX SOURCE IS TWICE AS LONG                  
         GOTOX VHEXIN,BOPARM,FVIFLD,(R3),(RF),0                                 
*                                                                               
         CLI   FVIEW,FVIEWE        IN ELEMENT MODE?                             
         BNE   EXITOK              NO                                           
         CLI   TLTYP,TTELEM        IS THIS AN ELEMENT?                          
         BNE   EXITOK              NO                                           
*                                                                               
         CLI   TLFIL+1,1           MAKE SURE LENGTH IS VALID                    
         BH    *+12                                                             
         MVI   FVERRNDX,2                                                       
         B     EXITNV                                                           
*                                                                               
         CLC   TLLEN,TLFIL+1       IS LENGTH SAME?                              
         BE    EXITOK              YES                                          
*                                                                               
         XC    BOELEM,BOELEM                                                    
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   BOELEM(0),TLFIL     SAVE OFF CURRENT ELEMENT                     
*                                                                               
         MVC   TLLEN,TLFIL+1       SET NEW LENGTH                               
         XC    TLFIL(256),TLFIL    CLEAR ELEMENT AREA IN TSAR RECORD            
         IC    RF,TLLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TLFIL(0),BOELEM     SET ELEMENT WITH CORRECT LENGTH              
*                                                                               
         LA    RF,1(RF)                                                         
         AH    RF,=Y(TLLENQ)                                                    
         STCM  RF,3,TLRLEN         ADJUST TSAR RECORD LENGTH                    
*                                                                               
         OI    DOFLAG,DOROWS       SET RECALCULATE ROWS & SIZES                 
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DECIMAL INFORMATION ON MAINTENANCE SCREEN           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DEC1DTA  LA    RF,DEC1TBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DEC1TBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEC1)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDEC1)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DECIMAL INFORMATION                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISDEC1  OC    TLUSTAT,TLUSTAT                                                  
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
         OC    TLUSTAT2,TLUSTAT2                                                
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         CLI   TLTYP,TTKEY         ALSO HIGHLIGHT KEY                           
         BNE   *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         MH    R0,=Y(DISWIDTH)                                                  
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         LA    R1,DISWIDTH(R1)     DISP TO END OF MOVE BLOCK                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         CR    R1,RF               CAN WE MOVE IT ALL IN?                       
         BH    DDEC102             NO                                           
         MVC   FVIFLD(DISWIDTH),0(R3)                                           
         TR    FVIFLD,TRTABL                                                    
         B     EXITOK                                                           
*                                                                               
DDEC102  SR    RF,R0               LENGTH WE CAN MOVE IN                        
         BNP   EXITOK                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   FVIFLD(0),0(R3)                                                  
         TR    FVIFLD,TRTABL                                                    
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DECIMAL INFORMATION                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
VALDEC1  LH    R0,LSROWREP                                                      
         SH    R0,=H'1'                                                         
         MH    R0,=Y(DISWIDTH)                                                  
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R1               MOVE FROM TO                                 
*                                                                               
         LA    R1,DISWIDTH(R1)     DISP TO END OF MOVE BLOCK                    
*                                                                               
         LA    RE,DISWIDTH         NUMBER OF CHARACTERS TO MOVE IN              
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         CR    R1,RF               CAN WE MOVE IT ALL IN?                       
         BNH   *+8                 YES                                          
         SR    RF,R0                                                            
         LR    RE,RF               LENGTH THAT CAN BE MOVED IN                  
         LA    RF,FVIFLD                                                        
*                                                                               
VDEC106  CLI   0(RF),C'?'          SPECIAL CHARACTER?                           
         BE    *+10                YES - IGNORE                                 
         MVC   0(1,R3),0(RF)                                                    
*                                                                               
         LA    RF,1(RF)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,VDEC106                                                       
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                       *         
* -------------                                                       *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS A(INITIAL LIST KEY)                                        *         
* P4 HOLDS A(MEMORY COVERED BY FESD)                                  *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING FCRRECD,R2                                                       
         USING FESD,R3                                                          
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(XITOUT)                              
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER (EXIT FROM)                *         
***********************************************************************         
         SPACE 1                                                                
XITOUT   B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (ENTER IN)               *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (EXIT BACK INTO)         *         
***********************************************************************         
         SPACE 1                                                                
XITIN    L     R3,ATWA                                                          
         AH    R3,GSDSPOVR         FIRST FIELD ON OVERLAY SCREEN                
         USING FHD,R3                                                           
         XR    R5,R5                                                            
*                                                                               
XIN02    ICM   R5,1,FHLN           END OF TWA?                                  
         BZ    EXITOK              NO                                           
         TM    FHAT,FHATXH         INPUT FIELD?                                 
         BZ    XIN06               NO                                           
*                                                                               
         LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       COPY ACROSS EXTENDED FIELD HEADER            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2                                                     
         BZ    XIN06               NOT ONE OF OURS                              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,AFDRADDR                                                      
         ICM   RF,15,0(RF)                                                      
*                                                                               
         CLC   =Y(00085),FDRNUM-FDRELD(RF)                                      
         BNE   XIN04               NOT KEY LINE 1                               
*                                                                               
         LR    RF,R5                                                            
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         OI    FHOI,FHOITR                                                      
         EX    RF,*+4                                                           
         MVC   FHDA(0),KEYBLK                                                   
         LA    RF,1(RF)                                                         
         STH   RF,BOHALF1                                                       
         B     XIN06                                                            
*                                                                               
XIN04    CLC   =Y(00086),FDRNUM-FDRELD(RF)                                      
         BNE   XIN06               NOT KEY LINE 2                               
         LR    RF,R5                                                            
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         LH    RE,BOHALF1                                                       
         LA    RE,KEYBLK(RE)                                                    
         OI    FHOI,FHOITR                                                      
         EX    RF,*+4                                                           
         MVC   FHDA(0),0(RE)                                                    
         B     XIN06                                                            
*                                                                               
XIN06    LA    R3,0(R5,R3)                                                      
         B     XIN02                                                            
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
* P5 HOLDS EQUATE FOR IOREC IF REDEFINED                              *         
***********************************************************************         
         SPACE 1                                                                
LIST     LM    R0,R3,SVPARMS                                                    
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
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(LLSTLAST),AL1(0,0,0),AL4(LSTLAST)                            
*                                                                               
         DC    AL1(LINIT),AL1(0,0,1),AL4(ILST1)                                 
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LSCRLAST),AL1(0,0,1),AL4(SCRLAST1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ILST     OI    LSSTAT3,LS3RFIX                                                  
         MVI   LSSUBLEN,SBLEN                                                   
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R0,R0                                                            
         ICM   R0,3,NFIMAXL-NFITABD(RF)                                         
         SRDL  R0,32                                                            
         LA    RE,LISWIDTH                                                      
         DR    R0,RE                                                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STCM  R1,3,LSROWLIN    SET REQUIRED NUMBER OF LINES                    
*                                                                               
         MVC   LSNUMHED,=AL2(1)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     L     RF,IOLOOP           NUMBER OF READS                              
         LA    RF,1(RF)                                                         
         ST    RF,IOLOOP                                                        
         NI    GSINDSL3,FF-(GSI2LMSG)                                           
*                                                                               
         MVC   IOKEY,0(R2)                                                      
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFINUM-NFITABD(RF)                                            
         SLL   R1,8                                                             
         A     R1,SVPARMS5                                                      
         A     R1,=A(XOHID)                                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     L     RF,IOLOOP           INCREMENT READ COUNT                         
         LA    RF,1(RF)                                                         
         ST    RF,IOLOOP                                                        
         CLC   RECNUM,IOLOOP       I WON'T LET YOU DO MORE THAN THIS            
         BL    EXITL                                                            
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFINUM-NFITABD(RF)                                            
         SLL   R1,8                                                             
         A     R1,SVPARMS5                                                      
         A     R1,=A(XOSQD)                                                     
         GOTOX ('XIO',AGROUTS)                                                  
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
*                                                                               
NLST02   CLI   SCANLEN,0           ANYTHING TO COMPARE FOR?                     
         BE    NLST04              NO                                           
         BAS   RE,SCANREC                                                       
         BE    NLST04                                                           
*                                                                               
         L     RF,IOLOOP           MESSAGE EVERY OUTNUM RECORDS                 
         LH    R0,OUTNUM                                                        
         XR    RE,RE                                                            
         DR    RE,R0                                                            
         LTR   RE,RE                                                            
         BNZ   NLST                                                             
*                                                                               
         BAS   RE,PROGRESS                                                      
         B     NLST                                                             
*                                                                               
NLST04   MVC   0(L'IOKEY,R2),IOKEY                                              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
LSTLAST  CLC   RECNUM,IOLOOP       TOO MANY I/O'S                               
         BH    EXITOK              NO                                           
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R0,R0                                                            
         IC    R0,NFIKEYL-NFITABD(RF)                                           
         GOTOX VHEXOUT,BOPARM,IOKEY,KEYBLK,(R0),0                               
         NI    LSLTIND1,FF-(LSLTIBLD+LSLTIEOL)                                  
*                                                                               
         L     R3,ATWA                                                          
         AH    R3,GSDSPOVR         FIRST FIELD ON OVERLAY SCREEN                
         USING FHD,R3                                                           
         XR    R5,R5                                                            
*                                                                               
LLST02   ICM   R5,1,FHLN           END OF TWA?                                  
         BNZ   LLST03              YES                                          
         OI    GSINDSL3,GSI2LMSG                                                
         MVC   FVMSGNO,=AL2(FVFIOLIM)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITL                                                            
*                                                                               
LLST03   TM    FHAT,FHATXH         INPUT FIELD?                                 
         BZ    LLST06              NO                                           
*                                                                               
         LA    RF,FHD(R5)                                                       
         SH    RF,=Y(FHDAD)                                                     
         MVC   FVIXHDR,0(RF)       COPY ACROSS EXTENDED FIELD HEADER            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2                                                     
         BZ    LLST06              NOT ONE OF OURS                              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,AFDRADDR                                                      
         ICM   RF,15,0(RF)                                                      
*                                                                               
         CLC   =Y(00085),FDRNUM-FDRELD(RF)                                      
         BNE   LLST04              NOT KEY LINE 1                               
*                                                                               
         LR    RF,R5                                                            
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         OI    FHOI,FHOITR                                                      
         EX    RF,*+4                                                           
         MVC   FHDA(0),KEYBLK                                                   
         LA    RF,1(RF)                                                         
         STH   RF,BOHALF1                                                       
         B     LLST06                                                           
*                                                                               
LLST04   CLC   =Y(00086),FDRNUM-FDRELD(RF)                                      
         BNE   LLST06              NOT KEY LINE 2                               
         LR    RF,R5                                                            
         SH    RF,=Y(FHDAD+FHDAD+1)                                             
         LH    RE,BOHALF1                                                       
         LA    RE,KEYBLK(RE)                                                    
         OI    FHOI,FHOITR                                                      
         EX    RF,*+4                                                           
         MVC   FHDA(0),0(RE)                                                    
         B     LLST06                                                           
*                                                                               
LLST06   LA    R3,0(R5,R3)                                                      
         B     LLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM DIRECTORY RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R3                                                         
TSARDIR  LM    R2,R3,SVPARMS3                                                   
         MVI   TLTYP,TTKEY         SET DISPLAYING KEY                           
         L     RF,RTDSPDIR                                                      
         XR    R1,R1               DIRECTORY RECORD LENGTH                      
         ICM   R1,3,NFIMAXL-NFITABD(RF)                                         
         STC   R1,TLLEN            SET LENGTH OF KEY                            
         AH    R1,=Y(TLLENQ)                                                    
         STCM  R1,3,TLRLEN         SET LENGTH OF TSAR RECORD                    
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R0,R0                                                            
         ICM   R0,3,NFIMAXL-NFITABD(RF)                                         
         SRDL  R0,32                                                            
         LA    RE,LISWIDTH                                                      
         DR    R0,RE                                                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,TLROWS           SET REQUIRED NUMBER OF LINES                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TLFIL(0),0(R2)                                                   
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* INITIALISE LIST 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
ILST1    MVI   LSSUBLEN,SBLEN                                                   
         MVI   LSSUBHLP,99                                                      
         OI    LSSTAT3,LS3RVAR                                                  
         OI    LSSTAT1,LSSBALL                                                  
         NI    LSSTAT2,FF-LSSADD                                                
         OI    LSSTAT2,LSSXSPAC                                                 
         MVC   LSNUMHED,=AL2(1)                                                 
         XC    CCOUNT,CCOUNT                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST PAGE 1                                          *         
***********************************************************************         
         SPACE 1                                                                
FLST1    XC    LINENOW,LINENOW                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST PAGE 1                                                *         
***********************************************************************         
         SPACE 1                                                                
NLST1    OC    LINENOW,LINENOW     FIRST TIME IN NLST?                          
         BNZ   NLS110              NO                                           
         CLI   FVIEW,FVIEWR        VIEW IN RECORD MODE                          
         BE    NLS108              YES                                          
*                                                                               
         L     RF,RTDSPREC         CHECK ELEMENTS ARE IN ORDER                  
         XR    R1,R1                                                            
         IC    R1,NFIKEYL-NFITABD(RF)                                           
         A     R1,AIOREC                                                        
         XR    R0,R0                                                            
         ICM   R0,3,0(R1)          R0=RECORD LENGTH                             
*                                                                               
         XR    R1,R1                                                            
         IC    R1,NFIDISP-NFITABD(RF)                                           
         A     R1,AIOREC           R1=FIRST ELEMENT                             
         XR    RE,RE                                                            
*                                                                               
NLS102   LR    RF,R1               SEE IF REACHED END OF RECORD                 
         S     RF,AIOREC                                                        
         CR    RF,R0                                                            
         BE    NLS108                                                           
         BH    NLS106              GONE PAST END OF RECORD                      
*                                                                               
         CLI   0(R1),0             REACHED AN END POINT                         
         BNE   NLS104                                                           
*                                                                               
         LA    RF,1(R1)            SEE IF THIS 0 IS AT RECORD END               
         S     RF,AIOREC                                                        
         CR    RF,R0                                                            
         BE    NLS108              YES - STRUCTURE IS OK                        
*                                                                               
NLS104   ICM   RE,1,1(R1)                                                       
         BZ    NLS106              ZERO LENGTH ELEMENT IS INVALID               
         AR    R1,RE                                                            
         B     NLS102                                                           
*                                                                               
NLS106   MVI   FVIEW,FVIEWR        SET VIEW TO RECORD MODE                      
         ICM   RF,15,AVIEW                                                      
         BZ    NLS112                                                           
         MVC   FHDAD(L'FL@RECD,RF),FL@RECD                                      
         OI    FHOID(RF),FHOITR                                                 
*                                                                               
NLS108   L     RF,RTDSPREC         SET DISPLACEMENT TO FIRST DATA               
         XR    RE,RE                                                            
         IC    RE,NFIDISP-NFITABD(RF)                                           
         STH   RE,LINENOW                                                       
         B     EXITOK              SET DISPLACEMENT TO FIRST ELEMENT            
*                                                                               
*                              *** NOT FIRST TIME FOR NLST                      
*                                                                               
NLS110   CLI   FVIEW,FVIEWR        VIEW IS IN RECORD MODE?                      
         BE    NLS112              YES                                          
*                                                                               
         LH    RF,LINENOW          PROCESS ELEMENT FORMAT                       
         A     RF,AIOREC                                                        
         CLI   0(RF),0             PROCESSED LAST BYTE OF RECORD?               
         BE    EXITL               YES                                          
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,1(RF)                                                       
         BZ    EXITL               ZERO LENGTH ELEMENT                          
*                                                                               
         AR    RF,RE                                                            
         S     RF,AIOREC                                                        
         STH   RF,LINENOW                                                       
         B     EXITOK                                                           
*                                                                               
NLS112   L     RF,RTDSPDIR         PROCESS RECORD FORMAT                        
         XR    R1,R1                                                            
         IC    R1,NFIKEYL-NFITABD(RF)                                           
         A     R1,AIOREC                                                        
         XR    RF,RF                                                            
         ICM   RF,3,0(R1)          RECORD LENGTH                                
*                                                                               
         LH    RE,LINENOW          CURRENT DISPLACEMENT INTO THE RECORD         
         LA    RE,DISWIDTH(RE)                                                  
         STH   RE,LINENOW                                                       
*                                                                               
         CR    RE,RF                                                            
         BH    EXITL               ALREADY PAST THE END OF THE RECORD           
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM FILE RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,SVPARMS4                                                      
         USING TLSTD,R3                                                         
         L     R2,AIOREC                                                        
*                                                                               
         LH    RF,CCOUNT                                                        
         LA    RF,1(RF)                                                         
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        SET LINE NUMBER IN KEY                       
*                                                                               
         OC    LINENOW,LINENOW     FIRST LINE?                                  
         BNZ   TSAF102             NO                                           
*                                                                               
         MVI   TLTYP,TTKEY         SET DISPLAYING KEY                           
         L     RF,RTDSPREC                                                      
         XR    R1,R1                                                            
         IC    R1,NFIDISP-NFITABD(RF)                                           
         STC   R1,TLLEN            SET LENGTH OF KEY+CONTROL,ETC                
         AH    R1,=Y(TLLENQ)                                                    
         STCM  R1,3,TLRLEN         SET LENGTH OF TSAR RECORD                    
*                                                                               
         L     RF,RTDSPREC                                                      
         XR    RE,RE                                                            
         IC    RE,NFIDISP-NFITABD(RF)                                           
         SRDL  RE,32                                                            
         LA    R1,DISWIDTH         WIDTH OF A SINGLE LINE                       
         DR    RE,R1                                                            
         LTR   RE,RE               REMAINDER?                                   
         BZ    *+8                 NO                                           
         LA    RF,1(RF)            DON'T FORGET THE BIT ON THE END              
         STC   RF,TLROWS           NUMBER OF LINES REQUIRED                     
*                                                                               
         L     RF,RTDSPREC                                                      
         XR    RE,RE                                                            
         IC    RE,NFIDISP-NFITABD(RF)                                           
         BCTR  RE,0                                                             
         L     RF,AIOREC                                                        
         EX    RE,*+4                                                           
         MVC   TLFIL(0),0(RF)                                                   
         B     EXITOK                                                           
*                                                                               
TSAF102  CLI   FVIEW,FVIEWR        VIEW IS IN RECORD MODE?                      
         BNE   TSAF106             NO                                           
         MVI   TLTYP,TTREC         RECORD MODE                                  
         MVI   TLROWS,1                                                         
*                                                                               
         L     RF,RTDSPDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFIKEYL-NFITABD(RF)                                           
         A     R1,AIOREC                                                        
         XR    RE,RE                                                            
         ICM   RE,3,0(R1)          RECORD LENGTH                                
*                                                                               
         LH    RF,LINENOW                                                       
         LA    RF,DISWIDTH(RF)     END OF MOVE BLOCK                            
         CR    RE,RF               WILL WHOLE LINE FIT?                         
         BL    *+12                NO                                           
         LA    R1,DISWIDTH                                                      
         B     TSAF104                                                          
*                                                                               
         SH    RF,=Y(DISWIDTH)                                                  
         SR    RE,RF                                                            
         LR    R1,RE               PARTIAL LINE LENGTH TO SAVE                  
*                                                                               
TSAF104  STC   R1,TLLEN                                                         
         LR    RF,R1                                                            
         AH    R1,=Y(TLLENQ)                                                    
         STCM  R1,3,TLRLEN         SET LENGTH TO SAVE                           
         BCTR  RF,0                                                             
         LH    RE,LINENOW                                                       
         A     RE,AIOREC                                                        
         EX    RF,*+4                                                           
         MVC   TLFIL(0),0(RE)                                                   
         B     EXITOK                                                           
*                                                                               
TSAF106  MVI   TLTYP,TTELEM        SET DISPLAYING ELEMENT                       
         LH    RF,LINENOW                                                       
         A     RF,AIOREC           POINT TO ELEMENT                             
         XR    R1,R1                                                            
         ICM   R1,1,1(RF)          LENGTH OF ELEMENT                            
         BNZ   *+8                                                              
         LA    R1,1                FOR '00' ON END OF RECORD                    
         STC   R1,TLLEN                                                         
         LR    RE,R1                                                            
         SRDL  RE,32               SAVE ELEMENT LENGTH                          
*                                                                               
         AH    R1,=Y(TLLENQ)                                                    
         STCM  R1,3,TLRLEN         SET NEW TSAR RECORD LENGTH                   
*                                                                               
         LA    R1,DISWIDTH         LENGTH OF A SINGLE LINE                      
         DR    RE,R1                                                            
         LTR   RE,RE               EXTRA BIT ON END?                            
         BZ    *+8                 NO                                           
         LA    RF,1(RF)                                                         
         STC   RF,TLROWS           SAVE LINES REQUIRED FOR THIS ELEMENT         
*                                                                               
         LH    RF,LINENOW          POINT BACK TO ELEMENT                        
         A     RF,AIOREC                                                        
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,1,1(RF)          LENGTH OF ELEMENT                            
         BNZ   *+8                                                              
         LA    RE,1                                                             
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   TLFIL(0),0(RF)                                                   
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* LAST FOR SCREEN - PAGE 1                                            *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST1 MVC   GS1STREC,LS1STLIN                                                
         TM    DOFLAG,DOFROM+DOTO                                               
         BO    SLAST02             FROM & TO ACTIONS BOTH SET                   
         BNZ   SLAST06             EITHER FROM OR TO ACTION SET                 
*                                                                               
         TM    DOFLAG,DODEL+DOREP+DOINS+DOPST                                   
         BNZ   SLAST02             ACTION SET                                   
         TM    DOFLAG,DOROWS       RECALCULATE ROWS                             
         BO    SLAST04                                                          
         B     SLAST12             NO ACTIONS SET                               
*                                                                               
SLAST02  BAS   RE,SORT             PROVIDE CODE TO RESOLVE RESULTS              
         BE    SLAST04                                                          
*                                                                               
         BAS   RE,REACT            ACTIONS NEED REDRAWING                       
         MVC   BOCURSOR,AFRSTFR    MOVE/COPY BEFORE/AFTER CONFLICT              
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL                                                            
*                                                                               
SLAST04  BAS   RE,REDRAW                                                        
         OI    GCINDS2,GCIANYCH    SET CHANGES THIS TIME                        
         B     SLAST12                                                          
*                                                                               
SLAST06  BAS   RE,REACT            ACTIONS NEED REDRAWING                       
*                                                                               
         CLC   DOFRCNT,=H'1'       TEST DUPLICATE FROM FIELDS                   
         BNH   SLAST08             NO - GOOD                                    
         MVC   BOCURSOR,AFRSTFR                                                 
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL               SET COMMAND CONFLICT                         
*                                                                               
SLAST08  CLC   DOTOCNT,=H'1'       TEST DUPLICATE TO FIELDS                     
         BNH   SLAST10             NO - GOOD                                    
         MVC   BOCURSOR,AFRSTTO                                                 
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL                                                            
*                                                                               
SLAST10  MVC   FVMSGNO,=AL2(GE$MCIP)                                            
*                                                                               
SLAST12  OC    AINSFLD,AINSFLD      WANT TO BUNG CURSOR ON INSERT LINE?         
         BZ    *+10                 NO                                          
         MVC   BOCURSOR,AINSFLD     DO IT BEAVIS...                             
*                                                                               
         CLC   FVMSGNO,=AL2(GE$MCIP)                                            
         BNE   EXITOK                                                           
         B     EXITL               SETTING OWN MESSAGE                          
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD FROM TSAR RECORDS - FIRST                             *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 XC    UPDNOW,UPDNOW                                                    
         XC    UPDFLG,UPDFLG                                                    
         L     RE,AIOREC                                                        
         LA    RF,512                                                           
         XR    R0,R0                                                            
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* UPDATE RECORD FROM TSAR RECORDS                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R3                                                         
UPDREC1  LM    R2,R3,SVPARMS3                                                   
         CLI   TLTYP,TTKEY         KEY ELEMENT MUST BE FIRST                    
         BNE   URC102                                                           
         MVI   UPDFLG,C'K'         SET HAVE A KEY ELEMENT                       
*                                                                               
         L     RF,RTDSPREC                                                      
         XR    RE,RE                                                            
         IC    RE,NFIDISP-NFITABD(RF)                                           
         STH   RE,UPDNOW                                                        
*                                                                               
         L     R1,AIOREC                                                        
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,R1),TLFIL       COPY KEY INTO I/O AREA                       
         LA    RE,2(RE)            GET L' OF EMPTY RECORD + 1                   
         XR    R1,R1                                                            
         IC    R1,NFIKEYL-NFITABD(RF)                                           
         A     R1,AIOREC                                                        
         STCM  RE,3,0(R1)          SET LENGTH OF RECORD = EMPTY                 
*                                                                               
         CLI   CSACT,A#DEL                                                      
         BNE   URC101                                                           
         XR    R1,R1                                                            
         IC    R1,NFIKEYL-NFITABD(RF)                                           
         LA    R1,TLFIL+2(R1)                                                   
         OI    0(R1),X'80'         FLAG DELETED ON TSAR RECORD                  
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         B     URC101A                                                          
*                                                                               
URC101   CLI   CSACT,A#RES                                                      
         BNE   URC101A                                                          
         XR    R1,R1                                                            
         IC    R1,NFIKEYL-NFITABD(RF)                                           
         LA    R1,TLFIL+2(R1)                                                   
         NI    0(R1),255-X'80'     FLAG NOT DELETED ON TSAR RECORD              
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
URC101A  B     EXITOK                                                           
*                                                                               
URC102   CLI   UPDFLG,C'K'         SET HAVE A KEY ELEMENT                       
         BE    *+6                                                              
         DC    H'0'                WHERE IS KEY?                                
*                                                                               
         CLI   TLLEN,2             IGNORE DUMMY ELEMENTS                        
         BL    EXITOK                                                           
         CLI   TLTYP,TTREC         BLOCKED RECORD DISPLAY?                      
         BE    URC104              YES                                          
         CLI   TLTYP,TTELEM        ELEMENT DISPLAY?                             
         BE    *+6                 YES                                          
         DC    H'0'                                                             
*                                                                               
         GOTOX VHELLO,BCPARM,(C'P',GCFILNAM),AIOREC,TLFIL,=C'ADD=END'           
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
URC104   L     R2,AIOREC                                                        
         LH    RF,UPDNOW                                                        
         AR    RF,R2           A(NEXT DATA BLOCK)                               
*                                                                               
         XR    RE,RE                                                            
         IC    RE,TLLEN                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),TLFIL   COPY IN DATA                                     
*                                                                               
         LA    RF,1(RE,RF)                                                      
         MVI   0(RF),0             ENSURE RECORD ENDS WITH X'00'                
*                                                                               
         SR    RF,R2                                                            
         STH   RF,UPDNOW           SET NEXT DATA BLOCK                          
*                                                                               
         L     RE,RTDSPDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFIKEYL-NFITABD(RE)                                           
         AR    R1,R2                                                            
         STCM  RF,3,0(R1)          SET NEW RECORD LENGTH                        
*                                                                               
         XR    R1,R1                                                            
         IC    R1,NFICTLL-NFITABD(RE)                                           
         XR    RF,RF                                                            
         IC    RF,NFIKEYL-NFITABD(RE)                                           
         LA    RF,2(RF,R2)         THE 2 IS FOR THE LENGTH ITSELF               
*                                                                               
         BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   GSRECSTA(0),0(RF)                                                
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SUB ACTION OBJECT                                                   *         
* P3 = A(SUB-ACTION FIELD)                                            *         
***********************************************************************         
         SPACE 1                                                                
SUB      OC    GSSMPAGE,GSSMPAGE   VALIDATION ONLY FOR MAINT LIST               
         BZ    EXITH                                                            
         TM    LSSCIND2,LSSCIDIS   IGNORE IF WE ARE DISPLAYING                  
         BO    EXITOK                                                           
*                                                                               
         LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,SUBTABL                                                       
         B     ITER                                                             
*                                                                               
SUBTABL  DC    AL1(SAVAL),AL1(0,0,0),AL4(SUBVAL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUB ACTION FOR LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
SNGL     USING SUBACTSD,LSNTRY                                                  
MLTI     USING SUBACTSD,LMNTRY                                                  
SUBVAL   XC    LSNTRY,LSNTRY       CLEAR CURRENT SUB-ACTION ELEMENT             
         OI    LSSCIND1,LSSCIINP   KEEP CURRENT SCREEN                          
         L     R2,SVPARMS3         R2=A(SUB-ACTION FIELD)                       
         USING FHD,R2                                                           
         L     R4,ATLST            R4=A(TSAR RECORD)                            
         USING TLSTD,R4                                                         
*                                                                               
         OC    TLUSTAT,TLUSTAT     ANY ACTIONS FOR THIS RECORD                  
         BZ    SVAL04              NO                                           
         TM    FHII,FHIITH         FIELD INPUT BY USER?                         
         BO    SVAL04              YES - OVERRIDE CURRENT SETTING               
*                                                                               
         LA    RF,TLUACTS          ACTION NAME DISPLAY TABLE                    
         USING TLUACTSD,RF                                                      
SVAL02   CLI   0(RF),EOT           UNKNOWN FLAG IN TSAR RECORD                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BOBYTE1,TLUSTAT     COPY STATUS FLAG                             
         NC    BOBYTE1,TLUFLAG     IS THIS ACTION REQUEST SET?                  
         BNZ   *+12                YES                                          
         LA    RF,TLULQ(RF)                                                     
         B     SVAL02              NEXT KNOWN ACTION                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,TLUNAME                                                     
         A     RE,AOVERWRK         RE=A(NAME OF ACTION)                         
         MVC   FHDA(SBLEN),0(RE)   MOVE OUT NAME INTO FIELD                     
         OI    FHOI,FHOITR         TRANSMIT IT                                  
         OI    FHII,FHIIVA         SET FIELD VALID                              
         DROP  RF                                                               
*                                                                               
SVAL04   TM    FHII,FHIIVA         FIELD VALIDATED?                             
         BO    *+8                 NO                                           
         OI    LSSCIND1,LSSCIINP   YES - KEEP CURRENT SCREEN                    
*                                                                               
         GOTOX ('FLDVAL',AGROUTS),FHD                                           
         CLI   FVILEN,0            SUB-ACTION ENTERED?                          
         BE    SVAL18              NO                                           
         CLI   FVIFLD,C'*'         IGNORE THIS FIELD?                           
         BE    SVAL18              YES                                          
         CLI   CSACT,A#CHA                                                      
         BNE   EXITNV              INPUT INVALID UNLESS ACTION CHANGE           
*                                                                               
         XC    LMCOUNT,LMCOUNT     RESET REPEAT COUNT                           
         XR    RE,RE                                                            
         ICM   RE,1,FVXLEN                                                      
         BZ    SVAL12              ONLY 1 CHARACTER INPUT                       
         LA    RF,FVIFLD(RE)       TEST SUFFIX CHARACTER                        
*                                                                               
         CLI   0(RF),C'0'          NUMERICAL ENDING?                            
         BL    SVAL12              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL12              NO                                           
*                                                                               
         XR    R1,R1               R1 HOLDS LENGTH                              
SVAL06   CLI   0(RF),C'0'          STILL NUMERIC?                               
         BL    SVAL08              NO                                           
         CLI   0(RF),C'9'                                                       
         BH    SVAL08              NO                                           
         LA    R1,1(R1)                                                         
         BCTR  RF,0                                                             
         BCT   RE,SVAL06                                                        
*                                                                               
SVAL08   BCTR  R1,0                                                             
         EX    R1,SVALPAK          OBTAIN PACKED NUMBER                         
         CVB   R0,GCDUB1                                                        
         EX    R1,SVALMVE          CLEAR NUMBER FROM INPUT FIELD                
         B     SVAL10                                                           
*                                                                               
SVALPAK  PACK  GCDUB1,1(0,RF)      PACK NUMBER INTO GCDUB1                      
SVALMVE  MVC   1(0,RF),BCSPACES    CLEAR NUMERIC PORTION OF FIELD               
*                                                                               
SVAL10   STH   R0,LMCOUNT          SAVE MULTILINE ACTION REPEAT NUMBER          
         XR    R0,R0                                                            
         IC    R0,FVILEN           REVALIDATE FVIFLD                            
         GOTOX ('FLDVAL',AGROUTS),0                                             
*                                                                               
SVAL12   LA    R3,SUBACTS          TRY TO MATCH SUB-ACTION                      
         USING SUBACTSD,R3                                                      
         XR    R1,R1                                                            
         IC    R1,FVXLEN           LENGTH OF INPUT                              
*                                                                               
SVAL14   CLC   SUBUPR,=AL2(EOT)    REACHED END OF TABLE?                        
         BE    SVALL               YES - INVALID SUB-ACTION                     
         XR    RF,RF                                                            
         ICM   RF,3,SUBUPR         TRY TO MATCH UPPERCASE NAME                  
         A     RF,AOVERWRK                                                      
         EX    R1,SUBMCH                                                        
         BE    SVAL16                                                           
         XR    RF,RF                                                            
         ICM   RF,3,SUBLWR         TRY TO MATCH LOWERCASE NAME                  
         A     RF,AOVERWRK                                                      
         EX    R1,SUBMCH                                                        
         BE    SVAL16                                                           
         LA    R3,SUBACTLQ(R3)                                                  
         B     SVAL14                                                           
*                                                                               
SUBMCH   CLC   FVIFLD(0),0(RF)                                                  
*                                                                               
SVAL16   MVC   LSNTRY,0(R3)        SAVE THIS SINGLE ENTRY                       
         OI    FHII,FHIIVA         SET FIELD VALID                              
*                                                                               
         ICM   RF,15,SNGL.SUBRTN   PROCESSING ROUTINE                           
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  R3                                                               
*                                                                               
SVAL18   OC    TLUSTAT,TLUSTAT     ACTION FLAG ON THIS FIELD BEFORE?            
         BZ    EXITOK              NO - SAFE TO IGNORE IT                       
*                                                                               
         XR    RE,RE                                                            
         IC    RE,FHLN                                                          
         LA    R1,FHD(RE)          R1=A(FIRST DATA FIELD)                       
FLD      USING FHD,R1                                                           
*                                                                               
         LA    RF,FHD              START OF THIS LINE                           
         AH    RF,LSLINLEN         LENGTH OF A LINE                             
         BCTR  RF,0                RF=A(END OF THIS LINE-1)                     
*                                                                               
SVAL20   IC    RE,FLD.FHLN         SET INDEX IN RE                              
         OI    FLD.FHOI,FHOITR     TRANSMIT FIELD                               
         NI    FLD.FHAT,FF-FHATHI  TURN OFF HIGHLIGHT                           
         BXLE  R1,RE,SVAL20        REPEAT FOR ALL FIELDS ON LINE                
         DROP  FLD                                                              
*                                                                               
         TM    TLUSTAT,TLUSCPY+TLUSMVE   'FROM' FLAG ON THIS ONE                
         BZ    SVAL22              NO                                           
         LH    RF,DOFRCNT          DECREMENT 'FROM' COUNT                       
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOFROM    TURN OFF FLAG IF NO 'FROM'                   
         STH   RF,DOFRCNT          SAVE NEW 'FROM' COUNT                        
*                                                                               
SVAL22   TM    TLUSTAT,TLUSBEF+TLUSAFT   'TO' FLAG ON THIS ONE                  
         BZ    SVAL24              NO                                           
         LH    RF,DOTOCNT          DECREMENT 'TO' COUNT                         
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOTO      TURN OFF FLAG IF NO 'TO'                     
         STH   RF,DOTOCNT          SAVE NEW 'TO' COUNT                          
*                                                                               
SVAL24   TM    TLUSTAT,TLUSDEL     'DELETE' FLAG ON THIS ONE                    
         BZ    SVAL26              NO                                           
         LH    RF,DODLCNT          DECREMENT 'DELETE' COUNT                     
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DODEL           TURN OFF FLAG IF NO 'DELETE'           
         STH   RF,DODLCNT                                                       
*                                                                               
SVAL26   TM    TLUSTAT,TLUSREP     'REPLICATE' FLAG ON THIS ONE                 
         BZ    SVAL28              NO                                           
         LH    RF,DORPCNT          DECREMENT 'REPLICATE' COUNT                  
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOREP                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL28   TM    TLUSTAT,TLUSINS     'INSERT' FLAG ON THIS ONE                    
         BZ    SVAL30              NO                                           
         LH    RF,DOINCNT          DECREMENT 'INSERT' COUNT                     
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOINS                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL30   TM    TLUSTAT,TLUSPST     'PASTE' FLAG ON THIS ONE                     
         BZ    SVAL32              NO                                           
         LH    RF,DOPSCNT          DECREMENT 'PASTE' COUNT                      
         SH    RF,=H'1'                                                         
         BP    *+8                                                              
         NI    DOFLAG,FF-DOPST                                                  
         STH   RF,DOPSCNT                                                       
*                                                                               
SVAL32   XC    TLUSTAT,TLUSTAT     RESET ACTION FLAG ON THIS ONE                
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         OI    FHII,FHIIVA                                                      
         B     EXITOK                                                           
*                                                                               
SVALL    B     EXITL                                                            
         DROP  R2,R4                                                            
*                                                                               
SUBACTS  DS    0H              *** TABLE OF VALID SUB-ACTIONS ***               
         DC    AL2(FU@COPY-OVERWRKD,FL@COPY-OVERWRKD),AL4(SUB_CPY)              
         DC    AL2(FU@MOVE-OVERWRKD,FL@MOVE-OVERWRKD),AL4(SUB_MVE)              
         DC    AL2(FU@DEL-OVERWRKD,FL@DEL-OVERWRKD),AL4(SUB_DEL)                
         DC    AL2(FU@BFR-OVERWRKD,FL@BFR-OVERWRKD),AL4(SUB_BFR)                
         DC    AL2(FU@AFTER-OVERWRKD,FL@AFTER-OVERWRKD),AL4(SUB_AFT)            
         DC    AL2(FU@REPL-OVERWRKD,FL@REPL-OVERWRKD),AL4(SUB_REP)              
         DC    AL2(FU@INSRT-OVERWRKD,FL@INSRT-OVERWRKD),AL4(SUB_INS)            
         DC    AL2(FU@SAVE-OVERWRKD,FL@SAVE-OVERWRKD),AL4(SUB_SAV)              
         DC    AL2(FU@PASTE-OVERWRKD,FL@PASTE-OVERWRKD),AL4(SUB_PST)            
         DC    AL2(EOT)                                                         
*                                                                               
SUBACTSD DSECT                                                                  
SUBUPR   DS    AL2                 DISPLACEMENT TO UPPERCASE NAME               
SUBLWR   DS    AL2                 DISPLACEMENT TO LOWER CASE NAME              
SUBRTN   DS    AL4                 DISPLACEMENT TO VALIDATION ROUTINE           
SUBACTLQ EQU   *-SUBACTSD                                                       
*                                                                               
NFI10    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE A LINE FROM THE LIST                              *         
***********************************************************************         
         SPACE 1                                                                
SUB_DEL  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         CLI   TLTYP,TTELEM                                                     
         BE    SDEL02                                                           
         MVC   FVMSGNO,=AL2(GE$YCDTK)                                           
         CLI   TLTYP,TTKEY                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$YMBIE)                                           
         B     EXITL                                                            
*                                                                               
SDEL02   TM    TLUSTAT,TLUSDEL     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSDEL     SET TO DELETE THIS LINE                      
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SDEL04              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SDEL04   LH    RF,DODLCNT          INCREMENT COUNT OF DELETE FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DODLCNT                                                       
         OI    DOFLAG,DODEL        SET A DELETE ACTION REQUIRED                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO REPLICATE A LINE IN THE LIST                             *         
***********************************************************************         
         SPACE 1                                                                
SUB_REP  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         CLI   TLTYP,TTELEM                                                     
         BE    SREP02                                                           
         MVC   FVMSGNO,=AL2(GE$YCDTK)                                           
         CLI   TLTYP,TTKEY                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$YMBIE)                                           
         B     EXITL                                                            
*                                                                               
SREP02   TM    TLUSTAT,TLUSREP     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSREP     SET TO REPLICATE THIS LINE                   
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SREP04              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SREP04   LH    RF,DORPCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(RF)                                                         
         STH   RF,DORPCNT                                                       
         OI    DOFLAG,DOREP        SET A REPLICATE ACTION REQUIRED              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO INSERT A LINE IN THE LIST                                *         
***********************************************************************         
         SPACE 1                                                                
SUB_INS  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSINS     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSINS     SET TO REPLICATE THIS LINE                   
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SINS02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SINS02   LH    RF,DOINCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(RF)                                                         
         STH   RF,DOINCNT                                                       
         OI    DOFLAG,DOINS        SET A INSERT ACTION REQUIRED                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO COPY A LINE IN A LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
SUB_CPY  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         CLI   TLTYP,TTELEM                                                     
         BE    SCPY01                                                           
         MVC   FVMSGNO,=AL2(GE$YCDTK)                                           
         CLI   TLTYP,TTKEY                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$YMBIE)                                           
         B     EXITL                                                            
*                                                                               
SCPY01   TM    TLUSTAT,TLUSCPY     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSCPY     SET TO COPY THIS FIELD                       
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SCPY02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SCPY02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM       SET A 'FROM' ACTION REQUIRED                 
*                                                                               
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK              NO - GOOD                                    
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO MOVE A LINE IN A LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
SUB_MVE  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         CLI   TLTYP,TTELEM                                                     
         BE    SMVE01                                                           
         MVC   FVMSGNO,=AL2(GE$YCDTK)                                           
         CLI   TLTYP,TTKEY                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$YMBIE)                                           
         B     EXITL                                                            
*                                                                               
SMVE01   TM    TLUSTAT,TLUSMVE     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSMVE                                                  
         XR    R0,R0                                                            
         ICM   R0,3,LMCOUNT        SET NUMBER OF REPEATS REQUIRED               
         BNZ   *+8                                                              
         LA    R0,1                                                             
         STC   R0,TLURPT                                                        
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SMVE02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SMVE02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(RF)                                                         
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM                                                    
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO COPY BEFORE THIS LINE IN A LIST                          *         
***********************************************************************         
         SPACE 1                                                                
SUB_BFR  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         CLI   TLTYP,TTELEM                                                     
         BE    SBEF01                                                           
         MVC   FVMSGNO,=AL2(GE$YCDTK)                                           
         CLI   TLTYP,TTKEY                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$YMBIE)                                           
         B     EXITL                                                            
*                                                                               
SBEF01   TM    TLUSTAT,TLUSBEF     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSBEF                                                  
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SBEF02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SBEF02   LH    RF,DOTOCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(RF)                                                         
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO COPY AFTER THIS LINE IN A LIST                           *         
***********************************************************************         
         SPACE 1                                                                
SUB_AFT  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSAFT     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSAFT                                                  
*                                                                               
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SAFT02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SAFT02   LH    RF,DOTOCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(RF)                                                         
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SAVE ELEMENT FOR PASTE INTO ANOTHER RECORD               *         
***********************************************************************         
         SPACE 1                                                                
SUB_SAV  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         USING FHD,R2                                                           
         CLI   TLTYP,TTELEM                                                     
         BE    SSAV02                                                           
         MVC   FVMSGNO,=AL2(GE$YCDTK)                                           
         CLI   TLTYP,TTKEY                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$YMBIE)                                           
         B     EXITL                                                            
*                                                                               
SSAV02   XC    SAVEL,SAVEL                                                      
         XR    RF,RF                                                            
         ICM   RF,1,TLLEN            LENGTH OF ELEMENT                          
         BZ    EXITNV                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   SAVEL(0),TLFIL                                                   
*                                                                               
         MVC   FHDA(SBLEN),BCSPACES                                             
         OI    FHOI,FHOITR                                                      
         MVC   FVMSGNO,=AL2(GI$TPTI)                                            
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITL                                                            
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO PASTE EXTENDED SAVE ELEMENT                              *         
***********************************************************************         
         SPACE 1                                                                
SUB_PST  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         CLI   TLTYP,TTELEM                                                     
         BE    SPST01                                                           
         MVC   FVMSGNO,=AL2(GE$YCDTK)                                           
         CLI   TLTYP,TTKEY                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(GE$YMBIE)                                           
         B     EXITL                                                            
*                                                                               
SPST01   TM    TLUSTAT,TLUSPST     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         CLI   SAVEL+1,2                                                        
         BL    EXITNV              NOTHING TO PASTE                             
*                                                                               
         OI    TLUSTAT,TLUSPST                                                  
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SPST02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SPST02   LH    RF,DOPSCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(RF)                                                         
         STH   RF,DOPSCNT                                                       
         OI    DOFLAG,DOPST                                                     
         CLC   DOPSCNT,=H'1'       TEST ANOTHER PASTE FIELD SET UP              
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(GE$CCONF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* RECALCULATE ROWS FOR TSAR RECORDS BASED ON TLU2ROWS FLAG            *         
***********************************************************************         
         SPACE 1                                                                
ROWS     NTR1  ,                                                                
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
*                                                                               
ROWS02   LA    R1,TSANXT           DEAL WITH ALL DELETE REQUESTS                
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    EXITOK              END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   EXITOK              DONE ALL FOR THIS LEVEL                      
*                                                                               
         CLI   TLTYP,TTELEM        ONLY ELEMENTS GET THIS                       
         BNE   ROWS02                                                           
*                                                                               
         CLI   TLLEN,1             SPECIAL END BYTE                             
         BE    *+10                                                             
         MVC   TLLEN,TLFIL+1       SET LENGTH CORRECT                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,TLLEN                                                         
         SRDL  RE,32                                                            
         LA    R1,DISWIDTH                                                      
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(RF)                                                         
         STC   RF,TLROWS           SET NEW REQUIREMENT                          
*                                                                               
         XR    RE,RE                                                            
         IC    RE,TLLEN                                                         
         AH    RE,=Y(TLLENQ)                                                    
         STCM  RE,3,TLRLEN         MAKE SURE RECORD LENGTH IS GOOD              
*                                                                               
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         B     ROWS02                                                           
         EJECT                                                                  
***********************************************************************         
* SORT OUT TSAR RECORDS BASED ON FLAGS SET IN THEM                    *         
***********************************************************************         
         SPACE 1                                                                
SORT     NTR1  ,                                                                
         MVI   BOBYTE1,0                                                        
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
*                                                                               
SORT02   LA    R1,TSANXT           FIND ANY MOVE/COPY RECORDS FLAGGED           
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SORT06              EOF                                          
         CLC   TLKSES,SESNL        CORRECT NEST LEVEL?                          
         BNE   SORT06              EOF                                          
         TM    TLUSTAT,(TLUSCPY+TLUSMVE)                                        
         BZ    SORT02              NOT A COPY OR A MOVE RECORD                  
*                                                                               
         MVI   BOBYTE1,FF          FLAG COPY/MOVE RECORDS FOUND                 
         XR    R0,R0                                                            
         ICM   R0,1,TLURPT         MULTI-LINE COPY/MOVE COUNTER                 
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
SORT04   TM    TLUSTAT,(TLUSAFT+TLUSBEF+TLUSDEL+TLUSREP+TLUSINS)                
         BNZ   SORTERR             COMMAND CONFLICT                             
*                                                                               
         MVC   TLMSVE,TLNUM        SAVE THIS RECORD NUMBER                      
         MVI   TLKSES,C'M'         TEMPORARY MOVE/COPY RECORD STAMP             
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
*                                                                               
         MVC   TLNUM,TLMSVE        RESTORE THIS RECORD NUMBER                   
         GOTO1 (RF),TSAGET         AND GET RECORD BACK                          
         GOTO1 (RF),TSANXT         GET NEXT RECORD IN LIST                      
         BL    SORT06              END OF FILE                                  
         CLC   TLKSES,SESNL        CORRECT NEST LEVEL?                          
         BNE   SORT06              FINISHED                                     
         BCT   R0,SORT04                                                        
*                                                                               
SORT06   XC    CCOUNT,CCOUNT       RESET CURRENT LIST COUNT                     
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET NEST LEVEL                               
         LA    R1,TSARDH                                                        
         B     *+8                                                              
*                                                                               
SORT08   LA    R1,TSANXT                                                        
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SORT26              END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   SORT26              DONE ALL FOR THIS LEVEL                      
*                                                                               
         TM    TLUSTAT,(TLUSDEL+TLUSMVE)                                        
         BZ    SORT12              NOT MOVING OR DELETING THIS LINE             
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,TLURPT         MULTI-LINE DELETE/MOVE?                      
         BZ    SORT08              NO                                           
         SH    R0,=H'1'                                                         
         BNP   SORT08              ONLY SINGLE LINE                             
*                                                                               
         LA    R1,TSANXT                                                        
SORT10   GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SORT26              END OF FILE                                  
         CLC   TLKSES,SESNL        TEST NEST LEVEL                              
         BNE   SORT26              DONE ALL FOR THIS LEVEL                      
         BCT   R0,SORT10                                                        
         B     SORT08              PROCESS NEXT RECORD                          
*                                                                               
SORT12   MVI   BOBYTE2,0           RESET ADD INTO LIST FLAG                     
         NI    TLSTAT,FF-TLSKEYC   TURN OFF KEY CHANGED FLAG                    
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
*                                                                               
         MVC   TLMSVE,TLNUM        SAVE CURRENT LIST NUMBER                     
*                                                                               
         CLI   BOBYTE1,FF          COPY/MOVE RECORD(S) TO ADD IN?               
         BNE   SORT14              NO                                           
         TM    TLUSTAT,(TLUSAFT+TLUSBEF)                                        
         BZ    SORT14              NO ADD BEFORE/AFTER THIS RECORD              
*                                                                               
         TM    TLUSTAT,TLUSAFT     ADDING AFTER THIS RECORD?                    
         BZ    *+12                NO                                           
         MVI   BOBYTE2,FF          SET ADD AFTER FLAG                           
         B     SORT14                                                           
*                                                                               
         BAS   RE,ADDIN            ADD RECORD(S) BEFORE THIS RECORD             
*                                                                               
         MVC   TLNUM,TLMSVE        RESTORE CURRENT RECORD                       
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         NI    TLSTAT,FF-TLSKEYC   TURN OFF KEY CHANGED FLAG                    
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
*                                                                               
SORT14   TM    TLUSTAT,(TLUSREP+TLUSINS+TLUSPST)                                
         BZ    SORT24              NOT PASTE/INSERT/REPLICATE                   
         XC    TLUSTAT2,TLUSTAT2                                                
*                                                                               
         LA    R0,TLREC                                                         
         XR    R1,R1                                                            
         ICM   R1,3,TLRLEN                                                      
         L     RE,AIO1                                                          
         LR    RF,R1                                                            
         MVCL  RE,R0               SAVE TSAR RECORD TO AIO1                     
*                                                                               
         XR    R0,R0               GET REPEAT COUNT                             
         ICM   R0,1,TLURPT                                                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
SORT16   STH   R0,BOHALF1                                                       
         TM    TLUSTAT,TLUSPST     PASTING AN ELEMENT?                          
         BZ    SORT18              NO                                           
         XR    RF,RF               COPY EXTENDED ELEMENT INTO TLFIL             
         IC    RF,SAVEL+1                                                       
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   TLFIL(0),SAVEL                                                   
         LA    RF,1(RF)                                                         
         STC   RF,TLLEN                                                         
         B     SORT20                                                           
*                                                                               
SORT18   TM    TLUSTAT,TLUSINS     INSERTING A LINE?                            
         BZ    SORT22              NO                                           
*                                                                               
         OI    TLUSTAT2,TLU2INS    FLAG LINE INSERTED                           
         XR    RF,RF                                                            
         ICM   RF,1,TLILEN                                                      
         BNZ   *+8                                                              
         LA    RF,2                                                             
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         XC    TLFIL(0),TLFIL                                                   
         LA    RF,1(RF)                                                         
*                                                                               
         STC   RF,TLLEN            SAVE ELEMENT LENGTH                          
         STC   RF,TLFIL+1          SET IT WITHIN ELEMENT                        
*                                                                               
         ICM   RF,1,TLICODE                                                     
         BNZ   *+8                                                              
         LA    RF,255                                                           
         STC   RF,TLFIL            SET ELEMENT CODE                             
*                                                                               
SORT20   XR    R0,R0                                                            
         ICM   R0,1,TLLEN                                                       
         BNZ   *+6                                                              
         DC    H'0'                SO HOW DID WE MANAGE THAT THEN?              
*                                                                               
         LH    RF,=Y(TLLENQ)                                                    
         AR    RF,R0                                                            
         STCM  RF,3,TLRLEN         SET NEW LENGTH                               
*                                                                               
         SRDL  R0,32               SET ROW COUNT REQUIRED TO DISPLAY            
         LA    RF,DISWIDTH                                                      
         DR    R0,RF                                                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(R1)                                                         
         STC   R1,TLROWS                                                        
*                                                                               
SORT22   LH    RE,CCOUNT           SET NEW SORT NUMBER                          
         LA    RE,1(RE)                                                         
         STH   RE,CCOUNT                                                        
         STCM  RE,3,TLKSNUM                                                     
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         XC    TLUSTAT,TLUSTAT     RESET STATUS BYTE                            
         XC    TLURPT,TLURPT       RESET REPEAT COUNT                           
         XC    TLINS,TLINS         REMOVE INSERT CHARACTERS                     
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
*                                                                               
         L     RE,AIO1             RESTORE ORIGINAL TSAR RECORD                 
         XR    RF,RF                                                            
         ICM   RF,3,TLRLEN-TLREC(RE)                                            
         LA    R0,TLREC                                                         
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         LH    R0,BOHALF1                                                       
         BCT   R0,SORT16           LOOP FOR REQUIRED REPEAT COUNT               
*                                                                               
SORT24   LH    RE,CCOUNT           SET NEW ORDERING FOR LINE COUNT              
         LA    RE,1(RE)                                                         
         STH   RE,CCOUNT                                                        
         STCM  RE,3,TLKSNUM        RESET KEY NUMBER                             
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         XC    TLUSTAT,TLUSTAT     RESET STATUS BYTE                            
         XC    TLUSTAT2,TLUSTAT2   RESET STATUS BYTE                            
         XC    TLURPT,TLURPT       RESET REPEAT COUNT                           
         XC    TLINS,TLINS         REMOVE INSERT CHARACTERS                     
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
*                                                                               
         CLI   BOBYTE2,FF          ADD AFTER FLAG SET?                          
         BNE   *+8                 NO                                           
         BAS   RE,ADDIN            ADD RECORD AFTER THIS RECORD                 
*                                                                               
         MVC   TLNUM,TLMSVE        RESTORE READ SEQUENCE                        
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         B     SORT08              NEXT IN LIST                                 
*                                                                               
SORT26   XC    TLKEY,TLKEY         DELETE ALL CURRENT RECORDS                   
         MVC   TLKSES,SESNL                                                     
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SORT28              END OF FILE                                  
         CLC   TLKSES,SESNL        SAME NEST LEVEL?                             
         BNE   SORT28              NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         B     SORT26                                                           
*                                                                               
SORT28   XC    TLKEY,TLKEY         MOVE SHUFFLED TO CURRENT SESSION             
         MVI   TLKSES,TLKSKEYC     TEMPORARY SHUFFLED NEST LEVEL                
         GOTOX (RF),TSARDH                                                      
         BL    SORTX               END OF FILE                                  
         CLI   TLKSES,TLKSKEYC     STILL TEMPORARY LEVEL?                       
         BNE   SORTX               NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         MVC   TLKSES,SESNL        RESTORE TO CURRENT NEST LEVEL                
         GOTOX (RF),TSAADD                                                      
         B     SORT28                                                           
*                                                                               
SORTX    MVC   LSLST#X,LSLST#1     SET START                                    
         NI    LSLTIND1,FF-LSLTISOL                                             
         XR    RF,RF                                                            
         ICM   RF,3,CCOUNT         COUNT OF ITEMS ADDED TO LIST                 
         BZ    EXITOK              NOTHING                                      
         AH    RF,LSLST#X                                                       
         BCTR  RF,0                                                             
         STH   RF,LSLST#X          SET CORRECT VALUE FOR END                    
         OI    LSLTIND1,LSLTISOL                                                
         B     EXITOK                                                           
*                                                                               
SORTERR  XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,C'M'         TEMP KEY FOR MOVE/COPY RECORDS               
*                                                                               
SRTERR02 GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SRTERR04                                                         
         CLI   TLKSES,C'M'                                                      
         BNE   SRTERR04                                                         
         GOTOX (RF),TSADEL         DELETE THIS RECORD                           
         B     SRTERR02                                                         
*                                                                               
SRTERR04 XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,TLKSKEYC     TEMPORARY SHUFFLED NEST LEVEL                
         GOTOX (RF),TSARDH                                                      
         BL    SORTERRX                                                         
         CLI   TLKSES,TLKSKEYC                                                  
         BNE   SORTERRX                                                         
         GOTOX (RF),TSADEL         DELETE THIS RECORD                           
         B     SRTERR04                                                         
*                                                                               
SORTERRX B     EXITL                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A RECORD SAVED IN SVLST TO CURRENT POINTER IN LIST   *         
***********************************************************************         
         SPACE 1                                                                
ADDIN    NTR1  ,                                                                
*                                                                               
ADDIN02  XC    TLKEY,TLKEY                                                      
         MVI   TLKSES,C'M'         TEMP KEY FOR MOVE/COPY RECORDS               
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    ADDINX                                                           
         CLI   TLKSES,C'M'                                                      
         BNE   ADDINX                                                           
         GOTOX (RF),TSADEL         DELETE THIS RECORD                           
*                                                                               
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         XC    TLUSTAT,TLUSTAT                                                  
         XC    TLUSTAT2,TLUSTAT2                                                
         LH    R1,CCOUNT           SET NEW ORDERING FOR LINE COUNT              
         LA    R1,1(R1)                                                         
         STH   R1,CCOUNT                                                        
         STCM  R1,3,TLKSNUM        RESET KEY NUMBER                             
         GOTO1 (RF),TSAADD         ADD TEMPORARY COPY                           
         B     ADDIN02                                                          
*                                                                               
ADDINX   B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* SCAN RECORD FOR STRING                                              *         
***********************************************************************         
         SPACE 1                                                                
SCANREC  NTR1  ,                                                                
         L     RF,RTDSPDIR                                                      
         USING NFITABD,RF                                                       
         XR    R0,R0                                                            
         IC    R0,NFIKEYL                                                       
         XR    RE,RE                                                            
         IC    RE,NFICTLL                                                       
         AR    RE,R0                                                            
         LA    RE,IOKEY(RE)                                                     
         MVC   IODAOVER,0(RE)       MOVE IN D/A                                 
*                                                                               
         L     RF,RTDSPREC                                                      
         XR    R1,R1                                                            
         IC    R1,NFINUM                                                        
         SLL   R1,8                                                             
         A     R1,=A(XOGET+XIO1)                                                
         GOTOX ('XIO',AGROUTS)                                                  
         CLI   IOERR,0                                                          
         BE    SCREC02                                                          
         TM    IOERR,IOEDEL        DELETED ARE ALLOWED                          
         BO    SCREC02                                                          
         DC    H'0'                                                             
*                                                                               
SCREC02  L     R2,AIO1                                                          
         L     RF,RTDSPDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFIKEYL                                                       
         AR    R1,R2                                                            
         XR    R0,R0                                                            
         ICM   R0,3,0(R1)                                                       
*                                                                               
         XR    RF,RF                                                            
         IC    RF,SCANLEN                                                       
         BCTR  RF,0                                                             
         SR    R0,RF                                                            
*                                                                               
SCREC04  EX    RF,SCANMCH                                                       
         BE    EXITOK                                                           
         LA    R2,1(R2)                                                         
         BCT   R0,SCREC04                                                       
         B     EXITL                                                            
         DROP  RF                                                               
*                                                                               
SCANMCH  CLC   SCANCHR(0),0(R2)                                                 
         EJECT                                                                  
***********************************************************************         
* PROGRESS REPORT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PROGRESS NTR1  ,                                                                
         L     R2,ATWA                                                          
         USING TWAD,R2                                                          
         MVC   TWAMSG,BCSPACES                                                  
         MVC   TWAMSG(L'FL@PRGRS),FL@PRGRS                                      
         LA    R4,TWAMSG+L'FL@PRGRS                                             
         L     R0,IOLOOP                                                        
         CURED (R0),(9,(R4)),0,DMCB=BOPARM,ALIGN=LEFT                           
         AR    R4,R0                                                            
         LA    R4,1(R4)                                                         
         MVC   0(L'FL@SCAND,R4),FL@SCAND                                        
         OI    TWAMSGH+FHOID,FHOITR                                             
*                                                                               
         MVC   FVADDR,AKEY1                                                     
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         GOTOX ('SETMSG',AGROUTS)                                               
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CPROTOFF-COMFACSD(RF)                                         
         BASR  RE,RF                                                            
*                                                                               
         GOTOX VGETFACT,BOPARM,(X'80',BOWORK1),F#WRITE                          
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CPROTON-COMFACSD(RF)                                          
         BASR  RE,RF                                                            
                                                                                
         L     RF,RTDSPDIR                                                      
         XR    R1,R1                                                            
         IC    R1,NFINUM-NFITABD(RF)                                            
         SLL   R1,8                                                             
         A     R1,=A(XOHI+XORDEL)                                               
         GOTOX ('XIO',AGROUTS)                                                  
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO REDISPLAY A SCREENS WORTH OF DATA BASED ON NEW RECORDS   *         
***********************************************************************         
         SPACE 1                                                                
REDRAW   NTR1  ,                                                                
         TM    DOFLAG,DOROWS       RECALCULATE ROW INFORMATION?                 
         BZ    RDRW02              NO                                           
         BAS   RE,ROWS                                                          
*                                                                               
RDRW02   ICM   R1,7,=XL3'FFFFFF'   SET UP PAGE INFO AGAIN                       
         ICM   R1,8,=AL1(AGLSTPAG)                                              
         GOTOX AGENLST                                                          
*                                                                               
         MVC   LSLINE#,LSPAG#1     REDISPLAY FROM LINE 1                        
         BAS   RE,DISPAG                                                        
*                                                                               
RDRWX    XC    DOFLAG,DOFLAG                                                    
         XC    DOFRCNT,DOFRCNT                                                  
         XC    DOTOCNT,DOTOCNT                                                  
         XC    DODLCNT,DODLCNT                                                  
         XC    DORPCNT,DORPCNT                                                  
         XC    DOINCNT,DOINCNT                                                  
         XC    DOPSCNT,DOPSCNT                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DISPLAY LIST LINES                                       *         
*                                                                     *         
* NTRY: LSLINE# = FIRST LINE TO BE DISPLAYED                          *         
* EXIT: LSLINE# + ON PAGE DISPLAYED                                   *         
***********************************************************************         
         SPACE 1                                                                
DISPAG   NTR1  ,                                                                
         L     RF,ATLST                                                         
         USING TLSTD,RF                                                         
         XC    TLNUM,TLNUM         CLEAR TSAR BUFFER                            
         DROP  RF                                                               
*                                                                               
         LH    R2,LS1STLIN         YES - CLEAR ALL LIST LINES                   
         A     R2,ATWA             R1=A(FIRST LIST LINE)                        
         LH    RF,GSDSPMAX                                                      
         A     RF,ATWA                                                          
         USING FHD,R2                                                           
*                                                                               
         XR    RE,RE                                                            
DPAG02   ICM   RE,1,FHLN                                                        
         BZ    DPAG06                                                           
         TM    FHAT,FHATXH                                                      
         BZ    DPAG04                                                           
         LA    R1,FHD(RE)                                                       
         SH    R1,=Y(FHDAD)                                                     
         CLI   0(R1),FD#PFK                                                     
         BE    DPAG06                                                           
*                                                                               
DPAG04   LR    R1,RE                                                            
         SH    R1,=Y(FHDAD+1)                                                   
         TM    FHAT,FHATXH                                                      
         BZ    *+8                                                              
         SH    R1,=Y(FHDAD)                                                     
         EX    R1,*+4                                                           
         MVC   FHDA(0),BCSPACES    CLEAR FIELD TO SPACES                        
         OI    FHOI,FHOITR                                                      
         BXLE  R2,RE,DPAG02                                                     
         DROP  R2                                                               
*                                                                               
DPAG06   XC    LSROWXTR,LSROWXTR   FOR ANY EXTRA ROWS USED                      
*                                                                               
DPAG08   CLC   LSLINE#,LSPAG#X                                                  
         BH    EXITOK                                                           
         BAS   RE,DISLINE                                                       
         LH    RE,LSLINE#                                                       
         LA    RE,1(RE)                                                         
         STH   RE,LSLINE#                                                       
         B     DPAG08                                                           
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY LIST LINE                                        *         
*                                                                     *         
* NTRY: LSLINE# = LIST LINE NUMBER TO BE DISPLAYED                    *         
***********************************************************************         
         SPACE 1                                                                
DISLINE  NTR1  ,                                                                
         LH    R2,LSLINE#          R2=LINE NUMBER                               
         SH    R2,LSPAG#1                                                       
         MH    R2,LSLINLEN                                                      
         AH    R2,LS1STLIN                                                      
*                                                                               
         LH    R0,LSROWXTR         'EXTRA' ROWS USED THUS FAR                   
         MH    R0,LSLINLEN                                                      
         AR    R2,R0                                                            
         STH   R2,LSCURLIN         SAVE DISPLACEMENT TO CURRENT LINE            
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         L     R5,ATLST                                                         
         USING TLSTD,R5                                                         
         MVC   TLNUM,LSLINE#                                                    
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
*                                                                               
         NI    S.FHAT,FF-FHATPR    UNPROTECT SUB-ACTION FIELD                   
         OI    S.FHOI,FHOITR                                                    
         OI    S.FHII,FHIIVA                                                    
*                                                                               
         TM    TLUSTAT2,TLU2INS                                                 
         BZ    DISLN05                                                          
         OC    AINSFLD,AINSFLD                                                  
         BNZ   DISLN05                                                          
         ST    R2,AINSFLD                                                       
*                                                                               
DISLN05  XR    R0,R0               LOOP COUNT SAVED ON TSAR RECORD              
         ICM   R0,1,TLROWS                                                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
         XC    LSROWREP,LSROWREP   CURRENT ROW REPEAT COUNTER                   
*                                                                               
DISLN06  LA    R3,LSLIN            TABLE OF COLUMNS ON THE LINE                 
         USING LINTABD,R3                                                       
         LH    RE,LSROWREP         INCREMENT ROW REPEAT COUNT                   
         LA    RE,1(RE)                                                         
         STH   RE,LSROWREP         (THIS MAKES IT 1 BASED)                      
*                                                                               
         CLM   R0,1,TLROWS                                                      
         BE    DISLN08             YES - HANDLED NORMALLY THEN                  
         OC    LSSUBLEN,LSSUBLEN   SUB-ACTION FIELD?                            
         BZ    DISLN08             NO                                           
         OI    S.FHAT,FHATPR       PROTECT SUB-ACTION FIELD                     
         OI    S.FHOI,FHOITR                                                    
         OI    S.FHII,FHIIVA                                                    
*                                                                               
DISLN08  CLI   LINTABD,LINTEOT     DISPLAYED ALL FIELDS?                        
         BE    DISLN14             YES                                          
         LH    RE,LINHDR                                                        
         LA    R4,S.FHD(RE)                                                     
         USING FHD,R4              R4=A(FIELD HEADER FOR OUTPUT)                
*                                                                               
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         OI    FHII,FHIIVA                                                      
         NI    FHAT,FF-FHATHI                                                   
*                                                                               
         TM    TLUSTAT2,TLU2INS    THIS LINE INSERTED?                          
         BZ    *+8                 NO                                           
         OI    FHAT,FHATHI         INSERTED ARE HIGHLIGHTED                     
*                                                                               
         TM    LININDS,LINIOPEN    ENSURE INPUT FIELD IS OPEN                   
         BZ    *+8                                                              
         NI    FHAT,FF-FHATPR                                                   
         MVC   FVIHDR,FHD          SAVE FIELD HEADER INFORMATION                
*                                                                               
         TM    LININDS,LINIOPEN    OPEN FIELD?                                  
         BZ    DISLN10             NO                                           
         GOTOX AGEN,BODMCB,ODATA,DDIS,FHD,TLSTD                                 
         B     DISLN12                                                          
*                                                                               
DISLN10  GOTOX AGEN,BODMCB,ODATA,DNDIS,LINFLD#,TLSTD                            
*                                                                               
         LH    R1,LINFLD#          GET FDREL FOR CLOSED FIELD                   
         BCTR  R1,0                                                             
         SLL   R1,2                                                             
         A     R1,AFDRADDR                                                      
         L     R1,0(R1)                                                         
         USING FDRELD,R1                                                        
*                                                                               
         LH    RE,LINDSP           DISPLACEMENT TO COLUMN WITHIN FIELD          
         LA    RF,FHDA(RE)                                                      
         IC    RE,FDRLCLEN         COLUMN WIDTH                                 
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   0(0,RF),FVIFLD      COPY FIELD INTO CLOSED FIELD                 
         DROP  R1                                                               
*                                                                               
DISLN12  LA    R3,LINTABL(R3)      NEXT FIELD                                   
         B     DISLN08             REPEAT FOR ALL FIELDS ON 1 LINE              
*                                                                               
DISLN14  AH    R2,LSLINLEN                                                      
         BCT   R0,DISLN06          REPEAT FOR ALL LINES FOR 1 RECORD            
         DROP  R3,R4                                                            
*                                                                               
         TM    TLUSTAT2,TLU2INS    THIS LINE INSERTED?                          
         BZ    DISLN16             NO                                           
         NI    TLUSTAT2,255-TLU2INS                                             
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
DISLN16  XR    R0,R0                                                            
         ICM   R0,1,TLROWS                                                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
         BCTR  R0,0                FIRST ROW HANDLED NORMALLY                   
*                                                                               
         LH    RF,LSROWXTR         INCREMENT 'EXTRA' ROW COUNT                  
         AR    RF,R0                                                            
         STH   RF,LSROWXTR                                                      
         B     EXITOK                                                           
         DROP  S,R5                                                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CLEAR A LIST LINE ON THE SCREEN                          *         
***********************************************************************         
         SPACE 1                                                                
         USING FHD,R1                                                           
CLRLINE  NTR1  ,                                                                
         LA    RF,FHD                                                           
         AH    RF,LSLINLEN                                                      
         BCTR  RF,0                SET RF TO END OF LINE - 1                    
         XR    RE,RE                                                            
*                                                                               
CLINE02  ICM   RE,1,FHLN           SET INDEX IN RE                              
         BZ    EXITOK                                                           
         TM    LSSTAT2,LSSADD      ALLOWED TO ADD ITEMS TO LIST?                
         BO    *+8                 YES                                          
         OI    FHAT,FHATPR         NO - PROTECT ALL FIELDS                      
         OI    FHII,FHIIVA         SET FIELD VALIDATED                          
         OI    FHOI,FHOITR         TRANSMIT FIELD                               
         LR    R2,RE                                                            
         SH    R2,=Y(FHDAD+FHDAD+1)                                             
         TM    FHAT,FHATXH         EXTENDED FIELD HEADER                        
         BO    *+8                                                              
         LA    R2,FHDAD(R2)                                                     
         EX    R2,*+4                                                           
         MVC   FHDA(0),BCSPACES    SPACE FILL TEXT PORTION                      
         BXLE  R1,RE,CLINE02                                                    
         B     EXITOK                                                           
         DROP  R1                                                               
***********************************************************************         
* ROUTINE TO REDISPLAY A SCREENS WORTH OF SUB-ACTION FIELDS           *         
***********************************************************************         
         SPACE 1                                                                
REACT    NTR1  ,                                                                
         TM    LSLTIND1,LSLTISOL   LIST HAS DATA IN IT?                         
         BZ    REACX               NO                                           
*                                                                               
         XC    AFRSTFR,AFRSTFR     RESET FIELD ADDRESSES                        
         XC    AFRSTTO,AFRSTTO                                                  
         XC    LSROWXTR,LSROWXTR                                                
*                                                                               
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   LSLINE#,LSPAG#1     SET CURRENT LINE TO FIRST ON SCREEN          
*                                                                               
REAC02   LH    R2,LSLINE#          CURRENT LINE OF SCREEN                       
         CH    R2,LSPAG#X          FINISHED DRAWING SCREEN YET?                 
         BH    REACX               YES                                          
         SH    R2,LSPAG#1                                                       
         MH    R2,LSLINLEN                                                      
         TM    LSSTAT3,LS3RFIX     REPEATED SINGLE ROWS?                        
         BZ    *+8                                                              
         MH    R2,LSROWLIN         NUMBER OF ROWS REQUIRED FOR 1 LINE           
                                                                                
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE ROWS?                       
         BZ    *+14                                                             
         LH    R0,LSROWXTR         'EXTRA' ROWS USED THUS FAR                   
         MH    R0,LSLINLEN                                                      
         AR    R2,R0                                                            
*                                                                               
         AH    R2,LS1STLIN                                                      
         STH   R2,LSCURLIN         SET DISPLACEMENT TO CURRENT LINE             
         A     R2,ATWA                                                          
S        USING FHD,R2              R2=A(SUB-ACTION FIELD)                       
*                                                                               
         CLC   TLNUM,LSLINE#       ALREADY HAVE RECORD?                         
         BE    REAC04              YES                                          
         MVC   TLNUM,LSLINE#                                                    
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
*                                                                               
REAC04   OC    TLUSTAT,TLUSTAT     ANY ACTIONS FOR THIS RECORD                  
         BZ    REAC12              NO                                           
*                                                                               
         LA    RF,TLUACTS          ACTION NAME DISPLAY TABLE                    
         USING TLUACTSD,RF                                                      
REAC06   CLI   0(RF),EOT           UNKNOWN FLAG IN TSAR RECORD                  
         BNE   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BOBYTE1,TLUSTAT     COPY STATUS FLAG                             
         NC    BOBYTE1,TLUFLAG     IS THIS ACTION REQUEST SET?                  
         BNZ   *+12                YES                                          
         LA    RF,TLULQ(RF)                                                     
         B     REAC06              NEXT KNOWN ACTION                            
*                                                                               
         XR    RE,RE                                                            
         ICM   RE,3,TLUNAME                                                     
         A     RE,AOVERWRK         RE=A(NAME OF ACTION)                         
         MVC   S.FHDA(SBLEN),0(RE) MOVE OUT NAME INTO FIELD                     
         OI    S.FHOI,FHOITR       TRANSMIT IT                                  
         OI    S.FHII,FHIIVA       SET FIELD VALID                              
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,1,TLURPT         GET REPEAT COUNT                             
         BZ    REAC08                                                           
         CLM   R0,1,=AL1(1)        SHOW ONLY IF >1                              
         BNH   REAC08                                                           
*                                                                               
         CVD   R0,BODUB1                                                        
         OI    BODUB1+L'BODUB1-1,X'0F'                                          
*                                                                               
         XR    RF,RF               SCOPE NUMBER OF DIGITS TO DISPLAY            
         LA    RE,S.FHDA+SBLEN-1                                                
         CLI   TLURPT,9                                                         
         BNH   *+10                                                             
         LA    RF,1                                                             
         BCTR  RE,0                                                             
         CLI   TLURPT,99                                                        
         BNH   *+10                                                             
         LA    RF,2                                                             
         BCTR  RE,0                                                             
         EX    RF,REACUNPK                                                      
         B     REAC08                                                           
*                                                                               
REACUNPK UNPK  0(0,RE),BODUB1        SET LINE NUMBER IN TSAR RECORD             
*                                                                               
REAC08   TM    TLUSTAT,TLUSCPY+TLUSMVE                                          
         BZ    *+18                                                             
         OC    AFRSTFR,AFRSTFR     SET ADDRESS OF FIRST 'FROM' FIELD            
         BNZ   *+8                                                              
         ST    R2,AFRSTFR                                                       
*                                                                               
         TM    TLUSTAT,TLUSBEF+TLUSAFT                                          
         BZ    *+18                                                             
         OC    AFRSTTO,AFRSTTO     SET ADDRESS OF FIRST 'TO' FIELD              
         BNZ   *+8                                                              
         ST    R2,AFRSTTO                                                       
*                                                                               
         XR    RE,RE                                                            
         IC    RE,S.FHLN                                                        
         LA    R1,S.FHD(RE)        R1=A(FIRST DATA FIELD)                       
FLD      USING FHD,R1                                                           
*                                                                               
         LA    RF,S.FHD            START OF THIS LINE                           
         AH    RF,LSLINLEN         LENGTH OF A LINE                             
         BCTR  RF,0                RF=A(END OF THIS LINE-1)                     
*                                                                               
REAC10   IC    RE,FLD.FHLN         SET INDEX IN RE                              
         OI    FLD.FHII,FHIIVA     SET FIELD VALIDATED                          
         OI    FLD.FHOI,FHOITR     TRANSMIT FIELD                               
         OI    FLD.FHAT,FHATHI     TURN ON HIGHLIGHT                            
         BXLE  R1,RE,REAC10        REPEAT FOR ALL FIELDS ON LINE                
         DROP  RF,S,FLD                                                         
*                                                                               
REAC12   LH    RF,LSLINE#          NEXT LINE ON SCREEN                          
         LA    RF,1(RF)                                                         
         STH   RF,LSLINE#                                                       
*                                                                               
         TM    LSSTAT3,LS3RVAR     SPECIAL VARIABLE BUILD?                      
         BZ    REAC02              NO                                           
         XR    R0,R0                                                            
         ICM   R0,1,TLROWS                                                      
         BNZ   *+8                                                              
         LA    R0,1                                                             
*                                                                               
         LH    RF,LSROWXTR         INCREMENT 'EXTRA' ROW COUNT                  
         AR    RF,R0                                                            
         BCTR  RF,0                FIRST ROW HANDLED NORMALLY                   
         STH   RF,LSROWXTR                                                      
         B     REAC02              DO NEXT LINE                                 
*                                                                               
REACX    B     EXITOK              FINISHED                                     
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                               *         
***********************************************************************         
         SPACE 1                                                                
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
*                                                                               
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
*                                                                               
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       DEFINATELY NOT WANTED                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         DS    0D                                                               
DCLIST   DCDDL GE#SCAN,8,L                                                      
         DCDDL GE#COPY,SBLEN,L                                                  
         DCDDL GE#MOVE,SBLEN,L                                                  
         DCDDL GE#DEL,SBLEN,L                                                   
         DCDDL GE#AFTER,SBLEN,L                                                 
         DCDDL GE#BFR,SBLEN,L                                                   
         DCDDL GE#INSRT,SBLEN,L                                                 
         DCDDL GE#REPL,SBLEN,L                                                  
         DCDDL GE#SAVE,SBLEN,L                                                  
         DCDDL GE#PASTE,SBLEN,L                                                 
         DCDDL GE#RECD,8,L                                                      
         DCDDL GE#ELEM,8,L                                                      
         DCDDL GE#PRGRS,17,L                                                    
         DCDDL GE#SCAND,20,L                                                    
         DC    X'00'                                                            
*                                                                               
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
SBLEN    EQU   4                                                                
K_LEN    EQU   70                                                               
LISWIDTH EQU   20                  WIDTH TO DISPLAY ON LIST                     
DISWIDTH EQU   20                  WIDTH TO DISPLAY ON MAINT                    
RECNUM   DC    F'50000'            MAX NUMBER OF SCAN I/OS ALLOWED              
OUTNUM   DC    H'10000'            NUMBER OF READS PER MESSAGE                  
*                                                                               
FVFIOLIM EQU   X'FF00'+045         I/O LIMIT REACHED                            
GI$TPTI  EQU   X'FF00'+038         PASTE TO INSERT                              
GE$CCONF EQU   X'FF00'+077         COMMAND CONFLICT                             
GE$CCKEY EQU   X'FF00'+079         CANT CHANGE KEY                              
GE$MCIP  EQU   X'FF00'+080         MOVE/COPY IN PROGRESS                        
GE$YCDTK EQU   X'FF00'+081         YOU CAN'T DO THAT                            
GE$YMBIE EQU   X'FF00'+082         YOU MUST BE IN ELEMENT MODE                  
GE$INKY  EQU   X'FF00'+083         INVALID KEY                                  
*                                                                               
*&&UK                                                                           
EG$INVDA EQU   X'FF00'+065         INVALID DISK ADDRESS                         
*&&                                                                             
*&&US                                                                           
EG$INVDA EQU   X'FF00'+078         INVALID DISK ADDRESS                         
*&&                                                                             
*                                                                               
TRTABL   DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  00-0F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  10-1F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  20-2F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  30-3F                    
         DC    XL16'406F6F6F6F6F6F6F6F6F4A4B4C4D4E4F'  40-4F                    
         DC    XL16'506F6F6F6F6F6F6F6F6F5A5B5C5D5E5F'  50-5F                    
         DC    XL16'60616F6F6F6F6F6F6F6F6A6B6C6D6E6F'  60-6F                    
         DC    XL16'6F6F6F6F6F6F6F6F6F797A7B7C7D7E7F'  70-7F                    
         DC    XL16'6F8182838485868788896F6F6F6F6F6F'  80-8F                    
         DC    XL16'6F9192939495969798996F6F6F6F6F6F'  90-9F                    
         DC    XL16'6FA1A2A3A4A5A6A7A8A96F6F6F6F6F6F'  A0-AF                    
         DC    XL16'6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F6F'  B0-BF                    
         DC    XL16'C0C1C2C3C4C5C6C7C8C96F6F6F6F6F6F'  C0-CF                    
         DC    XL16'D0D1D2D3D4D5D6D7D8D96F6F6F6F6F6F'  D0-D1                    
         DC    XL16'E06FE2E3E4E5E6E7E8E96F6F6F6F6F6F'  E0-EF                    
         DC    XL16'F0F1F2F3F4F5F6F7F8F96F6F6F6F6F6F'  F0-FF                    
*                                                                               
TLUACTS  DS    0H              *** TABLE OF TSAR SUB-ACTION NAMES ***           
         DC    AL1(TLUSDEL),AL2(FL@DEL-OVERWRKD)                                
         DC    AL1(TLUSREP),AL2(FL@REPL-OVERWRKD)                               
         DC    AL1(TLUSINS),AL2(FL@INSRT-OVERWRKD)                              
         DC    AL1(TLUSCPY),AL2(FL@COPY-OVERWRKD)                               
         DC    AL1(TLUSMVE),AL2(FL@MOVE-OVERWRKD)                               
         DC    AL1(TLUSAFT),AL2(FL@AFTER-OVERWRKD)                              
         DC    AL1(TLUSBEF),AL2(FL@BFR-OVERWRKD)                                
         DC    AL1(EOT)                                                         
*                                                                               
TLUACTSD DSECT                                                                  
TLUFLAG  DS    XL1                                                              
TLUNAME  DS    AL2                                                              
TLULQ    EQU   *-TLUACTSD                                                       
*                                                                               
NFI10    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE OPTIONS FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   NTR1  BASE=*,LABEL=*                                                   
         XC    OPTBLK,OPTBLK                                                    
         XR    R2,R2                                                            
         ICM   R2,3,GSDSPOPT                                                    
         BZ    EXITOK                                                           
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         ST    R2,FVADDR                                                        
         CLI   FHIL,0                                                           
         BNE   VOPT01                                                           
         XC    SCANLEN,SCANLEN                                                  
         BE    EXITOK                                                           
*                                                                               
VOPT01   GOTOX VSCANNER,BOPARM,(R2),('SBLKNUM',SBLK),0                          
         CLI   4(R1),SBLKNUM                                                    
         BH    EXITNV                                                           
         XR    R0,R0                                                            
         ICM   R0,1,4(R1)                                                       
         BZ    EXITOK                                                           
*                                                                               
         LA    R3,SBLK                                                          
         USING SCANBLKD,R3                                                      
*                                                                               
VOPT02   LA    R4,VOPTS                                                         
         USING VOPTSD,R4                                                        
*                                                                               
         XR    R1,R1                                                            
         IC    R1,SC1STLEN                                                      
         BCTR  R1,0                                                             
*                                                                               
VOPT04   CLC   =AL2(EOT),VOPTDDU                                                
         BNE   VOPT06                                                           
         MVC   FVERRNDX,SC1STNUM                                                
         MVC   FVMSGNO,=AL2(GE$IARGO)                                           
         B     EXITL                                                            
*                                                                               
VOPT06   XR    RF,RF                                                            
         ICM   RF,3,VOPTDDU                                                     
         A     RF,AOVERWRK                                                      
         XR    RE,RE                                                            
         ICM   RE,3,VOPTDDL                                                     
         A     RE,AOVERWRK                                                      
*                                                                               
         EX    R1,*+8                                                           
         BE    VOPT08                                                           
         CLC   SC1STFLD(0),0(RF)                                                
         EX    R1,*+8                                                           
         BE    VOPT08                                                           
         CLC   SC1STFLD(0),0(RE)                                                
         LA    R4,VOPTLQ(R4)                                                    
         B     VOPT04                                                           
*                                                                               
VOPT08   ICM   RF,15,VOPTVAL                                                    
         A     RF,BORELO                                                        
         BASR  RE,RF                                                            
*                                                                               
         LA    R3,SCBLKLQ(R3)                                                   
         BCT   R0,VOPT02                                                        
         B     EXITOK                                                           
*                                                                               
VALSCAN  NTR1  ,                   'SCAN=' PARAMETER                            
         CLC   SCANLEN,SC2NDLEN                                                 
         BE    *+8                                                              
         MVI   GSFRP.FRPTYPE,FRPTRFRS                                           
         MVC   SCANLEN,SC2NDLEN                                                 
         XR    RF,RF                                                            
         IC    RF,SCANLEN                                                       
         BCTR  RF,0                                                             
         EX    RF,VOPSCAN                                                       
         BE    *+8                                                              
         MVI   GSFRP.FRPTYPE,FRPTRFRS                                           
         MVC   SCANCHR,SC2NDFLD                                                 
         B     EXITOK                                                           
*                                                                               
VOPSCAN  CLC   SCANCHR(0),SC2NDFLD                                              
*                                                                               
VOPTS    DC    AL2(FU@SCAN-OVERWRKD,FL@SCAN-OVERWRKD),AL4(VALSCAN)              
         DC    AL2(EOT)                                                         
*                                                                               
VOPTSD   DSECT                                                                  
VOPTDDU  DS    AL2                                                              
VOPTDDL  DS    AL2                                                              
VOPTVAL  DS    AL4                                                              
VOPTLQ   EQU   *-VOPTSD                                                         
NFI10    CSECT                                                                  
*                                                                               
         LTORG                                                                  
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
ADECODE  DS    A                                                                
AKEY1    DS    A                                                                
AKEY2    DS    A                                                                
AVIEW    DS    A                                                                
ADSK     DS    A                                                                
AINSFLD  DS    A                   A(CURSOR) FOR INSERTED FIELD                 
AFRSTFR  DS    A                   A(FIRST COPY FROM FIELD)                     
AFRSTTO  DS    A                   A(FIRST COPY TO FIELD)                       
DSKADDR  DS    F                                                                
*                                                                               
IOLOOP   DS    F                                                                
RTDSPDIR DS    A                                                                
RTDSPREC DS    A                                                                
LINENOW  DS    H                   CURRENT LINE NUMBER FOR LIST BUILD           
UPDNOW   DS    H                                                                
UPDFLG   DS    X                                                                
SESNL    DS    X                                                                
FLAG     DS    X                                                                
TLMSVE   DS    XL2                 SAVED TSAR NUMBER                            
*                                                                               
DSLISTU  DS    0F              *** UPPERCASE DICTIONARY                         
FU@SCAN  DS    XL8                                                              
FU@COPY  DS    CL(SBLEN)                                                        
FU@MOVE  DS    CL(SBLEN)                                                        
FU@DEL   DS    CL(SBLEN)                                                        
FU@AFTER DS    CL(SBLEN)                                                        
FU@BFR   DS    CL(SBLEN)                                                        
FU@INSRT DS    CL(SBLEN)                                                        
FU@REPL  DS    CL(SBLEN)                                                        
FU@SAVE  DS    CL(SBLEN)                                                        
FU@PASTE DS    CL(SBLEN)                                                        
FU@RECD  DS    CL8                                                              
FU@ELEM  DS    CL8                                                              
FU@PRGRS DS    CL17                                                             
FU@SCAND DS    CL20                                                             
*                                                                               
DSLISTL  DS    0F              *** LOWERCASE DICTIONARY                         
FL@SCAN  DS    XL8                                                              
FL@COPY  DS    CL(SBLEN)                                                        
FL@MOVE  DS    CL(SBLEN)                                                        
FL@DEL   DS    CL(SBLEN)                                                        
FL@AFTER DS    CL(SBLEN)                                                        
FL@BFR   DS    CL(SBLEN)                                                        
FL@INSRT DS    CL(SBLEN)                                                        
FL@REPL  DS    CL(SBLEN)                                                        
FL@SAVE  DS    CL(SBLEN)                                                        
FL@PASTE DS    CL(SBLEN)                                                        
FL@RECD  DS    CL8                                                              
FL@ELEM  DS    CL8                                                              
FL@PRGRS DS    CL17                                                             
FL@SCAND DS    CL20                                                             
*                                                                               
OPTBLK   DS    0XL20           *** OPTIONS BLOCK                                
         ORG   OPTBLK+L'OPTBLK                                                  
*                                                                               
SBLKNUM  EQU   10                                                               
SBLK     DS    (SBLKNUM)XL(SCBLKLQ)                                             
*                                                                               
SVLST    DS    XL(L'TLST)                                                       
*                                                                               
*        SPNFIWORK                                                              
         PRINT OFF                                                              
       ++INCLUDE SPNFIWORK                                                      
         PRINT ON                                                               
*        DDCOMFACS                                                              
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
*        DDFLDHDR                                                               
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*        DDSCANBLKD                                                             
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
*                                                                               
SAVED    DSECT                                                                  
KEYBLK   DS    CL150                                                            
REALKEY  DS    XL64                REAL KEY REQUIRED                            
LSNTRY   DS    XL(SUBACTLQ)        SINGLE ENTRY ACTION                          
LMNTRY   DS    XL(SUBACTLQ)        MULTIPLE ENTRY ACTION                        
LMCOUNT  DS    XL(SUBACTLQ)        REPEAT COUNT FOR MULTIPLES                   
*                                                                               
FVIEW    DS    X                   VIEW TYPE                                    
FVIEWR   EQU   C'R'                                                             
FVIEWE   EQU   C'E'                                                             
*                                                                               
ADDFLAG  DS    X                                                                
ADD1ST   EQU   X'80'                                                            
*                                                                               
CCOUNT   DS    H                   CURRENT BUILD COUNT                          
*                                                                               
DOFLAG   DS    XL1                 FLAG SUB-ACTIONS                             
DOFROM   EQU   X'80'               MOVE FROM ACTION REQUESTED                   
DOTO     EQU   X'40'               MOVE TO ACTION REQUESTED                     
DODEL    EQU   X'20'               DELETE ACTION REQUESTED                      
DOREP    EQU   X'10'               REPLICATE ACTION REQUESTED                   
DOINS    EQU   X'08'               INSERT ACTION REQUESTED                      
DOROWS   EQU   X'04'               RECALCULATE ROWS REQUIRED                    
DOPST    EQU   X'02'               PASTE EXTENDED ELEMENT                       
*                                                                               
DOTOCNT  DS    H                   COUNT OF 'DO FROM' ACTIONS                   
DOFRCNT  DS    H                   COUNT OF 'DO TO' ACTIONS                     
DODLCNT  DS    H                   COUNT OF 'DELETE' ACTIONS                    
DORPCNT  DS    H                   COUNT OF 'REPLICATE' ACTIONS                 
DOINCNT  DS    H                   COUNT OF 'INSERT' ACTIONS                    
DOPSCNT  DS    H                   COUNT OF 'PASTE' ACTIONS                     
*                                                                               
SCANLEN  DS    X                                                                
SCANCHR  DS    CL10                                                             
*                                                                               
SAVEL    DS    XL256               EXTENDED PASTE ELEMENT                       
*                                                                               
         EJECT                                                                  
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLKSNUM  DS    XL2                 SORT NUMBER                                  
         ORG   TLUSER                                                           
TLUSTAT  DS    XL1                 STATUS BYTE                                  
TLUSDEL  EQU   X'80'               DELETE THIS LINE                             
TLUSCPY  EQU   X'40'               COPY THIS LINE                               
TLUSMVE  EQU   X'20'               MOVE THIS LINE                               
TLUSAFT  EQU   X'10'               CPY/MVE AFTER THIS LINE                      
TLUSBEF  EQU   X'08'               CPY/MVE BEFORE THIS LINE                     
TLUSREP  EQU   X'04'               REPLICATE THIS LINE                          
TLUSINS  EQU   X'02'               INSERT A LINE                                
TLUSPST  EQU   X'01'               THIS LINE INSERTED                           
*                                                                               
TLUSTAT2 DS    XL1                 STATUS BYTE 2                                
TLU2INS  EQU   X'80'               THIS LINE INSERTED                           
*                                                                               
TLTYP    DS    XL1                 TYPE OF DATA                                 
TTKEY    EQU   C'K'                PART OF KEY                                  
TTELEM   EQU   C'E'                ONE OF RECORD ELEMENTS                       
TTREC    EQU   C'R'                IN RECORD VIEW MODE                          
*                                                                               
TDISP    DS    XL1                 DISPLAY TYPE                                 
TDREC    EQU   C'R'                UNBLOCKED RECORD                             
TDELEM   EQU   C'E'                BLOCKED ELEMENTS                             
*                                                                               
TLURPT   DS    XL1                 REPEAT COUNT                                 
*                                                                               
TLINS    DS    0XL2                INSERT DETAILS                               
TLICODE  DS    XL1                 INSERT CODE                                  
TLILEN   DS    XL1                 INSERT LENGTH                                
*                                                                               
TLLEN    DS    XL1                 LENGTH OF TLDIR                              
TLFIL    DS    0X                                                               
TLLENQ   EQU   *-TLSTD                                                          
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'172RENFI10   08/31/00'                                      
         END                                                                    
