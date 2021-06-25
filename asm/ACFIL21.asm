*          DATA SET ACFIL21    AT LEVEL 007 AS OF 08/10/11                      
*PHASE T62321C,*                                                                
*INCLUDE DECODE                                                                 
         SPACE 1                                                                
FIL21    TITLE 'FILE MAINTENANCE OVERLAY FOR ACC SYSTEM'                        
         SPACE 2                                                                
*YNGX 002 250203 ALLOW TO DELETE PASSIVE POINTER RECORD                         
*YNGX 003 021003 ALLOW TO MAINTAIN PROFIT AND LOSS DIRECTORY RECORD             
*YNGX 004 030105 <1024105> BUG FIX SWITCH FROM APPROVER TO FILE                 
         SPACE 2                                                                
FIL21    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL21**,RA,R7,RR=RE                                           
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
         SPACE 1                                                                
*                                                                               
GSFRP    USING FRPELD,GSFRPEL                                                   
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
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
*                                                                               
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITACNA MVC   FVMSGNO,=AL2(AE$INACT)   INVALID ACTION                          
         B     *+10                                                             
EXITARNA MVC   FVMSGNO,=AL2(AE$ACTNA)   ACTION NOT ALLOWED FOR ARCHIVE          
         LH    RF,GSDSPACT         SET CURSOR TO ACTION FIELD                   
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         B     EXITL                                                            
*                                                                               
FLTXL    MVI   SVPARMS,DFLTL       EXIT LOW FOR FILTER                          
         B     EXITOK                                                           
FLTXE    MVI   SVPARMS,DFLTE       EXIT EQUAL FOR FILTER                        
         B     EXITOK                                                           
FLTXH    MVI   SVPARMS,DFLTH       EXIT HIGH FOR FILTER                         
         B     EXITOK                                                           
FLTXX    MVI   SVPARMS,DFLTX       DEFINATELY NOT WANTED                        
         B     EXITOK                                                           
         EJECT ,                                                                
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
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
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
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUBACT)                              
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTO1 VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTO1 (RF),(R1),C'LL  ',,DSLISTL                                       
*                                                                               
         L     RF,=V(DECODE)       RELOCATE DECODE                              
         A     RF,BORELO                                                        
         ST    RF,ADECODE                                                       
         L     RF,ACOM                                                          
*&&UK*&& MVC   VTOBACCO,CTOBACCO-COMFACSD(RF)                                   
*                                                                               
*&&UK*&& MVC   COMPCURR,BCCPYCUR   PRIMARY CURRENCY CODE                        
*&&UK*&& MVC   COMPCURS,BCCPYSEC   2ND CURRENCY CODE                            
         OI    GSINDSL1,GSIXKEY    DON'T ASK 'ENTER KEY'                        
         OI    GCINDS3,GCINOACT    LEAVE ACTIVITY ALONE                         
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* SCREEN OBJECT                                                       *         
***********************************************************************         
         SPACE 1                                                                
SCRN     LM    R0,R3,SVPARMS                                                    
         LA    RF,SCRTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
SCRTABL  DC    AL1(SKSET),AL1(0,0,0),AL4(SCRKSET)                               
         DC    AL1(SSET),AL1(0,0,0),AL4(SCRMSET)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* SET KEY SCREEN CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
SCRKSET  XC    GSSKCODE,GSSKCODE                                                
         CLI   CSACT,A#CPY                                                      
         BNE   EXITOK                                                           
         MVI   GSSKCODE,C'A'                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DATA SCREEN CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
SCRMSET  XC    GSSMCODE,GSSMCODE                                                
         CLI   CSACT,A#CPY                                                      
         BNE   EXITOK                                                           
         MVI   GSSMCODE,C'A'                                                    
         B     EXITOK                                                           
         EJECT ,                                                                
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
KFKDIS   CLI   CSACT,A#CPY         ONLY IF COPYING                              
         BNE   EXITOK                                                           
         L     RF,AIOREC                                                        
         GOTO1 VHEXOUT,BOPARM,(RF),KEYBLK,L'ACCKEY,0                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  DS    0H                                                               
         GOTO1 VALOPT                                                           
         BL    EXITL                                                            
         B     EXITOK                                                           
         SPACE 2                                                                
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
KLKVAL   CLI   CSACT,A#CPY         ONLY IF KEY FIELDS ARE ON SCREEN             
         BNE   EXITOK                                                           
*                                                                               
         LA    R0,L'GSRECKEY                                                    
         GOTO1 ADECODE,BOPARM,((R0),KEYBLK),(0,(R2)),0                          
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         MVC   BOCURSOR,AKEY1                                                   
         B     EXITNV                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  LA    R0,L'LSINIKEY                                                    
         GOTO1 ADECODE,BOPARM,((R0),KEYBLK),(0,(R2)),0                          
         CLI   8(R1),0                                                          
         BE    EXITOK                                                           
         MVC   BOCURSOR,AKEY1                                                   
         B     EXITNV                                                           
         EJECT ,                                                                
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
RFTABL   DC    AL1(RCPY),AL1(0,0,0),AL4(RFCPY)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - COPY                                 *         
***********************************************************************         
         SPACE 1                                                                
RFCPY    TM    PSRECSTA,X'04'      IS IT AN ARCHIVE FILE?                       
         BNZ   EXITARNA            ACTION NOT ALLOW FOR ARCHIVE RECORD          
         OC    PSRECDA,PSRECDA                                                  
         BNZ   EXITOK              ACTION NOT ALLOW FOR P&L RECORD              
         MVC   FVMSGNO,=AL2(AE$INACT)                                           
         B     EXITL               INVALID ACTION                               
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE AND RESTORE                   *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    TM    GSRECSTA,X'04'      IS IT AN ARCHIVE FILE?                       
         BNZ   EXITARNA            ACTION NOT ALLOW FOR ARCHIVE RECORD          
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE                                                                  
RLWRT    CLI   UPDOCAEL,YES        UPDATE OCAEL?                                
         BNE   EXITOK                                                           
         GOTO1 AGETEL,BOPARM,('OCAELQ',AIOREC),0                                
         BE    RLWRT02                                                          
         OC    SVOCAEL,SVOCAEL                                                  
         BZ    EXITOK                                                           
         B     RLWRTX              OCAEL CHANGE - REBUILD THE LIST              
*                                                                               
RLWRT02  CLC   SVOCAEL,BOELEM      HAS OCAEL BEEN CHANGED?                      
         BE    EXITOK              NO - OK                                      
*                                                                               
RLWRTX   NI    LSLTIND1,FF-LSLTIBLD  REBUILD THE LIST                           
         OI    GENINDS,GENIENTR      ASK ROOT TO PRESS ENTER                    
         B     EXITOK                                                           
         DROP  R2                                                               
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
         LA    RF,KNOWLQ(,RF)                                                   
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
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(F#FILE#KEYL1),AL4(LN1DTA)  KEY LINE 1                        
         DC    AL2(F#FILE#KEYL2),AL4(LN2DTA)  KEY LINE 2                        
         DC    AL2(F#FILE#DSPLC),AL4(DSPDTA)  DISPLACEMENT - LIST SCR           
         DC    AL2(F#FILE#LHEXI),AL4(HEXDTA)  FILE HEX - LIST SCR               
         DC    AL2(F#FILE#LDECI),AL4(DECDTA)  FILE DEC - LIST SCR               
*                                                                               
         DC    AL2(F#FILE#FILNM),AL4(FNMDTA)  FILE NAME    - MAINT SCR          
         DC    AL2(F#FILE#DSKA1),AL4(DSKDTA)  DISK ADDRESS - MAINT SCR          
         DC    AL2(F#FILE#DSKA2),AL4(DSKDTA)  DISK ADDRESS - COPY SCR           
         DC    AL2(F#FILE#RECLN),AL4(LENDTA)  RECORD LENGTH                     
*&&UK*&& DC    AL2(F#FILE#UPDOC),AL4(UOCDTA)  UPDATE OCAEL ELEMENT              
         DC    AL2(F#FILE#MDPLC),AL4(MDSPDTA) DISPLACEMENT - DATA SCR           
         DC    AL2(F#FILE#RVIEW),AL4(VEWDTA)  RECORD VIEW TYPE                  
         DC    AL2(F#FILE#MHEXI),AL4(MHEXDTA) FILE HEX - DATA SCR               
         DC    AL2(F#FILE#MDECI),AL4(MDECDTA) FILE DEC - DATA SCR               
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
FIL21    CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* FIRST TIME FOR DATA OBJECT                                          *         
***********************************************************************         
         SPACE 1                                                                
DTAFRST  L     R1,SVPARMS3                                                      
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** FIRST TIME FOR DATA OBJECT ***                
*                                 -------------------------                     
DFTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A DATA OBJECT                            *         
***********************************************************************         
         SPACE 1                                                                
DFDVAL   CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BE    EXITOK              YES - EXIT                                   
         GOTO1 AGETEL,BOPARM,('OCAELQ',AIOREC),0                                
         MVC   SVOCAEL,BOELEM      SAVE OCAEL                                   
*&&UK                                                                           
         TM    GSRECSTA,X'04'      IS IT AN ARCHIVE FILE?                       
         BO    *+16                YES                                          
         CLI   CSACT,A#PRO                                                      
         BNE   EXITOK                                                           
         B     EXITACNA            CAN ONLY PROMOTE ARCHIVE RECORD              
                                                                                
         CLI   CSACT,A#PRO         PROMOTE ARCHIVE RECORD - OK                  
         BE    EXITOK                                                           
*&&                                                                             
         CLI   CSACT,A#DIS         ALLOW DISPLAY/LIST ARCHIVE FILE ONLY         
         BE    EXITOK                                                           
         CLI   CSACT,A#LST                                                      
         BE    EXITOK                                                           
         B     EXITARNA            ACTION NOT ALLOW FOR ARCHIVE RECORD          
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR DATA OBJECT                                           *         
***********************************************************************         
         SPACE 1                                                                
DTALAST  L     R1,SVPARMS3                                                      
         LA    RF,DLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR DATA OBJECT ***                 
*                                 ------------------------                      
DLTABL   DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
*&&US*&& B     EXITOK                                                           
*&&UK                                                                           
         BE    EXITOK              YES - DON'T UPDATE ACCOUNT RECORD            
         CLI   CSACT,A#PRO         PROMOTE ARCHIVE RECORD TO ACCMST             
         BNE   EXITOK                                                           
         GOTO1 VPROMOTE,BOPARM,AIOREC,ACOM                                      
         MVC   GSRECDA,BOPARM+8    MOVE BACK DISK ADDRESS                       
         NI    GSRECSTA,FF-X'04'   SWITCH OFF ARCHIVE RECORD STATUS             
         GOTOX AGEN,BOPARM,OKEY,KDIS,GSRECKEY                                   
         GOTOX AGEN,BOPARM,OACT,ASET,('A#CHA',0)                                
         GOTOX AGEN,BOPARM,OACT,ADIS   CHANGE ACTION TO 'CHANGE'                
         NI    GSINDSL1,FF-GSIXMNT     RE-BUILD THE LIST                        
         LH    RF,GSDSPACT             SET CURSOR TO ACTION FIELD               
         A     RF,ATWA                                                          
         ST    RF,FVADDR                                                        
         MVI   FVOMTYP,GTMINF          DISPLAY SPECIAL MESSAGE                  
         MVC   FVMSGNO,=AL2(AI$PROAR)                                           
         NI    GCINDS2,FF-GCIACTCP                                              
         B     EXITL               ARCHIVE RECORD PROMOTED TO FILE              
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR KEY LINE 1                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LN1DTA   LA    RF,LN1TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LN1TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN1)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN1)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DNTRLN1)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(SETLN1)                                 
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFLFLN1)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLN1)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFLN1)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLN1)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FOR NTRSES                                                  *         
***********************************************************************         
         SPACE 1                                                                
DNTRLN1  GOTO1 VHEXOUT,BOPARM,GSRECKEY,KEYBLK,L'ACCKEY,0                        
         B     DISLN1                                                           
         SPACE 2                                                                
***********************************************************************         
* SET PROTECTION FOR NESTED FIELDS                                    *         
***********************************************************************         
         SPACE 1                                                                
SETLN1   B     FLTXX                                                            
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR KEY LINE 1 FILTER                                   *         
***********************************************************************         
         SPACE 1                                                                
DFLFLN1  TM    NFILINDS,NFILPKEY   ANY KEY PASSED BY OTHER RECORD               
         BZ    EXITOK              NO - EXIT                                    
         MVC   FVIFLD(K_LEN),KEYBLK                                             
         B     EXITOK                                                           
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
         ST    RE,0(,R1)                                                        
         LA    RE,SNTR                                                          
         ST    RE,4(,R1)                                                        
         L     RF,AGEN                                                          
         L     RA,ATWA                                                          
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         DROP  N                                                                
*                                                                               
VLN102   MVC   AKEY1,FVADDR                                                     
         MVC   KEYBLK(K_LEN),FVIFLD                                             
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY LINE 1 AS A FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTLN1  MVC   FVIFLD(K_LEN),FLTIFLD                                            
         B     EXITOK                                                           
         SPACE 2                                                                
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
         ST    RE,0(,R1)                                                        
         LA    RE,SNTR                                                          
         ST    RE,4(,R1)                                                        
         L     RF,AGEN                                                          
         L     RA,ATWA                                                          
         BASR  RE,RF                                                            
         DC    H'0'                                                             
         DROP  N                                                                
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
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR KEY LINE 2                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LN2DTA   LA    RF,LN2TBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
LN2TBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLN2)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALLN2)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISLN2)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(SETLN2)                                 
         DC    AL1(DDFLTF),AL1(0,0,0),AL4(DFLFLN2)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTLN2)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VALFLN2)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFLN2)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY LINE 2                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISLN2   MVC   FVIFLD(K_LEN),KEYBLK+K_LEN                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET PROTECTION FOR NESTED FIELDS                                    *         
***********************************************************************         
         SPACE 1                                                                
SETLN2   B     FLTXX                                                            
         SPACE 2                                                                
***********************************************************************         
* VALIDATE KEY LINE 2                                                 *         
***********************************************************************         
         SPACE 1                                                                
VALLN2   MVC   AKEY2,FVADDR        SAVE A(SECOND LINE)                          
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         XC    KEYBLK+K_LEN(K_LEN),KEYBLK+K_LEN                                 
         B     EXITOK                                                           
*                                                                               
         MVC   KEYBLK+K_LEN(K_LEN),FVIFLD                                       
         MVI   KEYBLK+(K_LEN*2),C' '                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY KEY LINE 2 AS A FILTER                                      *         
***********************************************************************         
         SPACE 1                                                                
DFLTLN2  MVC   FVIFLD(K_LEN),FLTIFLD                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE KEY LINE 2 AS A FILTER                                     *         
***********************************************************************         
         SPACE 1                                                                
VALFLN2  MVC   AKEY2,FVADDR        SAVE A(SECOND LINE)                          
         MVC   FLTIFLD(K_LEN),FVIFLD                                            
         CLI   FVILEN,0                                                         
         BNE   *+14                                                             
         XC    KEYBLK+K_LEN(K_LEN),KEYBLK+K_LEN                                 
         B     EXITOK                                                           
*                                                                               
         CLC   KEYBLK+K_LEN(K_LEN),FVIFLD                                       
         BE    *+8                                                              
         OI    LSSCIND1,LSSCIFLT                                                
         MVC   KEYBLK+K_LEN(K_LEN),FVIFLD                                       
         MVI   KEYBLK+(K_LEN*2),C' '                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR KEY LINE 2 FILTERING                                *         
***********************************************************************         
         SPACE 1                                                                
DFLFLN2  TM    NFILINDS,NFILPKEY   ANY KEY PASSED BY OTHER RECORD               
         BZ    EXITOK              NO - EXIT                                    
         MVC   FVIFLD(K_LEN),KEYBLK+K_LEN                                       
         NI    NFILINDS,FF-NFILPKEY     RESET                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO KEY LINE 2 FILTERING                                             *         
***********************************************************************         
         SPACE 1                                                                
DOFLN2   B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR HEXADECIMAL DATA                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
HEXDTA   LA    RF,HEXTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                ITERATE TABLE                                
*                                                                               
HEXTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISHEX)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALHEX)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY HEXADECIMAL DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
DISHEX   CLC   LSROWREP,=AL2(2)                                                 
         BH    *+8                                                              
         OI    FVATRB,FVAPROT                                                   
*                                                                               
         LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         MHI   R0,LISWIDTH                                                      
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         LA    R1,LISWIDTH(,R1)    DISP TO END OF THIS LINE                     
         CR    R1,RF               WHOLE LINE REQUIRED?                         
         BH    *+12                NO                                           
         LA    RF,LISWIDTH         SET FULL WIDTH TO MOVE OUT                   
         B     DHEX02                                                           
*                                                                               
         SR    RF,R0                                                            
         BM    EXITOK                                                           
*                                                                               
DHEX02   GOTO1 VHEXOUT,BOPARM,(R3),FVIFLD,(RF),0                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE HEXADECIMAL DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
VALHEX   TM    FVIIND,FVIHEX                                                    
         BO    VHEX08                                                           
         XR    R0,R0                                                            
         IC    R0,FVILEN                                                        
         XR    RF,RF                                                            
         LA    R1,FVIFLD                                                        
*                                                                               
VHEX02   CLI   0(R1),C'A'                                                       
         BL    VHEX06                                                           
         CLI   0(R1),C'F'                                                       
         BNH   VHEX04                                                           
*                                                                               
         CLI   0(R1),C'0'                                                       
         BL    VHEX06                                                           
         CLI   0(R1),C'9'                                                       
         BH    VHEX06                                                           
*                                                                               
VHEX04   LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VHEX02                                                        
         DC    H'0'                                                             
*                                                                               
VHEX06   MVC   FVMSGNO,=AL2(GE$INHEX)                                           
         STC   RF,FVERRNDX                                                      
         B     EXITL                                                            
*                                                                               
VHEX08   MVC   SACTKSTA,TLFIL+(ACTKSTA-ACTRECD) SAVE STATUS                     
         LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         MHI   R0,LISWIDTH                                                      
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM TO                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         LA    R1,LISWIDTH(,R1)    DISP TO END OF THIS LINE                     
         CR    R1,RF               WHOLE LINE REQUIRED?                         
         BH    *+12                NO                                           
         LA    RF,LISWIDTH         SET FULL WIDTH TO MOVE IN                    
         B     VHEX10                                                           
*                                                                               
         SR    RF,R0                                                            
         BP    *+6                                                              
         DC    H'0'                ODD LENGTH                                   
*                                                                               
VHEX10   SLL   RF,1                HEX IS TWICE AS LONG                         
         GOTO1 VHEXIN,BOPARM,FVIFLD,(R3),(RF),0                                 
*                                                                               
         CLC   SACTKSTA,0(R3)                                                   
         BE    VHEX12              STATUS HASN'T BEEN CHANGED                   
*&&UK*&& TM    SACTKSTA,X'04'      ARCHIVE FILE?                                
*&&UK*&& BZ    *+14                NO - OK                                      
*&&UK*&& MVC   FVMSGNO,=AL2(AE$CCARC)                                           
*&&UK*&& B     VHEXER              CAN'T CHANGE ARCHIVE RECORD                  
         TM    0(R3),X'04'         SET ARCHIVE BIT ON?                          
         BZ    *+14                NO - OK                                      
         MVC   FVMSGNO,=AL2(AE$INVIN)                                           
         B     VHEXER              INVALID INPUT                                
*                                                                               
         CLC   TLFIL(L'ACTKEY),GSRECKEY PASSIVE POINTER RECORD?                 
         BNE   *+14                     YES - CHANGE DIR. ONLY                  
         OC    GSRECDA,GSRECDA          PROFIT AND LOSS RECORD                  
         BNE   VHEX11                   NO - CHANGE BOTH DIR AND FILE           
         CLC   GSRECDA,L'ACCKSTA(R3)                                            
         BE    *+14                     DISK ADDRESS UNCHANGED                  
         MVC   FVMSGNO,=AL2(AE$CHANA)   CHANGE NOT ALLOWED                      
         B     VHEXER                                                           
*                                                                               
         MVC   IOKEY(L'ACTKEY),TLFIL                                            
         L     R1,=AL4(XIO3+XOACCDIR+XORDUPD)                                   
         GOTO1 AIO                                                              
         BE    VHEX10A                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BZ    VHEX10A                                                          
         DC    H'0'                BAD PASSIVE POINTER RECORD!!!                
*                                                                               
VHEX10A  LA    RF,IOKEY                                                         
         USING ACTRECD,RF          RF=A(ACCOUNT RECORD KEY)                     
         MVC   ACTKSTA,TLFIL+(ACTKSTA-ACTRECD)                                  
         L     R1,=AL4(XIO3+XOACCDIR+XOWRITE)                                   
         GOTO1 AIO                 CHANGE DIR. ONLY FOR PASSIVE POINTER         
         BE    EXITOK                                                           
         DC    H'0'                                                             
         DROP  RF                                                               
*                                                                               
VHEX11   MVC   GSRECSTA(L'ACCKSTA),0(R3)                                        
         MVC   TLRSTA(L'ACCKSTA),0(R3)                                          
*                                                                               
VHEX12   CLC   GSRECDA,L'ACCKSTA(R3)                                            
         BE    EXITOK                   DISK ADDRESS UNCHANGED                  
         MVC   FVMSGNO,=AL2(AE$CHANA)   CHANGE NOT ALLOWED                      
VHEXER   XC    CCOUNT,CCOUNT                                                    
         NI    LSLTIND1,FF-(LSLTIBLD)   REBUILD THE LIST                        
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLACEMENT                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DSPDTA   LA    RF,DSPTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISPLACEMENT                                                *         
***********************************************************************         
         SPACE 1                                                                
DISDSP   LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         BNZ   *+8                                                              
         MVI   FVIFLD+4,C'-'       INDICATES FIRST LINE OF ELEMENT              
*                                                                               
         MHI   R0,LISWIDTH         DISPLACEMENT FROM                            
         CURED (R0),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         OC    FVIFLD(4),=CL4'0000'                                             
*                                                                               
         LH    R0,LSROWREP                                                      
         MHI   R0,LISWIDTH         MAX DISPLACEMENT REACHED                     
         XR    R1,R1                                                            
         IC    R1,TLLEN            LENGTH OF DATA                               
                                                                                
         CR    R1,R0               MORE DATA AFTER THIS?                        
         BH    *+6                 YES                                          
         LR    R0,R1               SET FULL WIDTH TO MOVE OUT                   
         SHI   R0,1                                                             
         CURED (R0),(4,FVIFLD+5),0,DMCB=BOPARM,ZERO=NOBLANK                     
         OC    FVIFLD+5(4),=CL4'0000'                                           
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DECIMAL INFORMATION                                 *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DECDTA   LA    RF,DECTBL           TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DECTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DECIMAL INFORMATION                                         *         
***********************************************************************         
         SPACE 1                                                                
DISDEC   OI    FVATRB,FVAPROT                                                   
         LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         MHI   R0,LISWIDTH                                                      
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         LA    R1,LISWIDTH(,R1)    DISP TO END OF MOVE BLOCK                    
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
         MVC   FVIFLD(0),0(R3)                                                  
         EX    RF,*-6                                                           
         TR    FVIFLD,TRTABL                                                    
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR FILE NAME                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNMDTA   LA    RF,FNMTBL           TABLE OF KNOWN VERBS                         
         USING ACCRECD,R2                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
FNMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFNM)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFLTFNM)                               
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETFNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFNM  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FILE NAME                                                   *         
***********************************************************************         
         SPACE 1                                                                
DISFNM   MVC   FVIFLD(L'ACCMST),ACCMST                                          
         TM    GSRECSTA,X'04'      IS IT AN ARCHIVE FILE?                       
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'ACCARC),ACCARC                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FILE NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
VALFNM   MVI   ARCHIVE,NO                                                       
         CLI   FVILEN,4                                                         
         BH    *+14                                                             
         MVC   FVMSGNO,=AL2(FVFSHRT)                                            
         B     EXITL               INPUT LENGTH TOO SHORT                       
         SR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BE    EXITOK                                                           
         CLC   FVIFLD(0),ACCMST                                                 
         EX    RF,*+8                                                           
         BNE   EXITNV                                                           
         CLC   FVIFLD(0),ACCARC                                                 
         MVI   ARCHIVE,YES                                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULTS FOR FILE NAME                                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTFNM  MVC   FVIFLD(L'ACCMST),ACCMST                                          
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISK ADDRESS                                        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DSKDTA   LA    RF,DSKTBL           TABLE OF KNOWN VERBS                         
         USING ACCRECD,R2                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
DSKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSK)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDSK)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETDSK)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETDSK  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISK ADDRESS                                                *         
***********************************************************************         
         SPACE 1                                                                
DISDSK   GOTO1 VHEXOUT,BOPARM,GSRECDA,FVIFLD,L'GSRECDA,0                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DISK ADDRESS                                               *         
***********************************************************************         
         SPACE 1                                                                
VALDSK   TM    FVIIND,FVIHEX       DISK ADDRESSES MUST BE HEX                   
         BO    *+14                                                             
         MVC   FVMSGNO,=AL2(GE$INHEX)                                           
         B     EXITL                                                            
         CLI   FVILEN,8                MUST BE CHARS                            
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INDKA)  INVALID DISK ADDRESS                     
         B     EXITL                                                            
*                                                                               
         GOTO1 VHEXIN,BOPARM,FVIFLD,DSKADDR,8,0                                 
         OC    12(4,R1),12(R1)                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   GSRECDA,DSKADDR                                                  
         MVC   IODAOVER,DSKADDR                                                 
         LHI   R1,XOGET+XOACCMST+XIO1                                           
         CLI   ARCHIVE,YES                                                      
         BNE   *+8                                                              
         LHI   R1,XOGET+XOACCARC+XIO1                                           
         GOTO1 AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(EG$INVDA)                                           
         B     EXITL                                                            
*                                                                               
         L     RF,AIO1                                                          
         MVC   0(L'ACCKEY,R2),0(RF)                                             
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR VIEW TYPE                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
VEWDTA   LA    RF,VEWTBL           TABLE OF KNOWN VERBS                         
         USING ACCRECD,R2                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
VEWTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISVEW)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALVEW)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DFLTVEW)                               
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
DISVEW   MVC   FVIFLD(L'UC@ELEM),UC@ELEM                                        
         CLI   FVIEW,FVIEWR                                                     
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'UC@RECD),UC@RECD                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE RECORD VIEW TYPE                                           *         
***********************************************************************         
         SPACE 1                                                                
VALVEW   MVC   BOBYTE1,FVIEW       SAVE OLD VALUE                               
         XR    RF,RF                                                            
         IC    RF,FVXLEN                                                        
         EX    RF,*+8                                                           
         BNE   VVEW04                                                           
         CLC   FVIFLD(0),UC@ELEM                                                
         MVI   FVIEW,FVIEWE        FORMAT ELEMENT                               
         B     VVEW08                                                           
*                                                                               
VVEW04   EX    RF,*+8                                                           
         BNE   EXITNV              INVALID VIEW TYPE                            
         CLC   FVIFLD(0),UC@RECD                                                
         MVI   FVIEW,FVIEWR        FORMAT RECORD                                
*                                                                               
VVEW08   CLC   FVIEW,BOBYTE1       VIEW TYPE CHANGED?                           
         BE    EXITOK              NO                                           
         XC    CCOUNT,CCOUNT                                                    
         NI    LSLTIND1,FF-(LSLTIBLD)  REBUILD THE LIST                         
         XC    GCLASKEY,GCLASKEY       SET KEY HAS BEEN CHANGED                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULTS FOR VIEW TYPE                                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTVEW  MVC   FVIFLD(L'UC@ELEM),UC@ELEM                                        
         CLI   FVIEW,FVIEWR                                                     
         BE    *+12                                                             
         MVI   FVIEW,FVIEWE        FORMAT ELEMENT                               
         B     EXITOK                                                           
         MVC   FVIFLD(L'UC@RECD),UC@RECD                                        
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR RECORD LENGTH                                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LENDTA   LA    RF,LENTBL           TABLE OF KNOWN VERBS                         
         USING ACCRECD,R2                                                       
         B     ITER                ITERATE TABLE                                
*                                                                               
LENTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISLEN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY RECO RD LENGTH                                              *         
***********************************************************************         
         SPACE 1                                                                
DISLEN   XR    RF,RF                                                            
         ICM   RF,3,ACCRLEN                                                     
         CURED (RF),(4,FVIFLD),0,ALIGN=LEFT,DMCB=BOPARM                         
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
* DATA OBJECT FOR UPDATE OCAEL ELEMENT FIELD                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
UOCDTA   LA    RF,UOCTBL                                                        
         B     ITER                                                             
*                                                                               
UOCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISUOC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALUOC)                                 
         DC    AL1(DDFLT),AL1(0,0,0),AL4(DDFTUOC)                               
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY A UPDATE OCAEL ELEMENT FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DISUOC   MVC   FVIFLD(L'BC@NO),BC@NO     DEFAULT IS NO                          
         CLI   UPDOCAEL,YES                                                     
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'BC@YES),BC@YES                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A UPDATE OCAEL ELEMENT FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VALUOC   MVI   UPDOCAEL,NO         DEFAULT IS NO                                
         CLC   FVIFLD(1),BC@NO                                                  
         BE    EXITOK                                                           
         CLC   FVIFLD(1),BC@YES                                                 
         BNE   EXITNV                                                           
         TM    BCCPYST7,CPYSSCNV   COMPANY HAS 2ND CURRENCY IN USE?             
         BZ    EXITNV                                                           
         MVI   UPDOCAEL,YES                                                     
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET DEFAULT FOR UPDATE OCAEL ELEMENT FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
DDFTUOC  MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
*&&                                                                             
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLACEMENT ON MAINTENANCE SCREEN                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MDSPDTA  LA    RF,MDSPTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MDSPTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMDSP)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DISPLACEMENT                                                *         
***********************************************************************         
         SPACE 1                                                                
DISMDSP  OC    TLUSTAT,TLUSTAT                                                  
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
         OC    TLUSTAT2,TLUSTAT2                                                
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         CLI   FVIEW,FVIEWR                                                     
         BNE   DMDSP02                                                          
         XR    R0,R0                                                            
         ICM   R0,3,TLKSNUM                                                     
         SHI   R0,1                                                             
         BNZ   *+8                                                              
         MVI   FVIFLD+4,C'-'       INDICATES FIRST LINE OF ELEMENT              
*                                                                               
         MHI   R0,DISWIDTH         DISPLACEMENT FROM                            
         CURED (R0),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         OC    FVIFLD(4),=CL4'0000'                                             
*                                                                               
         ICM   R0,3,TLKSNUM                                                     
         MHI   R0,DISWIDTH         DISPLACEMENT TO                              
         L     R1,AIOREC                                                        
         XR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R1)                                         
         CR    R0,RF                                                            
         BNH   *+6                                                              
         LR    R0,RF                                                            
*                                                                               
         SHI   R0,1                                                             
         CURED (R0),(4,FVIFLD+5),0,DMCB=BOPARM,ZERO=NOBLANK                     
         OC    FVIFLD+5(4),=CL4'0000'                                           
         B     EXITOK                                                           
*                                                                               
DMDSP02  LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         BNZ   *+8                                                              
         MVI   FVIFLD+4,C'-'       INDICATES FIRST LINE OF ELEMENT              
*                                                                               
         MHI   R0,DISWIDTH         DISPLACEMENT FROM                            
         CURED (R0),(4,FVIFLD),0,DMCB=BOPARM,ZERO=NOBLANK                       
         OC    FVIFLD(4),=CL4'0000'                                             
*                                                                               
         LH    R0,LSROWREP                                                      
         MHI   R0,DISWIDTH         MAX DISPLACEMENT REACHED                     
         XR    R1,R1                                                            
         IC    R1,TLLEN            LENGTH OF DATA                               
                                                                                
         CR    R1,R0               MORE DATA AFTER THIS?                        
         BH    *+6                 YES                                          
         LR    R0,R1               SET FULL WIDTH TO MOVE OUT                   
         SHI   R0,1                                                             
         CURED (R0),(4,FVIFLD+5),0,DMCB=BOPARM,ZERO=NOBLANK                     
         OC    FVIFLD+5(4),=CL4'0000'                                           
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR HEXADECIMAL DATA ON MAINTENANCE SCREEN              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MHEXDTA  LA    RF,MHEXTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MHEXTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMHEX)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMHEX)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY HEXADECIMAL DATA                                            *         
***********************************************************************         
         SPACE 1                                                                
DISMHEX  NI    FVATRB,FF-(FVAHIGH+FVAPROT)                                      
*&&UK                                                                           
         CLI   CSACT,A#PRO         ACTION PROMOTE - PROTECT ALL LINES           
         BNE   DMHEX02                                                          
         CLI   TLTYP,TTKEY                                                      
         BNE   DMHEX04                                                          
         OI    FVATRB,FVAHIGH+FVAPROT                                           
         B     DMHEX06                                                          
*&&                                                                             
DMHEX02  CLI   TLTYP,TTKEY         PART OF RECORD KEY?                          
         BNE   DMHEX06             NO                                           
         CLI   CSACT,A#CHA         ACTION CHANGE OR DISPLAY?                    
         BE    *+8                                                              
         CLI   CSACT,A#DIS                                                      
         BE    *+12                                                             
         OI    FVATRB,FVAHIGH      HIGHLIGHT KEY IF NOT CHANGE                  
         B     DMHEX06                                                          
*                                                                               
         CLC   LSROWREP,=AL2(3)    OTHERWISE PROTECT FIRST 3 LINES              
         BH    DMHEX06                                                          
DMHEX04  OI    FVATRB,FVAPROT                                                   
*                                                                               
DMHEX06  OC    TLUSTAT,TLUSTAT                                                  
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
         OC    TLUSTAT2,TLUSTAT2                                                
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         MHI   R0,DISWIDTH                                                      
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         LA    R1,DISWIDTH(,R1)    DISP TO END OF THIS LINE                     
         CR    R1,RF               WHOLE LINE REQUIRED?                         
         BH    *+12                NO                                           
         LA    RF,DISWIDTH         SET FULL WIDTH TO MOVE OUT                   
         B     DMHEX08                                                          
*                                                                               
         SR    RF,R0                                                            
         BM    EXITOK                                                           
*                                                                               
DMHEX08  GOTO1 VHEXOUT,BOPARM,(R3),FVIFLD,(RF),0                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE HEXADECIMAL DATA                                           *         
***********************************************************************         
         SPACE 1                                                                
VALMHEX  DS    0H                                                               
*&&UK*&& CLI   CSACT,A#PRO         ACTION PROMOTE - EXIT                        
*&&UK*&& BE    EXITOK                                                           
         CLI   FVIEW,FVIEWR        IN RECORD MODE?                              
         BNE   *+14                NO - OK                                      
         MVC   FVMSGNO,=AL2(AE$CHANA) CHANGE NOT ALLOWED                        
         B     VMHEXER             ERROR - CAN'T CHANGE ELEMENT CODE            
*                                                                               
         TM    FVIIND,FVIHEX       VALID HEX INPUT?                             
         BO    VMHEX08             YES                                          
         XR    R0,R0                                                            
         IC    R0,FVILEN           FIND FIRST NON-HEX FIELD                     
         XR    RF,RF                                                            
         LA    R1,FVIFLD                                                        
*                                                                               
VMHEX02  CLI   0(R1),C'A'                                                       
         BL    VMHEX06                                                          
         CLI   0(R1),C'F'                                                       
         BNH   VMHEX04                                                          
*                                                                               
         CLI   0(R1),C'0'                                                       
         BL    VMHEX06                                                          
         CLI   0(R1),C'9'                                                       
         BH    VMHEX06                                                          
*                                                                               
VMHEX04  LA    R1,1(,R1)                                                        
         LA    RF,1(,RF)                                                        
         BCT   R0,VMHEX02                                                       
         DC    H'0'                                                             
*                                                                               
VMHEX06  MVC   FVMSGNO,=AL2(GE$INHEX)                                           
         STC   RF,FVERRNDX                                                      
         B     EXITL                                                            
*                                                                               
VMHEX08  LH    R0,LSROWREP         REPEAT COUNT                                 
         SHI   R0,1                                                             
         BNZ   VMHEX10                                                          
         TM    TLUSTAT,TLUSINS     INSERTING AT THIS RECORD?                    
         BZ    VMHEX10                                                          
         CLI   FVIEW,FVIEWR            RECORD VIEW TYPE                         
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ANACR)  ACTION NOT ALLOWED                       
         B     EXITL                                                            
         GOTO1 VHEXIN,BOPARM,FVIFLD,TLINS,4,0                                   
         B     EXITOK                                                           
*                                                                               
VMHEX10  MHI   R0,DISWIDTH                                                      
         LR    R1,R0               DISP TO START OF THIS BLOCK                  
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM TO                                 
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN            LENGTH OF DATA                               
         LA    R1,DISWIDTH(,R1)    DISP TO END OF THIS LINE                     
         CR    R1,RF               WHOLE LINE REQUIRED?                         
         BH    *+12                NO                                           
         LA    RF,DISWIDTH         SET FULL WIDTH TO MOVE IN                    
         B     VMHEX12                                                          
*                                                                               
         SR    RF,R0                                                            
         BP    *+6                                                              
         DC    H'0'                ODD LENGTH                                   
*                                                                               
VMHEX12  MVC   BOBYTE1,0(R3)       SAVE PREVIOUS ELEMENT CODE                   
         SLL   RF,1                HEX SOURCE IS TWICE AS LONG                  
         GOTO1 VHEXIN,BOPARM,FVIFLD,(R3),(RF),0                                 
*                                                                               
         LH    R0,LSROWREP         IS THIS THE FIRST LINE?                      
         SHI   R0,1                                                             
         BNZ   EXITOK              NO                                           
         CLI   BOBYTE1,FF          IS IT A DUMMY ELEMENT                        
         BE    VMHEX18             YES - OK                                     
         CLC   BOBYTE1,0(R3)       HAS ELEMENT CODE BEEN CHANGED                
         BE    VMHEX18             NO - OK                                      
         MVC   FVMSGNO,=AL2(AE$ICCEC)                                           
         B     VMHEXER             ERROR - CAN'T CHANGE ELEMENT CODE            
*                                                                               
VMHEX18  CLI   TLFIL+1,1           MAKE SURE LENGTH IS VALID                    
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
         MVC   BOELEM(0),TLFIL     SAVE OFF CURRENT ELEMENT                     
         EX    RF,*-6                                                           
*                                                                               
         MVC   TLLEN,TLFIL+1       SET NEW LENGTH                               
         XC    TLFIL(256),TLFIL    CLEAR ELEMENT AREA IN TSAR RECORD            
         IC    RF,TLLEN                                                         
         BCTR  RF,0                                                             
         MVC   TLFIL(0),BOELEM     SET ELEMENT WITH CORRECT LENGTH              
         EX    RF,*-6                                                           
*                                                                               
         LA    RF,1(RF)                                                         
         AHI   RF,TLLENQ                                                        
         STCM  RF,3,TLRLEN         SET LENGTH TO SAVE                           
         OI    DOFLAG,DOROWS       RECALCULATE ROWS AFTER ALL VALIDATED         
         OI    TLUSTAT2,TLU2ROWS   RECALCULATE THIS ROW                         
         B     EXITOK                                                           
*                                                                               
VMHEXER  LH    R0,LSCURLIN                                                      
         A     R0,ATWA                                                          
         STCM  R0,15,BOCURSOR     SET CURSOR TO CURRENT LIST LINE               
         XC    CCOUNT,CCOUNT                                                    
         NI    LSLTIND1,FF-(LSLTIBLD) REBUILD THE LIST                          
         B     EXITL                                                            
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DECIMAL INFORMATION ON MAINTENANCE SCREEN           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
MDECDTA  LA    RF,MDECTBL          TABLE OF KNOWN VERBS                         
         USING TLSTD,R2                                                         
         B     ITER                ITERATE TABLE                                
*                                                                               
MDECTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISMDEC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMDEC)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY DECIMAL INFORMATION                                         *         
***********************************************************************         
         SPACE 1                                                                
DISMDEC  NI    FVATRB,FF-(FVAHIGH+FVAPROT)                                      
*&&UK                                                                           
         CLI   CSACT,A#PRO         ACTION PROMOTE - PROTECT ALL LINES           
         BNE   DMDEC02                                                          
         CLI   TLTYP,TTKEY                                                      
         BNE   DMDEC04                                                          
         OI    FVATRB,FVAHIGH+FVAPROT                                           
         B     DMDEC06                                                          
*&&                                                                             
DMDEC02  CLI   TLTYP,TTKEY         PART OF RECORD KEY?                          
         BNE   DMDEC06             NO                                           
         CLI   CSACT,A#CHA         ACTION CHANGE OR DISPLAY?                    
         BE    *+8                                                              
         CLI   CSACT,A#DIS                                                      
         BE    *+12                                                             
         OI    FVATRB,FVAHIGH      HIGHLIGHT KEY IF NOT CHANGE                  
         B     DMDEC06                                                          
                                                                                
         CLC   LSROWREP,=AL2(3)    OTHERWISE PROTECT FIRST 3 LINES              
         BH    DMDEC06                                                          
DMDEC04  OI    FVATRB,FVAPROT                                                   
*                                                                               
DMDEC06  OC    TLUSTAT,TLUSTAT                                                  
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
         OC    TLUSTAT2,TLUSTAT2                                                
         BZ    *+8                                                              
         OI    FVATRB,FVAHIGH                                                   
*                                                                               
         LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         MHI   R0,DISWIDTH                                                      
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R0               MOVE FROM R3                                 
*                                                                               
         LA    R1,DISWIDTH(,R1)    DISP TO END OF MOVE BLOCK                    
*                                                                               
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         CR    R1,RF               CAN WE MOVE IT ALL IN?                       
         BH    DMDEC08             NO                                           
         MVC   FVIFLD(DISWIDTH),0(R3)                                           
         TR    FVIFLD,TRTABL                                                    
         B     EXITOK                                                           
*                                                                               
DMDEC08  SR    RF,R0               LENGTH WE CAN MOVE IN                        
         BNP   EXITOK                                                           
         BCTR  RF,0                                                             
         MVC   FVIFLD(0),0(R3)                                                  
         EX    RF,*-6                                                           
         TR    FVIFLD,TRTABL                                                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE DECIMAL INFORMATION                                        *         
***********************************************************************         
         SPACE 1                                                                
VALMDEC  DS    0H                                                               
*&&UK*&& CLI   CSACT,A#PRO            ACTION PROMOTE - EXIT                     
*&&UK*&& BE    EXITOK                                                           
         CLI   FVIEW,FVIEWR           IN RECORD MODE                            
         BNE   VMDEC02                                                          
         MVC   FVMSGNO,=AL2(AE$CHANA) CHANGE NOT ALLOWED                        
         XC    CCOUNT,CCOUNT                                                    
         NI    LSLTIND1,FF-(LSLTIBLD) REBUILD THE LIST                          
         B     EXITL                                                            
VMDEC02  LH    R0,LSROWREP                                                      
         SHI   R0,1                                                             
         MHI   R0,DISWIDTH                                                      
         LR    R1,R0                                                            
         LA    R3,TLFIL                                                         
         AR    R3,R1               MOVE FROM TO                                 
*                                                                               
         LA    R1,DISWIDTH(,R1)    DISP TO END OF MOVE BLOCK                    
*                                                                               
         LA    RE,DISWIDTH         NUMBER OF CHARACTERS TO MOVE IN              
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         CR    R1,RF               CAN WE MOVE IT ALL IN?                       
         BNH   *+8                 YES                                          
         SR    RF,R0               LENGTH THAT CAN BE MOVED IN                  
         LR    RE,RF                                                            
         LA    RF,FVIFLD                                                        
         LH    R0,LSROWREP         IS THIS THE FIRST LINE?                      
         SHI   R0,1                                                             
         BNZ   VMDEC06             NO                                           
         LA    RF,2(,RF)           DON'T CHANGE ELEMENT CODE AND LENGTH         
         LA    R3,2(,R3)                                                        
         BCTR  RE,0                                                             
         BCTR  RE,0                                                             
*                                                                               
VMDEC06  CLI   0(RF),C'?'          SPECIAL CHARACTER?                           
         BE    *+10                YES - IGNORE                                 
         MVC   0(1,R3),0(RF)                                                    
*                                                                               
         LA    RF,1(,RF)                                                        
         LA    R3,1(,R3)                                                        
         BCT   RE,VMDEC06                                                       
         B     EXITOK                                                           
         DROP  R2                                                               
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
* P4 HOLDS A(MEMORY COVERED BY FESD)                                  *         
***********************************************************************         
         SPACE 1                                                                
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(XITIN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                            *         
***********************************************************************         
         SPACE 1                                                                
NTROUT   CLI   SACT,A#LST          ARE WE GOING TO LIST A RECORD?               
         BNE   EXITOK                                                           
         CLI   SREC,R#FILE         FILE RECORD                                  
         BE    *+12                                                             
         CLI   SREC,R#KEY          KEY RECORD                                   
         BNE   EXITOK                                                           
         OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM PREVIOUS LEVEL      *         
* --------------------------------------------------------------      *         
* SVPARMS3 = A(PSSAV)                                                 *         
* SVPARMS4 = A(FESD TO BE RESTORED)                                   *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    CLI   PSREC,R#KEY         ARE WE COMING FROM KEY RECORD                
         BE    NIN02                                                            
         OC    PSRECKEY(L'ACCKEY),PSRECKEY                                      
         BZ    EXITOK              NO PREVIOUS KEY - EXIT                       
         XC    KEYBLK,KEYBLK                                                    
         GOTO1 VHEXOUT,BOPARM,PSRECKEY,KEYBLK,L'ACCKEY,0                        
         NI    LSSCIND2,FF-LSSCIDIS    SET TO ALLOW VALIDATION                  
         B     NTRINX                                                           
*                                                                               
NIN02    CLC   KEYBLK(L'BCSPACES),BCSPACES  ANY KEY PASSED                      
         BNH   EXITOK                                                           
NTRINX   OI    NFILINDS,NFILPKEY   YES - SET KEY PASSED BIT ON                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER (EXIT BACK INTO)         *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
         BNE   EXITH                                                            
         NI    SNINDS1,FF-SNIUSECR   TURN OFF USE CURRENT RECORD BIT            
         MVI   LSLTIND1,0          TURN OFF LIST INDICATORS                     
         OI    LSSCIND1,LSSCIBLD   AND REBUILD LIST                             
*                                                                               
         CLI   SREC,R#KEY          ARE WE COMING FROM KEY RECORD                
         BNE   EXITOK              NO - EXIT                                    
         L     R3,ATWA                                                          
         AH    R3,GSDSPOVR         FIRST FIELD ON OVERLAY SCREEN                
         USING FHD,R3                                                           
         XR    R5,R5                                                            
XIN04    ICM   R5,1,FHLN           END OF TWA?                                  
         BZ    XIN10               NO                                           
         TM    FHAT,FHATXH         INPUT FIELD?                                 
         BZ    XIN08               NO                                           
*                                                                               
         LA    RF,FHD(R5)                                                       
         SHI   RF,FHDAD                                                         
         MVC   FVIXHDR,0(RF)       COPY ACROSS EXTENDED FIELD HEADER            
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,FVIXUS2                                                     
         BZ    XIN08               NOT ONE OF OURS                              
         BCTR  RF,0                                                             
         SLL   RF,2                                                             
         A     RF,AFDRADDR                                                      
         ICM   RF,15,0(RF)                                                      
*                                                                               
         CLC   =Y(F#FILE#KEYL1),FDRNUM-FDRELD(RF)                               
         BNE   XIN06               NOT KEY LINE 1                               
*                                                                               
         LR    RF,R5                                                            
         SHI   RF,FHDAD+FHDAD+1                                                 
         OI    FHOI,FHOITR                                                      
         MVC   FHDA(0),KEYBLK                                                   
         EX    RF,*-6                                                           
         LA    RF,1(,RF)                                                        
         STH   RF,BOHALF1                                                       
         B     XIN08                                                            
*                                                                               
XIN06    CLC   =Y(F#FILE#KEYL2),FDRNUM-FDRELD(RF)                               
         BNE   XIN08               NOT KEY LINE 2                               
         LR    RF,R5                                                            
         SHI   RF,FHDAD+FHDAD+1                                                 
         LH    RE,BOHALF1                                                       
         LA    RE,KEYBLK(RE)                                                    
         OI    FHOI,FHOITR                                                      
         MVC   FHDA(0),0(RE)                                                    
         EX    RF,*-6                                                           
         B     XIN08                                                            
*                                                                               
XIN08    LA    R3,0(R5,R3)                                                      
         B     XIN04                                                            
*                                                                               
XIN10    NI    GCINDS2,FF-GCIXITS                                               
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT ,                                                                
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
         USING FCRRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING ACCRECD,R2                                                       
         LA    RF,LISTABL                                                       
         USING OBJTABD,RF                                                       
LITER    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK                                                           
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    LITER02             MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     LITER               ITERATE TABLE                                
*                                                                               
LITER02  CLC   OBJIND3,GSSMPAGE    CHECK PAGE OK (0 FOR LIST)                   
         BE    LITER04                                                          
         LA    RF,OBJTABL(,RF)                                                  
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
         MVI   LSSUBLEN,3                                                       
*                                                                               
         MVC   LSROWLIN,=AL2(3)    SET REQUIRED NUMBER OF LINES                 
         MVC   LSNUMHED,=AL2(1)                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
FLST     L     RF,IOLOOP           NUMBER OF READS                              
         LA    RF,1(,RF)                                                        
         ST    RF,IOLOOP                                                        
         NI    GSINDSL3,FF-(GSI2LMSG)                                           
         NI    NFILINDS,FF-NFILRDLS                                             
*                                                                               
         MVC   IOKEY(L'ACCKEY),THIS.ACCRECD                                     
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=AL4(XOHIGH+XORDEL+XOACCDIR)                                  
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
NLST     L     RF,IOLOOP           INCREMENT READ COUNT                         
         LA    RF,1(,RF)                                                        
         ST    RF,IOLOOP                                                        
         CLC   RECNUM,IOLOOP       I WON'T LET YOU DO MORE THAN THIS            
         BL    EXITL                                                            
*                                                                               
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=AL4(XOSEQ+XORDEL+XOACCDIR)                                   
         GOTO1 AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
*                                                                               
NLST02   CLI   SCANLEN,0           ANYTHING TO COMPARE FOR?                     
         BE    NLST04              NO                                           
         CLC   IOKEY(1),THIS.ACCKEY                                             
         BE    *+18                                                             
         MVC   FVMSGNO,=AL2(AE$RECNF) RECORD NOT FOUND                          
         OI    NFILINDS,NFILRDLS   REBUILD THE LIST                             
         B     EXITL                                                            
*                                                                               
         ICM   R1,15,SVPARMS5                                                   
         A     R1,=AL4(XOGET+XOACCMST)                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
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
NLST04   MVC   THIS.ACCKEY(ACCKLEN),IOKEY                                       
         B     EXITOK                                                           
         DROP  R2,THIS                                                          
         SPACE 2                                                                
***********************************************************************         
* LAST FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
LSTLAST  CLC   RECNUM,IOLOOP       TOO MANY I/O'S                               
         BH    EXITOK              NO                                           
*                                                                               
         GOTO1 VHEXOUT,BOPARM,IOKEY,KEYBLK,L'ACCKEY,0                           
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
         SHI   RF,FHDAD                                                         
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
         CLC   =Y(F#FILE#KEYL1),FDRNUM-FDRELD(RF)                               
         BNE   LLST04              NOT KEY LINE 1                               
*                                                                               
         LR    RF,R5                                                            
         SHI   RF,FHDAD+FHDAD+1                                                 
         OI    FHOI,FHOITR                                                      
         MVC   FHDA(0),KEYBLK                                                   
         EX    RF,*-6                                                           
         LA    RF,1(,RF)                                                        
         STH   RF,BOHALF1                                                       
         B     LLST06                                                           
*                                                                               
LLST04   CLC   =Y(F#FILE#KEYL2),FDRNUM-FDRELD(RF)                               
         BNE   LLST06              NOT KEY LINE 2                               
         LR    RF,R5                                                            
         SHI   RF,FHDAD+FHDAD+1                                                 
         LH    RE,BOHALF1                                                       
         LA    RE,KEYBLK(RE)                                                    
         OI    FHOI,FHOITR                                                      
         MVC   FHDA(0),0(RE)                                                    
         EX    RF,*-6                                                           
         B     LLST06                                                           
*                                                                               
LLST06   LA    R3,0(R5,R3)                                                      
         B     LLST02                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM DIRECTORY RECORD                   *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R2                                                       
         USING TLSTD,R3                                                         
TSARDIR  LM    R2,R3,SVPARMS3                                                   
         MVI   TLTYP,TTKEY         SET DISPLAYING KEY                           
         LA    R1,ACCKLEN                                                       
         STC   R1,TLLEN            SET LENGTH OF KEY                            
         AHI   R1,TLLENQ                                                        
         STCM  R1,3,TLRLEN         SET LENGTH OF TSAR RECORD                    
*                                                                               
         LA    RE,ACCKLEN          LENGTH OF DIRECTORY RECORD                   
         SRDL  RE,32                                                            
         LA    R1,LISWIDTH         WIDTH OF A SINGLE LINE                       
         DR    RE,R1                                                            
         LTR   RE,RE               REMAINDER?                                   
         BZ    *+8                 NO                                           
         LA    RF,1(,RF)           DON'T FORGET THE BIT ON THE END              
         STC   RF,TLROWS           NUMBER OF LINES REQUIRED                     
*                                                                               
         MVC   TLFIL(ACCKLEN),0(R2)                                             
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* INITIALISE LIST 1                                                   *         
***********************************************************************         
         SPACE 1                                                                
ILST1    XC    CCOUNT,CCOUNT                                                    
         OI    LSSTAT1,LSSBALL                                                  
         NI    LSSTAT2,FF-LSSADD                                                
         OI    LSSTAT3,LS3RVAR                                                  
         MVC   LSNUMHED,=AL2(1)                                                 
*                                                                               
         MVI   LSSUBLEN,0          RESET SUB ACTION LENGTH                      
         MVI   LSSUBHLP,0          RESET SUB ACTION HELP PANEL                  
         MVI   LSLSTSCR,0          RESET LIST SCREEN                            
         CLI   CSACT,A#CHA         CHANGE                                       
         BNE   EXITOK                                                           
         MVI   LSSUBLEN,SBLEN                                                   
         MVI   LSSUBHLP,1          USE HELP PANEL FOR SUBACTS                   
         MVC   LSLSTSCR,CSREC                                                   
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
NLST1    CLI   FVIEW,FVIEWR        VIEW IS IN RECORD MODE?                      
         BNE   NLS100              NO                                           
*                                                                               
         L     R1,AIOREC           RECORD LENGTH                                
         XR    RF,RF                                                            
         ICM   RF,3,ACCRLEN-ACCRECD(R1)                                         
*                                                                               
         LH    RE,LINENOW          CURRENT DISPLACEMENT INTO THE RECORD         
         LA    RE,DISWIDTH(,RE)                                                 
         STH   RE,LINENOW                                                       
         CR    RE,RF                                                            
         BH    EXITL               ALREADY PAST THE END OF THE RECORD           
         B     EXITOK                                                           
*                                                                               
NLS100   OC    LINENOW,LINENOW     DISP TO CURRENT ELEMENT                      
         BNZ   NLST022                                                          
         LA    RF,ACCRFST-ACCRECD                                               
         STH   RF,LINENOW                                                       
         B     EXITOK              SET DISPLACEMENT TO FIRST ELEMENT            
*                                                                               
NLST022  LH    RF,LINENOW          CURRENTLY PROCESSED ELEMENT                  
         A     RF,AIOREC                                                        
*                                                                               
         CLI   0(RF),0             PROCESSED LAST BYTE OF RECORD?               
         BE    EXITL               YES                                          
         XR    RE,RE                                                            
         ICM   RE,1,1(RF)                                                       
         BZ    EXITL               ZERO LENGTH ELEMENT                          
         AR    RF,RE                                                            
         S     RF,AIOREC                                                        
         STH   RF,LINENOW                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM FILE RECORD                        *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,SVPARMS4                                                      
         USING TLSTD,R3                                                         
         L     R2,AIOREC                                                        
         USING ACCRECD,R2                                                       
*                                                                               
         LH    RF,CCOUNT                                                        
         LA    RF,1(,RF)                                                        
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        SET LINE NUMBER IN KEY                       
*                                                                               
         CLI   FVIEW,FVIEWR        VIEW IS IN RECORD MODE?                      
         BNE   TS100               NO                                           
*                                                                               
         MVI   TLTYP,TTREC     *** RECORD MODE                                  
         MVI   TLROWS,1                                                         
         LH    RF,LINENOW                                                       
         LA    RF,DISWIDTH(,RF)    END OF MOVE BLOCK                            
         XR    RE,RE                                                            
         ICM   RE,3,ACCRLEN        LENGTH OF RECORD                             
*                                                                               
         CR    RE,RF               WILL WHOLE LINE FIT?                         
         BL    *+12                NO                                           
         LA    R1,DISWIDTH                                                      
         B     TS02                                                             
*                                                                               
         SHI   RF,DISWIDTH                                                      
         SR    RE,RF                                                            
         LR    R1,RE               PARTIAL LINE LENGTH TO SAVE                  
*                                                                               
TS02     STC   R1,TLLEN                                                         
         LR    RF,R1                                                            
         AHI   R1,TLLENQ                                                        
         STCM  R1,3,TLRLEN         SET LENGTH TO SAVE                           
         BCTR  RF,0                                                             
         LH    RE,LINENOW                                                       
         A     RE,AIOREC                                                        
         MVC   TLFIL(0),0(RE)                                                   
         EX    RF,*-6                                                           
         B     EXITOK                                                           
*                                                                               
TS100    XR    RF,RF                                                            
         ICM   RF,3,LINENOW        DISP TO CURRENT ELEMENT                      
         BNZ   TSAF102                                                          
*                                                                               
         MVI   TLTYP,TTKEY         SET DISPLAYING KEY                           
         LA    R1,ACCRFST-ACCRECD                                               
         STC   R1,TLLEN            SET LENGTH OF KEY                            
         AHI   R1,TLLENQ                                                        
         STCM  R1,3,TLRLEN         SET LENGTH TO SAVE                           
*                                                                               
         LA    RE,ACCRFST-ACCRECD  LENGTH OF RECORD INFO                        
         SRDL  RE,32                                                            
         LA    R1,DISWIDTH         WIDTH OF A SINGLE LINE                       
         DR    RE,R1                                                            
         LTR   RE,RE               REMAINDER?                                   
         BZ    *+8                 NO                                           
         LA    RF,1(,RF)           DON'T FORGET THE BIT ON THE END              
         STC   RF,TLROWS           NUMBER OF LINES REQUIRED                     
*                                                                               
         L     RF,AIOREC                                                        
         LA    RE,ACCRFST-ACCRECD-1                                             
         MVC   TLFIL(0),0(RF)                                                   
         EX    RE,*-6                                                           
         B     EXITOK                                                           
*                                                                               
TSAF102  MVI   TLTYP,TTELEM        SET DISPLAYING ELEMENT                       
         A     RF,AIOREC           POINT TO ELEMENT                             
         XR    R1,R1                                                            
         ICM   R1,1,1(RF)          LENGTH OF ELEMENT                            
         BNZ   *+8                                                              
         LA    R1,1                FOR '00' ON END OF RECORD                    
         STC   R1,TLLEN                                                         
         LR    RE,R1                                                            
         SRDL  RE,32               SAVE ELEMENT LENGTH                          
*                                                                               
         AHI   R1,TLLENQ                                                        
         STCM  R1,3,TLRLEN         SET NEW TSAR RECORD LENGTH                   
*                                                                               
         LA    R1,DISWIDTH         LENGTH OF A SINGLE LINE                      
         DR    RE,R1                                                            
         LTR   RE,RE               EXTRA BIT ON END?                            
         BZ    *+8                 NO                                           
         LA    RF,1(,RF)                                                        
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
         MVC   TLFIL(0),0(RF)                                                   
         EX    RE,*-6                                                           
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST FOR SCREEN - PAGE 1                                            *         
***********************************************************************         
         SPACE 1                                                                
SCRLAST1 TM    DOFLAG,DOROWS       RECALCULATE ROWS ON RECORD(S)?               
         BZ    SLAST02             NO                                           
         NI    DOFLAG,FF-(DOROWS)                                               
         BAS   RE,ROWS                                                          
*                                                                               
         ICM   R1,7,=XL3'FFFFFF'   SET UP PAGE INFO AGAIN                       
         ICM   R1,8,=AL1(AGLSTPAG)                                              
         GOTOX AGENLST                                                          
*                                                                               
         MVC   LSLINE#,LSPAG#1     REDISPLAY FROM LINE 1                        
         ICM   R1,8,=AL1(AGLDSPAG)                                              
         GOTOX (RF)                                                             
*                                                                               
         OI    GCINDS2,GCIANYCH    SET CHANGES THIS TIME                        
         OI    GENINDS,GENIENTR    ASK ROOT TO PRESS ENTER                      
*                                                                               
SLAST02  TM    DOFLAG,DOFROM+DOTO                                               
         BO    SLAST04             FROM & TO ACTIONS BOTH SET                   
         BNZ   SLAST06             EITHER FROM OR TO ACTION SET                 
*                                                                               
         TM    DOFLAG,DODEL+DOREP+DOINS+DOPST                                   
         BZ    EXITOK                                                           
*                                                                               
SLAST04  BAS   RE,SORT             ACTIONS NEED RESOLVING                       
*                                                                               
         ICM   R1,7,=XL3'FFFFFF'   SET UP PAGE INFO AGAIN                       
         ICM   R1,8,=AL1(AGLSTPAG)                                              
         GOTOX AGENLST                                                          
*                                                                               
         MVC   LSLINE#,LSPAG#1     REDISPLAY FROM LINE 1                        
         ICM   R1,8,=AL1(AGLDSPAG)                                              
         GOTOX (RF)                                                             
*                                                                               
         XC    DOFLAG,DOFLAG       RESET THE FLAGS                              
         XC    DOFRCNT,DOFRCNT                                                  
         XC    DOTOCNT,DOTOCNT                                                  
         XC    DODLCNT,DODLCNT                                                  
         XC    DORPCNT,DORPCNT                                                  
         XC    DOINCNT,DOINCNT                                                  
         XC    DOPSCNT,DOPSCNT                                                  
                                                                                
         OI    GCINDS2,GCIANYCH    SET CHANGES THIS TIME                        
         B     SLAST08                                                          
*                                                                               
SLAST06  CLC   DOFRCNT,=H'1'       TEST DUPLICATE FROM FIELDS                   
         BNH   *+14                NO - GOOD                                    
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         B     EXITL                                                            
*                                                                               
         CLC   DOTOCNT,=H'1'       TEST DUPLICATE TO FIELDS                     
         BNH   *+14                NO - GOOD                                    
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         B     EXITL                                                            
*                                                                               
SLAST08  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* UPDATE RECORD FROM TSAR RECORDS - FIRST                             *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R2                                                       
UPDFRST1 CLI   CSACT,A#CPY                                                      
         BE    EXITOK                                                           
         CLI   CSACT,A#RES                                                      
         BE    EXITOK                                                           
*                                                                               
         L     R2,AIOREC                                                        
         CLI   FVIEW,FVIEWR                                                     
         BNE   UPF02                                                            
         XC    UPDNOW,UPDNOW                                                    
         LA    R3,2000             L' AN IO AREA                                
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R2,RE                                                            
         B     EXITOK                                                           
*                                                                               
UPF02    LA    RF,ACCRFST-ACCRECD+1                                             
         STCM  RF,3,ACCRLEN                                                     
         MVI   ACCRFST,0                                                        
         CLI   UPDOCAEL,YES        UPDATE OCAEL?                                
         BNE   EXITOK                                                           
*&&UK                                                                           
         GOTO1 VTOBACCO,BODMCB,('TOBAACNV',COMPCURD),AIOREC,ACOM,0              
*&&                                                                             
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
***********************************************************************         
* UPDATE RECORD FROM TSAR RECORDS                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  LM    R2,R3,SVPARMS3                                                   
         CLI   TLTYP,TTKEY         IGNORE KEY ELEMENT                           
         BE    EXITOK                                                           
         CLI   CSACT,A#CPY         DO NOTHING FOR ACTION 'COPY'                 
         BE    EXITOK                                                           
         CLI   CSACT,A#RES                                                      
         BE    EXITOK                                                           
*                                                                               
         CLI   TLLEN,2             IGNORE DUMMY ELEMENTS                        
         BL    EXITOK                                                           
         CLI   TLTYP,TTREC         BLOCKED RECORD DISPLAY?                      
         BE    URC102              YES                                          
*&&US*&& B     URC101                                                           
*&&UK                                                                           
         CLI   UPDOCAEL,YES        UPDATE OCAEL?                                
         BNE   URC101                                                           
         CLI   TLFIL,OCAELQ                                                     
         BE    EXITOK              YES - DON'T ADD OCAEL INTO FILE              
         GOTO1 VTOBACCO,BODMCB,('TOBAAADD',COMPCURD),AIOREC,ACOM,0,    X        
               TLFIL                                                            
         BE    EXITOK                                                           
         DC    H'0'                SOMETHING WRONG                              
*&&                                                                             
*                                                                               
URC101   GOTO1 VHELLO,BOPARM,(C'P',GCFILNAM),AIOREC,TLFIL,=C'ADD=END'           
         CLI   12(R1),0                                                         
         BE    EXITOK                                                           
         DC    H'0'                                                             
*                                                                               
URC102   L     R2,AIOREC                                                        
         LH    RF,UPDNOW                                                        
         AR    RF,R2           A(NEXT DATA BLOCK)                               
*                                                                               
         XR    RE,RE                                                            
         IC    RE,TLLEN                                                         
         BCTR  RE,0                                                             
         MVC   0(0,RF),TLFIL   COPY IN DATA                                     
         EX    RE,*-6                                                           
*                                                                               
         LA    RF,1(RE,RF)                                                      
         MVI   0(RF),0         ENSURE RECORD ENDS WITH X'00'                    
*                                                                               
         SR    RF,R2                                                            
         STH   RF,UPDNOW       SET NEXT DATA BLOCK                              
         STCM  RF,3,ACCRLEN    SET NEW RECORD LENGTH                            
*                                                                               
*        MVC   GSRECSTA(L'ACCRSTA),ACCRSTA                                      
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* VALIDATE OPTIONS FIELD                                              *         
***********************************************************************         
         SPACE 1                                                                
VALOPT   NTR1  ,                                                                
         XC    OPTBLK,OPTBLK                                                    
         MVI   SCANLEN,0                                                        
         XR    R2,R2                                                            
         ICM   R2,3,GSDSPOPT                                                    
         BZ    EXITOK                                                           
         A     R2,ATWA                                                          
         USING FHD,R2                                                           
         ST    R2,FVADDR                                                        
         TM    NFILINDS,NFILRDLS   REBUILD THE LIST                             
         BZ    *+8                                                              
         OI    LSSCIND1,LSSCIFLT   SET FILTER HAS BEEN CHANGED                  
         CLI   FHIL,0                                                           
         BE    EXITOK                                                           
*                                                                               
         GOTO1 VSCANNER,BOPARM,(R2),('SBLKNUM',SBLK),0                          
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
         LA    R3,SCBLKLQ(,R3)                                                  
         BCT   R0,VOPT02                                                        
         B     EXITOK                                                           
*                                                                               
VALSCAN  NTR1  ,                   'SCAN=' PARAMETER                            
         CLC   =C'X''',SC2NDFLD                                                 
         BNE   VALSCAN2                                                         
         XR    RF,RF                                                            
         IC    RF,SC2NDLEN                                                      
         BCTR  RF,0                                                             
         LA    R5,SC2NDFLD(RF)     LAST CHAR                                    
         CLI   0(R5),C''''                                                      
         BNE   EXITNV                                                           
         BCTR  RF,0                                                             
         BCTR  RF,0                                                             
         LR    RE,RF                                                            
         SRL   RE,1                HEX SOURCE IS TWICE AS LONG                  
         STC   RE,SC2NDLEN                                                      
         XC    BOWORK1,BOWORK1                                                  
         GOTO1 VHEXIN,BOPARM,SC2NDFLD+2,BOWORK1,(RF),0                          
         MVC   SC2NDFLD,BOWORK1                                                 
VALSCAN2 CLC   SCANLEN,SC2NDLEN                                                 
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
         DROP  R2,R3,R4                                                         
*                                                                               
VOPTS    DC    AL2(UC@SCAN-OVERWRKD,LC@SCAN-OVERWRKD),AL4(VALSCAN)              
         DC    AL2(EOT)                                                         
*                                                                               
VOPTSD   DSECT                                                                  
VOPTDDU  DS    AL2                                                              
VOPTDDL  DS    AL2                                                              
VOPTVAL  DS    AL4                                                              
VOPTLQ   EQU   *-VOPTSD                                                         
FIL21    CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* SUB ACTION OBJECT                                                   *         
* P3 = A(SUB-ACTION FIELD)                                            *         
***********************************************************************         
         SPACE 1                                                                
*UB      OC    GSSMPAGE,GSSMPAGE   VALIDATION ONLY FOR MAINT LIST               
*        BZ    EXITH                                                            
SUBACT   TM    LSSCIND2,LSSCIDIS   IGNORE IF WE ARE DISPLAYING                  
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
SUBVAL   L     R2,SVPARMS3         R2=A(SUB-ACTION FIELD)                       
         USING FHD,R2                                                           
         OC    GSSMPAGE,GSSMPAGE   VALIDATION ONLY FOR MAINT LIST               
         BNZ   SVAL01                                                           
         OC    GSRECDA,GSRECDA     PROFIT AND LOSS RECORD?                      
         BNZ   EXITH                                                            
         GOTOX ('FLDVAL',AGROUTS),0(R2)                                         
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         ST    R2,BOCURSOR         SET CURSOR TO SUB-ACTION FIELD               
         MVC   FVMSGNO,=AL2(AE$SUBNV)                                           
         B     EXITL               SUB-ACTION NOT VALID FOR P&L                 
*                                                                               
SVAL01   XC    LSNTRY,LSNTRY       CLEAR CURRENT SUB-ACTION ELEMENT             
         OI    LSSCIND1,LSSCIINP   KEEP CURRENT SCREEN                          
         CLI   FHDA,C'?'           HELP WANTED?                                 
         BE    EXITH                                                            
*                                                                               
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
         LA    RF,TLULQ(,RF)                                                    
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
         CLI   FVIEW,FVIEWR        RECORD VIEW TYPE                             
         BNE   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$ANACR)  ACTION NOT ALLOWED                       
         B     EXITL                                                            
*                                                                               
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
         LA    R1,1(,R1)                                                        
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
         LA    R3,SUBACTLQ(,R3)                                                 
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
         BZ    SVAL22                    NO                                     
         LH    RF,DOFRCNT                DECREMENT 'FROM' COUNT                 
         SHI   RF,1                                                             
         BP    *+8                                                              
         NI    DOFLAG,FF-DOFROM          TURN OFF FLAG IF NO 'FROM'             
         STH   RF,DOFRCNT                SAVE NEW 'FROM' COUNT                  
*                                                                               
SVAL22   TM    TLUSTAT,TLUSBEF+TLUSAFT   'TO' FLAG ON THIS ONE                  
         BZ    SVAL24                    NO                                     
         LH    RF,DOTOCNT                DECREMENT 'TO' COUNT                   
         SHI   RF,1                                                             
         BP    *+8                                                              
         NI    DOFLAG,FF-DOTO            TURN OFF FLAG IF NO 'TO'               
         STH   RF,DOTOCNT                SAVE NEW 'TO' COUNT                    
*                                                                               
SVAL24   TM    TLUSTAT,TLUSDEL           'DELETE' FLAG ON THIS ONE              
         BZ    SVAL26                    NO                                     
         LH    RF,DODLCNT                DECREMENT 'DELETE' COUNT               
         SHI   RF,1                                                             
         BP    *+8                                                              
         NI    DOFLAG,FF-DODEL           TURN OFF FLAG IF NO 'DELETE'           
         STH   RF,DODLCNT                                                       
*                                                                               
SVAL26   TM    TLUSTAT,TLUSREP     'REPLICATE' FLAG ON THIS ONE                 
         BZ    SVAL28              NO                                           
         LH    RF,DORPCNT          DECREMENT 'REPLICATE' COUNT                  
         SHI   RF,1                                                             
         BP    *+8                                                              
         NI    DOFLAG,FF-DOREP                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL28   TM    TLUSTAT,TLUSINS     'INSERT' FLAG ON THIS ONE                    
         BZ    SVAL30              NO                                           
         LH    RF,DOINCNT          DECREMENT 'INSERT' COUNT                     
         SHI   RF,1                                                             
         BP    *+8                                                              
         NI    DOFLAG,FF-DOINS                                                  
         STH   RF,DORPCNT                                                       
*                                                                               
SVAL30   XC    TLUSTAT,TLUSTAT     RESET ACTION FLAG ON THIS ONE                
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
         OI    FHII,FHIIVA                                                      
         B     EXITOK                                                           
*                                                                               
SVALL    B     EXITL                                                            
         DROP  SNGL,MLTI,R2,R4                                                  
*                                                                               
SUBACTS  DS    0H              *** TABLE OF VALID SUB-ACTIONS ***               
         DC    AL2(UC@COPY-OVERWRKD,LC@COPY-OVERWRKD),AL4(SUB_CPY)              
         DC    AL2(UC@MOVE-OVERWRKD,LC@MOVE-OVERWRKD),AL4(SUB_MVE)              
         DC    AL2(UC@DEL-OVERWRKD,LC@DEL-OVERWRKD),AL4(SUB_DEL)                
         DC    AL2(UC@BFR-OVERWRKD,LC@BFR-OVERWRKD),AL4(SUB_BFR)                
         DC    AL2(UC@AFTER-OVERWRKD,LC@AFTER-OVERWRKD),AL4(SUB_AFT)            
         DC    AL2(UC@REPL-OVERWRKD,LC@REPL-OVERWRKD),AL4(SUB_REP)              
         DC    AL2(UC@INSRT-OVERWRKD,LC@INSRT-OVERWRKD),AL4(SUB_INS)            
         DC    AL2(UC@SAVE-OVERWRKD,LC@SAVE-OVERWRKD),AL4(SUB_SAV)              
         DC    AL2(UC@PASTE-OVERWRKD,LC@PASTE-OVERWRKD),AL4(SUB_PST)            
         DC    AL2(EOT)                                                         
*                                                                               
SUBACTSD DSECT                                                                  
SUBUPR   DS    AL2                 DISPLACEMENT TO UPPERCASE NAME               
SUBLWR   DS    AL2                 DISPLACEMENT TO LOWER CASE NAME              
SUBRTN   DS    AL4                 DISPLACEMENT TO VALIDATION ROUTINE           
SUBACTLQ EQU   *-SUBACTSD                                                       
*                                                                               
FIL21    CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO DELETE A LINE FROM THE LIST                              *         
***********************************************************************         
         SPACE 1                                                                
SUB_DEL  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSDEL     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSDEL     SET TO DELETE THIS LINE                      
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SDEL02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SDEL02   LH    RF,DODLCNT          INCREMENT COUNT OF DELETE FIELDS             
         LA    RF,1(,RF)                                                        
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
         TM    TLUSTAT,TLUSREP     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSREP     SET TO REPLICATE THIS LINE                   
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SREP02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SREP02   LH    RF,DORPCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(,RF)                                                        
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
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SINS02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SINS02   LH    RF,DOINCNT          INCREMENT COUNT OF REPLICATE FIELDS          
         LA    RF,1(,RF)                                                        
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
         TM    TLUSTAT,TLUSCPY     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSCPY     SET TO COPY THIS FIELD                       
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SCPY02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SCPY02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(,RF)                                                        
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM       SET A 'FROM' ACTION REQUIRED                 
*                                                                               
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK              NO - GOOD                                    
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO MOVE A LINE IN A LIST                                    *         
***********************************************************************         
         SPACE 1                                                                
SUB_MVE  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSMVE     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSMVE                                                  
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SMVE02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SMVE02   LH    RF,DOFRCNT          INCREMENT COUNT OF 'FROM' FIELDS             
         LA    RF,1(,RF)                                                        
         STH   RF,DOFRCNT                                                       
         OI    DOFLAG,DOFROM                                                    
         CLC   DOFRCNT,=H'1'       TEST ANOTHER FROM FIELD SET UP               
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         B     EXITL                                                            
         SPACE  2                                                               
***********************************************************************         
* ROUTINE TO COPY BEFORE THIS LINE IN A LIST                          *         
***********************************************************************         
         SPACE 1                                                                
SUB_BFR  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT,TLUSBEF     HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         OI    TLUSTAT,TLUSBEF                                                  
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SBEF02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SBEF02   LH    RF,DOTOCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(,RF)                                                        
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
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
         LA    RF,1(,RF)                                                        
         STH   RF,DOTOCNT                                                       
         OI    DOFLAG,DOTO                                                      
         CLC   DOTOCNT,=H'1'       TEST ANOTHER COPY TO FIELD SET UP            
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         B     EXITL                                                            
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO SAVE ELEMENT FOR PASTE INTO ANOTHER RECORD               *         
***********************************************************************         
         SPACE 1                                                                
SUB_SAV  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    SAVEL,SAVEL                                                      
         XR    RF,RF                                                            
         ICM   RF,1,TLLEN            LENGTH OF ELEMENT                          
         BZ    EXITNV                                                           
         BCTR  RF,0                                                             
         MVC   SAVEL(0),TLFIL                                                   
         EX    RF,*-6                                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO PASTE EXTENDED SAVE ELEMENT                              *         
***********************************************************************         
         SPACE 1                                                                
SUB_PST  L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         TM    TLUSTAT2,TLU2PST    HAS THIS FIELD ALREADY BEEN FLAGGED?         
         BO    EXITOK              YES - DON'T REVALIDATE IT THEN               
*                                                                               
         MVC   FVMSGNO,=AL2(AE$NOELM)                                           
         CLI   SAVEL+1,2                                                        
         BL    EXITL               NOTHING TO PASTE                             
*                                                                               
         OI    TLUSTAT2,TLU2PST                                                 
         CLC   TLNUM,LSLST#X       IS THIS A NEW LINE                           
         BH    SPST02              YES - DON'T PUT IT BACK THEN                 
         GOTOX ('TSARIO',AGROUTS),TSAPUT                                        
*                                                                               
SPST02   LH    RF,DOPSCNT          INCREMENT COUNT OF 'TO' FIELDS               
         LA    RF,1(,RF)                                                        
         STH   RF,DOPSCNT                                                       
         OI    DOFLAG,DOPST                                                     
         CLC   DOPSCNT,=H'1'       TEST ANOTHER PASTE FIELD SET UP              
         BNH   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CCONF)                                           
         B     EXITL                                                            
         DROP  R3                                                               
         EJECT ,                                                                
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
         TM    TLUSTAT2,TLU2ROWS   RECALCULATE ROWS?                            
         BZ    ROWS02                                                           
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,TLLEN                                                         
         LA    R1,DISWIDTH                                                      
         DR    RE,R1                                                            
         LTR   RE,RE                                                            
         BZ    *+8                                                              
         LA    RF,1(,RF)                                                        
         STC   RF,TLROWS           SET NEW REQUIREMENT                          
*                                                                               
         NI    TLUSTAT2,FF-(TLU2ROWS)                                           
         GOTOX ('TSARIO',AGROUTS),TSAWRT                                        
         B     ROWS02                                                           
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* SORT OUT TSAR RECORDS BASED ON FLAGS SET IN THEM                    *         
***********************************************************************         
         SPACE 1                                                                
SORT     NTR1  ,                                                                
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
SORT02   LA    R1,TSANXT           DEAL WITH ALL DELETE REQUESTS                
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SORT04              END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   SORT04              DONE ALL FOR THIS LEVEL                      
*                                                                               
         TM    TLUSTAT,TLUSCPY+TLUSMVE                                          
         BZ    SORT02              NOT A COPY OR A MOVE RECORD                  
*                                                                               
         MVI   BOBYTE1,FF          FLAG RECORD TO COPY/MOVE                     
         LA    R0,SVLST            TSAR RECORD SAVE AREA                        
         LHI   R1,L'SVLST                                                       
*        LA    R1,L'SVLST                                                       
         LA    RE,TLSTD                                                         
         LR    RF,R1                                                            
         MVCL  R0,RE               SAVE A COPY OF THIS RECORD                   
         B     SORT02              NEXT IN LIST                                 
*                                                                               
SORT04   XC    CCOUNT,CCOUNT       RESET CURRENT LIST COUNT                     
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET NEST LEVEL                               
         LA    R1,TSARDH                                                        
         B     *+8                                                              
SORT06   LA    R1,TSANXT                                                        
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    SORT20              END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   SORT20              DONE ALL FOR THIS LEVEL                      
         TM    TLUSTAT,TLUSDEL+TLUSMVE                                          
         BNZ   SORT06              IGNORE IF COPY OR MOVE                       
*                                                                               
         MVI   BOBYTE2,0           RESET ADD INTO LIST FLAG                     
         NI    TLSTAT,FF-TLSKEYC   TURN OFF KEY CHANGED FLAG                    
         MVI   TLKSES,TLKSKEYC     SET TEMPORARY SHUFFLE KEY                    
         MVC   BCHALF,TLNUM        SAVE CURRENT LIST NUMBER                     
*                                                                               
         CLI   BOBYTE1,FF          CPY/MVE RECORD TO ADD IN?                    
         BNE   SORT08              NO                                           
         TM    TLUSTAT,TLUSAFT+TLUSBEF                                          
         BZ    SORT08              NO ADD BEFORE OR AFTER THIS RECORD           
         TM    TLUSTAT,TLUSAFT     ADDING AFTER THIS RECORD?                    
         BZ    *+12                NO                                           
         MVI   BOBYTE2,FF          SET ADD AFTER FLAG                           
         B     SORT08                                                           
*                                                                               
         BAS   RE,ADDIN            ADD RECORD BEFORE THIS RECORD                
*                                                                               
SORT08   TM    TLUSTAT,TLUSREP+TLUSINS REPLICATING OR INSERTING                 
         BNZ   *+12                YES                                          
         TM    TLUSTAT2,TLU2PST    PASTING SAVED ELEMENT                        
         BZ    SORT16              NO                                           
*                                                                               
         L     RE,AIO1             SAVE THIS RECORD                             
         LA    R0,TLREC                                                         
         XR    RF,RF                                                            
         ICM   RF,3,TLRLEN                                                      
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
*                                                                               
         TM    TLUSTAT,TLUSREP     REPLICATING A LINE?                          
         BO    SORT14              YES - JUST ADD RECORD TWICE                  
         TM    TLUSTAT,TLUSINS     INSERTING AN ELEMENT?                        
         BO    SORT10              YES                                          
*                                                                               
         XR    RF,RF               COPY EXTENDED ELEMENT INTO TLFIL             
         IC    RF,SAVEL+1                                                       
         BCTR  RF,0                                                             
         MVC   TLFIL(0),SAVEL                                                   
         EX    RF,*-6                                                           
*                                                                               
         LA    RF,1(,RF)                                                        
         STC   RF,TLLEN                                                         
         LR    R1,RF               SAVE LENGTH OF ELEMENT                       
         AHI   RF,TLLENQ                                                        
         STCM  RF,3,TLRLEN                                                      
*                                                                               
         XR    R0,R0               SET ROW COUNT REQUIRED TO DISPLAY            
         LA    RE,DISWIDTH                                                      
         DR    R0,RE                                                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(,R1)                                                        
         STC   R1,TLROWS                                                        
         B     SORT14                                                           
*                                                                               
SORT10   XR    RF,RF                                                            
         ICM   RF,1,TLILEN                                                      
         BNZ   *+8                                                              
         LA    RF,2                DEFAULT LENGTH IS 2                          
         XC    TLFIL(0),TLFIL                                                   
         EX    RF,*-6                                                           
*                                                                               
         XR    RF,RF               SET ELEMENT CODE                             
         ICM   RF,1,TLICODE                                                     
         BNZ   *+8                                                              
         LA    RF,FF                                                            
         STC   RF,TLFIL                                                         
*                                                                               
         ICM   RF,1,TLILEN                                                      
         BNZ   *+8                                                              
         LA    RF,2                DEFAULT LENGTH IS 2                          
*                                                                               
SORT12   STC   RF,TLLEN                                                         
         STC   RF,TLFIL+1          SET ELEMENT LENGTH                           
         LR    R1,RF               SAVE LENGTH OF ELEMENT                       
         AHI   RF,TLLENQ                                                        
         STCM  RF,3,TLRLEN                                                      
*                                                                               
         XR    R0,R0               SET ROW COUNT REQUIRED TO DISPLAY            
         LA    RE,DISWIDTH                                                      
         DR    R0,RE                                                            
         LTR   R0,R0                                                            
         BZ    *+8                                                              
         LA    R1,1(,R1)                                                        
         STC   R1,TLROWS                                                        
*                                                                               
         XC    TLINS,TLINS                                                      
*                                                                               
SORT14   LH    RF,CCOUNT                                                        
         LA    RF,1(,RF)                                                        
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        RESET KEY NUMBER                             
         XC    TLUSTAT,TLUSTAT     RESET STATUS BYTE                            
         XC    TLUSTAT2,TLUSTAT2   RESET STATUS BYTE                            
         GOTOX ('TSARIO',AGROUTS),TSAADD                                        
*                                                                               
         L     RE,AIO1             RESTORE THIS RECORD                          
         LA    R0,TLREC                                                         
         XR    RF,RF                                                            
         ICM   RF,3,TLRLEN-TLREC(RE)                                            
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         XC    TLINS,TLINS                                                      
*                                                                               
SORT16   LH    RF,CCOUNT           SET NEW ORDERING FOR LINE COUNT              
         LA    RF,1(,RF)                                                        
         STH   RF,CCOUNT                                                        
         STCM  RF,3,TLKSNUM        RESET KEY NUMBER                             
         XC    TLUSTAT,TLUSTAT     RESET STATUS BYTE                            
         XC    TLUSTAT2,TLUSTAT2   RESET STATUS BYTE                            
*                                                                               
         GOTOX ('TSARIO',AGROUTS),TSAADD  ADD TEMPORARY COPY OF RECORD          
*                                                                               
         CLI   BOBYTE2,FF          ADD AFTER FLAG SET?                          
         BNE   SORT18              NO                                           
         BAS   RE,ADDIN            ADD RECORD AFTER THIS RECORD                 
*                                                                               
SORT18   MVC   TLNUM,BCHALF        RE-GET ORIGINAL FOR READ SEQUENCE            
         GOTOX ('TSARIO',AGROUTS),TSAGET                                        
         B     SORT06              NEXT IN LIST                                 
*                                                                               
SORT20   XC    TLKEY,TLKEY         DELETE ALL CURRENT RECORDS                   
         MVC   TLKSES,SESNL                                                     
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SORT22              END OF FILE                                  
         CLC   TLKSES,SESNL        SAME NEST LEVEL?                             
         BNE   SORT22              NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         B     SORT20                                                           
*                                                                               
SORT22   XC    TLKEY,TLKEY         MOVE SHUFFLED TO CURRENT SESSION             
         MVI   TLKSES,TLKSKEYC     TEMPORARY SHUFFLED NEST LEVEL                
         GOTOX ('TSARIO',AGROUTS),TSARDH                                        
         BL    SORTX               END OF FILE                                  
         CLI   TLKSES,TLKSKEYC     STILL TEMPORARY LEVEL?                       
         BNE   SORTX               NO - FINISHED                                
         GOTOX (RF),TSADEL                                                      
         MVC   TLKSES,SESNL        RESTORE TO CURRENT NEST LEVEL                
         GOTOX (RF),TSAADD                                                      
         B     SORT22                                                           
*                                                                               
SORTX    MVC   LSLST#X,LSLST#1     SET START                                    
         NI    LSLTIND1,FF-LSLTISOL                                             
         XR    RF,RF                                                            
         ICM   RF,3,CCOUNT         COUNT OF ITEMS ADDED TO LIST                 
         BZ    EXITOK              NOTHING                                      
*                                                                               
         AH    RF,LSLST#X                                                       
         BCTR  RF,0                                                             
         STH   RF,LSLST#X          SET CORRECT VALUE FOR END                    
         OI    LSLTIND1,LSLTISOL                                                
         B     EXITOK                                                           
         DROP  R3                                                               
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO ADD A RECORD SAVED IN SVLST TO CURRENT POINTER IN LIST   *         
***********************************************************************         
         SPACE 1                                                                
IN       USING TLSTD,SVLST                                                      
ADDIN    NTR1  ,                                                                
         MVI   IN.TLKSES,TLKSKEYC  SET TEMPORARY SHUFFLE KEY                    
         LH    RF,CCOUNT           SET NEW ORDERING FOR LINE COUNT              
         LA    RF,1(,RF)                                                        
         STH   RF,CCOUNT                                                        
         STCM  RF,3,IN.TLKSNUM     RESET KEY NUMBER                             
         XC    IN.TLUSTAT,IN.TLUSTAT                                            
         XC    IN.TLUSTAT2,IN.TLUSTAT2                                          
         GOTOX ('TSARIO',AGROUTS),BOPARM,('TSAADD',IN.TLSTD)                    
         B     EXITOK                                                           
         DROP  IN                                                               
         EJECT ,                                                                
***********************************************************************         
* SCAN RECORD FOR STING                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING ACCRECD,R2                                                       
SCANREC  NTR1  ,                                                                
         L     R2,AIOREC                                                        
         XR    R0,R0                                                            
         ICM   R0,3,ACCRLEN                                                     
         XR    RF,RF                                                            
         IC    RF,SCANLEN                                                       
         BCTR  RF,0                                                             
         SR    R0,RF                                                            
*                                                                               
SCREC02  EX    RF,SCANMCH                                                       
         BE    EXITOK                                                           
         LA    R2,1(,R2)                                                        
         BCT   R0,SCREC02                                                       
         B     EXITL                                                            
*                                                                               
SCANMCH  CLC   SCANCHR(0),0(R2)                                                 
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* PROGRESS REPORT                                                     *         
***********************************************************************         
         SPACE 1                                                                
PROGRESS NTR1  ,                                                                
         L     R2,ATWA                                                          
         USING TWAD,R2                                                          
         MVC   TWAMSG,BCSPACES                                                  
         MVC   TWAMSG(L'LC@PRGRS),LC@PRGRS                                      
         LA    R4,TWAMSG+L'LC@PRGRS                                             
         L     RF,IOLOOP                                                        
         CURED (RF),(9,(R4)),0,DMCB=BOPARM,ALIGN=LEFT                           
         AR    R4,R0                                                            
         LA    R4,1(,R4)                                                        
         MVC   0(L'LC@SCAND,R4),LC@SCAND                                        
         OI    TWAMSGH+FHOID,FHOITR                                             
*                                                                               
         MVC   FVADDR,AKEY1                                                     
         MVC   FVMSGNO,=AL2(FVFSET)                                             
         GOTOX ('SETMSG',AGROUTS)                                               
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CPROTOFF-COMFACSD(,RF)                                        
         BASR  RE,RF                                                            
*                                                                               
         GOTO1 VGETFACT,BOPARM,(X'80',BOWORK1),F#WRITE                          
*                                                                               
         L     RF,ACOM                                                          
         L     RF,CPROTON-COMFACSD(,RF)                                         
         BASR  RE,RF                                                            
                                                                                
         L     R1,=AL4(XOHIGH+XORDEL+XOACCDIR)                                  
         GOTO1 AIO                                                              
         B     EXITOK                                                           
         DROP  R2                                                               
         SPACE 2                                                                
         EJECT ,                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
FF       EQU   X'FF'                                                            
NO       EQU   C'N'                                                             
YES      EQU   C'Y'                                                             
SBLEN    EQU   4                                                                
K_LEN    EQU   70                                                               
LISWIDTH EQU   21                  WIDTH TO DISPLAY ON LIST                     
DISWIDTH EQU   20                  WIDTH TO DISPLAY ON MAINT                    
RECNUM   DC    F'50000'            MAX NUMBER OF SCAN I/OS ALLOWED              
OUTNUM   DC    H'10000'            NUMBER OF READS PER MESSAGE                  
ACCMST   DC    C'ACCMST'                                                        
ACCARC   DC    C'ACCARC'                                                        
*                                                                               
FVFIOLIM EQU   X'FF00'+045         I/O LIMIT REACHED                            
EG$INVDA EQU   X'FF00'+065         INVALID DISK ADDRESS                         
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
         DC    AL1(TLUSDEL),AL2(LC@DEL-OVERWRKD)                                
         DC    AL1(TLUSREP),AL2(LC@REPL-OVERWRKD)                               
         DC    AL1(TLUSINS),AL2(LC@INSRT-OVERWRKD)                              
         DC    AL1(TLUSCPY),AL2(LC@COPY-OVERWRKD)                               
         DC    AL1(TLUSMVE),AL2(LC@MOVE-OVERWRKD)                               
         DC    AL1(TLUSAFT),AL2(LC@AFTER-OVERWRKD)                              
         DC    AL1(TLUSBEF),AL2(LC@BFR-OVERWRKD)                                
         DC    AL1(EOT)                                                         
*                                                                               
TLUACTSD DSECT                                                                  
TLUFLAG  DS    XL1                                                              
TLUNAME  DS    AL2                                                              
TLULQ    EQU   *-TLUACTSD                                                       
*                                                                               
FIL21    CSECT                                                                  
         SPACE 2                                                                
DCLIST   DS    0D                                                               
         DCDDL GE#SCAN,8,L                                                      
         DCDDL AC#COPY,SBLEN,L                                                  
         DCDDL AC#MOVE,SBLEN,L                                                  
         DCDDL AC#DEL,SBLEN,L                                                   
         DCDDL AC#AFT,SBLEN,L                                                   
         DCDDL AC#BEF,SBLEN,L                                                   
         DCDDL AC#INSRT,SBLEN,L                                                 
         DCDDL AC#RPLIC,SBLEN,L                                                 
         DCDDL AC#SAVE,SBLEN,L                                                  
         DCDDL GE#PASTE,SBLEN,L                                                 
         DCDDL GE#RECD,8,L                                                      
         DCDDL GE#ELEM,8,L                                                      
         DCDDL GE#PRGRS,17,L                                                    
         DCDDL GE#SCAND,20,L                                                    
DCLISTX  DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
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
VTOBACCO DS    V                                                                
ADECODE  DS    A                                                                
AKEY1    DS    A                                                                
AKEY2    DS    A                                                                
ADSK     DS    A                                                                
DSKADDR  DS    F                                                                
*                                                                               
IOLOOP   DS    F                                                                
LINENOW  DS    H                   CURRENT LINE NUMBER FOR LIST BUILD           
UPDNOW   DS    H                                                                
SESNL    DS    X                                                                
ARCHIVE  DS    X                                                                
UPDOCAEL DS    CL1                 UPDATE OCAEL ELEMENT                         
COMPCURD DS    0CL6                CASHVAL CURRENCY CODES                       
COMPCURR DS    CL(L'BCCPYCUR)      PRIMARY CURRENCY CODE                        
*&&UK                                                                           
COMPCURS DS    CL(L'BCCPYSEC)      2ND CURRENCY CODE                            
*&&                                                                             
SACTKSTA DS    CL(L'ACTKSTA)                                                    
*                                                                               
DSLISTU  DS    0F              *** UPPERCASE DICTIONARY                         
UC@SCAN  DS    XL8                                                              
UC@COPY  DS    CL(SBLEN)                                                        
UC@MOVE  DS    CL(SBLEN)                                                        
UC@DEL   DS    CL(SBLEN)                                                        
UC@AFTER DS    CL(SBLEN)                                                        
UC@BFR   DS    CL(SBLEN)                                                        
UC@INSRT DS    CL(SBLEN)                                                        
UC@REPL  DS    CL(SBLEN)                                                        
UC@SAVE  DS    CL(SBLEN)                                                        
UC@PASTE DS    CL(SBLEN)                                                        
UC@RECD  DS    CL8                                                              
UC@ELEM  DS    CL8                                                              
UC@PRGRS DS    CL17                                                             
UC@SCAND DS    CL20                                                             
*                                                                               
DSLISTL  DS    0F              *** LOWERCASE DICTIONARY                         
LC@SCAN  DS    XL8                                                              
LC@COPY  DS    CL(SBLEN)                                                        
LC@MOVE  DS    CL(SBLEN)                                                        
LC@DEL   DS    CL(SBLEN)                                                        
LC@AFTER DS    CL(SBLEN)                                                        
LC@BFR   DS    CL(SBLEN)                                                        
LC@INSRT DS    CL(SBLEN)                                                        
LC@REPL  DS    CL(SBLEN)                                                        
LC@SAVE  DS    CL(SBLEN)                                                        
LC@PASTE DS    CL(SBLEN)                                                        
LC@RECD  DS    CL8                                                              
LC@ELEM  DS    CL8                                                              
LC@PRGRS DS    CL17                                                             
LC@SCAND DS    CL20                                                             
*                                                                               
OPTBLK   DS    0XL20           *** OPTIONS BLOCK                                
         ORG   OPTBLK+L'OPTBLK                                                  
*                                                                               
SBLKNUM  EQU   10                                                               
SBLK     DS    (SBLKNUM)XL(SCBLKLQ)                                             
*                                                                               
SVOCAEL  DS    XL(L'BOELEM)                                                     
SVLST    DS    XL(L'TLST)                                                       
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT ,                                                                
***********************************************************************         
* DSECT                                                               *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
KEYBLK   DS    CL150                                                            
LSNTRY   DS    XL(SUBACTLQ)        SINGLE ENTRY ACTION                          
LMNTRY   DS    XL(SUBACTLQ)        MULTIPLE ENTRY ACTION                        
LMCOUNT  DS    XL(SUBACTLQ)        REPEAT COUNT FOR MULTIPLES                   
*                                                                               
FVIEW    DS    X                   VIEW TYPE                                    
FVIEWR   EQU   C'R'                                                             
FVIEWE   EQU   C'E'                                                             
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
NFILINDS DS    XL1                 FILE PROGRAM INDICTOR                        
NFILRDLS EQU   X'80'               REBUILD THE LIST                             
NFILPKEY EQU   X'40'               KEY PASSED FROM KEY BUILDER                  
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
         EJECT ,                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
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
TLUTINS  EQU   X'01'               THIS LINE INSERTED                           
*                                                                               
TLUSTAT2 DS    XL1                 STATUS BYTE 2                                
TLU2ROWS EQU   X'80'               RECALCULATE NUMBER OF ROWS                   
TLU2PST  EQU   X'40'               PASTE EXTENDED ELEMENT                       
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
TLINS    DS    0XL2                INSERT DETAILS                               
TLICODE  DS    XL1                 INSERT CODE                                  
TLILEN   DS    XL1                 INSERT LENGTH                                
*                                                                               
TLLEN    DS    XL1                 LENGTH OF TLDIR                              
TLFIL    DS    0X                                                               
TLLENQ   EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007ACFIL21   08/10/11'                                      
         END                                                                    
