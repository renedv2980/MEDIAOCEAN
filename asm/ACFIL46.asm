*          DATA SET ACFIL46    AT LEVEL 031 AS OF 11/20/18                      
*PHASE T62346A                                                                  
                                                                                
         TITLE 'LIMIT LIST RECORD'                                              
*                                                                               
*YNGX 002 07JUN05 <LO01-4233> LIMLIST TO BE HELD AT PRO/JOB LEVELS              
*NSHE 003 21JUL05 <LO01-4423> ADD WORKCODES AND NON CLIENT ACCOUNTS             
*NSHE 004 06OCT05 <LO01-4786> CHANGES FOLLOWING RECORD LAYOUT CHANGE            
*DPEA 005 17MAR06 <1030515> BUG FIX                                             
*SMAN 006 25MAY06 <LO01-5373> ADD 1R ACCOUNTS, FILTERING + BUG FIXES            
*DKEL 007 14AUG06 <LO01-5644> ADD ACCENT SCRIBE SPECS TO RECORD                 
*SMAN 008 05APR07 <LO01-5992> BRAND OCEAN REPORTING FOR NON (VB)                
*                  ACCENT MODULES                                               
*MPEN 009 19AUG08 <DU01-7035> LIMLIST DOWNLOAD REPORT                           
*NSHE 010 20OCT08 REMOVE CALLS TO PIDPAS                                        
*MPEN 011 18DEC08 <BR21980L> REMOVE IOCHK ROUTINE                               
*MPEN 011 18DEC08 <LO01-8320> ADD ESTIMATE SCHEMES TO LIMLIST/GRPLIST           
*MPEN 012 23DEC08 <LO01-8355> NEW LIMLIST GROUPLIST SUPPLIER PAGE               
*MPEN 013 01SEP09 <BR26910L> FIX NOW DOWNLOAD REQUEST BUG                       
*TKLU 014 22JUL09 <LO01-8970> ALLOW SEQUENTIALS AND IMPLEMENT NEW LIDEL         
*                  STRUCTURE (APPLICATION BOUND SUB LIST ENTRIES)               
*         11SEP09 <LO01-9254> ADD 'DEFAULT ACCESS' AND 'MODULE COLUMN'          
*                 FIELDS FOR MAINTENANCE AND D/LOAD                             
*YNGX 015 23SEP09 <BR27526L> DISABLE NOW REPORT                                 
*YNGX 016 24SEP09 <BR27640L> REPLACE WC NAME COLUMN WITH WC DESC COLUMN         
*MPEN 017 12OCT09 <BR14765D> FIX BUG WHERE NOT PUTTING OFF IN LIMLIST           
*NSHE 018 09DEC09 <UKCR26125> ADD OFFICE COLUMN FOR CLIENT/PRO PAGE             
*MPEN     26OCT09 <LO01-9355> NEW FILTERING ON PID                              
*MPEN     11NOV09 <DU01-9467> NEW QUICK REPORT FLAG                             
*         22FEB10 <LO01-9681> ALLOW USE TO REMOVE OFFICE IF CLIENT              
*JFOS 019 21OCT10 <PR000995> USE PADDLE TO GENERATE PASSIVES                    
*YNGX 020 31JAN11 <PR001426> RELINK TO INCLUDE NEW TLSTD                        
*JFOS 021 22AUG11 <PR002088> ADD ESTIMATES TO 1R APPS                           
*DCUR 025 NOV04/11 MERGE WITH UK (TAKE LVL 21 CHANGES)                          
*MPEN     25OCT18 <DSRD-20447> RELINK FOR NEW DPAPASD                           
*                                                                               
FIL46    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**FL46**,RA,RR=RE                                              
         USING WORKD,R9                                                         
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         LHI   R6,TWUSER-TWAD                                                   
         A     R6,ATWA                                                          
         USING SAVED,R6                                                         
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         ST    R1,CALLR1                                                        
         MVC   SVPARMS,0(R1)                                                    
*                                                                               
         MVC   ACONST(CONSTX),CONSTS                                            
         LA    R1,OVROUT1                                                       
         LA    R0,OVROUT1N                                                      
         XR    RE,RE                                                            
         LR    R2,RF                                                            
         L     RF,=A(OVROU1)                                                    
         A     RF,BORELO                                                        
STRT01   STCM  RE,1,0(R1)                                                       
         STCM  RF,7,1(R1)                                                       
         LA    R1,4(R1)                                                         
         LA    RE,1(RE)                                                         
         BCT   R0,STRT01                                                        
         L     RE,ATWA                                                          
         MVC   SESNL,TWASESNL-TWAD(RE)                                          
*                                                                               
         LR    RF,R2                                                            
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
EXITLONG MVC   FVMSGNO,=AL2(FVFLONG)                                            
         B     EXITL               EXIT WITH FIELD TOO LONG SET                 
EXITOFF  MVC   FVMSGNO,=AL2(AE$EXPNV) EXPENDITURE TYPE NOT VALID FOR            
         B     EXITL                  THIS OFFICE                               
*                                                                               
EXITIACT MVC   FVMSGNO,=AL2(AE$IACTS)   INVALID ACTION FOR THIS SCREEN          
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
FLTXX    MVI   SVPARMS,DFLTX       EXIT DEFINITELY NOT VALID                    
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     BRAS  RE,INITR            INIT RELOCATED                               
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
         LA    RF,OBJTABL(,RF)                                                  
         B     ITER                ITERATE TABLE                                
*                                                                               
ITERH    CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITH               * NOT OVERRIDE                               
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATE                              
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(,RF)                                                  
         B     ITERH               ITERATE TABLE                                
*                                                                               
ITER02   ICM   RF,15,OBJADR        INVOKE OBJECT                                
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
         LA    RF,TABLEOO                                                       
         B     ITERH                                                            
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECORD)                                
         DC    AL1(ODLOAD),AL1(0,0,0),AL4(DLOAD)                                
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OOPT),AL1(0,0,0),AL4(OPT)                                    
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
         USING LLSRECD,R2                                                       
         LA    RF,KEYTABL                                                       
         B     ITER                                                             
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
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
*                             *** FIRST TIME FOR KEY OBJECT ***                 
*                                 -------------------------                     
KFTABL   DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(KFVAL),AL1(0,0,0),AL4(KFKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
KFKVAL   XC    LLSKEY,LLSKEY       INITIALIZE KEY OF RECORD                     
         MVC   LLSKCPY,CUABIN      CONNECTED ID                                 
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR VALIDATE OF A KEY FILTER                             *         
***********************************************************************         
         SPACE 1                                                                
KFKFVAL  XC    LLSKEY,LLSKEY       INITIALIZE KEY OF RECORD                     
         MVC   LLSKCPY,CUABIN      CONNECTED ID                                 
         MVI   LLSKTYP,LLSKTYPQ                                                 
         MVI   LLSKSUB,LLSKSUBQ                                                 
*                                                                               
         XC    SVFLTS(SVFLTLQ),SVFLTS                                           
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR KEY OBJECT                                            *         
***********************************************************************         
         SPACE 1                                                                
KEYLAST  L     R1,SVPARMS4         SUB ACTION                                   
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                                                             
*                                                                               
*                             *** LAST TIME FOR KEY OBJECT ***                  
*                                 ------------------------                      
KLTABL   DC    AL1(KFVAL),AL1(0,0,0),AL4(KLKFVAL)    VALIDATE FILTER            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A KEY FILTER                              *         
***********************************************************************         
         SPACE 1                                                                
KLKFVAL  XC    CDOPTION,CDOPTION                                                
         GOTO1 AVALDOPT,0                                                       
         BL    EXITL                                                            
         CLC   CDOPTION,SDOPTION   HAS OPTIONS BEEN CHANGED?                    
         BE    EXITOK                                                           
         MVI   LSSCIND1,LSSCIFLT   REFRESH LIST                                 
         MVC   SDOPTION(SDOPTSL),CDOPTION                                       
         B     EXITOK                                                           
         DROP  R2                                                               
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
         USING LLSRECD,R2                                                       
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
RFTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RFADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RFDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RFRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFWRT)                                  
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - RESTORE                              *         
***********************************************************************         
         SPACE 1                                                                
RFRES    CLI   LLSKSUB,LLSKSUBQ                                                 
         BE    RFRES02                                                          
         MVC   FVMSGNO,=AL2(AE$MUGLS) MAINTAIN THROUGH GRPLIST                  
         B     EXITL                                                            
PAS      USING LLSRECD,IOKEY                                                    
RFRES02  XC    PAS.LLSKEY,PAS.LLSKEY                                            
         MVC   PAS.LLSKPIDB,LLSKPIDB                                            
         MVI   PAS.LLSKTYP,LLSKTYPQ                                             
         MVI   PAS.LLSKSUB,LLSKSUBQ                                             
         MVC   PAS.LLSKCPY,CUABIN      CONNECTED ID                             
         MVC   SVIOKEY,PAS.LLSKEY                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTOR AIO                                                              
         CLC   SVIOKEY(LLSKGRP-LLSKEY),PAS.LLSKEY                               
         BNE   EXITOK                                                           
         OC    PAS.LLSKGRP,PAS.LLSKGRP                                          
         BZ    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$PIDGP) PID EXISTS ON A GRPLIST RECORD            
         B     EXITL                                                            
         DROP  PAS                                                              
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - WRITE                                *         
***********************************************************************         
         SPACE 1                                                                
RFWRT    DS    0H                                                               
         GOTO1 AMYRFWRT                                                         
         BL    EXITL                                                            
         BH    EXITIACT                                                         
*&&UK*&& GOTO1 ADELPAS,BOPARM,LLSRECD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - ADD OR COPY                          *         
***********************************************************************         
         SPACE 1                                                                
RFADD    DS    0H                                                               
         GOTO1 AMYRFADD                                                         
         BL    EXITL                                                            
         BH    EXITIACT                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR RECORD OBJECT - DELETE                               *         
***********************************************************************         
         SPACE 1                                                                
RFDEL    MVC   FVADDR,APIDFLD      SET CURSOR TO PID FIELD                      
         CLI   LLSKSUB,LLSKSUBQ                                                 
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$MUGLS) MAINTAIN THROUGH GRPLIST                  
         B     EXITL                                                            
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT                                         *         
***********************************************************************         
         SPACE                                                                  
RECLAST  L     R1,SVPARMS4         R1=INVOKER`S VERB                            
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DC    AL1(RADD),AL1(0,0,0),AL4(RLADD)                                  
         DC    AL1(RDEL),AL1(0,0,0),AL4(RLDEL)                                  
         DC    AL1(RRES),AL1(0,0,0),AL4(RLRES)                                  
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLWRT)                                  
         DC    AL1(RCPY),AL1(0,0,0),AL4(RLCPY)                                  
         DC    AL1(EOT)                                                         
         SPACE                                                                  
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - ADD                                   *         
***********************************************************************         
         SPACE                                                                  
RLADD    DS    0H                                                               
*&&UK*&& GOTO1 AADDPAS,BOPARM,LLSRECD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - DELETE                                *         
***********************************************************************         
         SPACE                                                                  
T        USING LLSRECD,R4                                                       
RLDEL    DS    0H                                                               
*&&UK*&& GOTO1 ADELPAS,BOPARM,T.LLSRECD                                         
         MVC   IOKEY(L'LLSKEY),LLSKEY                                           
*                                                                               
RLDEL10  LA    R4,IOKEY            READ NEXT LIMLST SUB-RECORD                  
         LLC   RF,T.LLSKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.LLSKSEQ                                                     
*                                                                               
         MVC   LLSSAVDA,IOKEY+LLSKDA-LLSRECD                                    
         L     R1,=AL4(XORDUP+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         OI    T.LLSKSTAT,LLSSDELT DELETE LIMLST DIR                            
         L     R1,=AL4(XOWRITE+XOACCDIR+XIO2)                                   
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         OI    T.LLSRSTAT,LLSSDELT DELETE MASTER RECORD                         
         L     R1,=AL4(XOPUTREC+XOACCMST+XIO2)                                  
         GOTOR AIO                                                              
                                                                                
*&&UK*&& GOTO1 ADELPAS,BOPARM,AIO2                                              
         B     RLDEL10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - COPY                                  *         
***********************************************************************         
         SPACE                                                                  
T        USING LLSRECD,R4                                                       
RLCPY    DS    0H                                                               
*&&UK*&& GOTO1 AADDPAS,BOPARM,LLSRECD                                           
         MVC   SVIOKEY,0(R2)       SAVE NEW KEY (COPY TO)                       
         LA    R4,IOKEY                                                         
         MVC   T.LLSKEY,GSCPYKEY   RESTORE OLD KEY (COPY FROM)                  
*                                                                               
RLCPY10  LA    R4,IOKEY            READ NEXT LIMLST SUB-RECORD                  
         LLC   RF,T.LLSKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.LLSKSEQ                                                     
*                                                                               
         MVC   LLSSAVDA,IOKEY+LLSKDA-LLSRECD                                    
         L     R1,=AL4(XORDD+XOACCDIR+XIO2)                                     
         GOTOR AIO                                                              
         BE    RLCPY20                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   RLCPY20                                                          
         B     EXITOK              END OF RECORDS - EXIT                        
*                                                                               
RLCPY20  L     R1,=AL4(XOGET+XOACCMST+XIO2)                                     
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
NEW      USING LLSRECD,R1                                                       
         LA    R1,SVIOKEY                                                       
         L     R4,AIO2                                                          
         MVC   T.LLSKEY(L'LLSKEY-1),NEW.LLSKEY                                  
         NI    T.LLSRSTAT,FF-LLSSDELT                                           
         L     R1,=AL4(XOADDREC+XOACCMST+XIO2)                                  
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*&&UK*&& GOTO1 AADDPAS,BOPARM,AIO2                                              
         B     RLCPY10                                                          
                                                                                
         DROP  T,NEW                                                            
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - RESTORE                               *         
***********************************************************************         
         SPACE                                                                  
T        USING LLSRECD,R4                                                       
RLRES    DS    0H                                                               
*&&UK*&& GOTO1 AADDPAS,BOPARM,LLSRECD                                           
                                                                                
         MVC   IOKEY(L'LLSKEY),LLSKEY                                           
*                                                                               
RLRES10  LA    R4,IOKEY            READ NEXT LIMLST SUB-RECORD                  
         SR    RF,RF               THEY ARE ALWAYS IN SEQUENCE                  
         IC    RF,T.LLSKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.LLSKSEQ                                                     
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO2)                                   
         GOTOR AIO                                                              
         BE    RLRES10             RECORD FOUND - GET NEXT                      
         TM    IOERR,IOERRS-(IOEDEL+IOERNF)                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         TM    IOERR,IOERNF        RECORD NOT FOUND - END                       
         BO    EXITOK                                                           
*                                                                               
         NI    T.LLSKSTAT,FF-LLSSDELT     RESTORE IT                            
         L     R1,=AL4(XOWRITE+XOACCDIR+XIO2)                                   
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD DIR RECORD                               
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R4,AIO2                                                          
         NI    T.LLSRSTAT,FF-LLSSDELT                                           
         L     R1,=AL4(XOPUTREC+XOACCMST+XIO2)                                  
         GOTOR AIO                                                              
*&&UK*&& GOTO1 AADDPAS,BOPARM,AIO2                                              
         B     RLRES10                                                          
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR RECORD OBJECT - WRITE                                 *         
***********************************************************************         
         SPACE                                                                  
T        USING LLSRECD,R4                                                       
RLWRT    DS    0H                                                               
*&&US*&& B     EXITOK                                                           
*                                                                               
*&&UK                                                                           
         GOTO1 AADDPAS,BOPARM,LLSRECD                                           
         MVC   IOKEY(L'APPKEY),LLSKEY                                           
*                                                                               
RLWRT04  LA    R4,IOKEY            READ NEXT SUB-RECORD                         
         LLC   RF,T.LLSKSEQ        THEY ARE ALWAYS IN SEQUENCE                  
         AHI   RF,1                                                             
         STC   RF,T.LLSKSEQ                                                     
*                                                                               
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         BNE   EXITOK              DELETE OR NO FOUND - END                     
*                                                                               
         GOTO1 AADDPAS,BOPARM,AIO2                                              
         B     RLWRT04                                                          
         DROP  T                                                                
*&&                                                                             
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
DATA     ICM   R1,15,SVPARMS2      R1 HOLDS DATA IDENTIFIER                     
         BNZ   DATA02              ACTION IS ON A DATA OBJECT                   
*                                                                               
         L     R2,SVPARMS+12                                                    
         USING LLSRECD,R2                                                       
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
         LA    RF,KNOWLQ(,RF)                                                   
         B     DATA04                                                           
         SPACE 1                                                                
DATA06   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
*                                                                               
         LM    R1,R3,SVPARMS3      R1 HOLDS VERB                                
         USING LLSRECD,R2          R2 HOLDS A(RECORD)                           
         USING FDRELD,R3           R3 HOLDS A(FIELD TABLE ENTRY)                
*        BR    RF                                                               
         BASR  RE,RF                                                            
         B     EXIT                                                             
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
KNOWTAB  DC    AL2(F#LIML#PID),AL1(0),AL3(PID)      PID CODE                    
         DC    AL2(F#LIML#PIDL),AL1(0),AL3(PILD)    PID CODE IN LIST            
         DC    AL2(F#LIML#LNAM),AL1(0),AL3(LNMD)    LAST NAME                   
         DC    AL2(F#LIML#LNML),AL1(0),AL3(LNLD)    LAST NAME IN LIST           
         DC    AL2(F#LIML#FNAM),AL1(0),AL3(FNMD)    FIRST NAME                  
         DC    AL2(F#LIML#FNML),AL1(0),AL3(FNLD)    FIRST NAME IN LIST          
         DC    AL2(F#LIML#GRPC),AL1(0),AL3(GRCD)    GROUP CODE                  
         DC    AL2(F#LIML#GRPL),AL1(0),AL3(GRLD)    GROUP CODE IN LIST          
         DC    AL2(F#LIML#GRPN),AL1(0),AL3(GRND)    GROUP NAME                  
         DC    AL2(F#LIML#GRNL),AL1(0),AL3(GNLD)    GROUP NAME IN LIST          
         DC    AL2(F#LIML#FCPJ),AL1(0),AL3(FCPJ)    CLI/PRO/JOB FILTER          
         DC    AL2(F#LIML#FETY),AL1(0),AL3(FETYD)   EXPENDITURE FILTER          
         DC    AL2(F#LIML#FWC),AL1(0),AL3(FWCD)     WORKCODE FILTER             
         DC    AL2(F#LIML#FNCL),AL1(0),AL3(FNCLD)   NON CLIENT AC FILT          
         DC    AL2(F#LIML#FMED),AL1(0),AL3(FMED)    MEDIA FILTER                
         DC    AL2(F#LIML#F1RA),AL1(0),AL3(F1RAD)   1R ACCOUNT FILTER           
         DC    AL2(F#LIML#FSFR),AL1(0),AL3(FFORD)   FORMAT CODE FILTER          
         DC    AL2(F#LIML#FSUC),AL1(0),AL3(FSUC)    SUPPLIER CODE FILT          
         DC    AL2(F#LIML#SCHFL),AL1(0),AL3(SCHFL)  SCHEME CODE FILTER          
         DC    AL2(F#LIML#CPJC),AL1(0),AL3(CPJCD)   CLI/PRO/JOB CODE            
         DC    AL2(F#LIML#COFF),AL1(0),AL3(COFF)    CLI/PRO/JOB OFFICE          
         DC    AL2(F#LIML#CPJN),AL1(0),AL3(CPJND)   CLI/PRO/JOB NAME            
         DC    AL2(F#LIML#EXTY),AL1(0),AL3(EXTYD)   EXPENDITURE TYPE            
         DC    AL2(F#LIML#EXNM),AL1(0),AL3(EXNMD)   EXPENDITURE NAME            
         DC    AL2(F#LIML#TWCL),AL1(0),AL3(WRKCD)   WORK CODE                   
         DC    AL2(F#LIML#TWCN),AL1(0),AL3(WRKND)   WORK CODE NAME              
         DC    AL2(F#LIML#TWCD),AL1(0),AL3(WRKDD)   WORK CODE DESCRIP           
         DC    AL2(F#LIML#NCLC),AL1(0),AL3(NCLCD)   NON CLIENT CODE             
         DC    AL2(F#LIML#NCLN),AL1(0),AL3(NCLND)   NON CLIENT NAME             
         DC    AL2(F#LIML#MEDL),AL1(0),AL3(MEDCD)   MEDIA CODE                  
         DC    AL2(F#LIML#MEDN),AL1(0),AL3(MEDND)   MEDIA NAME                  
         DC    AL2(F#LIML#C1RA),AL1(0),AL3(C1RAD)   1R ACCOUNT CODE             
         DC    AL2(F#LIML#N1RA),AL1(0),AL3(N1RAD)   1R ACCOUNT NAME             
         DC    AL2(F#LIML#SFCD),AL1(0),AL3(FRCDD)   FORMAT CODE                 
         DC    AL2(F#LIML#REPT),AL1(0),AL3(FRTYD)   REPORT TYPE                 
*        DC    AL2(F#LIML#SFTY),AL1(0),AL3(FRTYD)   ACCENT REPORT TYPE          
         DC    AL2(F#LIML#TRAN),AL1(0),AL3(FRTTD)   TRANSMISSION TYPE           
         DC    AL2(F#LIML#SFNM),AL1(0),AL3(FRNMD)   FORMAT NAME                 
         DC    AL2(F#LIML#FPID),AL1(0),AL3(PID)     PID FILTER (DOWN)           
         DC    AL2(F#LIML#APSUC),AL4(SUC)           SUPPLIER CODE               
         DC    AL2(F#LIML#APSUN),AL4(SUN)           SUPPLIER NAME               
         DC    AL2(F#LIML#SCHCD),AL4(SCHCD)         SCHEME CODE                 
         DC    AL2(F#LIML#SCHNM),AL4(SCHNM)         SCHEME NAME                 
         DC    AL2(F#LIML#DEFAC),AL4(DEFAC)         DEFAULT ACCESS              
         DC    AL2(F#LIML#APPSJ),AL4(APPSJ)         APPL C/P/J                  
         DC    AL2(F#LIML#APPET),AL4(APPET)         APPL EXPEND TYP             
         DC    AL2(F#LIML#APPWC),AL4(APPWC)         APPL WORK CODE              
         DC    AL2(F#LIML#APP1N),AL4(APP1N)         APPL 1N                     
         DC    AL2(F#LIML#APPME),AL4(APPME)         APPL MEDIA                  
         DC    AL2(F#LIML#APP1R),AL4(APP1R)         APPL 1R                     
         DC    AL2(F#LIML#APPFO),AL4(APPFO)         APPL FORMAT                 
         DC    AL2(F#LIML#APPSC),AL4(APPSC)         APPL SCHEME                 
         DC    AL2(F#LIML#APPSU),AL4(APPSU)         APPL SUPPLIER               
         DC    AL2(F#LIML#DEFAD),AL4(DEFAD)         DEF ACC (D/LOAD)            
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
         SPACE 2                                                                
*                                                                               
***********************************************************************         
* MACRO BRANCH TO DATA OBJECT                                         *         
***********************************************************************         
         SPACE 1                                                                
         MACRO                                                                  
&NTRDO   NTRDO                                                                  
         DC    CL8'&NTRDO'                                                      
         DS    0H                                                               
         USING *,RF                                                             
&NTRDO   NTR1                                                                   
         DROP  RF                                                               
         LR    R7,RF                                                            
         USING &NTRDO,R7                                                        
         LA    RF,&NTRDO.TBL       TABLE OF KNOWN VERBS                         
         B     ITER                                                             
         MEND                                                                   
         SPACE 1                                                                
FIL46    CSECT                                                                  
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
DFDVAL   NI    CPJINDS,FF-(CPJIVCPJ+CPJINCLI+CPJICCPJ)                          
         CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BNE   DFDV02                                                           
         B     EXITOK              YES - EXIT                                   
*                                                                               
DFDV02   GOTO1 ARSTADD,(R2)                                                     
         B     EXITOK                                                           
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
         DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* LAST TIME FOR DISPLAY OF A DATA OBJECT                              *         
***********************************************************************         
         SPACE 1                                                                
DLDDIS   CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BE    EXITOK              YES - DON'T UPDATE ACCOUNT RECORD            
*                                                                               
         XC    SVGRPCDE,SVGRPCDE                                                
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR VALIDATE OF A DATA OBJECT                             *         
***********************************************************************         
         SPACE 1                                                                
DLDVAL   CLM   R2,B'0111',ATLST+1  ARE WE PROCESSING MAINTENANCE LIST?          
         BE    EXITOK              YES - DON'T UPDATE ACCOUNT RECORD            
*                                                                               
         XC    SVGRPCDE,SVGRPCDE                                                
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* DOWNLOAD OBJECT - LEVEL 1 (NEED 2 LEVELS FOR NTRDO)                 *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 A(KEY)                                                           *         
* P4 HOLDS SUB-ACTION                                                 *         
***********************************************************************         
         SPACE 1                                                                
DLOAD    DS    0H                                                               
         L     RF,=AL4(DOWN)       SHORTCUT TO LEVEL 2                          
         A     RF,BORELO                                                        
         LM    R0,R3,SVPARMS                                                    
         BASR  RE,RF                                                            
         B     EXIT                                                             
         SPACE 2                                                                
FIL46N   CSECT                                                                  
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR PID (PERSONAL ID)                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
PID      NTRDO                                                                  
*                                                                               
PIDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPID)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAAPID)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(DISPID)                                 
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETPID)                                
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTPID)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTPID)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(EXITOK)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHPID)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETPID  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PID FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISPID   CLI   CSACT,A#DLOAD                                                    
         BE    EXITOK                                                           
         MVC   FVIFLD(L'SVPID),SVPID                                            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A PERSONAL ID FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHPID  CLI   CSACT,A#DLOAD                                                    
         BE    EXITOK                                                           
         MVC   BODUB1,BCSPACES                                                  
         L     R0,FVADDR                                                        
         S     R0,ATWA                                                          
         GOTO1 VSRCHCAL,BOPARM,('STMPSTRQ',(R0)),ATWA,ACOM,0,          X        
               (1,=CL8'PERSON'),0                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PERSONAL FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
VAAPID   CLI   CSACT,A#DLOAD                                                    
         BE    VFLTPID                                                          
         MVC   APIDFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         CLC   FVIFLD(L'SVPID),BCSPACES                                         
         BH    *+14                                                             
         MVC   FVIFLD,SVPID                                                     
         MVI   FVILEN,8                                                         
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
         MVC   SVPID,FVIFLD                                                     
*                                                                               
VPID02   GOTOX ('VALPID',AGROUTS),FVIFLD                                        
*&&US*&& BE    VPID04                                                           
*&&UK                                                                           
         BNE   VPID03                                                           
         GOTO1 ACHKLPID,BOPARM,BCWORK CHECK PID IS VALID FOR LOGON              
         BNL   VPID04                                                           
*&&                                                                             
VPID03   MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
VPID04   MVC   SVPIDLNM,BCWORK+22                                               
         MVC   SVPIDFNM,BCWORK+2                                                
         MVC   SVPIDBIN,BCWORK                                                  
         MVC   LLSKPIDB,BCWORK                                                  
         MVC   SVPIDMNM,BCWORK+42                                               
*&&US                                                                           
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    VPID10                                                           
         GOTO1 ACHKPID,BOPARM,SVPIDBIN                                          
         BE    VPID10                                                           
         MVC   FVMSGNO,=AL2(AE$WAOOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         CLI   CSACT,A#DIS                                                      
         BE    EXITL                                                            
         MVC   FVMSGNO,=AL2(AE$CMCOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*&&                                                                             
*                                                                               
VPID10   MVC   FLTIFLD(L'SAPALPID),FVIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PERSONAL FILTER FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DFLTPID  MVC   FVIFLD(L'SAPALPID),FLTIFLD                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A PERSONAL FILTER FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
VFLTPID  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTOX ('VALPID',AGROUTS),FVIFLD                                        
*&&US*&& BE    VFLTP04                                                          
*&&UK                                                                           
         BNE   VFLTP02                                                          
         GOTO1 ACHKLPID,BOPARM,BCWORK CHECK PID IS VALID FOR LOGON              
         BNL   VFLTP04                                                          
*&&                                                                             
VFLTP02  MVC   FVMSGNO,=AL2(AE$INPID) INVALID PERSONAL ID                       
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
*                                                                               
*FLTP04  MVC   LLSKPIDB,BCWORK                                                  
VFLTP04  MVC   FLTIFLD(L'SAPALPID),FVIFLD                                       
         MVC   SVPIDFL(L'SAPALPID),FVIFLD                                       
         MVC   SVPIDFLN,FVXLEN                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING ON PERSONAL ID                                         *         
***********************************************************************         
         SPACE 1                                                                
DOFPID   OC    LLSKPIDB,LLSKPIDB   HAVE WE A PERSONAL ID TO FILTER ON           
         BZ    FLTXX               NO - WE DON`T WANT IT THEN                   
         GOTOX ('GETPID',AGROUTS),LLSKPIDB                                      
         MVC   IOKEY,LLSKEY                                                     
         L     R1,=AL4(XOHID+XOACCDIR+XIO11) RESET SEQUENCE                     
         GOTOR AIO                                                              
         BE    DOFPID02                                                         
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   DOFPID02                                                         
         DC    H'0'                                                             
*                                                                               
DOFPID02 CLC   BCWORK(L'SAPALPID),FLTIFLD                                       
         BL    FLTXL                                                            
         BE    FLTXE                                                            
         BH    FLTXH                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR LAST NAME                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
LNMD     NTRDO                                                                  
*                                                                               
LNMDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNM)                                 
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY LAST NAME FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISLNM   MVC   FVIFLD(L'SVPIDLNM),SVPIDLNM                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIRST NAME                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNMD     NTRDO                                                                  
*                                                                               
FNMDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIRST NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFNM   MVC   FVIFLD(L'SVPIDFNM),SVPIDFNM                                      
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR GROUP CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
GRCD     NTRDO                                                                  
*                                                                               
GRCDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISGRPC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALGRPC)                                
         DC    AL1(DNTR),AL1(0,0,0),AL4(DNTGRPC)                                
         DC    AL1(DSET),AL1(0,0,0),AL4(DSETGRPC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETGRPC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
***********************************************************************         
* DISPLAY A GROUP CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DNTGRPC  OC    SVGRPCDE,SVGRPCDE                                                
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'SVGRPC),SVGRPCDE                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A GROUP CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISGRPC  OC    SVGRPC,SVGRPC                                                    
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'SVGRPC),SVGRPC                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A GROUP CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALGRPC  XC    SVGRPC,SVGRPC                                                    
         CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLC   FVIFLD(L'SVGRPC),BCSPACES                                        
         BNH   EXITOK                                                           
         MVC   LLSKGRP,FVIFLD                                                   
         MVC   SVGRPC,FVIFLD                                                    
         LA    R4,IOKEY                                                         
         USING GLSRECD,R4                                                       
         XC    GLSKEY,GLSKEY       READ GROUP RECORD                            
         MVC   GLSKGRP,SVGRPC                                                   
         MVI   GLSKTYP,GLSKTYPQ                                                 
         MVI   GLSKSUB,GLSKSUBQ                                                 
         MVC   GLSKCPY,CUABIN      CONNECTED ID                                 
         L     R1,=AL4(XORDD+XOACCDIR+XIO2)                                     
         GOTOR AIO                                                              
         BE    EXITOK                                                           
         TM    IOERR,IOEDEL                                                     
         BNZ   EXITOK                                                           
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   FVMSGNO,=AL2(AE$IVGPC) INVALID GROUP CODE                        
         MVI   FVOSYS,QSACC                                                     
         B     EXITL                                                            
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR GROUP NAME FIELD                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
GRND     NTRDO                                                                  
*                                                                               
GRNDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISGRN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A GROUP NAME FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISGRN   OC    SVGRPC,SVGRPC       DO WE HAVE A GROUP PRESENT                   
         BZ    EXITOK              NO                                           
         LA    R4,IOKEY                                                         
         USING GLSRECD,R4                                                       
         XC    GLSKEY,GLSKEY       READ GROUP RECORD                            
         MVC   GLSKGRP,SVGRPC                                                   
         MVI   GLSKTYP,GLSKTYPQ                                                 
         MVI   GLSKSUB,GLSKSUBQ                                                 
         MVC   GLSKCPY,CUABIN      CONNECTED ID                                 
         L     R1,=AL4(XORDD+XOACCMST+XIO2)                                     
         GOTOR AIO                                                              
         BE    DISGRN02                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   DISGRN02                                                         
         B     EXITOK                                                           
         DROP  R4                                                               
*                                                                               
DISGRN02 L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET GROUP NAME                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PID (PERSONAL ID) IN LIST SCREEN                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
PILD     NTRDO                                                                  
*                                                                               
PILDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPIL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A PID FIELD                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISPIL   CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKPID),TLKPID                                          
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLKPID),TLKPID                                          
         GOTOX ('VALPID',AGROUTS),TLKPID                                        
         BNE   EXITOK                                                           
*&&UK*&& GOTO1 ACHKLPID,BOPARM,BCWORK                                           
*&&UK*&& BL    EXITL                                                            
         MVC   SVPIDLNM,BCWORK+22                                               
         MVC   SVPIDFNM,BCWORK+2                                                
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR LAST NAME IN LIST                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
LNLD     NTRDO                                                                  
*                                                                               
LNLDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISLNL)                                 
         DC    AL1(EOT)                                                         
         EJECT ,                                                                
***********************************************************************         
* DISPLAY LAST NAME FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DISLNL   CLI   CSACT,A#DLOAD                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(L'SANAME),SVPIDLNM                                        
         B     EXITOK                                                           
         GOTOX ('VALPID',AGROUTS),TLKPID                                        
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'SANAME),BCWORK+22                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FIRST NAME IN LIST                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNLD     NTRDO                                                                  
*                                                                               
FNLDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISFNL)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FIRST NAME                                                  *         
***********************************************************************         
         SPACE 1                                                                
DISFNL   CLI   CSACT,A#DLOAD                                                    
         BNE   *+14                                                             
         MVC   FVIFLD(L'SANAME),SVPIDFNM                                        
         B     EXITOK                                                           
         GOTOX ('VALPID',AGROUTS),TLKPID                                        
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'SANAME),BCWORK+2                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR GROUP CODE                                          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
GRLD     NTRDO                                                                  
*                                                                               
GRLDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISGRPL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A GROUP CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISGRPL  OC    TLKGRPC,TLKGRPC                                                  
         BZ    EXITOK                                                           
         MVC   FVIFLD(L'TLKGRPC),TLKGRPC                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR GROUP NAME IN LIST                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
GNLD     NTRDO                                                                  
*                                                                               
GNLDTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISGRNL)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A GROUP NAME IN LIST FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DISGRNL  OC    TLKGRPC,TLKGRPC                                                  
         BZ    EXITOK                                                           
         LA    R4,IOKEY                                                         
         USING GLSRECD,R4                                                       
         XC    GLSKEY,GLSKEY       READ GROUP RECORD                            
         MVC   GLSKGRP,TLKGRPC                                                  
         MVI   GLSKTYP,GLSKTYPQ                                                 
         MVI   GLSKSUB,GLSKSUBQ                                                 
         MVC   GLSKCPY,CUABIN      CONNECTED ID                                 
         L     R1,=AL4(XORDD+XOACCMST+XIO2)                                     
         GOTOR AIO                                                              
         BE    DISGNL02                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   DISGNL02                                                         
         B     EXITOK                                                           
         DROP  R4                                                               
*                                                                               
DISGNL02 L     R1,AIO2                                                          
         GOTO1 AGETNAM             GET GROUP NAME                               
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A DEFAULT ACCESS FIELD (PER PAGE)        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
DEFAC    NTRDO                                                                  
*                                                                               
DEFACTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEFAC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDEFAC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEFAULT ACCESS FIELD (PER PAGE)                           *         
***********************************************************************         
         SPACE 1                                                                
DISDEFAC MVC   FVIFLD(L'BC@YES),BC@YES                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',LLSRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BNE   EXITOK                                                           
         USING RSTELD,R1                                                        
         L     R1,12(R1)                                                        
         LA    RE,RSTACST1                                                      
         MVI   BCBYTE1,RSTAJOBS                                                 
         CLI   GSSMPAGE,1                                                       
         BE    DISDEFA2                                                         
         MVI   BCBYTE1,RSTAETYP                                                 
         CLI   GSSMPAGE,2                                                       
         BE    DISDEFA2                                                         
         MVI   BCBYTE1,RSTAWC                                                   
         CLI   GSSMPAGE,3                                                       
         BE    DISDEFA2                                                         
         MVI   BCBYTE1,RSTA1NAC                                                 
         CLI   GSSMPAGE,4                                                       
         BE    DISDEFA2                                                         
         MVI   BCBYTE1,RSTAMED                                                  
         CLI   GSSMPAGE,5                                                       
         BE    DISDEFA2                                                         
         MVI   BCBYTE1,RSTASTAF                                                 
         CLI   GSSMPAGE,6                                                       
         BE    DISDEFA2                                                         
         MVI   BCBYTE1,RSTAREPF                                                 
         CLI   GSSMPAGE,7                                                       
         BE    DISDEFA2                                                         
         MVI   BCBYTE1,RSTASCHM                                                 
         CLI   GSSMPAGE,8                                                       
         BE    DISDEFA2                                                         
         LA    RE,RSTACST2                                                      
         MVI   BCBYTE1,RSTASUPP                                                 
         CLI   GSSMPAGE,9                                                       
         BE    DISDEFA2                                                         
         DC    H'0'                                                             
                                                                                
DISDEFA2 NC    BCBYTE1,0(RE)                                                    
         BZ    EXITOK                                                           
                                                                                
         MVC   FVIFLD(L'BC@NO),BC@NO                                            
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A DEFAULT ACCESS FIELD (PER PAGE)                          *         
***********************************************************************         
         SPACE 1                                                                
VALDEFAC GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',LLSRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    *+6                                                              
         DC    H'0'                                                             
         USING RSTELD,R1                                                        
         L     R1,12(R1)                                                        
         LA    RE,RSTACST1                                                      
         MVI   BCBYTE1,RSTAJOBS                                                 
         CLI   GSSMPAGE,1                                                       
         BE    VALDEFA2                                                         
         MVI   BCBYTE1,RSTAETYP                                                 
         CLI   GSSMPAGE,2                                                       
         BE    VALDEFA2                                                         
         MVI   BCBYTE1,RSTAWC                                                   
         CLI   GSSMPAGE,3                                                       
         BE    VALDEFA2                                                         
         MVI   BCBYTE1,RSTA1NAC                                                 
         CLI   GSSMPAGE,4                                                       
         BE    VALDEFA2                                                         
         MVI   BCBYTE1,RSTAMED                                                  
         CLI   GSSMPAGE,5                                                       
         BE    VALDEFA2                                                         
         MVI   BCBYTE1,RSTASTAF                                                 
         CLI   GSSMPAGE,6                                                       
         BE    VALDEFA2                                                         
         MVI   BCBYTE1,RSTAREPF                                                 
         CLI   GSSMPAGE,7                                                       
         BE    VALDEFA2                                                         
         MVI   BCBYTE1,RSTASCHM                                                 
         CLI   GSSMPAGE,8                                                       
         BE    VALDEFA2                                                         
         LA    RE,RSTACST2                                                      
         MVI   BCBYTE1,RSTASUPP                                                 
         CLI   GSSMPAGE,9                                                       
         BE    VALDEFA2                                                         
         DC    H'0'                                                             
                                                                                
VALDEFA2 CLI   FVILEN,0                                                         
         BE    VALDEFAN                                                         
         LLC   RF,FVILEN                                                        
         SHI   RF,1                                                             
         EX    RF,*+8                                                           
         BNE   VALDEFAN                                                         
         CLC   FVIFLD(0),BC@NO                                                  
                                                                                
VALDEFAY OC    0(1,RE),BCBYTE1                                                  
         B     EXITOK                                                           
                                                                                
VALDEFAN XI    BCBYTE1,X'FF'                                                    
         NC    0(1,RE),BCBYTE1                                                  
         B     EXITOK                                                           
         DROP  R1                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON CLI/PRO/JOB FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FCPJ     NTRDO                                                                  
*                                                                               
FCPJTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFCPJ)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFCPJ)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFCPJ)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFCPJ)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFCPJ)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFCPJ)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFCPJ DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON CLI/PRO/JOB FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTFCPJ MVC   FVIFLD(L'SVCPJ),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON CLI/PRO/JOB FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTFCPJ CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR CLI/PRO/JOB ACCOUNT RECORD          
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRODUL),PRODUL                                       
         MVC   T.ACTKACT,FVIFLD                                                 
         L     R1,=AL4(XOREAD+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INACC) INVALID ACCOUNT                           
         B     EXITL                                                            
*                                                                               
         MVC   SVCPJ,FVIFLD                                                     
         MVC   SVCPJXLN,FVXLEN                                                  
         MVC   FLTIFLD(L'SVCPJ),SVCPJ                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB  CODE FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
SRCHFCPJ DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   X        
               (X'13',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON CLI/PRO/JOB                              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFCPJ B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON EXPENDITURE FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FETYD    NTRDO                                                                  
*                                                                               
FETYDTBL DC    AL1(DSET),AL1(0,0,0),AL4(DSETFETY)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFETY)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFETY)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFETY)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFETY)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFETY)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFETY DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON EXPENDITURE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DFLTFETY MVC   FVIFLD(L'SVETY),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON EXPENDITURE FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTFETY CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ETYKCODE                                                
         BH    EXITLONG            FIELD TOO LONG                               
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   READ FOR EXPENDITURE TYPE REC                
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN                                                 
         MVC   T.ETYKCODE,FVIFLD                                                
         MVC   T.ETYKOFFC,BCSPACES                                              
         MVC   SVIOKEY,IOKEY                                                    
         LHI   R1,XOHIGH+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         CLC   SVIOKEY(ETYKOFFC-ETYRECD),IOKEY                                  
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INETY) INVALID EXPENDITURE TYPE                  
         B     EXITL                                                            
*                                                                               
         MVC   SVETY,FVIFLD                                                     
         MVC   FLTIFLD(L'SVETY),SVETY                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON EXPENDITURE TYPE NAME                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHFETY DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXTYP,ACOM,0            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON EXPENDITURE                              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFETY B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON WORK CODE FIELD             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FWCD     NTRDO                                                                  
*                                                                               
FWCDTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFWC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFWC)                                
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFWC)                               
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFWC)                               
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFWC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFWC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFWC  DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON WORK CODE FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
DFLTFWC  MVC   FVIFLD(L'SVWC),FLTIFLD                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON WORK CODE FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
VFLTFWC  CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'WCOKWRK                                                 
         BH    EXITLONG            FIELD TOO LONG                               
T        USING WCORECD,IOKEY                                                    
         MVC   T.WCOKEY,BCSPACES   READ FOR WORK CODE REC                       
         MVI   T.WCOKTYP,WCOKTYPQ                                               
         MVC   T.WCOKCPY,CUABIN                                                 
         MVI   T.WCOKUNT,C'S'                                                   
         MVI   T.WCOKLDG,C'J'                                                   
         MVC   T.WCOKWRK,FVIFLD                                                 
         L     R1,=AL4(XOREAD+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INWRK) INVALID WORKCODE                          
         B     EXITL                                                            
*                                                                               
         MVC   SVWC,FVIFLD                                                      
         MVC   FLTIFLD(L'SVWC),SVWC                                             
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON WORK CODE NAME                                            *         
***********************************************************************         
         SPACE 1                                                                
SRCHFWC  DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,WC,ACOM,0               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON WORKCODE                                 *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFWC  B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON NON CLIENT ACCOUNT CODE     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FNCLD    NTRDO                                                                  
*                                                                               
FNCLDTBL DC    AL1(DSET),AL1(0,0,0),AL4(DSETFNCL)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFNCL)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFNCL)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFNCL)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFNCL)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFNCL)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFNCL DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON NON CLIENT CODE FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
DFLTFNCL MVC   FVIFLD(L'SVNCL),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON NON CLIENT CODE FIELD                         *         
***********************************************************************         
         SPACE 1                                                                
VFLTFNCL CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR NON CLIENT ACCOUNT RECORD           
         MVC   T.ACTKCPY,CUABIN                                                 
         MVI   T.ACTKUNT,C'1'                                                   
         MVI   T.ACTKLDG,C'N'                                                   
         MVC   T.ACTKACT,FVIFLD                                                 
         L     R1,=AL4(XOREAD+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVAC) INVALID ACCOUNT                           
         B     EXITL                                                            
*                                                                               
         MVC   SVNCL,FVIFLD                                                     
         MVC   FLTIFLD(L'SVNCL),SVNCL                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON NON CLIENT CODE NAME                                      *         
***********************************************************************         
         SPACE 1                                                                
SRCHFNCL MVC   BODUB1(L'ACTKUNT+L'ACTKLDG),=C'1N'                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,BODUB1,ACOM,   X        
               (X'14',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON NON CLIENT ACCOUNT CODE                  *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFNCL B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN FILTER ON MEDIA CODE                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FMED     NTRDO                                                                  
*                                                                               
FMEDTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFMED)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFMED)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFMED)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFMED)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFMED)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFMED)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFMED DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON MEDIA CODE FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTFMED MVC   FVIFLD(L'SVMED),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON MEDIA CODE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VFLTFMED CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'PMDKMED                                                 
         BH    EXITLONG            FIELD TOO LONG                               
T        USING PMDRECD,IOKEY                                                    
         MVC   T.PMDKEY,BCSPACES   READ FOR MEDIA CODE RECORD                   
         MVC   T.PMDKCPY,CUABIN                                                 
         MVI   T.PMDKTYP,PMDKTYPQ                                               
         MVC   T.PMDKMED,FVIFLD                                                 
         L     R1,=AL4(XOREAD+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVCD) INVALID CODE                              
         B     EXITL                                                            
*                                                                               
         MVC   SVMED,FVIFLD                                                     
         MVC   FLTIFLD(L'SVMED),SVMED                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON MEDIA CODE NAME                                           *         
***********************************************************************         
         SPACE 1                                                                
SRCHFMED DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MED,ACOM,      X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON MEDIA CODE                               *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFMED B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON 1R ACCOUNT CODE              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
F1RAD    NTRDO                                                                  
*                                                                               
F1RADTBL DC    AL1(DSET),AL1(0,0,0),AL4(DSETF1RA)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTF1RA)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTF1RA)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTF1RA)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTF1RA)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHF1RA)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETF1RA DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON 1R ACCOUNT CODE FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
DFLTF1RA MVC   FVIFLD(L'SV1RA),FLTIFLD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON 1R ACCOUNT CODE FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
VFLTF1RA CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES   READ FOR 1R ACCOUNT CODE RECORD              
         MVC   T.ACTKCPY,CUABIN                                                 
         MVI   T.ACTKUNT,C'1'                                                   
         MVI   T.ACTKLDG,C'R'                                                   
         MVC   T.ACTKACT,FVIFLD                                                 
         L     R1,=AL4(XOREAD+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVAC) INVALID ACCOUNT                           
         B     EXITL                                                            
*                                                                               
         MVC   SV1RA,FVIFLD                                                     
         MVC   FLTIFLD(L'SV1RA),SV1RA                                           
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON 1R ACCOUNT CODE NAME                                      *         
***********************************************************************         
         SPACE 1                                                                
SRCHF1RA DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,AC1R,          X        
               ACOM,(X'14',0)                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON 1R ACCOUNT CODE                          *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTF1RA B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON FORMAT CODE                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FFORD    NTRDO                                                                  
*                                                                               
FFORDTBL DC    AL1(DSET),AL1(0,0,0),AL4(DSETFFOR)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFFOR)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFFOR)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFFOR)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFFOR)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFFOR DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FILTER ON FORMAT CODE FIELD                               *         
***********************************************************************         
         SPACE 1                                                                
DFLTFFOR MVC   FVIFLD(L'SVFORM),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN FILTER ON FORMAT CODE FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VFLTFFOR CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'RESKFORM                                                
         BH    EXITLONG            FIELD TOO LONG                               
T        USING RESRECD,IOKEY                                                    
         MVC   T.RESKEY,BCSPACES   READ FOR MEDIA CODE RECORD                   
         MVI   T.RESKTYP,RESKTYPQ                                               
         MVI   T.RESKSUB,RESKSUBQ                                               
         MVC   T.RESKCPY,CUABIN                                                 
         MVC   T.RESKFORM,FVIFLD                                                
         L     R1,=AL4(XOREAD+XOACCDIR+XIO2)                                    
         GOTOR AIO                                                              
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INVFM) INVALID FORMAT CODE                       
         B     EXITL                                                            
*                                                                               
         MVC   SVFORM,FVIFLD                                                    
         MVC   FLTIFLD(L'SVFORM),SVFORM                                         
         B     EXITOK                                                           
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON FORMAT CODE                              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFFOR B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON SUPPLIER CODE FIELD          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
FSUC     NTRDO                                                                  
*                                                                               
FSUCTBL  DC    AL1(DSET),AL1(0,0,0),AL4(DSETFSUC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFSUC)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFSUC)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFSUC)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFSUC)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFSUC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFSUC DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON SUPPLIER CODE FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DFLTFSUC MVC   FVIFLD(L'SVFSUC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON SUPPLIER CODE FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
VFLTFSUC CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         CLI   FVILEN,L'ACTKACT+2  ('SV' OR 'SX' FOLLOWED BY ACTKACT)           
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         CLC   FVIFLD(2),SVUL      CHECK FOR SV OR SX                           
         BE    VFLTS04                                                          
         CLI   NOSX,1                                                           
         BE    VFLTS02                                                          
         CLC   FVIFLD(2),SXUL                                                   
         BE    VFLTS04                                                          
VFLTS02  MVC   FVMSGNO,=AL2(AE$INVAC)  INVALID ACCOUNT                          
         B     EXITL                                                            
*                                                                               
T        USING ACTRECD,IOKEY                                                    
VFLTS04  MVC   T.ACTKEY,BCSPACES   READ FOR SUPPLIER ACCOUNT RECORD             
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'SVUL),FVIFLD                                         
         MVC   T.ACTKACT,FVIFLD+2                                               
         DROP  T                                                                
         GOTO1 AGETACT,0           READ ACCOUNT/TEST SECURITY                   
         BNE   EXITL                                                            
*                                                                               
         MVC   SVFSUC,FVIFLD                                                    
         MVC   FLTIFLD(L'SVFSUC),SVFSUC                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A SUPPLIER CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHFSUC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON SUPPLIER CODE                            *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFSUC B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FILTER ON SCHEME CODE FIELD            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
SCHFL    NTRDO                                                                  
*                                                                               
SCHFLTBL DC    AL1(DSET),AL1(0,0,0),AL4(DSETFSCH)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VFLTFSCH)                               
         DC    AL1(DFVAL),AL1(0,0,0),AL4(VFLTFSCH)                              
         DC    AL1(DFDIS),AL1(0,0,0),AL4(DFLTFSCH)                              
         DC    AL1(DFDO),AL1(0,0,0),AL4(DOFTFSCH)                               
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHFSCH)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* UNPROTECT FIELD ON NTRSES IF REQUIRED                               *         
***********************************************************************         
         SPACE 1                                                                
DSETFSCH DS    0H                                                               
         B     FLTXX               UNPROTECT FIELD                              
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN FILTER ON SUPPLIER CODE FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
DFLTFSCH MVC   FVIFLD(L'SVFSUC),FLTIFLD                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A FILTER ON SCHEME CODE FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
VFLTFSCH CLI   FVILEN,0                                                         
         BE    EXITOK                                                           
         GOTO1 AVALSCH,BOPARM,(FVILEN,FVIFLD)                                   
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INSCH)                                           
         B     EXITL               INVALID SCHEME CODE                          
*                                                                               
         MVC   SVFSCH,FVIFLD                                                    
         MVC   FLTIFLD(L'SVFSCH),SVFSCH                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DO FILTERING FOR FILTER ON SCHEME CODE                              *         
* OVERLAY WILL DO ITS OWN FILTERING - SEE DOFLT                       *         
***********************************************************************         
         SPACE 1                                                                
DOFTFSCH B     FLTXE                                                            
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A SCHEME CODE FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
SRCHFSCH DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DOWNLOADING A DEFAULT ACCESS FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
DEFAD    NTRDO                                                                  
*                                                                               
DEFADTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISDEFAD)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A DEFAULT ACCESS FIELD FOR DOWNLOAD                         *         
***********************************************************************         
         SPACE 1                                                                
DISDEFAD CLI   CSACT,A#DLOAD                                                    
         BNE   EXITOK                                                           
         L     R2,ATLST                                                         
         LA    RF,TLDDEFAD                                                      
         MVC   FVIFLD(9),=C'YYYYYYYYY'                                          
         TM    0(RF),RSTAJOBS                                                   
         BZ    *+8                                                              
         MVI   FVIFLD+0,C'N'                                                    
         TM    0(RF),RSTAETYP                                                   
         BZ    *+8                                                              
         MVI   FVIFLD+1,C'N'                                                    
         TM    0(RF),RSTAWC                                                     
         BZ    *+8                                                              
         MVI   FVIFLD+2,C'N'                                                    
         TM    0(RF),RSTA1NAC                                                   
         BZ    *+8                                                              
         MVI   FVIFLD+3,C'N'                                                    
         TM    0(RF),RSTAMED                                                    
         BZ    *+8                                                              
         MVI   FVIFLD+4,C'N'                                                    
         TM    0(RF),RSTASTAF                                                   
         BZ    *+8                                                              
         MVI   FVIFLD+5,C'N'                                                    
         TM    0(RF),RSTAREPF                                                   
         BZ    *+8                                                              
         MVI   FVIFLD+6,C'N'                                                    
         TM    0(RF),RSTASCHM                                                   
         BZ    *+8                                                              
         MVI   FVIFLD+7,C'N'                                                    
         TM    0(RF),RSTASUPP                                                   
         BZ    *+8                                                              
         MVI   FVIFLD+8,C'N'                                                    
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER SJ FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
APPSJ    NTRDO                                                                  
*                                                                               
APPSJTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPPSJ)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPPSJ)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER SJ FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISAPPSJ LA    RF,TLKAPPL1                                                      
         LA    RE,TLKCPJC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPPSJ2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDCPJAP                                                      
         LA    RE,TLDCPJ                                                        
DAPPSJ2  MVC   FVIFLD(8),NONOS     (EXIJORTQ)                                   
         CLC   0(L'TLKCPJC,RE),BCSPACES                                         
         BH    DAPPSJ4                                                          
         MVC   FVIFLD(8),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPPSJ4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLESTM                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         TM    0(RF),LIDLEXPN                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         TM    0(RF),LIDLINVC                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+2(1),BC@YES                                               
         TM    0(RF),LIDLJOBS                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+3(1),BC@YES                                               
         TM    0(RF),LIDLORDS                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+4(1),BC@YES                                               
         TM    0(RF),LIDLRESC                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+5(1),BC@YES                                               
         TM    0(RF),LIDLTIME                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+6(1),BC@YES                                               
         TM    0(RF),LIDLREPT                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+7(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER SJ FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VALAPPSJ MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPPSJ02                                                         
         CLC   FVIFLD(8),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLESTM                                                
         CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLEXPN                                                
         CLC   FVIFLD+2(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLINVC                                                
         CLC   FVIFLD+3(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLJOBS                                                
         CLC   FVIFLD+4(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLORDS                                                
         CLC   FVIFLD+5(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLRESC                                                
         CLC   FVIFLD+6(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLTIME                                                
         CLC   FVIFLD+7(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLREPT                                                
                                                                                
         USING FHD,RF                                                           
VAPPSJ02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPPSJ04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPPSJ04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER MEDIA FIELD        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPME    NTRDO                                                                  
*                                                                               
APPMETBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPPME)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPPME)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER MEDIA FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
DISAPPME LA    RF,TLKAPPL1                                                      
         LA    RE,TLKMEDC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPPME2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDMEDAP                                                      
         LA    RE,TLDMED                                                        
DAPPME2  MVC   FVIFLD(8),NONOS     (EXIJORT)                                    
         CLC   0(L'TLKMEDC,RE),BCSPACES                                         
         BH    DAPPME4                                                          
         MVC   FVIFLD(8),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPPME4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLESTM                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         TM    0(RF),LIDLEXPN                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         TM    0(RF),LIDLINVC                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+2(1),BC@YES                                               
         TM    0(RF),LIDLJOBS                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+3(1),BC@YES                                               
         TM    0(RF),LIDLORDS                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+4(1),BC@YES                                               
         TM    0(RF),LIDLRESC                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+5(1),BC@YES                                               
         TM    0(RF),LIDLTIME                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+6(1),BC@YES                                               
         TM    0(RF),LIDLREPT                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+7(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER MEDIA FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
VALAPPME MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPPME02                                                         
         CLC   FVIFLD(8),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLESTM                                                
         CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLEXPN                                                
         CLC   FVIFLD+2(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLINVC                                                
         CLC   FVIFLD+3(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLJOBS                                                
         CLC   FVIFLD+4(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLORDS                                                
         CLC   FVIFLD+5(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLRESC                                                
         CLC   FVIFLD+6(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLTIME                                                
         CLC   FVIFLD+7(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLREPT                                                
                                                                                
         USING FHD,RF                                                           
VAPPME02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPPME04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPPME04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER ETYPE FIELD        *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPET    NTRDO                                                                  
*                                                                               
APPETTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPPET)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPPET)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER ETYPE FIELD                           *         
***********************************************************************         
         SPACE 1                                                                
DISAPPET LA    RF,TLKAPPL1                                                      
         LA    RE,TLKEXPC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPPET2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDEXPAP                                                      
         LA    RE,TLDEXP                                                        
DAPPET2  MVC   FVIFLD(2),NONOS     (OX)                                         
         CLC   0(L'TLKEXPC,RE),BCSPACES                                         
         BH    DAPPET4                                                          
         MVC   FVIFLD(2),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPPET4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLORDS                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         TM    0(RF),LIDLEXPN                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER ETYPE FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
VALAPPET MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPPET02                                                         
         CLC   FVIFLD(2),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLORDS                                                
         CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLEXPN                                                
                                                                                
         USING FHD,RF                                                           
VAPPET02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPPET04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPPET04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER 1R FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APP1R    NTRDO                                                                  
*                                                                               
APP1RTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPP1R)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPP1R)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER 1R FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISAPP1R LA    RF,TLKAPPL1                                                      
         LA    RE,TLK1RAC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPP1R2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLD1RCAP                                                      
         LA    RE,TLD1RC                                                        
DAPP1R2  DS    0H                                                               
*&&US*&& MVC   FVIFLD(4),NONOS     (XTEQ)                                       
*&&UK*&& MVC   FVIFLD(3),NONOS     (XTE)                                        
         CLC   0(L'TLK1RAC,RE),BCSPACES                                         
         BH    DAPP1R4                                                          
*&&US*&& MVC   FVIFLD(4),BCSPACES                                               
*&&UK*&& MVC   FVIFLD(3),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPP1R4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLEXPN                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         TM    0(RF),LIDLTIME                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+1(1),BC@YES                                               
         TM    0(RF),LIDLESTM                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+2(1),BC@YES                                               
         TM    0(RF),LIDLREPT                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+3(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER 1R FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VALAPP1R MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPP1R02                                                         
*&&US*&& CLC   FVIFLD(4),BCSPACES                                               
*&&UK*&& CLC   FVIFLD(3),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLEXPN                                                
         CLC   FVIFLD+1(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLTIME                                                
         CLC   FVIFLD+2(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLESTM                                                
         CLC   FVIFLD+3(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLREPT                                                
                                                                                
         USING FHD,RF                                                           
VAPP1R02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPP1R04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPP1R04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER W/C FIELD          *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPWC    NTRDO                                                                  
*                                                                               
APPWCTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPPWC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPPWC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER W/C FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
DISAPPWC LA    RF,TLKAPPL1                                                      
         LA    RE,TLKWCC                                                        
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPPWC2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDWCDAP                                                      
         LA    RE,TLDWC                                                         
DAPPWC2  MVC   FVIFLD(1),NONOS     (T)                                          
         CLC   0(L'TLKWCC,RE),BCSPACES                                          
         BH    DAPPWC4                                                          
         MVC   FVIFLD(1),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPPWC4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLTIME                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER W/C FIELD                            *         
***********************************************************************         
         SPACE 1                                                                
VALAPPWC MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPPWC02                                                         
         CLC   FVIFLD(1),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLTIME                                                
                                                                                
         USING FHD,RF                                                           
VAPPWC02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPPWC04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPPWC04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER 1N FIELD           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APP1N    NTRDO                                                                  
*                                                                               
APP1NTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPP1N)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPP1N)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER 1N FIELD                              *         
***********************************************************************         
         SPACE 1                                                                
DISAPP1N LA    RF,TLKAPPL1                                                      
         LA    RE,TLKNCLC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPP1N2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDNCCAP                                                      
         LA    RE,TLDNCC                                                        
DAPP1N2  MVC   FVIFLD(1),NONOS     (T)                                          
         CLC   0(L'TLKNCLC,RE),BCSPACES                                         
         BH    DAPP1N4                                                          
         MVC   FVIFLD(1),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPP1N4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLTIME                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER 1N FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VALAPP1N MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPP1N02                                                         
         CLC   FVIFLD(1),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLTIME                                                
                                                                                
         USING FHD,RF                                                           
VAPP1N02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPP1N04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPP1N04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER REP FORMAT FIELD   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPFO    NTRDO                                                                  
*                                                                               
APPFOTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPPFO)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPPFO)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER REP FORMAT FIELD                      *         
***********************************************************************         
         SPACE 1                                                                
DISAPPFO LA    RF,TLKAPPL1                                                      
         LA    RE,TLKFORM                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPPFO2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDFCDAP                                                      
         LA    RE,TLQFC                                                         
DAPPFO2  MVC   FVIFLD(1),NONOS     (Q)                                          
         CLC   0(L'TLKFORM,RE),BCSPACES                                         
         BH    DAPPFO4                                                          
         MVC   FVIFLD(1),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPPFO4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLREPT                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER REP FORMAT FIELD                     *         
***********************************************************************         
         SPACE 1                                                                
VALAPPFO MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPPFO02                                                         
         CLC   FVIFLD(1),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLREPT                                                
                                                                                
         USING FHD,RF                                                           
VAPPFO02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPPFO04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPPFO04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER SUPPLIER FIELD     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPSU    NTRDO                                                                  
*                                                                               
APPSUTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPPSU)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPPSU)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER SUPPLIER FIELD                        *         
***********************************************************************         
         SPACE 1                                                                
DISAPPSU LA    RF,TLKAPPL1                                                      
         LA    RE,TLKSUC                                                        
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPPSU2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDSUCAP                                                      
         LA    RE,TLDSUC                                                        
DAPPSU2  MVC   FVIFLD(1),NONOS     (I)                                          
         CLC   0(L'TLKSUC,RE),BCSPACES                                          
         BH    DAPPSU4                                                          
         MVC   FVIFLD(1),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPPSU4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLINVC                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER SUPPLIER FIELD                       *         
***********************************************************************         
         SPACE 1                                                                
VALAPPSU MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPPSU02                                                         
         CLC   FVIFLD(1),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLINVC                                                
                                                                                
         USING FHD,RF                                                           
VAPPSU02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPPSU04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPPSU04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING AN APPLICATION FILTER SCHEME FIELD       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
APPSC    NTRDO                                                                  
*                                                                               
APPSCTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISAPPSC)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALAPPSC)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY AN APPLICATION FILTER SCHEME FIELD                          *         
***********************************************************************         
         SPACE 1                                                                
DISAPPSC LA    RF,TLKAPPL1                                                      
         LA    RE,TLKSCHCD                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   DAPPSC2                                                          
         L     R2,ATLST                                                         
         LA    RF,TLDSCHAP                                                      
         LA    RE,TLDSCHCD                                                      
DAPPSC2  MVC   FVIFLD(1),NONOS     (E)                                          
         CLC   0(L'TLKSCHCD,RE),BCSPACES                                        
         BH    DAPPSC4                                                          
         MVC   FVIFLD(1),BCSPACES                                               
         B     EXITOK              DON'T SHOW IT IF NO SJ ACCOUNT               
DAPPSC4  CLI   0(RF),0                                                          
         BE    EXITOK              YES - NOTHING TO DO                          
         TM    0(RF),LIDLESTM                                                   
         BZ    *+10                                                             
         MVC   FVIFLD+0(1),BC@YES                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE AN APPLICATION FILTER SCHEME FIELD                         *         
***********************************************************************         
         SPACE 1                                                                
VALAPPSC MVC   BCBYTE1,TLKAPPL1                                                 
         MVC   BCBYTE2,TLKAPPL2                                                 
         XC    TLKAPPLS,TLKAPPLS                                                
         CLI   FVILEN,0                                                         
         BE    VAPPSC02                                                         
         CLC   FVIFLD(1),BCSPACES                                               
         BE    EXITNV                                                           
         CLC   FVIFLD+0(1),BC@YES                                               
         BNE   *+8                                                              
         OI    TLKAPPL1,LIDLESTM                                                
                                                                                
         USING FHD,RF                                                           
VAPPSC02 CLC   BCBYTE1,TLKAPPL1    APPLICATION HAS BEEN CHANGED?                
         BNE   VAPPSC04                                                         
         CLC   BCBYTE2,TLKAPPL2                                                 
         BE    EXITOK                                                           
VAPPSC04 L     RF,FVADDR                                                        
         LLC   R1,FHLN                                                          
         AR    RF,R1                                                            
         OI    FHII,FHIITH         YES - REVALIDATE DATA FIELD                  
         B     EXITOK                                                           
         DROP  RF                                                               
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR CLI/PRO/JOB CODE FIELD                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
CPJCD    NTRDO                                                                  
*                                                                               
CPJCDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCPJC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCPJC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A CLI/PRO/JOB CODE FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISCPJC  LA    RF,TLKCPJC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDCPJ                                                        
         MVC   FVIFLD(L'TLKCPJC),0(RF)    UNKNOWN TYPE                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHCPJC MVC   BODUB1(L'BCCPYPRD),BCCPYPRD                                      
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,BODUB1,ACOM,   X        
               (X'13',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A CLI/PRO/JOB CODE FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VALCPJC  TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
         BZ    EXITOK                                                           
         MVC   ACPJFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         MVC   CURAPPLS,TLKAPPLS                                                
         MVC   FVIFLD+LIDLOFF-LIDLACT(L'LIDLOFF),TLKCPJPO                       
         CLC   TLKCPJC,BCSPACES    ANY CLIENT PRODUCT JOB BEFORE?               
         BNH   VCPJC0A                                                          
         LLC   RE,FVILEN                                                        
         CLM   RE,1,CLILEN         IS ENTERED LENGTH CLIENT?                    
         BE    VCPJC0A             THEN MAKE SURE WE GET CLIENT OFFICE          
         SHI   RE,1                                                             
         CLC   FVIFLD(0),TLKCPJC                                                
         EX    RE,*-6                                                           
         BE    VCPJC0B                                                          
*                                                                               
VCPJC0A  OI    CPJINDS,CPJICCPJ    CLI/PRO/JOB HAS BEEN CHANGED                 
*                                                                               
VCPJC0B  GOTO1 AVALCPJ,BOPARM,(FVILEN,FVIFLD) PASS OFFICE                       
         BE    VCPJC02                                                          
         CLC   FVILEN,CLILEN                                                    
*&&UK*&& BNE   *+14                                                             
*&&US*&& BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INCLI)                                           
         B     EXITL               INVALID CLIENT CODE                          
         CLC   FVILEN,PROLEN                                                    
*&&UK*&& BNE   *+14                                                             
*&&US*&& BH    *+14                                                             
         MVC   FVMSGNO,=AL2(AE$INPRO)                                           
         B     EXITL               INVALID PRODUCT CODE                         
         MVC   FVMSGNO,=AL2(AE$INJOB)                                           
         B     EXITL               INVALID JOB CODE                             
*                                                                               
VCPJC02  DS    0H                                                               
*&&US                                                                           
         TM    ERRIND,ERCOFIN      DO WE HAVE AN INVALID OFF ON C/P/J           
         BNO   VCPJC04                                                          
         NI    ERRIND,X'FF'-ERCOFIN                                             
         MVC   FVMSGNO,=AL2(AE$CMCOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*&&                                                                             
VCPJC04  OI    CPJINDS,CPJIVCPJ    VALCPJ HAS BEEN CALLED                       
         MVC   TLKCPJC,CPJCODE     SORT THE CLI/PRO/JOB CODES                   
         MVC   TLKCPJNM,CPJNAME    SAVE CLI/PRO/JOB NAME                        
         MVC   TLKCPJPO,CPJOFF     SAVE CLI/PRO/JOB OFFICE                      
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CLI/PRO/JOB OFFICE LIST                             *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
COFF     NTRDO                                                                  
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
*                                                                               
COFFTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISCOFF)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCOFF)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHCOFF)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLI/PRO/JOB CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISCOFF  LA    RF,TLKCPJPO                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDCPJOF                                                      
         MVC   FVIFLD(L'TLKCPJPO),0(RF)                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE CLI/PRO/JOB CODE LIST FIELD                                *         
***********************************************************************         
         SPACE 1                                                                
VALCOFF  MVC   TLRLEN,=AL2(TLLNQ)                                               
         CLI   FVILEN,L'TLKCPJPO                                                
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
*&&UK*&& TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
*&&UK*&& BZ    EXITOK                                                           
         CLI   FVILEN,0                                                         
         BH    VCOFF01                                                          
*&&US                                                                           
         TM    FVIIND,FVITHIS      ENTERED ANYTHING THIS TIME?                  
         BO    VCOFF00                                                          
         LA    RF,TLKCPJC                                                       
         LLC   R0,CLILEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          ANY PRODUCT CODE?                            
         BH    EXITOK              THEN CAN'T REMOVE OFFICE                     
         MVC   TLKCPJPO,BCSPACES                                                
         B     EXITOK                                                           
*&&                                                                             
VCOFF00  LA    RF,TLKCPJC                                                       
         LLC   R0,CLILEN                                                        
         AR    RF,R0                                                            
         CLI   0(RF),C' '          ANY PRODUCT CODE?                            
         BH    VCOFF01             THEN CAN'T REMOVE OFFICE                     
         MVC   TLKCPJPO,BCSPACES                                                
         B     EXITOK                                                           
*                                                                               
VCOFF01  TM    CPJINDS,CPJIVCPJ    TEST VALCPJ CALLED                           
         BO    VCOFF02             YES - GET OFFICE CODE FROM JOB               
         GOTO1 AVALCPJ,BOPARM,(L'TLKCPJC,TLKCPJC)                               
         OI    CPJINDS,CPJIVCPJ                                                 
*                                                                               
VCOFF02  TM    CPJINDS,CPJINCLI    NOT CLIENT LEVEL                             
         BNZ   VCOFF10             YES                                          
         LA    R1,CPJCOFF                                                       
VCOFF04  CLC   0(L'TRNOFFC,R1),BCSPACES  HAVEN'T FOUND MATCH ON OFFICE          
         BNH   VCOFF08                                                          
         CLC   0(L'TRNOFFC,R1),FVIFLD                                           
         BE    VCOFF12                                                          
         LA    R1,L'TRNOFFC(R1)                                                 
         B     VCOFF04                                                          
*                                                                               
VCOFF08  L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR            SET CURSOR TO FIRST LIST FIELD         
         B     EXITNV                                                           
*                                                                               
VCOFF10  MVC   FVIFLD(L'CPJOFF),CPJOFF                                          
VCOFF12  MVC   TLKCPJPO,FVIFLD                                                  
*&&US                                                                           
         GOTO1 ATSTOFF,FVIFLD      TEST OFFICE                                  
         BNE   EXITL               INVALID OFFICE                               
*&&                                                                             
         MVC   CPJOFF,FVIFLD                                                    
         OI    FVOIND,FVOXMT       TRANSMIT FIELD                               
         B     EXITOK                                                           
         EJECT ,                                                                
***********************************************************************         
* SEARCH ON A CLI/PRO/JOB CODE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHCOFF DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,PRODUL,ACOM,   +        
               (X'13',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR CLI/PRO/JOB NAME FIELD                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
CPJND    NTRDO                                                                  
*                                                                               
CPJNDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISCPJN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY CLI/PRO/JOB NAME FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISCPJN  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKCPJNM),TLKCPJNM                                      
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDCPJ,BCSPACES                                                  
         BNH   EXITOK                                                           
         GOTO1 AVALCPJ,BOPARM,(L'TLDCPJ,TLDCPJ)                                 
         MVC   FVIFLD(L'CPJNAME),CPJNAME                                        
         B     EXITOK                                                           
*&&DO                                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKUNT(L'PRODUL),PRODUL                                       
         MVC   T.ACTKACT,TLDCPJ                                                 
         DROP  T                                                                
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
*&&                                                                             
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR EXPENDITURE TYPE FIELD                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
EXTYD    NTRDO                                                                  
*                                                                               
EXTYDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISEXTY)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHEXTY)                              
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALEXTY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EXPENDITURE TYPE FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISEXTY  LA    RF,TLKEXPC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDEXP                                                        
         MVC   FVIFLD(L'TLKEXPC),0(RF)                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A EXPENDITURE TYPE FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
SRCHEXTY DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,EXTYP,ACOM,0            
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE EXPENDITURE TYPE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALEXTY  MVC   AEXPFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'ETYKCODE                                                
         BH    EXITLONG            FIELD TOO LONG                               
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALEXP,BOPARM,FVIFLD                                            
         BE    VEXTY02                                                          
         BH    EXITOFF                                                          
         MVC   FVADDR,AEXPFLD                                                   
         MVC   FVMSGNO,=AL2(AE$INETY)                                           
         B     EXITL               INVALID EXPENDITURE CODE                     
*                                                                               
VEXTY02  MVC   TLKEXPC,EXPCODE     SORT THE EXPENDITURE CODE                    
         MVC   TLKEXPNM,EXPNAME                                                 
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR EXPENDITURE TYPE NAME FIELD                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
EXNMD    NTRDO                                                                  
*                                                                               
EXNMDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISEXNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY EXPENDITURE TYPE NAME                                       *         
***********************************************************************         
         SPACE 1                                                                
DISEXNM  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKEXPNM),TLKEXPNM                                      
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDEXP,BCSPACES                                                  
         BNH   EXITOK                                                           
T        USING ETYRECD,IOKEY                                                    
         XC    T.ETYKEY,T.ETYKEY   READ EXPENDITURE TYPE RECORD                 
         MVI   T.ETYKTYP,ETYKTYPQ                                               
         MVI   T.ETYKSUB,ETYKSUBQ                                               
         MVC   T.ETYKCPY,CUABIN    CONNECTED ID                                 
         MVC   T.ETYKCODE,TLDEXP                                                
         MVC   T.ETYKOFFC,BCSPACES                                              
         MVC   SVIOKEY,IOKEY                                                    
         DROP  T                                                                
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         CLC   SVIOKEY(ETYKOFFC-ETYRECD),IOKEY                                  
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR NON CLIENT ACCOUNT CODE FIELD                       *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
NCLCD    NTRDO                                                                  
*                                                                               
NCLCDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCLC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALNCLC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHNCLC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A NON CLIENT CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
DISNCLC  LA    RF,TLKNCLC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDNCC                                                        
         MVC   FVIFLD(L'TLKNCLC),0(RF)                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A NON CLIENT ACCOUNT CODE                                 *         
***********************************************************************         
         SPACE 1                                                                
SRCHNCLC MVC   BODUB1(L'BCCPYPRD),=C'1N'                                        
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,BODUB1,ACOM,   X        
               (X'14',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A NON CLIENT ACOUNT CODE FIELD                             *         
***********************************************************************         
         SPACE 1                                                                
VALNCLC  MVC   ANCLFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALNCL,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VNCLC02                                                          
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
VNCLC02  MVC   TLKNCLC,NCLCODE     SORT THE NON CLIENT CODES                    
         MVC   TLKNCLNM,NCLNAME    SAVE NON CLIENT NAME                         
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR NON CLIENT NAME FIELD                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
NCLND    NTRDO                                                                  
*                                                                               
NCLNDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISNCLN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY NON CLIENT NAME FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISNCLN  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKNCLNM),TLKNCLNM                                      
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDNCC,BCSPACES                                                  
         BNH   EXITOK                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKUNT(L'NCTUL),NCTUL                                         
         MVC   T.ACTKACT,TLDNCC                                                 
         DROP  T                                                                
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR WORK CODE FIELD                                     *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
WRKCD    NTRDO                                                                  
*                                                                               
WRKCDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISWRKC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALWRKC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHWRKC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A WORK CODE FIELD                                           *         
***********************************************************************         
         SPACE 1                                                                
DISWRKC  LA    RF,TLKWCC                                                        
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDWC                                                         
         MVC   FVIFLD(L'TLKWCC),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A WORK CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
SRCHWRKC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,WC,ACOM,       X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A WORK CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
VALWRKC  MVC   AWRKFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'WCOKWRK                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALWC,BOPARM,(FVILEN,FVIFLD)                                    
         BE    VWRKC02                                                          
         MVC   FVMSGNO,=AL2(AE$INWRK)                                           
         B     EXITL               INVALID WORK CODE                            
*                                                                               
VWRKC02  MVC   TLKWCC,WRKCODE      SORT THE WORK CODES                          
         MVC   TLKWCNM,WRKNAME     SAVE WORK CODE NAME                          
         MVC   TLKWCDES,WRKDESC    SAVE WORK CODE DESCRIPTION                   
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR WORK CODE NAME FIELD                                *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
WRKND    NTRDO                                                                  
*                                                                               
WRKNDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISWRKN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WORD CODE NAME FIELD                                        *         
***********************************************************************         
         SPACE 1                                                                
DISWRKN  MVC   FVIFLD(L'TLKWCNM),TLKWCNM                                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR WORK CODE DESCRIPTION FIELD                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
WRKDD    NTRDO                                                                  
*                                                                               
WRKDDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISWRKD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY WORD CODE DESCRIPTION FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
DISWRKD  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKWCDES),TLKWCDES                                      
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         OC    TLDWC,TLDWC                                                      
         BZ    EXITOK                                                           
         USING WCORECD,IOKEY                                                    
         MVC   WCOKEY,BCSPACES     READ WORK CODE RECORD                        
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN      CONNECTED ID                                 
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         MVC   WCOKWRK,TLDWC                                                    
         LHI   R1,XOREAD+XOACCMST+XIO2                                          
         GOTOR AIO                                                              
         BNE   EXITOK                                                           
         GOTO1 AGETEL,BOPARM,('WCOELQ',AIO2),0                                  
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'WCODESC),BOELEM+(WCODESC-WCOELD)                        
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR MEDIA CODE FIELD                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
MEDCD    NTRDO                                                                  
*                                                                               
MEDCDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALMEDC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHMEDC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A MEDIA CODE FIELD                                          *         
***********************************************************************         
         SPACE 1                                                                
DISMEDC  LA    RF,TLKMEDC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDMED                                                        
         MVC   FVIFLD(L'TLKMEDC),0(RF)    UNKNOWN TYPE                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A MEDIA CODE                                              *         
***********************************************************************         
         SPACE 1                                                                
SRCHMEDC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,MED,ACOM,      X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE A MEDIA CODE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
VALMEDC  MVC   AMEDFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   DELETE THE LINE                              
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'PMDKMED                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALMED,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VMEDC02                                                          
         MVC   FVMSGNO,=AL2(AE$INVCD)                                           
         B     EXITL               INVALID CODE                                 
*                                                                               
VMEDC02  MVC   TLKMEDC,MEDCODE     SORT THE MEDIA CODES                         
         MVC   TLKMEDNM,MEDNAME    SAVE MEDIA NAME                              
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR MEDIA NAME FIELD                                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
MEDND    NTRDO                                                                  
*                                                                               
MEDNDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISMEDN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY MEDIA NAME FIELD                                            *         
***********************************************************************         
         SPACE 1                                                                
DISMEDN  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKMEDNM),TLKMEDNM                                      
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         OC    TLDMED,TLDMED                                                    
         BZ    EXITOK                                                           
         USING PMDRECD,R5                                                       
         LA    R5,IOKEY                                                         
         MVC   PMDKEY,BCSPACES     READ MEDIA CODE RECORD                       
         MVC   PMDKCPY,CUABIN      CONNECTED ID                                 
         MVI   PMDKTYP,PMDKTYPQ                                                 
         MVC   PMDKMED,TLDMED                                                   
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK                                                           
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('PMDELQ',PMDRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
T        USING PMDELD,BOELEM                                                    
         MVC   FVIFLD(L'T.PMDDESC),T.PMDDESC                                    
         B     EXITOK                                                           
         DROP  T,R5                                                             
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR 1R ACCOUNT CODE FIELD                               *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
C1RAD    NTRDO                                                                  
*                                                                               
C1RADTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DIS1RAC)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VAL1RAC)                                
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCH1RAC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY 1R ACCOUNT CODE LIST FIELD                                  *         
***********************************************************************         
         SPACE 1                                                                
DIS1RAC  LA    RF,TLK1RAC                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLD1RC                                                        
         MVC   FVIFLD(L'ACTKACT),0(RF)                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON 1R ACCOUNT CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCH1RAC DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,AC1R,          X        
               ACOM,(X'14',0)                                                   
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE 1R ACCOUNT CODE LIST FIELD                                 *         
***********************************************************************         
         SPACE 1                                                                
VAL1RAC  DS    0H                                                               
*&&US*&& NI    ERRIND,X'FF'-ERCOFIN                                             
         MVC   A1RAFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'ACTKACT                                                 
         BH    EXITLONG            FIELD TOO LONG                               
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALC1R,BOPARM,(FVILEN,FVIFLD)                                   
         BE    V1RC02                                                           
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
V1RC02   DS    0H                                                               
*&&US                                                                           
         TM    ERRIND,ERCOFIN      DO WE HAVE AN INVALID OFF ON C/P/J           
         BNO   V1RC04                                                           
         MVC   FVMSGNO,=AL2(AE$CMCOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*&&                                                                             
*                                                                               
V1RC04   MVC   TLK1RAC,C1RCODE     SORT THE COSTING 1R CODES                    
         MVC   TLKC1RNM,C1RNAME    SAVE COSTING 1R NAME                         
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A 1R ACCOUNT NAME FIELD                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
N1RAD    NTRDO                                                                  
*                                                                               
N1RADTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DIS1RAN)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A 1R NAME FIELD                                             *         
***********************************************************************         
         SPACE 1                                                                
DIS1RAN  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKC1RNM),TLKC1RNM                                      
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLD1RC,BCSPACES                                                  
         BNH   EXITOK                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKUNT(L'AC1R),AC1R                                           
         MVC   T.ACTKACT,TLD1RC                                                 
         DROP  T                                                                
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FORMAT CODE FIELD                                   *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
FRCDD    NTRDO                                                                  
*                                                                               
FRCDDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRCD)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALFRCD)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY FORMAT CODE LIST FIELD                                      *         
***********************************************************************         
         SPACE 1                                                                
DISFRCD  LA    RF,TLKFORM                                                       
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLQFC                                                         
         MVC   FVIFLD(L'TLKFORM),0(RF)                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE FORMAT CODE LIST FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
VALFRCD  MVC   AFORFLD,FVADDR      SAVE A(FIELD)                                
         L     RF,FVADDR                                                        
         STCM  RF,15,BOCURSOR      SET CURSOR TO FIRST LIST FIELD               
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         CLI   FVILEN,0                                                         
         BNE   *+12                                                             
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'RESKFORM                                                
         BH    EXITLONG            FIELD TOO LONG                               
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALFORM,BOPARM,(FVILEN,FVIFLD)                                  
         BE    VALFRCD2                                                         
* CC=LOW - BAD FORMAT                                                           
* CC=HIGH - NOT ACCENT OR QUICK REPORT FORMAT                                   
         MVC   FVMSGNO,=AL2(AE$INVFM)                                           
         B     EXITL               INVALID FORMAT CODE                          
*                                                                               
VALFRCD2 MVC   TLKRPTY,REPCODE     SAVE REPORT CODE OF FORMAT                   
         MVC   TLKFRNM,FORMNAME    SAVE FORMAT NAME                             
         MVC   TLKFORM,FORMCODE    FORMCODE IS TSAR KEY                         
         MVC   TLKTRAN,TRANTYPE    TRANSMISSION TYPE                            
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A FORMAT NAME FIELD                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
FRNMD    NTRDO                                                                  
*                                                                               
FRNMDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRNM)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A FORMAT NAME                                               *         
***********************************************************************         
         SPACE 1                                                                
DISFRNM  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKFRNM),TLKFRNM                                        
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         OC    TLQFC,TLQFC                                                      
         BZ    EXITOK                                                           
*                                                                               
         LA    R5,IOKEY                                                         
         USING RESRECD,R5                                                       
         MVC   RESKEY,BCSPACES     READ SCRIBE RECORD                           
         MVI   RESKTYP,RESKTYPQ                                                 
         MVI   RESKSUB,RESKSUBQ                                                 
         MVC   RESKCPY,CUABIN      CONNECTED ID                                 
         MVC   RESKFORM,TLQFC                                                   
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK              FORMAT DOESN'T EXIST                         
*                                                                               
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('RPFELQ',RESRECD),0                               
         BNE   EXITOK              NO R.L. PROFILE                              
*                                                                               
T        USING RPFELD,BOELEM                                                    
         MVC   TLQTT,BCSPACES                                                   
         TM    T.RPFXMIT,RPFXACNT  ACCENT TRANSMISSION TYPE?                    
         BNO   *+14                                                             
         MVC   TLQTT(6),=C'ACCENT'                                              
         B     DISFRM1                                                          
         TM    T.RPFXMIT,RPFXQREP  QUICK REPORT TRANSMISSION TYPE?              
         BZ    EXITOK              NOT ACCENT OR QUICK REPORT TYPE              
         MVC   TLQTT(8),=C'QREPORTS'                                            
         DROP  T                                                                
*                                                                               
DISFRM1  MVC   TLQNM,BCSPACES                                                   
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',RESRECD),0                               
         BNE   DISFRM2             SOME FORMATS DON'T HAVE NAMES                
                                                                                
T        USING NAMELD,BOELEM                                                    
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SHI   RF,NAMLN1Q+1                                                     
         MVC   TLQNM(0),T.NAMEREC                                               
         EX    RF,*-6                                                           
         MVC   FVIFLD(L'TLQNM),TLQNM                                            
*                                                                               
DISFRM2  GOTO1 AGETEL,BOPARM,('STYELQ',RESRECD),0                               
         BE    *+6                                                              
         DC    H'0'                STYELD MISSING                               
                                                                                
T        USING STYELD,BOELEM                                                    
         MVC   TLQRT,T.STYCODE     REPORT TYPE                                  
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A REPORT TYPE                            *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
FRTYD    NTRDO                                                                  
*                                                                               
FRTYDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRTY)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A REPORT TYPE FIELD                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFRTY  LA    RF,REPTBL                                                        
         USING REPTYPD,RF                                                       
         SR    R0,R0                                                            
         SR    R4,R4                                                            
         LA    R4,1                                                             
*                                                                               
DISFRTY1 CLI   REPTYLLN,0         EOT                                           
         BNE   *+6                                                              
         DC    H'0'               NO REPORT TYPE                                
         IC    R0,REPTYLLN                                                      
         LR    R5,RF                                                            
         AR    R5,R0              R5 POINTS AT NEXT REC                         
         BCTR  R5,0               R5 -> LAST CODE                               
         LA    R6,REPCODES                                                      
         CLI   CSACT,A#DLOAD                                                    
         BE    DISFRTY3                                                         
*                                                                               
DISFRTY2 CLC   0(1,R6),TLKRPTY                                                  
         BE    DISFRTYX                                                         
         BXLE  R6,R4,DISFRTY2                                                   
         LR    RF,R6              ON LOOP EXIT R6 -> NEXT REP TYPE              
         B     DISFRTY1                                                         
*                                                                               
DISFRTY3 L     R2,ATLST                                                         
         OC    TLQRT,TLQRT                                                      
         BZ    EXITOK                                                           
         CLC   0(1,R6),TLQRT                                                    
         BE    DISFRTYX                                                         
         BXLE  R6,R4,DISFRTY3                                                   
         LR    RF,R6                                                            
         B     DISFRTY1                                                         
*                                                                               
DISFRTYX XR    RE,RE                                                            
         ICM   RE,3,REPNAME                                                     
         LA    RE,OVERWRKD(RE)                                                  
         MVC   FVIFLD(L'UC@SRCV),0(RE)                                          
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
*                                                                               
REPTBL   DC    0H                                                               
REPRCV   DC    AL1(REPRCVLN)        LENGTH OF REC    - CREDITORS                
         DC    AL2(UC@SRCV-OVERWRKD) DDICT ENTRY                                
REPLN    EQU   *-REPTBL             LENGTH OF RECORD WITHOUT CODES              
         DC    AL1(REP#RCV,REP#ADV,REP#BAL)   CODE THAT GO WITH DDICT           
REPRCVLN EQU   *-REPRCV                                                         
*                                                                               
REPINC   DC    AL1(REPINCLN)                    - INCOME                        
         DC    AL2(UC@SINC-OVERWRKD)                                            
         DC    AL1(REP#INC,REP#SUP,REP#ICST)                                    
REPINCLN EQU   *-REPINC                                                         
*                                                                               
REPPAY   DC    AL1(REPPAYLN)                                                    
         DC    AL2(UC@PAY-OVERWRKD)                                             
         DC    AL1(REP#PAY,REP#PAYQ,REP#PAYS,REP#PAYT,REP#PAYU)                 
         DC    AL1(REP#PAYV,REP#PAYW,REP#PAYX,REP#PAYY,REP#PAYC)                
         DC    AL1(REP#PAYF)                                                    
REPPAYLN EQU   *-REPPAY                                                         
*                                                                               
REPEXP   DC    AL1(REPEXPLN)                                                    
         DC    AL2(UC@EXP-OVERWRKD)                                             
         DC    AL1(REP#EXP,REP#EXPF,REP#EXPL,REP#EXPD,REP#EXPB)                 
         DC    AL1(REP#EXPP)                                                    
REPEXPLN EQU   *-REPEXP                                                         
*                                                                               
REPPRO   DC    AL1(REPPROLN)                                                    
         DC    AL2(UC@497-OVERWRKD)                                             
         DC    AL1(REP#PROD)                                                    
REPPROLN EQU   *-REPPRO                                                         
*                                                                               
REPCST   DC    AL1(REPCSTLN)                                                    
         DC    AL2(UC@498-OVERWRKD)                                             
         DC    AL1(REP#CST)                                                     
REPCSTLN EQU   *-REPCST                                                         
*                                                                               
REPCSH   DC    AL1(REPCSHLN)                                                    
         DC    AL2(UC@540-OVERWRKD)                                             
         DC    AL1(REP#CASH)                                                    
REPCSHLN EQU   *-REPCSH                                                         
*                                                                               
REPPNL   DC    AL1(REPPNLLN)                                                    
         DC    AL2(UC@544-OVERWRKD)                                             
         DC    AL1(REP#PNL)                                                     
REPPNLLN EQU   *-REPPNL                                                         
*                                                                               
REPGLG   DC    AL1(REPGLGLN)                                                    
         DC    AL2(UC@GLG-OVERWRKD)                                             
         DC    AL1(REP#GNL,REP#GNLP)                                            
REPGLGLN EQU   *-REPGLG                                                         
*                                                                               
REPMED   DC    AL1(REPMEDLN)                                                    
         DC    AL2(UC@MED-OVERWRKD)                                             
         DC    AL1(REP#MEDA)                                                    
REPMEDLN EQU   *-REPMED                                                         
*                                                                               
REPFI    DC    AL1(REPFILN)                                                     
         DC    AL2(UC@FI-OVERWRKD)                                              
         DC    AL1(REP#FI)                                                      
REPFILN  EQU   *-REPFI                                                          
*                                                                               
REPMAN   DC    AL1(REPMANLN)                                                    
         DC    AL2(UC@M2-OVERWRKD)                                              
         DC    AL1(REP#M2)                                                      
REPMANLN EQU   *-REPMAN                                                         
*                                                                               
         DC    X'00'                                                            
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A TRANSMISSION TYPE                      *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
FRTTD    NTRDO                                                                  
*                                                                               
FRTTDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRTT)                                
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A TRANSMISSION TYPE                                         *         
***********************************************************************         
         SPACE 1                                                                
DISFRTT  CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLKTRAN),TLKTRAN                                        
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         MVC   FVIFLD(L'TLQTT),TLQTT                                            
         B     EXITOK                                                           
         POP   USING                                                            
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR SUPPLIER CODE LIST                                  *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SUC      NTRDO                                                                  
*                                                                               
SUCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSUC)                                 
         DC    AL1(DSRCH),AL1(0,0,0),AL4(SRCHSUP)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SUPPLIER CODE LIST FIELD                                    *         
***********************************************************************         
         SPACE 1                                                                
DISSUC   LA    RF,TLKSUC                                                        
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDSUC                                                        
         MVC   FVIFLD(L'TLKSUC),0(RF)                                           
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* VALIDATE SUPPLIER CODE LIST FIELD                                   *         
***********************************************************************         
         SPACE 1                                                                
VALSUC   MVC   TLRLEN,=AL2(TLLNQ)  SET TSAR RECLEN                              
         CLI   FVILEN,0                                                         
         BNE   VSUC04                                                           
         XC    TLKSUC,TLKSUC                                                    
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
VSUC04   LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 80 ITEMS                              
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
         CLI   FVILEN,L'TLKSUC                                                  
         BH    EXITLONG            FIELD TOO LONG                               
*                                                                               
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALSUP,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VSUC06                                                           
         MVC   FVMSGNO,=AL2(AE$INVAC)                                           
         MVC   BOCURSOR,FVADDR          SET CURSOR TO ACTION FIELD              
         B     EXITL               INVALID ACCOUNT CODE                         
*                                                                               
VSUC06   MVC   TLKSUC(L'SUPCODE),SUPCODE  SORT THE SUPPLIER CODES               
         MVC   TLSUPNM,SUPNAME     SAVE SUPPLIER NAME                           
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SEARCH ON A SUPPLIER CODE FIELD                                     *         
***********************************************************************         
         SPACE 1                                                                
SRCHSUP  DS    0H                                                               
*&&US*&& CLI   ASONOFF,ASOFF       RUNNING OFFLINE ?                            
*&&US*&& BE    EXITOK              YES EXIT.                                    
         GOTO1 VACSRCHC,BOPARM,('STMPSTRQ',FVADDR),ATWA,0,ACOM,        X        
               (X'44',0)                                                        
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR DISPLAYING A SUPPLIER NAME FIELD                    *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
* R3 HOLDS A(FIELD TABLE ENTRY)                                       *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SUN      NTRDO                                                                  
*                                                                               
SUNTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSUN)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A SUPPLIER NAME FIELD                                       *         
***********************************************************************         
         SPACE 1                                                                
DISSUN   CLI   CSACT,A#DLOAD                                                    
         BE    *+14                                                             
         MVC   FVIFLD(L'TLSUPNM),TLSUPNM                                        
         B     EXITOK                                                           
         L     R2,ATLST                                                         
         CLC   TLDSUC,BCSPACES                                                  
         BNH   EXITOK                                                           
T        USING ACTRECD,IOKEY                                                    
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN    CONNECTED USER                               
         MVC   T.ACTKULA,TLDSUC                                                 
         DROP  T                                                                
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   EXITOK                                                           
         L     R1,AIO2                                                          
         GOTO1 AGETNAM             DISPLAY NAME                                 
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT ,                                                                
***********************************************************************         
* DATA OBJECT FOR ESTIMATE SCHEME                                     *         
***********************************************************************         
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SCHCD    NTRDO                                                                  
                                                                                
SCHCDTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSCC)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSCC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SCHEME CODE                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISSCC   LA    RF,TLKSCHCD                                                      
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+12                                                             
         L     R2,ATLST                                                         
         LA    RF,TLDSCHCD                                                      
         MVC   FVIFLD(L'TLKSCHCD),0(RF)                                         
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE SCHEME CODE                                                *         
***********************************************************************         
         SPACE 1                                                                
VALSCC   CLI   FVILEN,0                                                         
         BNE   VALSC1                                                           
         XC    TLKSCHCD,TLKSCHCD                                                
         XC    TLKSCHNM,TLKSCHNM                                                
         OI    LSLNIND1,LSLNIDEL   NO INPUT - DELETE THIS LINE                  
         B     EXITOK                                                           
*                                                                               
VALSC1   LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 80 ITEMS                              
         BNH   *+14                                                             
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
         MVC   CURAPPLS,TLKAPPLS                                                
         GOTO1 AVALSCH,BOPARM,(FVILEN,FVIFLD)                                   
         BE    VALSC2                                                           
         MVC   FVMSGNO,=AL2(AE$INSCH)                                           
         MVC   BOCURSOR,FVADDR                                                  
         B     EXITL               INVALID SCHEME CODE                          
*                                                                               
VALSC2   MVC   TLKSCHCD,SCHCODE    SAVE CODE AND NAME FOR LATEER                
         MVC   TLKSCHNM(L'SCHENAM),SCHENAM                                      
         MVC   TLKAPPLS,CURAPPLS                                                
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* DATA OBJECT FOR ESTIMATE SCHEME NAME                                *         
***********************************************************************         
         PUSH  USING                                                            
         USING TLSTD,R2                                                         
SCHNM    NTRDO                                                                  
                                                                                
SCHNMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISSNM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY SCHEME NAME                                                 *         
***********************************************************************         
         SPACE 1                                                                
DISSNM   CLI   CSACT,A#DLOAD                                                    
         BE    DISSN2                                                           
         GOTO1 AVALSCH,BOPARM,(L'TLKSCHCD,TLKSCHCD)                             
         MVC   FVIFLD(L'SCHENAM),SCHENAM                                        
         B     EXITOK                                                           
*                                                                               
DISSN2   L     R2,ATLST                                                         
         GOTO1 AVALSCH,BOPARM,(L'TLDSCHCD,TLDSCHCD)                             
         BNE   EXITOK                                                           
         MVC   FVIFLD(L'SCHENAM),SCHENAM                                        
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* DOWNLOAD OBJECT LEVEL                                               *         
***********************************************************************         
         SPACE 1                                                                
THIS     USING LLSRECD,R2                                                       
LAST     USING LLSRECD,R3                                                       
DOWN     NTRDO                                                                  
*                                                                               
DOWNTBL  DC    AL1(DPQINIT),AL1(0,0,0),AL4(DLPQINI)                             
         DC    AL1(DDLINIT),AL1(0,0,0),AL4(DLINIT)                              
         DC    AL1(DAPPCOL),AL1(0,0,0),AL4(DLSCOL)                              
         DC    AL1(DSCREEN),AL1(0,0,0),AL4(DLSCR)                               
         DC    AL1(DSETCOLS),AL1(0,0,0),AL4(DLSETC)                             
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE PRINT QUEUE                                              *         
***********************************************************************         
         SPACE 1                                                                
DLPQINI  MVC   INSYSID,=C'AC'                                                   
*&&UK*&& MVC   INPRGID,=C'FL'      REPORT PROGRAM ID                            
*&&UK*&& MVC   INJCLID,=C'FL'      REPORT JCL ID                                
*&&US*&& MVC   INPRGID,=C'AF'      REPORT PROGRAM ID                            
*&&US*&& MVC   INJCLID,=C'AF'      REPORT JCL ID                                
         MVI   INPRTY1,0                                                        
         MVI   INPRTY2,0                                                        
         L     R5,AREP                                                          
         USING REPD,R5                                                          
         MVC   INOTYP,=CL6'DOWN'   DOWNLOAD TYPE                                
         MVC   REPSUBID,INUSER     SET REQUESTOR ID                             
         MVC   REPSYSID,INSYSID                                                 
         MVC   REPPRGID,INPRGID                                                 
         MVC   INDEST,REPUSRID     THE CONTROLLER RESET INDEST!!                
         OI    REPIND2,REPILOW                                                  
         OI    REPHEADI,REPHSPAC+REPHCLRA                                       
         OI    REPMIDSI,REPMSPAC+REPMCLRA                                       
         MVC   REPRLH,=Y(48)                                                    
         MVC   REPRDH,=Y(12)                                                    
*                                                                               
         CLI   ASONOFF,ASOFF       TEST OFFLINE                                 
         BNE   EXITOK                                                           
         L     R0,=F'9000000'      YES-GET 9M FOR CONTROLLER'S TSAR             
         STCM  R0,15,LTSOBUF       BUFFER (WAS 5M)                              
         GETMAIN RU,LV=(0),LOC=(ANY,ANY)                                        
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
         STCM  R1,15,ATSOBUF                                                    
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* INITIALISE FOR DOWNLOAD LIST                                        *         
***********************************************************************         
         SPACE 1                                                                
DLINIT   DS    0H                                                               
         OI    LSSTAT1,LSSTSAR     LIST IS TSAR RECORDS ONLY                    
         B     EXITOK                                                           
         SPACE 1                                                                
***********************************************************************         
* PRINT SCREEN PAGE 1                                                 *         
***********************************************************************         
         SPACE 1                                                                
DLSCR    B     EXITH               LET CONTROLLER PRINT SCREEN PAGE 1           
         SPACE 2                                                                
***********************************************************************         
* SET COLUMNS FOR DOWNLOAD REPORT                                     *         
***********************************************************************         
         SPACE 1                                                                
GSFRP    USING FRPELD,GSFRPEL                                                   
DLSCOL   LA    RE,DLCLMSX                                                       
         STH   RE,LSVARNUM         N'COLUMNS                                    
         LA    R1,LSVARCLM                                                      
         USING DCTABD,R1                                                        
         LA    RF,DLCLMS                                                        
*                                                                               
DLSCOL04 XC    0(DCTABL,R1),0(R1)  CLEAR COLUMN ENTRY                           
         MVC   DCTFLD#,0(RF)       SET FIELD NUMBER                             
         MVC   DCTCOL#,2(RF)       SET COLUMN NUMBER                            
         LA    R1,DCTABL(,R1)                                                   
         LA    RF,3(,RF)                                                        
         BCT   RE,DLSCOL04         SET NEXT COLUMN                              
*                                                                               
DLSCOL08 XC    0(DCTABL,R1),0(R1)                                               
         MVI   GSFRP.FRPTYPE,FRPTDWN                                            
         XC    LSFIXNUM,LSFIXNUM   NO FIXED COLUMNS                             
         B     EXITOK                                                           
         DROP  R1,GSFRP                                                         
         SPACE 4                                                                
DLCLMS   DC    AL2(F#LIML#PIDL),AL1(1)              PID CODE                    
         DC    AL2(F#LIML#FNML),AL1(2)              FIRST NAME                  
         DC    AL2(F#LIML#LNML),AL1(3)              LAST NAME                   
         DC    AL2(F#LIML#DEFAD),AL1(4)             DEF. ACCESS (D/L)           
         DC    AL2(F#LIML#APPSJ),AL1(5)             APPL C/P/J                  
         DC    AL2(F#LIML#CPJC),AL1(6)              CLI/PRO/JOB CODE            
         DC    AL2(F#LIML#COFF),AL1(7)              CLI/PRO/JOB OFF             
         DC    AL2(F#LIML#CPJN),AL1(8)              CLI/PRO/JOB NAME            
         DC    AL2(F#LIML#APPET),AL1(9)             APPL EXPEND TYP             
         DC    AL2(F#LIML#EXTY),AL1(10)             EXPENDITURE TYPE            
         DC    AL2(F#LIML#EXNM),AL1(11)             EXPENDITURE NAME            
         DC    AL2(F#LIML#APPWC),AL1(12)            APPL WORK CODE              
         DC    AL2(F#LIML#TWCL),AL1(13)             WORK CODE                   
         DC    AL2(F#LIML#TWCD),AL1(14)             WORK CODE DESC              
         DC    AL2(F#LIML#APP1N),AL1(15)            APPL 1N                     
         DC    AL2(F#LIML#NCLC),AL1(16)             NON CLIENT CODE             
         DC    AL2(F#LIML#NCLN),AL1(17)             NON CLIENT NAME             
         DC    AL2(F#LIML#APPME),AL1(18)            APPL MEDIA                  
         DC    AL2(F#LIML#MEDL),AL1(19)             MEDIA CODE                  
         DC    AL2(F#LIML#MEDN),AL1(20)             MEDIA NAME                  
         DC    AL2(F#LIML#APP1R),AL1(21)            APPL 1R                     
         DC    AL2(F#LIML#C1RA),AL1(22)             1R ACCOUNT CODE             
         DC    AL2(F#LIML#N1RA),AL1(23)             1R ACCOUNT NAME             
         DC    AL2(F#LIML#APPFO),AL1(24)            APPL FORMAT                 
         DC    AL2(F#LIML#SFCD),AL1(25)             FORMAT CODE                 
         DC    AL2(F#LIML#SFNM),AL1(26)             FORMAT NAME                 
         DC    AL2(F#LIML#REPT),AL1(27)             REPORT TYPE                 
         DC    AL2(F#LIML#TRAN),AL1(28)             TRANSMISSION TYPE           
         DC    AL2(F#LIML#APPSC),AL1(29)            APPL SCHEME                 
         DC    AL2(F#LIML#SCHCD),AL1(30)            SCHEME CODE                 
         DC    AL2(F#LIML#SCHNM),AL1(31)            SCHEME NAME                 
         DC    AL2(F#LIML#APPSU),AL1(32)            APPL SUPPLIER               
         DC    AL2(F#LIML#APSUC),AL1(33)            SUPPLIER CODE               
         DC    AL2(F#LIML#APSUN),AL1(34)            SUPPLIER NAME               
                                                                                
DLCLMSX  EQU   (*-DLCLMS)/3                                                     
         SPACE 2                                                                
***********************************************************************         
* SET LIST HEADERS FOR COLUMNS                                        *         
***********************************************************************         
         SPACE 1                                                                
DLSETC   DS    0H                                                               
         OI    DLINDS,DLBALL       BUILD LIST IN ONE GO                         
         B     EXITOK                                                           
FIL46    CSECT                                                                  
         SPACE 2                                                                
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
         BNE   NTRO02                                                           
         CLI   SREC,R#LIML         LIMIT LIST RECORD                            
         BE    NTRO01                                                           
*        CLI   SREC,R#GRPL         GROUP LIST RECORD                            
*        BE    NTRO01                                                           
         CLI   SREC,R#FILE         FILE RECORD                                  
         BNE   NTRO02                                                           
NTRO01   OI    SNINDS1,SNIPARMS                                                 
         B     EXITOK                                                           
*                                                                               
NTRO02   CLI   CSREC,R#LIML                                                     
         BNE   EXITOK                                                           
         CLI   CSACT,A#LST                                                      
         BNE   EXITOK                                                           
         L     R4,ATLST                                                         
         USING TLSTD,R4                                                         
         MVC   SVPID,TLKPID                                                     
         MVC   SVGRPCDE,TLKGRPC                                                 
         B     EXITOK                                                           
         DROP  R4                                                               
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM PREVIOUS LEVEL      *         
* --------------------------------------------------------------      *         
* SVPARMS3 = A(PSSAV)                                                 *         
* SVPARMS4 = A(FESD TO BE RESTORED)                                   *         
***********************************************************************         
         SPACE 1                                                                
NTRIN    CLI   PSREC,R#LIML                                                     
         BNE   EXITOK                                                           
         CLI   PSACT,A#LST                                                      
         BNE   EXITOK                                                           
         MVC   SVGRPC,SVGRPCDE                                                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER FROM HIGHER LEVEL        *         
***********************************************************************         
         SPACE 1                                                                
XITIN    CLI   SACT,A#LST          PAGE DISPLAY SCREEN FROM LIST SCREEN         
*        BNE   EXITH                                                            
         B     EXITOK                                                           
         DROP  R2                                                               
         EJECT ,                                                                
***********************************************************************         
* LIST OBJECT                                                         *         
*                                                                     *         
* P1 HOLDS EQUATED OBJECT                                             *         
* P2 HOLDS EQUATED VERB                                               *         
* P3 HOLDS CURRENT KEY BUILD AREA                                     *         
* P4 HOLDS PREVIOUS KEY                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R2                                                       
LIST     LM    R0,R3,SVPARMS                                                    
THIS     USING LLSRECD,R2                                                       
LAST     USING LLSRECD,R3                                                       
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
         SPACE 2                                                                
*                                                                               
LISTABL  DC    AL1(LINIT),AL1(0,0,0),AL4(ILST)                                  
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
*                                                                               
         DC    AL1(LINIT),AL1(0,0,254),AL4(ILST)                                
         DC    AL1(LGETFRST),AL1(0,0,254),AL4(FLST)                             
         DC    AL1(LGETNEXT),AL1(0,0,254),AL4(NLST)                             
         DC    AL1(LTSARDIR),AL1(0,0,254),AL4(TSARDIR)                          
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,1),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,1),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,1),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,1),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLST1)                            
*&&US*&& DC    AL1(LSCRLAST),AL1(0,0,1),AL4(SCRLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,2),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,2),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,2),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,2),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,2),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,2),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,2),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,2),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,3),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,3),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,3),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,3),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,3),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,3),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,3),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,3),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,4),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,4),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,4),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,4),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,4),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,4),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,4),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,4),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,5),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,5),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,5),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,5),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,5),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,5),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,5),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,5),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,6),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,6),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,6),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,6),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,6),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,6),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,6),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,6),AL4(UPDLST1)                            
*&&US*&& DC    AL1(LSCRLAST),AL1(0,0,6),AL4(SCRLAST1)                           
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,7),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,7),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,7),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,7),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,7),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,7),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,7),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,7),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,8),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,8),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,8),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,8),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,8),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,8),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,8),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,8),AL4(UPDLST1)                            
*                                                                               
         DC    AL1(LGETFRST),AL1(0,0,9),AL4(FLST1)                              
         DC    AL1(LGETNEXT),AL1(0,0,9),AL4(NLST1)                              
         DC    AL1(LLSTFRST),AL1(0,0,9),AL4(FTFLST1)                            
         DC    AL1(LINIT),AL1(0,0,9),AL4(INITL1)                                
         DC    AL1(LTSARFIL),AL1(0,0,9),AL4(TSARFIL1)                           
         DC    AL1(LUPDFRST),AL1(0,0,9),AL4(UPDFRST1)                           
         DC    AL1(LUPDREC),AL1(0,0,9),AL4(UPDREC1)                             
         DC    AL1(LUPDLAST),AL1(0,0,9),AL4(UPDLST1)                            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INIT LIST                                                           *         
***********************************************************************         
         SPACE 1                                                                
ILST     OI    LSSTAT1,LSSBALL+LSSTSAR                                          
         NI    CPJINDS,X'FF'-(CPJIVCPJ+CPJINCLI+CPJICCPJ)                       
         MVI   ERRIND,0                                                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST                                                      *         
***********************************************************************         
         SPACE 1                                                                
X        USING LLSRECD,IOKEY                                                    
FLST     MVC   X.LLSKEY,THIS.LLSKEY                                             
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTOR AIO                                                              
         BE    *+12                                                             
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
         MVI   DWNINDS,DWNGDATA    GET DATA INTO TSAR                           
         MVI   DWNIND2,DWNGDAT2    GET DATA INTO TSAR                           
         CLI   CSACT,A#DLOAD                                                    
         BNE   NLST02                                                           
         TM    IOERR,IOEDEL                                                     
         BNZ   NLST01A                                                          
         B     NLST02                                                           
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
         SPACE 1                                                                
NLST     CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   NLST01                                                           
         TM    DWNINDS,DWNNOALL    NO MORE LIST ELEMENTS?                       
         BNO   NLST06                                                           
         TM    DWNIND2,DWNNOAL2    NO - GET NEXT LIST ELEMENTS                  
         BNO   NLST06                                                           
*                                                                               
NLST01   MVI   DWNINDS,DWNGDATA    GET DATA INTO TSAR                           
         MVI   DWNIND2,DWNGDAT2    GET DATA INTO TSAR                           
         CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BNE   NLST01A                                                          
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTO1 AIO                                                              
*                                                                               
NLST01A  L     R1,=AL4(XOSQD+XOACCDIR+XIO11)                                    
         GOTOR AIO                                                              
         BE    NLST02                                                           
         TM    IOERR,FF-IOEDEL                                                  
         BNZ   EXITL                                                            
         CLI   CSACT,A#DLOAD                                                    
         BNE   NLST02                                                           
         TM    IOERR,IOEDEL                                                     
         BE    NLST01A                                                          
*                                                                               
NLST02   CLC   X.LLSKEY(LLSKREM-LLSRECD),THIS.LLSKEY                            
         BNE   EXITL               CHANGE COMPANY OR UNIT/LEDGER                
         CLI   X.LLSKSEQ,0         FIRST LIMLST RECORD                          
         BNE   NLST                NO - GET THE NEXT ONE                        
         CLI   CSACT,A#DLOAD       DOWNLOADING?                                 
         BE    NLST04                                                           
         OC    X.LLSKPIDB,X.LLSKPIDB                                            
         BZ    NLST                MUST HAVE A PID                              
*                                                                               
         CLI   CRECDEL,0           NO FILTER - DEFAULT IS DELETE=NO             
         BE    NLST04                                                           
         CLI   CRECDEL,YES         DELETE=YES                                   
         BE    NLST06                                                           
         CLI   CRECDEL,ONLY        DELETE=ONLY                                  
         BNE   NLST04                                                           
         TM    IOERR,IOEDEL        TEST IF RECORD IS DELETED                    
         BZ    NLST                NO - GET NEXT                                
         B     NLST06                                                           
*                                                                               
NLST04   TM    IOERR,IOEDEL        IT MUST BE DELETE=NO                         
         BO    NLST                GET NEXT                                     
*                                                                               
NLST06   MVC   THIS.LLSKEY(ACCKLEN),IOKEY  WE WANT THIS KEY HERE...             
         CLI   CSACT,A#DLOAD               DOWNLOADING?                         
         BNE   *+12                                                             
         CLI   DWNINDS,DWNGDATA            FIRST TIME READING RECORD?           
         BNE   EXITOK                      NO - SKIP CALLING DOFLT              
         GOTO1 ADOFLT                      FILTER UNWANT RECORDS                
         BE    EXITOK                                                           
         CLI   CSACT,A#DLOAD               DOWNLOADING?                         
         BNE   NLST                                                             
         MVI   DWNINDS,DWNNOALL            YES - READ THE NEXT RECORD?          
         MVI   DWNIND2,DWNNOAL2                                                 
         B     NLST                                                             
         DROP  THIS,LAST,X                                                      
         SPACE 2                                                                
***********************************************************************         
* GET TSAR RECORD INFORMATION FROM DIRECTORY RECORD                   *         
* NOTE - FOR THE LIST SCREEN THE RECORD LENGTH DOES NOT NEED TO BE AS *         
*        BIG AS TLLNQ. THIS WAS CAUSING THE TSAR BUFFER TO FILL UP FOR*         
*        GREY/GHG DUE TO THE AMOUNT OF RECORDS.  THE MINIMUM LENGTH   *         
*        CAN BE THE KEY(TLKEY) PLUS 2 SO THAT IS WHAT IS USED FOR THE *         
*        LIST SCREEN                                                  *         
** DECIDED NOT TO DO THIS.  TSAR BUFFER PAGES WERE INCREASED INSTEAD  *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R2                                                       
         USING TLSTD,R3                                                         
TSARDIR  LM    R2,R3,SVPARMS3                                                   
         CLI   CSACT,A#DLOAD                                                    
         BE    TSARD02                                                          
         MVC   TLRLEN,=AL2(TLLNQ)  SET LENGTH OF TSAR RECORD                    
         MVC   TLKGRPC,LLSKGRP                                                  
         MVC   TLKPID,SVPID                                                     
         B     EXITOK                                                           
*                                                                               
TSARD02  TM    DWNINDS,DWNGDATA    GET DATA FROM RECORD                         
         BZ    TSARD04             NO - PUT DATA INTO TSAR                      
         L     R1,=AL4(XOGET+XOACCMST+XIO11)                                    
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
         L     R2,AIOREC                                                        
*                                                                               
         XC    MNTDISPD(MNTDISPL),MNTDISPD    CHECK THAT ON THIS RECORD         
         GOTO1 AGETNLE,BOPARM,('LIDTCPJL',MNTCPJ) THERE IS CPJ.....             
         BE    *+8                                                              
         OI    DWNINDS,DWNNOCPJ                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTEXPL',MNTEXPD) EXP. TYPE CODE               
         BE    *+8                                                              
         OI    DWNINDS,DWNNOETC                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTWCL',MNTWC)    WORK CODE                    
         BE    *+8                                                              
         OI    DWNINDS,DWNNOWC                                                  
         GOTO1 AGETNLE,BOPARM,('LIDTNCLL',MNTNCLC) NON CLIENT CODE              
         BE    *+8                                                              
         OI    DWNINDS,DWNNONCC                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTMEDL',MNTMED)  MEDIA CODE                   
         BE    *+8                                                              
         OI    DWNINDS,DWNNOMC                                                  
         GOTO1 AGETNLE,BOPARM,('LIDT1RAC',MNTAP1R) 1R ACCOUNT CODE              
         BE    *+8                                                              
         OI    DWNINDS,DWNNOC1R                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTSCRB',MNTSCRC) SCRIBE FORMAT CODE           
         BE    *+8                                                              
         OI    DWNINDS,DWNNOSCR                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTESCH',MNTESCH) SCHEME CODE                  
         BE    *+8                                                              
         OI    DWNIND2,DWNNOSCH                                                 
         GOTO1 AGETNLE,BOPARM,('LIDTSUPP',MNTSUPP) SUPPLIER CODE                
         BE    *+8                                                              
         OI    DWNIND2,DWNNOSUP                                                 
*                                                                               
TSARD04  NI    DWNINDS,FF-DWNGDATA                                              
         NI    DWNIND2,FF-DWNGDAT2                                              
         L     R2,AIOREC           ADDRESS OF LLSRECD RECORD                    
         MVC   TLRLEN,=AL2(TLDLLNQ) LENGTH OF TSAR RECORD FOR DOWNLOAD          
         MVC   TLKPID(L'TLKPID),SVPID                                           
         XR    RF,RF                                                            
         LHI   RF,TLDLDATL                                                      
         SHI   RF,256                                                           
*                                                                               
         XC    TLDLDAT(RF),TLDLDAT                                              
*                                                                               
         USING RSTELD,RF                                                        
         LA    RF,LLSRFST-LLSRECD(R2)                                           
TSARD05A CLI   RSTEL,0                                                          
         BE    TSARD05X                                                         
         CLI   RSTEL,RSTELQ                                                     
         BE    TSARD05B                                                         
         LLC   R0,RSTLN                                                         
         AR    RF,R0                                                            
         B     TSARD05A                                                         
TSARD05B CLI   RSTLN,RSTLN3Q                                                    
         BL    TSARD05X                                                         
         MVC   TLDDEFAD(L'RSTACST1+L'RSTACST2),RSTACST1                         
TSARD05X DS    0H                                                               
         DROP  RF                                                               
*                                                                               
         TM    DWNINDS,DWNNOCPJ                                                 
         BO    TSARD06                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTCPJL',MNTCPJ) CPJ CODE                      
         BE    *+8                                                              
         OI    DWNINDS,DWNNOCPJ                                                 
         MVC   TLDCPJ,MYWORK+2                                                  
         OI    TLDCPJ,X'40'        CAPITALISE FIRST CHARACTER                   
         MVC   TLDCPJAP,MYWORK                                                  
         MVC   TLDCPJOF,MYWORK+2+(LIDLOFF-LIDLACT)                              
*                                                                               
TSARD06  TM    DWNINDS,DWNNOETC                                                 
         BO    TSARD08                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTEXPL',MNTEXPD) EXTY CODE                    
         BE    *+8                                                              
         OI    DWNINDS,DWNNOETC                                                 
         MVC   TLDEXP,MYWORK+2                                                  
         OI    TLDEXP,X'40'        CAPITALISE FIRST CHARACTER                   
         MVC   TLDEXPAP,MYWORK                                                  
*                                                                               
TSARD08  TM    DWNINDS,DWNNOWC                                                  
         BO    TSARD10                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTWCL',MNTWC)    WORK CODE                    
         BE    *+8                                                              
         OI    DWNINDS,DWNNOWC                                                  
         MVC   TLDWC,MYWORK+2                                                   
         OI    TLDWC,X'40'         CAPITALISE FIRST CHARACTER                   
         MVC   TLDWCDAP,MYWORK                                                  
*                                                                               
TSARD10  TM    DWNINDS,DWNNONCC                                                 
         BO    TSARD12                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTNCLL',MNTNCLC) NON CLIENT CODE              
         BE    *+8                                                              
         OI    DWNINDS,DWNNONCC                                                 
         MVC   TLDNCC,MYWORK+2                                                  
         OI    TLDNCC,X'40'        CAPITALISE FIRST CHARACTER                   
         MVC   TLDNCCAP,MYWORK                                                  
*                                                                               
TSARD12  TM    DWNINDS,DWNNOMC                                                  
         BO    TSARD14                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTMEDL',MNTMED)  MEDIA CODE                   
         BE    *+8                                                              
         OI    DWNINDS,DWNNOMC                                                  
         MVC   TLDMED,MYWORK+2                                                  
         OI    TLDMED,X'40'        CAPITALISE FIRST CHARACTER                   
         MVC   TLDMEDAP,MYWORK                                                  
*                                                                               
TSARD14  TM    DWNINDS,DWNNOC1R                                                 
         BO    TSARD18                                                          
         GOTO1 AGETLIT,BOPARM,('LIDT1RAC',MNTAP1R) 1R ACCOUNT CODE              
         BE    *+8                                                              
         OI    DWNINDS,DWNNOC1R                                                 
         MVC   TLD1RC,MYWORK+2                                                  
         OI    TLD1RC,X'40'        CAPITALISE FIRST CHARACTER                   
         MVC   TLD1RCAP,MYWORK                                                  
*                                                                               
TSARD18  TM    DWNINDS,DWNNOSCR                                                 
         BO    TSARD20                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTSCRB',MNTSCRC) SCRIBE FORMAT CODE           
         BE    *+8                                                              
         OI    DWNINDS,DWNNOSCR                                                 
         MVC   TLQFC,MYWORK+2                                                   
         OI    TLQFC,X'40'         CAPITALISE FIRST CHARACTER                   
         MVC   TLDFCDAP,MYWORK                                                  
*                                                                               
TSARD20  TM    DWNIND2,DWNNOSUP                                                 
         BO    TSARD22                                                          
         GOTO1 AGETLIT,BOPARM,('LIDTSUPP',MNTSUPP) SUPPLIER CODE                
         BE    *+8                                                              
         OI    DWNIND2,DWNNOSUP                                                 
         MVC   TLDSUC,MYWORK+2                                                  
         MVC   TLDSUCAP,MYWORK                                                  
*                                                                               
TSARD22  TM    DWNIND2,DWNNOSCH                                                 
         BO    EXITOK                                                           
         GOTO1 AGETLIT,BOPARM,('LIDTESCH',MNTESCH) SCHEME CODE                  
         BE    *+8                                                              
         OI    DWNIND2,DWNNOSCH                                                 
         MVC   TLDSCHCD,MYWORK+2                                                
         OI    TLDSCHCD,X'40'      CAPITALISE FIRST CHARACTER                   
         MVC   TLDSCHAP,MYWORK                                                  
         B     EXITOK                                                           
         DROP  R2,R3                                                            
***********************************************************************         
* INITIALISE FOR LIST 1/2/3/4/5/6/7/8/9                               *         
***********************************************************************         
         SPACE 1                                                                
INITL1   OI    LSSTAT1,LSSTSAR+LSSBALL                                          
         OI    LSSTAT2,LSSADD+LSSNOSEQ                                          
         NI    CPJINDS,X'FF'-(CPJIVCPJ+CPJINCLI+CPJICCPJ)                       
         MVC   LSCOLLIN,=AL2(240)  NUMBER OF COLUMNS PER LIST LINE              
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST 1/2/3/4/5/6/7/8/9                               *         
***********************************************************************         
         SPACE 1                                                                
FTFLST1  LA    RF,LLSRFST-LLSRECD                                               
         STH   RF,MNTDISP                                                       
         MVI   READSEQ#,0                                                       
*&&US*&& NI    ERRIND,FF-EREOFIN         RESET ERROR INDICATOR                  
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST 1/2/3/4/5/6/7/8/9                                    *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R5                                                       
FLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC           A(RECORD)                                    
         CLI   READSEQ#,0          FIRST XDATA RECORD?                          
         BE    *+8                                                              
         L     R1,AIO5                                                          
*                                                                               
         AR    R5,R1               A(RECORD)                                    
         CR    R5,R1               MAKE SURE MNTDISP INITIALISED                
         BH    *+8                                                              
         LA    R5,LLSRFST          IT IS NOW.                                   
         XR    RE,RE                                                            
*                                                                               
         USING LIDELD,R5                                                        
FML00    CLI   LIDEL,0             RECORD END?                                  
         BNE   FML01                                                            
         GOTO1 AREADNXT            READ NEXT LLS RECORD                         
         BNE   EXITL               NO MORE RECORD                               
         LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIO5             NEW RECORD IN AIO5                           
         XR    RE,RE                                                            
*                                                                               
FML01    CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               NO                                           
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   FML02               NO                                           
         CLI   LIDTYPE,LIDTCPJL    IS IT CLI/PRO/JOB LIST                       
         BE    FML10                                                            
         B     NML18                                                            
FML02    CLI   GSSMPAGE,2          EXPENDITURE TYPE PAGE                        
         BNE   FML03               NO                                           
         CLI   LIDTYPE,LIDTEXPL    IS IT EXPENDITURE LIST                       
         BE    FML10                                                            
         B     NML18                                                            
FML03    CLI   GSSMPAGE,3          ARE WE ON WORKCODE PAGE                      
         BNE   FML04               NO                                           
         CLI   LIDTYPE,LIDTWCL     IS IT WORKCODE LIST                          
         BE    FML10                                                            
         B     NML18                                                            
FML04    CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   FML05               NO                                           
         CLI   LIDTYPE,LIDTNCLL    IS IT NON CLIENT ACCOUNT LIST                
         BE    FML10                                                            
         B     NML18                                                            
FML05    CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   FML06                                                            
         CLI   LIDTYPE,LIDTMEDL    IS IT MEDIA LIST                             
         BE    FML10                                                            
         B     NML18                                                            
FML06    CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE                    
         BNE   FML07                                                            
         CLI   LIDTYPE,LIDT1RAC    IS IT 1R ACCOUNT LIST                        
         BE    FML10                                                            
         B     NML18                                                            
FML07    CLI   GSSMPAGE,7          ARE WE ON SCRIBE FORMAT PAGE                 
         BNE   FML08                                                            
         CLI   LIDTYPE,LIDTSCRB    IS IT SCRIBE FORMAT LIST                     
         BE    FML10                                                            
         B     NML18                                                            
FML08    CLI   GSSMPAGE,8          ARE WE ON ESTIMATE SCHEME PAGE               
         BNE   FML09                                                            
         CLI   LIDTYPE,LIDTESCH    IS IT ESTIMATE SCHEME LIST                   
         BE    FML10                                                            
         B     NML18                                                            
FML09    CLI   GSSMPAGE,9          ARE WE ON SUPPLIER CODE PAGE                 
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTSUPP    IS IT SUPPLIER CODE LIST                     
         BNE   NML18                                                            
*                                                                               
FML10    LA    R4,LIDDATA          ADDRESS OF DATA START OF ELEMENT             
         SR    R4,R5               MINUS ELEMENT START ADDRESS                  
         STH   R4,CURDISP          CURRENT DISPLACEMENT INTO ELEMENT            
         XR    R4,R4                                                            
         IC    R4,LIDITLN          GET LENGTH OF DATA                           
         STH   R4,DATALEN          SAVE THIS LENGTH                             
         IC    R4,LIDLN                                                         
         STH   R4,TOTELLN          SAVE TOTAL ELEMENT LENGTH                    
         LA    R4,LIDDATA+L'LIDLAPPL+L'LIDLAPP2                                 
         MVC   CURAPPLS,LIDLAPPL                                                
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   FML11               NO                                           
         L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
         B     FML20                                                            
FML11    CLI   GSSMPAGE,2          EXPENDITURE TYPE PAGE                        
         BNE   FML12               NO                                           
         L     RF,AVALEXP          SET TO VALIDATE EXPENDITURE TYPE             
         B     FML20                                                            
FML12    CLI   GSSMPAGE,3          ARE WE ON WORKCODE PAGE                      
         BNE   FML13               NO                                           
         L     RF,AVALWC           SET TO VALIDATE WORKCODE                     
         B     FML20                                                            
FML13    CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   FML14               NO                                           
         L     RF,AVALNCL          SET TO VALIDATE NON CLIENT ACCOUNT           
         B     FML20                                                            
FML14    CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   FML15                                                            
         L     RF,AVALMED          SET TO VALIDATE MEDIA                        
         B     FML20                                                            
FML15    CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE                    
         BNE   FML16                                                            
         L     RF,AVALC1R          SET TO VALIDATE 1R ACCOUNT CODE              
         B     FML20                                                            
FML16    CLI   GSSMPAGE,7          ARE WE ON SCRIBE FORMAT PAGE                 
         BNE   FML17                                                            
         L     RF,AVALFORM         SET TO VALIDATE FORMAT CODE                  
         B     FML20                                                            
FML17    CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE PAGE?                  
         BNE   FML18                                                            
         L     RF,AVALSCH          SET TO VALIDATE SCHEME CODE PAGE             
         B     FML20                                                            
FML18    CLI   GSSMPAGE,9          ARE WE ON SUPPLIER PAGE?                     
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AVALSUP          SET TO VALIDATE SUPPLIER CODE                
FML20    LLC   R1,LIDITLN                                                       
         SHI   R1,L'LIDLAPPL+L'LIDLAPP2                                         
         STC   R1,MYBYTE                                                        
         GOTO1 (RF),BOPARM,(MYBYTE,(R4))                                        
         BNE   NML20               NOT VALID CLI/PRO/JOB CODE                   
*&&US                                                                           
         TM    ERRIND,ERCOFIN      IS OFFICE CODE VALID                         
         BNO   FML22                                                            
         NI    ERRIND,X'FF'-ERCOFIN                                             
         OI    ERRIND,EREOFIN      EXISTING OFFICE IS INVALID                   
*&&                                                                             
FML22    L     R1,AIOREC                                                        
         CLI   READSEQ#,0                                                       
         BE    *+8                                                              
         L     R1,AIO5                                                          
         SR    R5,R1                                                            
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST 1/2/3/4/5/6/7/8/9                                     *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R5                                                       
NLST1    LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         L     R1,AIOREC                                                        
         CLI   READSEQ#,0                                                       
         BE    *+8                                                              
         L     R1,AIO5                                                          
         AR    R5,R1               A(RECORD)                                    
         CR    R5,R1               MAKE SURE MNTDISP INITIALISED                
         BH    NML20                                                            
         LA    R5,LLSRFST          IT IS NOW.                                   
*                                                                               
         USING LIDELD,R5                                                        
NML02    CLI   LIDEL,0             RECORD END?                                  
         BNE   NML03               YES                                          
         GOTO1 AREADNXT            READ NEXT LLS RECORD                         
         BNE   EXITL               NO MORE RECORD                               
         LH    R5,MNTDISP          CURRENT DISPLACEMENT INTO RECORD             
         A     R5,AIO5             NEW RECORD IN AIO5                           
         XR    RE,RE                                                            
*                                                                               
NML03    CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   NML18               YES                                          
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML04               NO                                           
         CLI   LIDTYPE,LIDTCPJL    IS IT CLI/PRO/JOB LIST                       
         BE    NML30                                                            
         B     NML18                                                            
NML04    CLI   GSSMPAGE,2          EXPENDITURE TYPE PAGE                        
         BNE   NML06               NO                                           
         CLI   LIDTYPE,LIDTEXPL    IS IT EXPENDITURE LIST                       
         BE    NML30                                                            
         B     NML18                                                            
NML06    CLI   GSSMPAGE,3          ARE WE ON WORKCODE PAGE                      
         BNE   NML08               NO                                           
         CLI   LIDTYPE,LIDTWCL     IS IT WORKCODE LIST                          
         BE    NML30                                                            
         B     NML18                                                            
NML08    CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   NML10               NO                                           
         CLI   LIDTYPE,LIDTNCLL    IS IT NON CLIENT ACCOUNT LIST                
         BE    NML30                                                            
         B     NML18                                                            
NML10    CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   NML12               NO                                           
         CLI   LIDTYPE,LIDTMEDL    IS IT MEDIA LIST                             
         BE    NML30                                                            
         B     NML18                                                            
NML12    CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE                    
         BNE   NML13               NO                                           
         CLI   LIDTYPE,LIDT1RAC    IS IT 1R ACCOUNT LIST                        
         BE    NML30                                                            
         B     NML18                                                            
NML13    CLI   GSSMPAGE,7          ARE WE ON SCRIBE FORMAT PAGE                 
         BNE   NML14               YES                                          
         CLI   LIDTYPE,LIDTSCRB    IS IT SCRIBE FORMAT LIST                     
         BE    NML30                                                            
         B     NML18                                                            
NML14    CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE PAGE                   
         BNE   NML15               YES                                          
         CLI   LIDTYPE,LIDTESCH    IS IT SCHEME CODE FORMAT LIST                
         BE    NML30                                                            
         B     NML18                                                            
NML15    CLI   GSSMPAGE,9          ARE WE ON SUPPLIER PAGE                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   LIDTYPE,LIDTSUPP    IS IT SUPPLIER LIST                          
         BE    NML30                                                            
*                                                                               
NML18    XR    RE,RE                                                            
         IC    RE,LIDLN            GET NEXT ELEMENT                             
         AR    R5,RE                                                            
         B     NML02                                                            
*                                                                               
NML20    LH    R4,DATALEN          LENGTH OF DATA                               
         AH    R4,CURDISP          CURRENT DISPLACEMENT TO DATA                 
         STH   R4,CURDISP          SAVE NEXT DISPLACEMENT TO DATA               
         CH    R4,TOTELLN          HAVE WE REACHED END OF ELEMENT               
         BNL   NML18               YES                                          
         AR    R4,R5                                                            
         MVC   CURAPPLS,0(R4)                                                   
         AHI   R4,L'LIDLAPPL+L'LIDLAPP2                                         
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML21               NO                                           
         L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
         B     NML28                                                            
NML21    CLI   GSSMPAGE,2          EXPENDITURE TYPE PAGE                        
         BNE   NML22               NO                                           
         L     RF,AVALEXP          SET TO VALIDATE EXPENDITURE TYPE             
         B     NML28                                                            
NML22    CLI   GSSMPAGE,3          ARE WE ON WORKCODE PAGE                      
         BNE   NML23               NO                                           
         L     RF,AVALWC           SET TO VALIDATE WORKCODE                     
         B     NML28                                                            
NML23    CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   NML24               NO                                           
         L     RF,AVALNCL          SET TO VALIDATE NON CLIENT ACCOUNT           
         B     NML28                                                            
NML24    CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   NML25               NO                                           
         L     RF,AVALMED          SET TO VALIDATE MEDIA                        
         B     NML28                                                            
NML25    CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE                    
         BNE   NML26               NO                                           
         L     RF,AVALC1R          SET TO VALIDATE 1R ACCOUNT CODE              
         B     NML28                                                            
NML26    CLI   GSSMPAGE,7          ARE WE ON SCRIBE ACCOUNT PAGE                
         BNE   NML26B              YES                                          
         L     RF,AVALFORM         SET TO VALIDATE FORMAT CODE                  
         B     NML28                                                            
NML26B   CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE PAGE                   
         BNE   NML27               YES                                          
         L     RF,AVALSCH          SET TO VALIDATE SCHEME CODE                  
         B     NML28                                                            
NML27    CLI   GSSMPAGE,9          ARE WE ON SUPPLIER CODE PAGE                 
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,AVALSUP          SET TO VALIDATE SUPPLIER CODE                
NML28    LLC   R1,LIDITLN                                                       
         SHI   R1,L'LIDLAPPL+L'LIDLAPP2                                         
         STC   R1,MYBYTE                                                        
         GOTO1 (RF),BOPARM,(MYBYTE,(R4))                                        
         BE    NML40               VALID CLI/PRO/JOB CODE                       
         B     NML20               INVALID CLI/PRO/JOB CODE                     
*                                                                               
NML30    LA    R4,LIDDATA          ADDRESS OF DATA START OF ELEMENT             
         SR    R4,R5                                                            
         STH   R4,CURDISP          CURRENT DISPLACEMENT INTO ELEMENT            
         XR    R4,R4                                                            
         IC    R4,LIDITLN          GET LENGTH OF DATA                           
         STH   R4,DATALEN          SAVE THIS LENGTH                             
         IC    R4,LIDLN                                                         
         STH   R4,TOTELLN          SAVE TOTAL ELEMENT LENGTH                    
         LA    R4,LIDDATA+L'LIDLAPPL+L'LIDLAPP2                                 
         MVC   CURAPPLS,LIDLAPPL                                                
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   NML31               NO                                           
         L     RF,AVALCPJ          SET TO VALIDATE CLIENT PROD JOB              
         B     NML38                                                            
NML31    CLI   GSSMPAGE,2          EXPENDITURE TYPE PAGE                        
         BNE   NML32               NO                                           
         L     RF,AVALEXP          SET TO VALIDATE EXPENDITURE TYPE             
         B     NML38                                                            
NML32    CLI   GSSMPAGE,3          ARE WE ON WORKCODE PAGE                      
         BNE   NML33               NO                                           
         L     RF,AVALWC           SET TO VALIDATE WORKCODE                     
         B     NML38                                                            
NML33    CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   NML34               NO                                           
         L     RF,AVALNCL          SET TO VALIDATE NON CLIENT ACCOUNT           
         B     NML38                                                            
NML34    CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   NML35               NO                                           
         L     RF,AVALMED          SET TO VALIDATE MEDIA CODE                   
         B     NML38                                                            
NML35    CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE                    
         BNE   NML36               NO                                           
         L     RF,AVALC1R          SET TO VALIDATE 1R ACCOUNT CODE              
         B     NML38                                                            
NML36    CLI   GSSMPAGE,7          ARE WE ON SCRIBE FORMAT PAGE                 
         BNE   NML36B              YES                                          
         L     RF,AVALFORM         SET TO VALIDATE FORMAT CODE                  
         B     NML38                                                            
NML36B   CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE PAGE                   
         BNE   NML37               YES                                          
         L     RF,AVALSCH          SET TO VALIDATE FORMAT CODE                  
         B     NML38                                                            
NML37    CLI   GSSMPAGE,9          ARE WE ON SUPPLIER CODE PAGE                 
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         L     RF,AVALSUP          SET TO VALIDATE FORMAT CODE                  
NML38    LLC   R1,LIDITLN                                                       
         SHI   R1,L'LIDLAPPL+L'LIDLAPP2                                         
         STC   R1,MYBYTE                                                        
         GOTO1 (RF),BOPARM,(MYBYTE,(R4))                                        
         BNE   NML20               NOT VALID CLI/PRO/JOB CODE                   
                                                                                
NML40    DS    0H                                                               
*&&US                                                                           
         TM    ERRIND,ERCOFIN      IS OFFICE CODE VALID                         
         BNO   *+12                                                             
         NI    ERRIND,X'FF'-ERCOFIN                                             
         OI    ERRIND,EREOFIN      EXISTING OFFICE IS INVALID                   
*&&                                                                             
         L     R1,AIOREC                                                        
         CLI   READSEQ#,0                                                       
         BE    *+8                                                              
         L     R1,AIO5                                                          
         SR    R5,R1                                                            
         STH   R5,MNTDISP                                                       
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT ,                                                                
***********************************************************************         
* SET UP TSAR FROM FILE 1                                             *         
***********************************************************************         
         SPACE 1                                                                
TSARFIL1 L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         MVC   TLRLEN,=AL2(TLLNQ)                                               
         MVC   TLKAPPLS,CURAPPLS                                                
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   TSFL02              NO                                           
         MVC   TLKCPJC,CPJCODE                                                  
         MVC   TLKCPJNM,CPJNAME                                                 
         MVC   TLKCPJPO,CPJOFF                                                  
         B     EXITOK                                                           
*                                                                               
TSFL02   CLI   GSSMPAGE,2                                                       
         BNE   TSFL04                                                           
         MVC   TLKEXPC,EXPCODE                                                  
         MVC   TLKEXPNM,EXPNAME                                                 
         B     EXITOK                                                           
*                                                                               
TSFL04   CLI   GSSMPAGE,3                                                       
         BNE   TSFL06                                                           
         MVC   TLKWCC,WRKCODE                                                   
         MVC   TLKWCDES,WRKDESC                                                 
         MVC   TLKWCNM,WRKNAME                                                  
         B     EXITOK                                                           
*                                                                               
TSFL06   CLI   GSSMPAGE,4                                                       
         BNE   TSFL08                                                           
         MVC   TLKNCLC,NCLCODE                                                  
         MVC   TLKNCLNM,NCLNAME                                                 
         B     EXITOK                                                           
*                                                                               
TSFL08   CLI   GSSMPAGE,5                                                       
         BNE   TSFL10                                                           
         MVC   TLKMEDC,MEDCODE                                                  
         MVC   TLKMEDNM,MEDNAME                                                 
         B     EXITOK                                                           
*                                                                               
TSFL10   CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE                    
         BNE   TSFL12                                                           
         MVC   TLK1RAC,C1RCODE                                                  
         MVC   TLKC1RNM,C1RNAME                                                 
         B     EXITOK                                                           
*                                                                               
TSFL12   CLI   GSSMPAGE,7          ARE WE ON SCRIBE FORMAT PAGE                 
         BNE   TSFL14                                                           
         MVC   TLKFORM,FORMCODE                                                 
         MVC   TLKFRNM,FORMNAME                                                 
         MVC   TLKRPTY,REPCODE                                                  
         MVC   TLKTRAN,TRANTYPE                                                 
         B     EXITOK                                                           
*                                                                               
TSFL14   CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE PAGE                   
         BNE   TSFL16                                                           
         MVC   TLKSCHCD,SCHCODE                                                 
         MVC   TLKSCHNM,SCHENAM                                                 
         B     EXITOK                                                           
*                                                                               
TSFL16   CLI   GSSMPAGE,9          ARE WE ON SUPPLIER CODE PAGE                 
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TLKSUC,SUPCODE                                                   
         MVC   TLSUPNM,SUPNAME                                                  
         B     EXITOK                                                           
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* LAST FOR LIST SCREEN PAGE 1/6                                       *         
***********************************************************************         
         SPACE 1                                                                
*&&US                                                                           
SCRLAST1 DS    0H                  MAX IOS SET FROM NLST?                       
         TM    ERRIND,EREOFIN         WE HAVE AN INVALID OFF ON LIST.           
         BNO   EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$WAOOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         OI    GCINDS3,GCIDSMSG    SET OWN DISPLAY MESSAGE                      
         MVI   FVOSYS,QSACC                                                     
         CLI   CSACT,A#DIS                                                      
         BE    EXITOK                                                           
         MVC   FVMSGNO,=AL2(AE$CMCOL)  ACCOUNT OUTSIDE OFF LIMITATIONS          
         B     EXITL                                                            
*&&                                                                             
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR UPDATE PAGE 1/2/3/4/5/6/7/8/9                        *         
***********************************************************************         
         SPACE 1                                                                
UPDFRST1 MVI   ANYLINES,NO                                                      
         CLI   CSACT,A#CHA         ONLY DELETE ELEMENTS IF WE HAVE              
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         MVC   ACURIO,AIOREC                                                    
         LH    RF,LSLINE#          CURRENT LINE NUMBER                          
         SH    RF,LSLST#1                                                       
         CHI   RF,MAXITMS          MAX OF 100 ITEMS                             
         BNH   UPDF00                                                           
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     EXITL               TOO MANY LINES IN LIST                       
*                                                                               
UPDF00   DS    0H                                                               
*&&UK*&& GOTO1 ADELPAS,BOPARM,ACURIO  DELETE PASSIVE POINTERS                   
                                                                                
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   UPDF02              NO                                           
         XC    LASTCODE,LASTCODE                                                
         LA    RF,=AL1(LIDLLN9Q,LIDTCPJL)                                       
         B     UPDF20                                                           
*                                                                               
UPDF02   CLI   GSSMPAGE,2          EXPENDITURE TYPE PAGE                        
         BNE   UPDF04              NO                                           
         LA    RF,=AL1(LIDLLN4Q,LIDTEXPL)                                       
         B     UPDF20                                                           
*                                                                               
UPDF04   CLI   GSSMPAGE,3          ARE WE ON WORKCODE PAGE                      
         BNE   UPDF06              NO                                           
         LA    RF,=AL1(LIDLLN2Q,LIDTWCL)                                        
         B     UPDF20                                                           
*                                                                               
UPDF06   CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   UPDF08              NO                                           
         LA    RF,=AL1(LIDLLN1Q,LIDTNCLL)                                       
         B     UPDF20                                                           
*                                                                               
UPDF08   CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   UPDF10              NO                                           
         LA    RF,=AL1(LIDLLN3Q,LIDTMEDL)                                       
         B     UPDF20                                                           
*                                                                               
UPDF10   CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE                    
         BNE   UPDF12                                                           
         XC    LASTCODE,LASTCODE                                                
         LA    RF,=AL1(LIDLLN1Q,LIDT1RAC)                                       
         B     UPDF20                                                           
*                                                                               
UPDF12   CLI   GSSMPAGE,7          ARE WE ON SCRIBE FORMAT PAGE                 
         BNE   UPDF14                                                           
         LA    RF,=AL1(LIDLLN5Q,LIDTSCRB)                                       
         B     UPDF20                                                           
*                                                                               
UPDF14   CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE PAGE                   
         BNE   UPDF16                                                           
         LA    RF,=AL1(LIDLLN5Q,LIDTESCH)                                       
         B     UPDF20                                                           
*                                                                               
UPDF16   CLI   GSSMPAGE,9          ARE WE ON SUPPLIER CODE PAGE                 
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    LASTCODE,LASTCODE                                                
         LA    RF,=AL1(LIDLLN8Q,LIDTSUPP)                                       
*                                                                               
UPDF20   GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),('LIDELQ',ACURIO),        X        
               (2,(RF))                                                         
                                                                                
T        USING LLSRECD,R4                                                       
         LA    R4,IOKEY            READ NEXT 'SEQUENCE' RECORD                  
         L     RF,ACURIO                                                        
         CLI   LLSKSEQ-LLSRECD(RF),0                                            
         BNE   UPDF22                                                           
         MVC   IOKEY(L'LLSKEY),0(RF)                                            
         MVC   ACURIO,AIO2                                                      
         B     UPDF26                                                           
*                                                                               
UPDF22   DS    0H                  UPDATE ANY PREVIOUS RECORD (EXCEPT           
         CLI   LLSRFST-LLSRECD(RF),0                                            
         BNE   UPDF24              MAIN RECORD) - AND DELETE IF EMPTY           
*                                                                               
         OI    LLSRSTAT-LLSRECD(RF),LLSSDELT                                    
         OI    T.LLSKSTAT,LLSSDELT                                              
         L     R1,=AL4(XOWRITE+XOACCDIR+XIO2)                                   
         GOTOR AIO                                                              
         BE    UPDF24                                                           
         DC    H'0'                BAD DIR RECORD                               
*                                                                               
UPDF24   L     R1,=AL4(XOPUTREC+XOACCMST+XIO2)                                  
         GOTOR AIO                                                              
         BE    UPDF26                                                           
         DC    H'0'                                                             
*                                                                               
UPDF26   LLC   RF,T.LLSKSEQ                                                     
         AHI   RF,1                                                             
         STC   RF,T.LLSKSEQ                                                     
         MVC   ACURIO,AIO2                                                      
         L     R1,=AL4(XOHIUPD+XOACCDIR+XIO2)                                   
         GOTOR AIO                                                              
         CLC   T.LLSKEY(LLSKSEQ-LLSRECD),IOKEYSAV                               
         BNE   EXITOK              EXIT - NOT SUB-RECORD                        
         TM    IOERR,FF-IOEDEL                                                  
         BZ    UPDF28                                                           
         DC    H'0'                ERROR READING THE RECORD                     
*                                                                               
UPDF28   L     R1,=AL4(XOGETRUP+XOACCMST+XIO2)                                  
         GOTOR AIO                                                              
         BE    UPDF00                                                           
         DC    H'0'                BAD MASTER RECORD                            
         DROP  T                                                                
         SPACE 2                                                                
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD 1/2/3/4/5/6/7/8/9                      *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R2                                                       
         USING TLSTD,R3                                                         
UPDREC1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
         MVI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         LM    R2,R3,SVPARMS3                                                   
         B     EXITOK                                                           
         DROP  R2,R3                                                            
         EJECT ,                                                                
***********************************************************************         
* LAST TIME FOR UPDATE 1                                              *         
* P3 = A (FILE RECORD)                                                *         
* P4 = A (TSAR RECORD)                                                *         
***********************************************************************         
         SPACE 1                                                                
UPDLST1  CLI   CSACT,A#CHA         ONLY ADD ELEMENT IF WE HAVE                  
         BE    *+12                A MAIN ACTION OF CHANGE                      
         CLI   CSACT,A#ADD         TREAT ADD AS CHANGE                          
         BNE   EXITOK                                                           
*                                                                               
         MVI   ANYERROR,NO                                                      
         CLI   ANYLINES,YES        WE HAVE AT LEAST ONE INPUT LINE              
         BNE   EXITOK                                                           
*                                                                               
T        USING LIDELD,BOELEM                                                    
         USING LLSRECD,R2                                                       
         L     R2,AIOREC                                                        
*                                                                               
ULST104  XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   ULST106             NO                                           
         MVI   T.LIDTYPE,LIDTCPJL                                               
         MVI   T.LIDITLN,LIDLLN9Q                                               
         MVI   MYBYTE3,L'LIDLACT                                                
         B     ULST122                                                          
ULST106  CLI   GSSMPAGE,2          ARE WE ON EXPENDITURE TYPE PAGE              
         BNE   ULST108             NO                                           
         MVI   T.LIDTYPE,LIDTEXPL                                               
         MVI   T.LIDITLN,LIDLLN4Q                                               
         MVI   MYBYTE3,L'LIDLETY                                                
         B     ULST122                                                          
ULST108  CLI   GSSMPAGE,3          ARE WE ON WORK CODE PAGE                     
         BNE   ULST110             NO                                           
         MVI   T.LIDTYPE,LIDTWCL                                                
         MVI   T.LIDITLN,LIDLLN2Q                                               
         MVI   MYBYTE3,L'LIDLWC                                                 
         B     ULST122                                                          
ULST110  CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   ULST112             NO                                           
         MVI   T.LIDTYPE,LIDTNCLL                                               
         MVI   T.LIDITLN,LIDLLN1Q                                               
         MVI   MYBYTE3,L'LIDLACT                                                
         B     ULST122                                                          
ULST112  CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   ULST114             NO                                           
         MVI   T.LIDTYPE,LIDTMEDL                                               
         MVI   T.LIDITLN,LIDLLN3Q                                               
         MVI   MYBYTE3,L'LIDLMED                                                
         B     ULST122                                                          
ULST114  CLI   GSSMPAGE,6          ARE WE ON 1R PAGE                            
         BNE   ULST116             YES                                          
         MVI   T.LIDTYPE,LIDT1RAC                                               
         MVI   T.LIDITLN,LIDLLN1Q                                               
         MVI   MYBYTE3,L'LIDLACT                                                
         B     ULST122                                                          
ULST116  CLI   GSSMPAGE,7          ARE WE ON SCRIBE PAGE                        
         BNE   ULST118             YES                                          
         MVI   T.LIDTYPE,LIDTSCRB                                               
         MVI   T.LIDITLN,LIDLLN5Q                                               
         MVI   MYBYTE3,L'LIDLREP                                                
         B     ULST122                                                          
ULST118  CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE                        
         BNE   ULST120             YES                                          
         MVI   T.LIDTYPE,LIDTESCH                                               
         MVI   T.LIDITLN,LIDLLN5Q                                               
         MVI   MYBYTE3,L'LIDLSCH                                                
         B     ULST122                                                          
ULST120  CLI   GSSMPAGE,9          ARE WE ON SUPPLIER ACCOUNT PAGE              
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTSUPP                                               
         MVI   T.LIDITLN,LIDLLN8Q                                               
         MVI   MYBYTE3,L'LIDLSULA                                               
*                                                                               
SUB      USING LIDDATA,R4                                                       
ULST122  LA    R4,T.LIDDATA                                                     
         L     R3,ATLST                                                         
         USING TLSTD,R3                                                         
         XC    TLNUM,TLNUM                                                      
         XC    TLKEY,TLKEY         RESET KEY                                    
         MVC   TLKSES,SESNL        SET CURRENT NEST LEVEL                       
         LA    R1,TSARDH                                                        
         B     *+8                                                              
ULST124  LA    R1,TSANXT           DEAL WITH ALL DELETE REQUEST                 
         GOTOX ('TSARIO',AGROUTS),(R1)                                          
         BL    ULST166             END OF FILE                                  
         CLC   TLKSES,SESNL        CHECK NEST LEVEL                             
         BNE   ULST166             DONE ALL FOR THIS LEVEL                      
*                                                                               
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE?           
         BE    ULST126                                                          
         CLI   GSSMPAGE,6          ARE WE ON 1R ACCOUNT PAGE?                   
         BE    ULST126                                                          
         CLI   GSSMPAGE,9          ARE WE ON SUPPLIER PAGE?                     
         BE    ULST140                                                          
         B     ULST144                                                          
                                                                                
ULST126  OC    LASTCODE,LASTCODE                                                
         BZ    ULST136                                                          
*                                                                               
         LA    RE,L'LASTCODE       FIND ACTUAL LENGTH OF LASTCODE               
         LA    R5,LASTCODE+L'LASTCODE-1                                         
ULST128  CLI   0(R5),C' '                                                       
         BNE   ULST129                                                          
         SHI   R5,1                                                             
         BCT   RE,ULST128                                                       
         B     ULST130             (?)                                          
*                                                                               
*&&US                                                                           
ULST129  DS    0H                                                               
         LA    R1,L11RLEN          POINT TO 1R LEVELS                           
         CLI   GSSMPAGE,1          IS IT C/P/J PAGE ?                           
         BNE   *+8                                                              
         LA    R1,CLILEN           POINT TO SJ LEVELS                           
*                                                                               
ULST129A CLI   0(R1),X'0C'         ARE WE AT THE LAST LEVEL                     
         BNE   *+12                                                             
         LHI   RE,L'ACTKACT                                                     
         B     ULST130                                                          
*                                                                               
         LLC   RF,0(R1)            GET LEVEL'S LENGTH                           
         CR    RE,RF                                                            
         BNH   ULST129B                                                         
         LA    R1,1(R1)            POINT TO NEXT LEVEL                          
         B     ULST129A                                                         
*                                                                               
ULST129B LLC   RE,0(R1)            GET LEVEL'S LENGTH                           
*&&                                                                             
*                                                                               
*&&UK                                                                           
ULST129  LLC   RF,L31RLEN                                                       
         CR    RE,RF                                                            
         BNH   ULST130                                                          
         LHI   RE,L'ACTKACT                                                     
*&&                                                                             
*                                                                               
ULST130  SHI   RE,1                                                             
         CLI   GSSMPAGE,1                                                       
         BNE   ULST132                                                          
         EXCLC RE,LASTCODE,TLKCPJC    COMPARE LAST CLI/PRO/JOB CODE             
         BNE   ULST136                WITH CURRENT ONE                          
         CLC   LASTOFF,TLKCPJPO       CHECK SAME OFFICE                         
         BNE   ULST136                                                          
         B     ULST134                                                          
ULST132  EXCLC RE,LASTCODE,TLK1RAC    COMPARE LAST 1R ACCOUNT CODE              
         BNE   ULST136                WITH CURRENT ONE                          
ULST134  MVC   FVMSGNO,=AL2(AE$HLEXS) HIGHER OR LOWER LEVEL ACC. EXISTS         
         NI    LSLTIND1,FF-LSLTIBLD   REBUILD THE LIST                          
         XC    GCLASKEY,GCLASKEY      SET KEY HAS BEEN CHANGED                  
         NI    GSINDSL1,FF-GSIXMNT    TURN OF MAINT SCREEN LOADED FLAG          
         MVI   ANYERROR,YES           REMEMBER ERROR AND SKIP IT                
         B     ULST124                                                          
ULST136  CLI   GSSMPAGE,1             CHECK IF ON CLI/PRO/JOB PAGE              
         BNE   ULST138                                                          
         MVC   LASTCODE,TLKCPJC       MOVE CLI/PRO/JOB CODE TO LASTCODE         
         MVC   LASTOFF,TLKCPJPO       SAVE LAST OFFICE TOO!                     
         B     ULST144                                                          
ULST138  MVC   LASTCODE,TLK1RAC       MOVE 1R ACCOUNT CODE TO LASTCODE          
         B     ULST144                                                          
*                                     SUPPLIER CODE PAGE                        
ULST140  OC    LASTCODE,LASTCODE      ANYTHING IN LASTCODE?                     
         BZ    ULST142                                                          
         GOTO1 ACHKLVL,BOPARM,LASTCODE,TLKSUC CHECK HIGHER/LOWER                
         BE    ULST142                      LEVEL ACCOUNTS EXIST                
         MVI   ANYERROR,YES                                                     
         MVC   FVMSGNO,=AL2(AE$HLEXS) HIGHER OR LOWER LEVEL ACC. EXIST          
         NI    LSLTIND1,FF-LSLTIBLD                                             
         XC    GCLASKEY,GCLASKEY                                                
         NI    GSINDSL1,FF-GSIXMNT                                              
         B     ULST124                                                          
***      MVC   FVMSGNO,=AL2(AE$HLEXS) HIGHER OR LOWER LEVEL ACC. EXISTS         
***      B     ULST1ERR                                                         
*                                                                               
ULST142  MVC   LASTCODE(L'TLKSUC),TLKSUC MOVE SUPPLIER CODE TO LASTCODE         
*                                                                               
ULST144  MVC   SUB.LIDLAPPL,TLKAPPL1                                            
         MVC   SUB.LIDLAPP2,TLKAPPL2                                            
*                                                                               
         LLC   RE,MYBYTE3                                                       
         SHI   RE,1                                                             
         MVC   SUB.LIDLACT(0),TLKSUC                                            
         EX    RE,*-6                                                           
         LLC   RE,T.LIDITLN                                                     
         CLI   T.LIDTYPE,LIDTCPJL                                               
         BNE   ULST145                                                          
         MVC   SUB.LIDLOFF,TLKSUC+TLKCPJPO-TLKCPJC                              
*                                                                               
ULST145  AR    R4,RE               R4=NEXT DATA BLOCK ON ELEMENT                
         LR    R5,R4                                                            
         LA    RF,T.LIDEL                                                       
         SR    R5,RF               TOTAL DISPLACEMENT OF ELEMENT                
         CHI   R5,240                                                           
         BL    ULST124                                                          
         STC   R5,T.LIDLN                                                       
         GOTO1 AADDLLS,BOPARM,LLSRECD                                           
         BNE   EXITL                                                            
         CLI   ADDSEQ#,0                                                        
         BE    *+8                                                              
         L     R2,AIO5             NEW LLS RECORD                               
*                                                                               
ULST146  XC    BOELEM,BOELEM                                                    
         MVI   T.LIDEL,LIDELQ                                                   
         LA    R4,T.LIDDATA                                                     
         XR    R5,R5                                                            
         CLI   GSSMPAGE,1          ARE WE ON CLIENT PRODUCT JOB PAGE            
         BNE   ULST148             NO                                           
         MVI   T.LIDTYPE,LIDTCPJL                                               
         MVI   T.LIDITLN,LIDLLN9Q                                               
         MVI   MYBYTE3,L'LIDLACT                                                
         B     ULST124                                                          
ULST148  CLI   GSSMPAGE,2          ARE WE ON EXPENDITURE TYPE PAGE              
         BNE   ULST150             NO                                           
         MVI   T.LIDTYPE,LIDTEXPL                                               
         MVI   T.LIDITLN,LIDLLN4Q                                               
         MVI   MYBYTE3,L'LIDLETY                                                
         B     ULST124                                                          
ULST150  CLI   GSSMPAGE,3          ARE WE ON WORK CODE PAGE                     
         BNE   ULST152             NO                                           
         MVI   T.LIDTYPE,LIDTWCL                                                
         MVI   T.LIDITLN,LIDLLN2Q                                               
         MVI   MYBYTE3,L'LIDLWC                                                 
         B     ULST124                                                          
ULST152  CLI   GSSMPAGE,4          ARE WE ON NON CLIENT ACCOUNT PAGE            
         BNE   ULST154             NO                                           
         MVI   T.LIDTYPE,LIDTNCLL                                               
         MVI   T.LIDITLN,LIDLLN1Q                                               
         MVI   MYBYTE3,L'LIDLACT                                                
         B     ULST124                                                          
ULST154  CLI   GSSMPAGE,5          ARE WE ON MEDIA PAGE                         
         BNE   ULST156             NO                                           
         MVI   T.LIDTYPE,LIDTMEDL                                               
         MVI   T.LIDITLN,LIDLLN3Q                                               
         MVI   MYBYTE3,L'LIDLMED                                                
         B     ULST124                                                          
ULST156  CLI   GSSMPAGE,6          ARE WE ON 1R PAGE                            
         BNE   ULST158             NO                                           
         MVI   T.LIDTYPE,LIDT1RAC                                               
         MVI   T.LIDITLN,LIDLLN1Q                                               
         MVI   MYBYTE3,L'LIDLACT                                                
         B     ULST124                                                          
ULST158  CLI   GSSMPAGE,7          ARE WE ON SCRIBE FORMAT PAGE                 
         BNE   ULST160             YES                                          
         MVI   T.LIDTYPE,LIDTSCRB                                               
         MVI   T.LIDITLN,LIDLLN5Q                                               
         MVI   MYBYTE3,L'LIDLREP                                                
         B     ULST124                                                          
ULST160  CLI   GSSMPAGE,8          ARE WE ON SCHEME CODE PAGE                   
         BNE   ULST162             YES                                          
         MVI   T.LIDTYPE,LIDTESCH                                               
         MVI   T.LIDITLN,LIDLLN5Q                                               
         MVI   MYBYTE3,L'LIDLSCH                                                
         B     ULST124                                                          
ULST162  CLI   GSSMPAGE,9          ARE WE ON SUPPLIER ACCOUNT PAGE              
         BE    *+6                 YES                                          
         DC    H'0'                                                             
         MVI   T.LIDTYPE,LIDTSUPP                                               
         MVI   T.LIDITLN,LIDLLN8Q                                               
         MVI   MYBYTE3,L'LIDLSULA                                               
         B     ULST124                                                          
*                                                                               
ULST166  LTR   R5,R5                                                            
         BZ    ULST168                                                          
         STC   R5,T.LIDLN                                                       
         GOTO1 AADDLLS,BOPARM,LLSRECD                                           
         BNE   EXITL                                                            
*                                                                               
ULST168  CLI   ANYERROR,NO                                                      
         BE    EXITOK                                                           
         OI    CSINDSG1,CSINDUNW     SET TO UNWIND VIA £ABEND                   
         B     EXITL                                                            
*                                                                               
***T1ERR NI    LSLTIND1,FF-LSLTIBLD   REBUILD THE LIST                          
***      XC    GCLASKEY,GCLASKEY      SET KEY HAS BEEN CHANGED                  
***      NI    GSINDSL1,FF-GSIXMNT    TURN OF MAINT SCREEN LOADED FLAG          
***      B     EXITL                  ALREADY EXISTS                            
         DROP  SUB,T,R2,R3                                                      
         SPACE 1                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                     CONSTANTS                                    
CONSTS   DC    0X                                                               
         DC    C'ET'                                                            
         DC    C'WC'                                                            
         DC    C'SJ'                                                            
         DC    C'MEDIA'                                                         
         DC    C'1R'                                                            
         DC    C'1N'                                                            
         DC    C'SV'                                                            
         DC    C'SX'                                                            
         DC    9C'N'                                                            
CONSTX   EQU   *-CONSTS                                                         
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
ONLY     EQU   C'O'                                                             
STMPSTRQ EQU   X'03'               TEMPSTORE PAGE NO. FOR USE IN SEARCH         
NAMFLDLQ EQU   36                                                               
MAXITMS  EQU   375                                                              
IOMAXLNQ EQU   2000-256                                                         
*                                                                               
DCLISTU  DS    0D                                                               
         DCDDL AC#RSRCV,L'UC@SRCV,C                                             
         DCDDL AC#RSINC,L'UC@SINC,C                                             
         DCDDL AC#RSPAY,L'UC@PAY,C                                              
         DCDDL AC#RSEXP,L'UC@EXP,C                                              
         DCDDL AC#RS497,L'UC@497,C                                              
         DCDDL AC#RS498,L'UC@498,C                                              
         DCDDL AC#RS540,L'UC@540,C                                              
         DCDDL AC#RS544,L'UC@544,C                                              
         DCDDL AC#GLG,L'UC@GLG,C                                                
         DCDDL AC#MED,L'UC@MED,C                                                
         DCDDL AC#SUP,L'AC@SUP,L                                                
DCLISTUX DC    AL1(EOT)                                                         
         EJECT ,                                                                
*                                                                               
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
T        USING LDGRECD,IOKEY                                                    
         USING LDGTABD,R4                                                       
INITR    NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   BCWORK,BCSPACES                                                  
         CLI   CSACT,A#DLOAD                                                    
         BNE   *+10                                                             
         MVC   BCWORK(L'FVMSGNO),FVMSGNO                                        
         MVC   BCWORK+2(L'FVXTRA),FVXTRA                                        
         MVI   LIMINDS,0                                                        
         MVI   LIMIND2,0                                                        
         MVI   NOSX,0                                                           
***      MVI   GSSMCODE,C'A'       ** TESTING ONLY **    SMAN                   
         MVC   T.LDGKEY,BCSPACES                                                
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'BCCPYPRD),BCCPYPRD                                   
         GOTO1 AGETLDG                                                          
         JE    *+6                                                              
         DC    H'0'                NO SJ LEDGER?                                
         ICM   R4,15,ACALDG                                                     
         MVC   CLILEN(L'CLILEN+L'PROLEN+L'JOBLEN),LDGTLVA                       
*                                                                               
         MVC   T.LDGKEY,BCSPACES                                                
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'BCCPYPRD),AC1R   1R                                  
         GOTO1 AGETLDG                                                          
         JE    *+6                                                              
         DC    H'0'                NO 1R LEDGER?                                
         ICM   R4,15,ACALDG                                                     
*&&US*&& MVC   L11RLEN(L'L11RLEN+L'L21RLEN+L'L31RLEN+L'L41RLEN),LDGTLVA         
*&&UK*&& MVC   L31RLEN,LDGTLVC                                                  
*                                                                               
INIT02   MVC   T.LDGKEY,BCSPACES   GET SV LENGTHS                               
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'SVUL),SVUL                                           
         GOTO1 AGETLDG                                                          
         JE    *+6                                                              
         DC    H'0'                NO SV LEDGER?                                
         ICM   R4,15,ACALDG                                                     
         MVC   LENSVA(LENSVLNQ),LDGTLVA                                         
*                                                                               
         L     RF,AIO1                                                          
         CLC   T.LDGKEY,LDGKEY-LDGRECD(RF)                                      
         JE    INIT04                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO1)                                    
         GOTOR AIO                                                              
         JE    *+6                                                              
         DC    H'0'                CAN'T READ SV LEDGER!!!                      
*                                                                               
INIT04   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO1),0                  
         CLI   12(R1),0                                                         
         JNE   INIT06                                                           
         L     R5,12(R1)                                                        
         USING NAMELD,R5                                                        
         MVC   LDGSVN,BCSPACES                                                  
         XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EXMVC RE,LDGSVN,NAMEREC                                                
*                                                                               
INIT06   MVC   T.LDGKEY,BCSPACES   GET SX LENGTHS                               
         MVC   T.LDGKCPY,CUABIN                                                 
         MVC   T.LDGKUNT(L'SXUL),SXUL                                           
         GOTO1 AGETLDG                                                          
         JE    INIT08                                                           
         MVC   FVXTRA,BCSPACES                                                  
         MVI   NOSX,1                                                           
         J     INIT12                                                           
*                                                                               
INIT08   ICM   R4,15,ACALDG                                                     
         MVC   LENSXA(LENSXLNQ),LDGTLVA                                         
         L     RF,AIO1                                                          
         CLC   T.LDGKEY,LDGKEY-LDGRECD(RF)                                      
         JE    INIT10                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO1)                                    
         GOTOR AIO                                                              
         JE    *+6                                                              
         DC    H'0'                CAN'T READ SX LEDGER!!!                      
*                                                                               
INIT10   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO1),0                  
         CLI   12(R1),0                                                         
         JNE   INIT12                                                           
         L     R5,12(R1)                                                        
         MVC   LDGSXN,BCSPACES                                                  
         XR    RE,RE                                                            
         IC    RE,NAMLN                                                         
         SHI   RE,NAMLN1Q+1                                                     
         EXMVC RE,LDGSXN,NAMEREC                                                
*                                                                               
INIT12   LARL  RF,DCLISTU                                                       
         GOTO1 VDICTAT,BOPARM,C'LU  ',(RF),DSLISTU                              
         CLI   CSACT,A#DLOAD       DOWNLOAD?                                    
         JNE   *+8                                                              
         MVI   WHENOK,WHENOV+WHENSOON         NOTIFY VALID INPUTS               
         NI    GCINDS3,FF-GCIRCHG                                               
*        OI    GCINDS3,GCIPNORM               OVERRIDE SHOW/HIDE                
         LH    RF,GSDSPPAG                                                      
         A     RF,ATWA                                                          
         NI    FVATRB-FVIHDR(RF),FF-FVAPROT   AND UNPROTECT                     
         CLI   CSACT,A#DLOAD                                                    
         JNE   EXIT                                                             
         MVC   FVMSGNO,BCWORK    IF LDGR DOESN'T EXIST THE ERROR MSG            
         MVC   FVXTRA,BCWORK+2   GETS OVERWRITTEN SO NEED TO RESTORE            
         J     EXIT                                                             
         LTORG                                                                  
         DROP  R4,R5,T                                                          
         SPACE 2                                                                
***********************************************************************         
* OVERLAY ROUTINES                                                    *         
***********************************************************************         
         SPACE 1                                                                
         DS    0F                                                               
OVROU1   NMOD1 0,**OVR1**,RA,R7                                                 
         L     RC,4(RD)                                                         
         L     RC,68(RC)                                                        
         USING WORKD,R9                                                         
         USING SAVED,R6                                                         
         USING GWORKD,R8                                                        
         USING OVERWRKD,RC                                                      
         SRL   RF,32-8                                                          
         SLL   RF,2                                                             
         B     *+4(RF)                                                          
         B     VALCPJ                                                           
         B     VALEXP                                                           
         B     VALWC                                                            
         B     VALNCL                                                           
         B     VALMED                                                           
         B     VALC1R                                                           
         B     VALFORM                                                          
         B     DOFLT                                                            
         B     GETNLE                                                           
         B     GETLIT                                                           
         B     VALSUP                                                           
         B     VALSCH                                                           
         B     CHKLVL                                                           
         B     ADDLLS                                                           
         B     UPDLLS                                                           
         B     READNXT                                                          
         B     NXTLLS                                                           
         B     MYRFADD                                                          
         B     MYRFWRT                                                          
         B     RSTADD                                                           
*&&US*&& B     CHKPID                                                           
*&&UK*&& B     ADDPAS                                                           
*&&UK*&& B     DELPAS                                                           
*                                                                               
OVROU1L  CLI   *,FF                                                             
         B     OVROU1X                                                          
OVROU1H  CLI   *,0                                                              
         B     OVROU1X                                                          
OVROU1E  CR    RB,RB                                                            
         L     R1,CALLR1           RETURN PARAMETERS TO CALLER                  
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
OVROU1X  XIT1  ,                                                                
         EJECT                                                                  
***********************************************************************         
* CHECK CLI/PRO/JOB CODE IS VALID AND RETURN CLI/PRO/JOB NAMES        *         
*                                                                     *         
* NTRY - P1  = CLI/PRO/JOB CODE                                       *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
* US   - NO CC IS SET BUT ERRIND ERCOFIN IS SET IF OFFICE IS INVALID  *         
***********************************************************************         
         SPACE 1                                                                
T        USING ACTRECD,IOKEY                                                    
VALCPJ   L     R2,0(R1)            R2=CLI/PRO/JOB CODE                          
         LR    RF,R2                                                            
         SR    RE,RE                                                            
*&&UK                                                                           
VCPJ02   CLI   0(RF),C' '                                                       
         BE    VCPJ03                                                           
         LA    RF,1(,RF)                                                        
         AHI   RE,1                                                             
         CHI   RE,L'ACTKACT                                                     
         BNE   VCPJ02                                                           
*                                                                               
VCPJ03   MVC   CPJOFF,BCSPACES                                                  
         STC   RE,MYBYTE           LENGTH OF CLI/PRO/JOB CODE                   
         CLC   MYBYTE,CLILEN                                                    
         BE    VCPJ04                                                           
         CLC   MYBYTE,PROLEN                                                    
         BE    *+8                                                              
         BL    OVROU1L             INVALID CLI/PRO/JOB LENGTH                   
         OI    CPJINDS,CPJINCLI    SET NOT CLIENT                               
*&&                                                                             
*&&US                                                                           
         LHI   RE,L'ACTKACT        LENGTH OF CLI/PRO/JOB CODE                   
         LA    RF,0(RE,R2)         POINT AT END OF CLI/PRO/JOB                  
         BCTR  RF,0                                                             
         MVC   CPJOFF,BCSPACES                                                  
VCPJ02   CLI   0(RF),C' '                                                       
         BH    VCPJ03                                                           
         BCTR  RF,0                                                             
         BCT   RE,VCPJ02                                                        
VCPJ03   STC   RE,MYBYTE           LENGTH OF CLI/PRO/JOB CODE                   
         CLC   MYBYTE,JOBLEN                                                    
         BH    OVROU1L             INVALID CLI/PRO/JOB LENGTH                   
         CLC   MYBYTE,CLILEN                                                    
         BNH   VCPJ04                                                           
         OI    CPJINDS,CPJINCLI    SET NOT CLIENT                               
*&&                                                                             
                                                                                
VCPJ04   MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRODUL),PRODUL                                       
*                                                                               
         MVI   TOTNAML,0                                                        
         MVC   BOWORK1(L'BOWORK1+L'BOWORK2),BCSPACES                            
         LA    R4,BOWORK1                                                       
*                                                                               
         IC    RE,CLILEN           READ CLIENT NAME                             
         SHI   RE,1                                                             
         EXMVC RE,T.ACTKACT,0(R2)  CLIENT CODE                                  
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('PPRELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,12(R1)                                                        
         TM    CPJINDS,CPJICCPJ             CHANGE OF CPJ?                      
         BNZ   VCPJ04A                                                          
         CLC   LIDLOFF-LIDLACT(L'LIDLOFF,R2),BCSPACES ALREADY HAVE OFF?         
         BNH   VCPJ05                                                           
         MVC   CPJOFF,LIDLOFF-LIDLACT(R2)   THEN DON'T REPLACE                  
         B     VCPJ05                                                           
*                                                                               
VCPJ04A  CLC   PPRGAOFF-PPRELD(L'LIDLOFF,RF),BCSPACES                           
         BNH   *+10                                                             
         MVC   CPJOFF,PPRGAOFF-PPRELD(RF)                                       
*                                                                               
VCPJ05   MVC   CPJCOFF,PPRGAOFF-PPRELD(RF)  SAVE CLI OFF IN CASE REVERT         
*&&US                                                                           
         TM    CPJINDS,CPJINCLI             NOT CLIENT LEVEL ?                  
         BO    VCPJ05A                                                          
         GOTO1 ATSTOFF,CPJCOFF                                                  
         BE    VCPJ05A                                                          
         OI    ERRIND,ERCOFIN                                                   
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
VCPJ05A  GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('LIDELQ',AIO2),          X        
               (2,=AL1(2,LIDTPOFC))                                             
         CLI   12(R1),0                                                         
         BNE   VCPJ06                                                           
         L     RF,12(R1)                                                        
         USING LIDELD,RF                                                        
         LLC   R1,LIDLN                                                         
         SHI   R1,LIDLNDQ+1                                                     
         MVC   CPJCOFF+L'LIDLOFF(0),LIDDATA                                     
         EX    R1,*-6                                                           
VCPJ06   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VCPJ10                                                           
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EXMVC R1,0(R4),NAMEREC                                                 
         AHI   R1,1                                                             
         STC   R1,TOTNAML          LENGTH OF CLIENT NAME                        
         AR    R4,R1               BUMP TO THE END OF CLIENT NAME               
         DROP  RF                                                               
*                                                                               
VCPJ10   CLC   MYBYTE,CLILEN       TEST CLIENT CODE ONLY                        
*&&UK*&& BE    VCPJ30                                                           
*&&US*&& BNH   VCPJ30                                                           
*                                                                               
*        MVC   IOKEY,IOKEYSAV      RESTORE IOKEY                                
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRODUL),PRODUL                                       
         SR    RE,RE                                                            
         IC    RE,PROLEN                                                        
         SHI   RE,1                                                             
         EXMVC RE,T.ACTKACT,0(R2)  PRODUCT CODE                                 
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('PPRELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,12(R1)                                                        
         TM    CPJINDS,CPJICCPJ             CHANGE OF CPJ?                      
         BNZ   VCPJ13A                                                          
         CLC   LIDLOFF-LIDLACT(L'LIDLOFF,R2),BCSPACES ALREADY HAVE OFF?         
         BNH   VCPJ14                                                           
         MVC   CPJOFF,LIDLOFF-LIDLACT(R2)   THEN DON'T REPLACE                  
*&&US*&& LA    RE,CPJOFF                                                        
         B     VCPJ14                                                           
*                                                                               
*&&UK                                                                           
VCPJ13A  CLC   PPRGAOFF-PPRELD(L'LIDLOFF,RF),BCSPACES                           
         BNH   *+10                                                             
         MVC   CPJOFF,PPRGAOFF-PPRELD(RF)                                       
*&&                                                                             
*&&US                                                                           
VCPJ13A  CLC   PPRGAOFF-PPRELD(L'LIDLOFF,RF),BCSPACES                           
         BH    *+12                                                             
         LA    RE,CPJCOFF                                                       
         B     VCPJ14                                                           
         MVC   CPJOFF,PPRGAOFF-PPRELD(RF)                                       
         LA    RE,CPJOFF                                                        
*&&                                                                             
                                                                                
VCPJ14   DS    0H                                                               
*&&US                                                                           
         GOTO1 ATSTOFF,0(RE)                                                    
         BE    *+14                                                             
         OI    ERRIND,ERCOFIN                                                   
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
*                                                                               
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VCPJ20                                                           
         MVI   0(R4),C'/'                                                       
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EXMVC R1,1(R4),NAMEREC                                                 
         AHI   R1,2                                                             
         SR    RE,RE                                                            
         IC    RE,TOTNAML                                                       
         AR    RE,R1                                                            
         STC   RE,TOTNAML          LENGTH OF CLIENT/PRODUCT NAMES               
         AR    R4,R1               BUMP TO THE END OF PRODUCT NAME              
         DROP  RF                                                               
*                                                                               
VCPJ20   CLC   MYBYTE,PROLEN       TEST PRODUCT CODE ONLY                       
*&&UK*&& BE    VCPJ30                                                           
*&&US*&& BNH   VCPJ30                                                           
*                                                                               
*        MVC   IOKEY,IOKEYSAV      RESTORE IOKEY                                
         MVC   T.ACTKEY,BCSPACES                                                
         MVC   T.ACTKCPY,CUABIN                                                 
         MVC   T.ACTKUNT(L'PRODUL),PRODUL                                       
         MVC   T.ACTKACT,0(R2)     JOB CODE                                     
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   VCPJ30                                                           
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('PPRELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,12(R1)                                                        
         TM    CPJINDS,CPJICCPJ             CHANGE OF CPJ?                      
         BNZ   VCPJ21A                                                          
         CLC   LIDLOFF-LIDLACT(L'LIDLOFF,R2),BCSPACES ALREADY HAVE OFF?         
         BNH   VCPJ22                                                           
         MVC   CPJOFF,LIDLOFF-LIDLACT(R2)   THEN DON'T REPLACE                  
         B     VCPJ22                                                           
*                                                                               
VCPJ21A  CLC   PPRGAOFF-PPRELD(L'LIDLOFF,RF),BCSPACES                           
         BNH   *+10                                                             
         MVC   CPJOFF,PPRGAOFF-PPRELD(RF)                                       
*                                                                               
VCPJ22   GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('NAMELQ',AIO2),0                  
         CLI   12(R1),0                                                         
         BNE   VCPJ30                                                           
         MVI   0(R4),C'/'                                                       
         L     RF,12(R1)                                                        
         USING NAMELD,RF                                                        
         SR    R1,R1                                                            
         IC    R1,NAMLN                                                         
         SHI   R1,NAMLN1Q+1                                                     
         EXMVC R1,1(R4),NAMEREC                                                 
         SR    RE,RE                                                            
         IC    RE,TOTNAML                                                       
         LA    RE,2(R1,RE)                                                      
         STC   RE,TOTNAML          LENGTH OF CLI/PRO/JOB NAMES                  
         DROP  RF                                                               
*                                                                               
VCPJ30   MVC   CPJCODE,T.ACTKACT                                                
*                                                                               
         LA    R4,BOWORK1                                                       
         SR    RE,RE                                                            
         ICM   RE,1,TOTNAML                                                     
         BZ    OVROU1E                                                          
         CHI   RE,NAMFLDLQ         ENOUGH SPACE TO STORE NAMES                  
         BNH   VCPJ34                                                           
         SHI   RE,NAMFLDLQ                                                      
         AR    R4,RE               TRANCATE THE NAMES                           
*                                                                               
VCPJ34   MVC   CPJNAME,0(R4)                                                    
         B     OVROU1E                                                          
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK EXPENDITURE TYPE CODE IS VALID AND RETURN NAME                *         
*                                                                     *         
* NTRY - P1  = EXPENDITURE CODE                                       *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALEXP   L     R2,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING ETYRECD,R5                                                       
         XC    ETYKEY,ETYKEY       READ EXPENDITURE TYPE RECORD                 
         MVI   ETYKTYP,ETYKTYPQ                                                 
         MVI   ETYKSUB,ETYKSUBQ                                                 
         MVC   ETYKCPY,CUABIN      CONNECTED ID                                 
         MVC   ETYKCODE,0(R2)                                                   
         MVC   ETYKOFFC,BCSPACES                                                
         MVC   SVIOKEY,IOKEY                                                    
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
         CLC   SVIOKEY(ETYKOFFC-ETYRECD),IOKEY                                  
         BNE   OVROU1L                                                          
*                                                                               
         CLC   ETYKOFFC,BCSPACES   NO OFFICE CODE THEN FINE                     
         BNH   VALEXP08                                                         
         CLI   CUACCS,0            GLOBAL LOGON                                 
         BE    VALEXP08            THEN FINE TO USE IT                          
         TM    BCCPYST4,CPYSOFF2   2 CHARACTER OFFICES?                         
         BNZ   VALEXP04                                                         
         CLI   CUACCS,C'$'         LIMIT LIST LOGON?                            
         BNE   VALEXP02                                                         
         CLI   ETYKOFFC,C'$'       OFFICE LIST EXPENDITURE TYPE                 
         BNE   VALEXP02                                                         
         CLC   CUACCS(2),ETYKOFFC  CHECK WHETHER OFFICE LIST MATCHES            
         BE    VALEXP08                                                         
         B     OVROU1H             EXPENDITURE TYPE NOT VALID ON THIS           
*                                                                               
VALEXP02 GOTO1 ATSTOFF,ETYKOFFC                                                 
         BE    VALEXP08                                                         
         B     OVROU1L             EXPENDITURE TYPE NOT VALID                   
*                                                                               
X        USING OFFRECD,IOKEY       CHECK WHETHER WE HAVE OFFICE LIST            
VALEXP04 MVC   SVIOKEY,IOKEY                                                    
         XC    IOKEY,IOKEY                                                      
         MVI   X.OFFKTYP,OFFKTYPQ                                               
         MVC   X.OFFKCPY,CUABIN                                                 
         MVC   X.OFFKOFF,CUACCS+2                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO3)                                      
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    X.OFFKSTAT,OFFSLIST  OFFICE LIST?                                
         BZ    VALEXP06             NO THEN VALIDATE OFFICE                     
         MVC   IOKEY,SVIOKEY                                                    
         CLC   CUACCS+2(2),ETYKOFFC CHECK WHETHER MATCH ON                      
         BE    VALEXP08             OFFICE LIST                                 
*                                                                               
VALEXP06 MVC   IOKEY,SVIOKEY                                                    
         GOTO1 ATSTOFF,ETYKOFFC                                                 
         BE    VALEXP08                                                         
         B     OVROU1L             EXPENDITURE TYPE NOT VALID                   
*                                                                               
VALEXP08 MVC   FVXTRA,BCSPACES                                                  
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',ETYRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
         MVC   EXPNAME,BCSPACES                                                 
                                                                                
T        USING NAMELD,BOELEM                                                    
         MVC   EXPCODE,ETYKCODE                                                 
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SH    RF,=Y(NAMLN1Q+1)                                                 
         MVC   EXPNAME(0),T.NAMEREC                                             
         EX    RF,*-6                                                           
         B     OVROU1E                                                          
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK WORK CODE IS VALID AND RETURN NAME                            *         
*                                                                     *         
* NTRY - P1  = WORK CODE                                              *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALWC    L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING WCORECD,R5                                                       
         MVC   WCOKEY,BCSPACES     READ WORK CODE RECORD                        
         MVI   WCOKTYP,WCOKTYPQ                                                 
         MVC   WCOKCPY,CUABIN      CONNECTED ID                                 
         MVC   WCOKUNT(L'BCCPYPRD),BCCPYPRD                                     
         SHI   RE,1                                                             
         MVC   WCOKWRK(0),0(R2)                                                 
         EX    RE,*-6                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
         L     R5,AIO2                                                          
         MVC   WRKCODE,WCOKWRK                                                  
         MVC   WRKNAME,BCSPACES                                                 
         MVC   WRKDESC,BCSPACES                                                 
         GOTO1 AGETEL,BOPARM,('NAMELQ',WCORECD),0                               
         BNE   VALWC02                                                          
                                                                                
T        USING NAMELD,BOELEM                                                    
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SHI   RF,NAMLN1Q+1                                                     
         MVC   WRKNAME(0),T.NAMEREC                                             
         EX    RF,*-6                                                           
T        USING WCOELD,BOELEM                                                    
VALWC02  GOTO1 AGETEL,BOPARM,('WCOELQ',WCORECD),0                               
         BNE   OVROU1E                                                          
         MVC   WRKDESC,T.WCODESC                                                
         B     OVROU1E                                                          
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK NON CLIENT CODE IS VALID AND RETURN NAME                      *         
*                                                                     *         
* NTRY - P1  = NON CLIENT ACCOUNT CODE                                *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALNCL   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES     READ NON CLIENT ACCOUNT RECORD               
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'BCCPYPRD),=C'1N'                                       
         SHI   RE,1                                                             
         MVC   ACTKACT(0),0(R2)                                                 
         EX    RE,*-6                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',ACTRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
         MVC   NCLNAME,BCSPACES                                                 
                                                                                
T        USING NAMELD,BOELEM                                                    
         MVC   NCLCODE,ACTKACT                                                  
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SHI   RF,NAMLN1Q+1                                                     
         MVC   NCLNAME(0),T.NAMEREC                                             
         EX    RF,*-6                                                           
         B     OVROU1E                                                          
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK MEDIA CODE IS VALID AND RETURN NAME                           *         
*                                                                     *         
* NTRY - P1  = MEDIA CODE                                             *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALMED   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING PMDRECD,R5                                                       
         MVC   PMDKEY,BCSPACES     READ MEDIA CODE RECORD                       
         MVC   PMDKCPY,CUABIN      CONNECTED ID                                 
         MVI   PMDKTYP,PMDKTYPQ                                                 
         SHI   RE,1                                                             
         MVC   PMDKMED(0),0(R2)                                                 
         EX    RE,*-6                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('PMDELQ',PMDRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
         MVC   MEDNAME,BCSPACES                                                 
                                                                                
T        USING PMDELD,BOELEM                                                    
         MVC   MEDCODE,PMDKMED                                                  
         MVC   MEDNAME,T.PMDDESC                                                
         B     OVROU1E                                                          
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK 1R CODE IS VALID AND RETURN NAME                              *         
*                                                                     *         
* NTRY - P1  = 1R ACCOUNT CODE                                        *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALC1R   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING ACTRECD,R5                                                       
         MVC   ACTKEY,BCSPACES     READ NON CLIENT ACCOUNT RECORD               
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'BCCPYPRD),=C'1R'                                       
         SHI   RE,1                                                             
         MVC   ACTKACT(0),0(R2)                                                 
         EX    RE,*-6                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
*&&US                                                                           
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    VALC1R10                                                         
         LLC   RE,L11RLEN          CHECK OFFICE TO MAKE SURE IT'S VALID         
         BCTR  RE,0                                                             
         MVC   BCWORK,BCSPACES                                                  
         MVC   BCWORK(0),ACTKACT                                                
         EX    RE,*-6                                                           
         GOTO1 ATSTOFF,BCWORK                                                   
         BE    *+14                                                             
         OI    ERRIND,ERCOFIN                                                   
         MVC   FVXTRA,BCSPACES                                                  
*&&                                                                             
VALC1R10 L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',ACTRECD),0                               
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
         MVC   C1RNAME,BCSPACES                                                 
                                                                                
T        USING NAMELD,BOELEM                                                    
         MVC   C1RCODE,ACTKACT                                                  
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SHI   RF,NAMLN1Q+1                                                     
         MVC   C1RNAME(0),T.NAMEREC                                             
         EX    RF,*-6                                                           
         B     OVROU1E                                                          
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK FORMAT CODE IS VALID AND RETURN NAME AND REPORT TYPE          *         
*                                                                     *         
* NTRY - P1  = FORMAT CODE                                            *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALFORM  L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         LA    R5,IOKEY                                                         
         USING RESRECD,R5                                                       
         MVC   RESKEY,BCSPACES     READ SCRIBE RECORD                           
         MVI   RESKTYP,RESKTYPQ                                                 
         MVI   RESKSUB,RESKSUBQ                                                 
         MVC   RESKCPY,CUABIN      CONNECTED ID                                 
         SHI   RE,1                                                             
         MVC   RESKFORM(0),0(R2)                                                
         EX    RE,*-6                                                           
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L             FORMAT DOESN'T EXIST                         
*                                                                               
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('RPFELQ',RESRECD),0                               
         BNE   OVROU1H             NO R.L. PROFILE                              
                                                                                
T        USING RPFELD,BOELEM                                                    
         MVC   TRANTYPE,BCSPACES                                                
         TM    T.RPFXMIT,RPFXACNT  ACCENT TRANSMISSION TYPE?                    
         BNO   *+14                                                             
         MVC   TRANTYPE(6),=C'ACCENT'                                           
         B     VALFORM1                                                         
         TM    T.RPFXMIT,RPFXQREP  QUICK REPORT TRANSMISSION TYPE?              
         BZ    OVROU1H             NOT ACCENT OR QUICK REPORT TYPE              
         MVC   TRANTYPE(8),=C'QREPORTS'                                         
         DROP  T                                                                
*                                                                               
VALFORM1 MVC   FORMCODE,RESKFORM                                                
         MVC   FORMNAME,BCSPACES                                                
*                                                                               
         L     R5,AIO2                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',RESRECD),0                               
         BNE   VALFORM2       SOME FORMATS DON'T HAVE NAMES                     
                                                                                
T        USING NAMELD,BOELEM                                                    
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SHI   RF,NAMLN1Q+1                                                     
         MVC   FORMNAME(0),T.NAMEREC                                            
         EX    RF,*-6                                                           
*                                                                               
VALFORM2 GOTO1 AGETEL,BOPARM,('STYELQ',RESRECD),0                               
         BE    *+6                                                              
         DC    H'0'                STYELD MISSING                               
                                                                                
T        USING STYELD,BOELEM                                                    
         MVC   REPCODE,T.STYCODE     REPORT TYPE                                
                                                                                
         B     OVROU1E                                                          
         DROP  R5                                                               
         DROP  T                                                                
         EJECT ,                                                                
*********************************************************************           
* FILTER LIMIT LIST RECORDS                                         *           
* ENTY - IOKEY = LIMIT LIST RECORD KEY                              *           
*********************************************************************           
         SPACE 2                                                                
THIS     USING LLSRECD,R2                                                       
X        USING LLSRECD,IOKEY                                                    
DOFLT    MVC   SVIOKEY,IOKEY                                                    
*&&UK                                                                           
         GOTO1 ACHKLPID,BOPARM,X.LLSKPIDB FILTER BAD PIDS                       
         BE    DOFLT00                                                          
         BH    DOFLT01                                                          
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         MVC   IOKEY,SVIOKEY                                                    
         GOTO1 AIO                                                              
         BE    DOFLTL              BAD PID READ NEXT                            
         DC    H'0'                                                             
*                                                                               
DOFLT00  L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         MVC   IOKEY,SVIOKEY                                                    
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
*&&                                                                             
DOFLT01  GOTOX ('GETPID',AGROUTS),X.LLSKPIDB                                    
*        CLC   =C'????????',BCWORK                                              
*        BE    EXITL               INVALID BINARY PID                           
         MVC   SVPID,BCWORK                                                     
         MVI   LSRIND,0                                                         
*&&US                                                                           
         MVC   SVPIDBIN,X.LLSKPIDB                                              
         L     RF,ATWA                                                          
         CLI   TWAACCS-TWAD(RF),0  TEST LIMIT ACCESS OR LIST ACCESS             
         BE    DOFLT01A                                                         
         GOTO1 ACHKPID,BOPARM,SVPIDBIN                                          
         BE    DOFLT01A                                                         
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         MVC   IOKEY,SVIOKEY                                                    
         GOTO1 AIO                                                              
         BE    DOFLTL                                                           
         TM    IOERR,IOEDEL                                                     
         BO    DOFLTL              BAD PID READ NEXT                            
         DC    H'0'                                                             
*&&                                                                             
*                                                                               
DOFLT01A MVC   X.LLSKEY,THIS.LLSKEY RESET KEY SEQUENCE                          
         L     R1,=AL4(XOHID+XOACCDIR+XIO11)                                    
         GOTOR AIO                                                              
         BE    DOFLT02                                                          
         TM    IOERR,FF-IOEDEL                                                  
         BZ    DOFLT02                                                          
         DC    H'0'                                                             
         DROP  THIS,X                                                           
*                                                                               
DOFLT02  CLI   CSACT,A#DLOAD       IF DOING DOWNLOAD                            
         BE    DOFLT03                                                          
         CLC   SVPID,SVPIDFL                                                    
         BL    DOFLTL                                                           
         B     DOFLT04                                                          
*                                                                               
DOFLT03  OC    SVPIDFL,SVPIDFL     CHECK IF THERE IS A FILTER...                
         BZ    DOFLT04                                                          
         XR    RF,RF                                                            
         IC    RF,SVPIDFLN                                                      
         EXCLC RF,SVPID,SVPIDFL    IF SO FILTER THE RESULTS                     
         BNE   DOFLTL                                                           
DOFLT04  OC    SVFLTS(SVFLTLQ),SVFLTS                                           
         BZ    DOFLTE              OK - NO FILTER                               
*                                                                               
         L     R1,=AL4(XOGET+XOACCMST+XIO11)                                    
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD RECORD                                   
*                                                                               
         MVI   READSEQ#,0                                                       
         MVC   SVIOKEY,IOKEY       SAVE IOKEY                                   
*                                                                               
         NI    LIMINDS,FF-(LIMIFCPJ+LIMIFETY+LIMIFWRK+LIMIFNCL+LIMIFMEDX        
               +LIMIF1RA+LIMIFFOR+LIMIFSCH)                                     
         NI    LIMIND2,FF-(LIMIFSUC)                                            
         OC    SVCPJ,SVCPJ         ANY CLI/PRO/JOB FILTER?                      
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIFCPJ    DON'T COMPARE CLI/PRO/JOB                    
         OC    SVETY,SVETY         ANY EXPENDITURE FILTER?                      
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIFETY    DON'T COMPARE EXPENDITURE CODE               
         OC    SVWC,SVWC           ANY WORK CODE FILTER?                        
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIFWRK    DON'T COMPARE WORK CODE                      
         OC    SVNCL,SVNCL         ANY NON CLIENT CODE FILTER?                  
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIFNCL    DON'T COMPARE NON CLIENT CODE                
         OC    SVMED,SVMED         ANY MEDIA CODE FILTER?                       
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIFMED    DON'T COMPARE MEDIA CODES                    
         OC    SV1RA,SV1RA         ANY 1R ACCOUNT CODE FILTER?                  
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIF1RA    DON'T COMPARE 1R ACCOUNT CODES               
         OC    SVFORM,SVFORM       ANY FORMAT CODE FILTER?                      
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIFFOR    DON'T COMPARE FORMAT CODES                   
         OC    SVFSCH,SVFSCH       ANY SCHEME CODE FILTER?                      
         BNZ   *+8                                                              
         OI    LIMINDS,LIMIFSCH    DON'T COMPARE SCHEME CODES                   
         OC    SVFSUC,SVFSUC       ANY SUPPLIER FILTER?                         
         BNZ   *+8                                                              
         OI    LIMIND2,LIMIFSUC    DON'T COMPARE SUPPLIER CODE                  
*                                                                               
DOFLT08  L     R4,AIOREC           A(LIMLIST RECORD)                            
         CLI   READSEQ#,0                                                       
         BE    *+8                                                              
         L     R4,AIO5             A(NEXT LLS RECORD)                           
         AHI   R4,LLSRFST-LLSRECD                                               
*                                                                               
DOFLT10  TM    LIMINDS,LIMIFCPJ+LIMIFETY+LIMIFWRK+LIMIFNCL+LIMIFMED+LIMX        
               IF1RA+LIMIFFOR+LIMIFSCH                                          
         BNO   DOFLT11                                                          
         TM    LIMIND2,LIMIFSUC                                                 
         BO    DOFLTE                                                           
*                                                                               
DOFLT11  CLI   0(R4),0                                                          
         BNE   DOFLT12                                                          
         MVI   LSRIND,1            RESET IO SEQUENCE                            
         GOTO1 ANXTLLS,AIOREC                                                   
         BE    DOFLT08                                                          
         B     DOFLTL              NO MORE LLS RECORDS                          
*                                                                               
DOFLT12  CLI   0(R4),LIDELQ                                                     
         BE    DOFLT20                                                          
*                                                                               
DOFLT14  SR    R0,R0                                                            
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     DOFLT10                                                          
*                                                                               
         USING LIDELD,R4                                                        
DOFLT20  MVC   BOWORK1,BCSPACES                                                 
         CLI   LIDTYPE,LIDTCPJL                                                 
         BNE   DOFLT21                                                          
         TM    LIMINDS,LIMIFCPJ    COMPARE CLI/PRO/JOB?                         
         BO    DOFLT14             NO - CLI/PRO/JOB FOUND                       
         MVC   BOWORK1(L'SVCPJ),SVCPJ                                           
         B     DOFLT30                                                          
DOFLT21  CLI   LIDTYPE,LIDTEXPL                                                 
         BNE   DOFLT22                                                          
         TM    LIMINDS,LIMIFETY    COMPARE EXPENDITURE CODE?                    
         BO    DOFLT14             NO - EXPENDITURE CODE FOUND                  
         MVC   BOWORK1(L'SVETY),SVETY                                           
         B     DOFLT30                                                          
DOFLT22  CLI   LIDTYPE,LIDTWCL                                                  
         BNE   DOFLT23                                                          
         TM    LIMINDS,LIMIFWRK    COMPARE WORK CODE?                           
         BO    DOFLT14             NO - WORK CODE FOUND                         
         MVC   BOWORK1(L'SVWC),SVWC                                             
         B     DOFLT30                                                          
DOFLT23  CLI   LIDTYPE,LIDTNCLL                                                 
         BNE   DOFLT24                                                          
         TM    LIMINDS,LIMIFNCL    COMPARE NON CLIENT CODE?                     
         BO    DOFLT14             NO - NON CLIENT CODE FOUND                   
         MVC   BOWORK1(L'SVNCL),SVNCL                                           
         B     DOFLT30                                                          
DOFLT24  CLI   LIDTYPE,LIDTMEDL                                                 
         BNE   DOFLT25                                                          
         TM    LIMINDS,LIMIFMED    COMPARE MEDIA CODE?                          
         BO    DOFLT14             NO - MEDIA CODE FOUND                        
         MVC   BOWORK1(L'SVMED),SVMED                                           
         B     DOFLT30                                                          
DOFLT25  CLI   LIDTYPE,LIDT1RAC                                                 
         BNE   DOFLT26                                                          
         TM    LIMINDS,LIMIF1RA    COMPARE 1R ACCOUNT CODE?                     
         BO    DOFLT14             NO - 1R ACCOUNT CODE FOUND                   
         MVC   BOWORK1(L'SV1RA),SV1RA                                           
         B     DOFLT30                                                          
DOFLT26  CLI   LIDTYPE,LIDTSCRB                                                 
         BNE   DOFLT27                                                          
         TM    LIMINDS,LIMIFFOR    COMPARE FORMAT CODE?                         
         BO    DOFLT14             NO - FORMAT CODE FOUND                       
         MVC   BOWORK1(L'SVFORM),SVFORM                                         
         B     DOFLT30                                                          
DOFLT27  CLI   LIDTYPE,LIDTESCH                                                 
         BNE   DOFLT28                                                          
         TM    LIMINDS,LIMIFSCH    COMPARE SCHEME CODE?                         
         BO    DOFLT14             NO - SCHEME CODE FOUND                       
         MVC   BOWORK1(L'SVFSCH),SVFSCH                                         
         B     DOFLT30                                                          
DOFLT28  CLI   LIDTYPE,LIDTSUPP                                                 
         BNE   DOFLT14                                                          
         TM    LIMIND2,LIMIFSUC    COMPARE SUPPLIER CODE?                       
         BO    DOFLT14             NO - SUPPLIER CODE FOUND                     
         MVC   BOWORK1(L'SVFSUC),SVFSUC                                         
*                                                                               
DOFLT30  SR    R5,R5                                                            
         IC    R5,LIDITLN          LENGTH OF ITEMS                              
         SR    R0,R0                                                            
         IC    R0,LIDLN            LENGTH OF ELEMENT                            
         SHI   R0,LIDDATA-LIDELD                                                
         SRDL  R0,32                                                            
         DR    R0,R5               R1=NUMBER OF ITEMS                           
*                                                                               
         LR    RE,R5               R5=LENGTH OF ITEM'S REAL DATA                
         SHI   RE,L'LIDLAPPL+L'LIDLAPP2+1                                       
         LA    RF,LIDDATA+L'LIDLAPPL+L'LIDLAPP2                                 
DOFLT34  CLI   LIDTYPE,LIDTCPJL                                                 
         BNE   *+8                                                              
         IC    RE,SVCPJXLN         INPUT CLI/PRO/JOB LENGTH -1                  
         EXCLC RE,0(RF),BOWORK1                                                 
         BNE   DOFLT38                                                          
         CLI   LIDTYPE,LIDTSUPP    SUPPLIER ACCOUNT CODE                        
         BNE   *+12                                                             
         OI    LIMIND2,LIMIFSUC                                                 
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTCPJL                                                 
         BNE   *+12                                                             
         OI    LIMINDS,LIMIFCPJ    FOUND CLI/PRO/JOB                            
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTEXPL                                                 
         BNE   *+12                ?????                                        
         OI    LIMINDS,LIMIFETY    FOUND EXPENDITURE CODE                       
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTWCL                                                  
         BNE   *+12                ?????                                        
         OI    LIMINDS,LIMIFWRK    FOUND WORK CODE                              
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTNCLL                                                 
         BNE   *+12                ?????                                        
         OI    LIMINDS,LIMIFNCL    FOUND NON CLIENT CODE                        
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTMEDL                                                 
         BNE   *+12                ?????                                        
         OI    LIMINDS,LIMIFMED    FOUND MEDIA CODE                             
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDT1RAC                                                 
         BNE   *+12                ?????                                        
         OI    LIMINDS,LIMIF1RA    FOUND 1R ACCOUNT CODE                        
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTSCRB                                                 
         BNE   *+12                ?????                                        
         OI    LIMINDS,LIMIFFOR    FOUND FORMAT CODE                            
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTESCH                                                 
         BNE   *+12                ?????                                        
         OI    LIMINDS,LIMIFSCH    FOUND SCHEME CODE                            
         B     DOFLT14                                                          
         CLI   LIDTYPE,LIDTSUPP                                                 
         BNE   DOFLT14             ?????                                        
         OI    LIMIND2,LIMIFSUC    FOUND SUPPLIER CODE                          
         B     DOFLT14                                                          
*                                                                               
DOFLT38  AR    RF,R5               CHECK NEXT ITEM                              
         BCT   R1,DOFLT34                                                       
         B     DOFLT14             GET NEXT ELEMENT                             
*                                                                               
DOFLTL   MVI   MYBYTE,FF                                                        
         B     DOFLTX                                                           
*                                                                               
DOFLTE   MVI   MYBYTE,0                                                         
*                                                                               
DOFLTX   CLI   LSRIND,1            RESET IO SEQUENCE?                           
         BNE   DOFLTX2             NO - OK                                      
         MVC   IOKEY,SVIOKEY                                                    
         L     R1,=AL4(XORDD+XOACCDIR+XIO11)                                    
         GOTOR AIO                                                              
         BE    DOFLTX2                                                          
         TM    IOERR,FF-IOEDEL     IF RESTORING, SHOULD EXIST                   
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
DOFLTX2  CLI   MYBYTE,0                                                         
         BE    OVROU1E                                                          
         B     OVROU1L                                                          
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET NEXT LIST ELEMENT                                    *         
*                                                                     *         
* NTRY    P1=(LIST TYPE,(CURR DISP INTO RECORD/ELEMENT PAIR)                    
* EXIT    SET DISPLACEMENT INTO RECORD/ELEMENT IN P1                            
***********************************************************************         
         SPACE 1                                                                
         USING LIDELD,R4                                                        
         USING LLSRECD,R2                                                       
GETNLE   MVC   MYBYTE,0(R1)                                                     
         ICM   R5,B'0111',1(R1)                                                 
         LH    R4,0(,R5)                                                        
         LTR   R4,R4               IF NO DISPLACEMENT, SET TO 1ST ELEM          
         BZ    *+10                                                             
         AR    R4,R2               R4=A(CURRENT ELEMENT)                        
         B     GETNL10                                                          
         LA    R4,LLSRFST                                                       
         B     *+12                                                             
*                                                                               
GETNL10  SR    RF,RF                                                            
         IC    RF,LIDLN                                                         
         AR    R4,RF                                                            
         CLI   LIDEL,0             RECORD END?                                  
         BE    OVROU1L             YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   GETNL10             NO                                           
         CLC   LIDTYPE,MYBYTE      MATCH LIST TYPE                              
         BNE   GETNL10                                                          
         SR    R4,R2                                                            
         STH   R4,0(,R5)           SET DISPLACEMENT INTO RECORD                 
         LHI   RF,LIDDATA-LIDELD                                                
         STH   RF,2(,R5)           SET DISPLACEMENT INTO ELEMENT                
         B     OVROU1E                                                          
*                                                                               
         DROP  R2,R4                                                            
         EJECT ,                                                                
***********************************************************************         
* ROUTINE TO GET LIST ITEM                                            *         
*                                                                     *         
* NTRY    P1=LIST TYPE,(CURR DISP INTO RECORD/ELEMENT PAIR)                     
* EXIT    SET LIST ENTRY IN MYWORK                                              
***********************************************************************         
         SPACE 1                                                                
         USING LIDELD,R4                                                        
         USING LLSRECD,R2                                                       
GETLIT   ICM   R5,B'0111',1(R1)                                                 
         LH    R4,0(,R5)                                                        
         AR    R4,R2               R4=A(CURRENT LIST ELEMENT)                   
*                                                                               
         MVC   MYWORK,BCSPACES                                                  
         LH    RE,2(,R5)                                                        
         AR    RE,R4               RE=A(CURRENT LIST ENTRY INTO ELEM)           
         SR    RF,RF                                                            
         IC    RF,LIDITLN          LENGTH OF LIST ENTRY                         
         SHI   RF,1                                                             
         EXMVC RF,MYWORK,0(RE)                                                  
*                                                                               
         AHI   RF,1                                                             
         AR    RE,RF               POINTS TO NEXT LIST ENTRY                    
         SR    RE,R4               RE=CURR DISP INTO ELEMENT                    
         IC    RF,LIDLN            LENGHT OF ELEMENT                            
         CR    RE,RF               ANY MORE LIST ENTRY IN THIS ELEM?            
         BNL   *+12                NO - GET THE NEXT LIST ELEMENT               
         STH   RE,2(,R5)                                                        
         B     OVROU1E                                                          
*                                                                               
         GOTO1 GETNLE,(R1)         GET NEXT LIST ELEMENT                        
         BNE   OVROU1L                                                          
         B     OVROU1E                                                          
         DROP  R2,R4                                                            
         SPACE 2                                                                
***********************************************************************         
* CHECK SUPPLIER CODE IS VALID AND RETURN NAME                        *         
*                                                                     *         
* NTRY - P1  = SUPPLIER ACCOUNT CODE                                  *         
* EXIT - CC EQUAL = CODE IS VALID                                     *         
*      - CC NOT EQUAL = CODE IS INVALID                               *         
***********************************************************************         
         SPACE 1                                                                
VALSUP   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         MVC   SUPCODE,BCSPACES                                                 
         MVC   SUPNAME,BCSPACES                                                 
VALSUP02 CLI   1(R2),C'V'                                                       
         BE    VALSUP04                                                         
         CLI   NOSX,1                                                           
         BE    OVROU1L                                                          
         CLI   1(R2),C'X'                                                       
         BE    VALSUP06                                                         
         B     OVROU1L                                                          
*                                                                               
VALSUP04 CLI   2(R2),C' '         CHECK IF JUST SV                              
         BH    VALSUP08                                                         
         LA    RF,SUPCODE                                                       
         MVC   0(2,RF),0(R2)                                                    
         OI    0(RF),X'40'                                                      
         MVC   SUPNAME,LDGSVN                                                   
         B     OVROU1E                                                          
*                                                                               
VALSUP06 CLI   2(R2),C' '         CHECK IF JUST SX                              
         BH    VALSUP08                                                         
         LA    RF,SUPCODE                                                       
         MVC   0(2,RF),0(R2)                                                    
         OI    0(RF),X'40'                                                      
         MVC   SUPNAME,LDGSXN                                                   
         B     OVROU1E                                                          
*                                                                               
         USING ACTRECD,R5                                                       
VALSUP08 LA    R5,IOKEY                                                         
         MVC   ACTKEY,BCSPACES     READ NON CLIENT ACCOUNT RECORD               
         MVC   ACTKCPY,CUABIN      CONNECTED ID                                 
         MVC   ACTKUNT(L'BCCPYPRD),0(R2)                                        
         SHI   RE,3                                                             
         MVC   ACTKACT(0),2(R2)                                                 
         EX    RE,*-6                                                           
         OI    ACTKUNT,X'40'                                                    
         L     R1,=AL4(XOREAD+XOACCMST+XIO2)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                                                          
         GOTO1 AGETEL,BOPARM,('NAMELQ',AIO2),0                                  
         BE    *+6                                                              
         DC    H'0'                NAMEL MISSING                                
         MVC   SUPCODE,ACTKUNT                                                  
         DROP  R5                                                               
                                                                                
T        USING NAMELD,BOELEM                                                    
         XR    RF,RF                                                            
         IC    RF,T.NAMLN                                                       
         SHI   RF,NAMLN1Q+1                                                     
         MVC   SUPNAME(0),T.NAMEREC                                             
         EX    RF,*-6                                                           
         B     OVROU1E                                                          
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK DUPLICATED 1R ACCOUNT  EXIST ON ANOTHER APPROVER              *         
*                                                                     *         
* NTRY - MYBYTE = APPLICATION (SEE DPAPAPPL)                          *         
***********************************************************************         
T        USING DPAPASD,IOKEY                                                    
CDEF1R   ST    RE,SAVERE                                                        
         XC    T.DPAPAS,T.DPAPAS                                                
         MVI   T.DPAPTYP,DPAPTYPQ                                               
         MVI   T.DPAPSUB,DPAPSUBQ                                               
         MVC   T.DPAPCPY,CUABIN   CONNECTED ID                                  
         MVC   T.DPAPAPPL,MYBYTE  APPLICATION                                   
         MVC   T.DPAP1RAC,1(R2)   1R ACCOUNT CODE                               
         MVC   SVIOKEY,IOKEY                                                    
         L     R1,=AL4(XIO4+XOACCDIR+XOHI)                                      
         B     *+8                                                              
CDEF1R10 L     R1,=AL4(XIO4+XOACCDIR+XOSEQ)                                     
         GOTOR AIO                                                              
         CLC   SVIOKEY(DPAPPIDB-DPAPASD),IOKEY                                  
         BNE   CDEFOK                                                           
         CLC   SVPIDBIN,T.DPAPPIDB                                              
         BE    CDEF1R10                                                         
         MVC   MYWORK(L'DPAPPIDB),T.DPAPPIDB                                    
         B     CDEFERR             DEFAULT EXISTS FOR ANOTHER APPROVER          
*                                                                               
CDEFOK   L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* CHECK DUPLICATED SJ ACCOUNT  EXIST ON ANOTHER APPROVER              *         
*                                                                     *         
* NTRY - MYBYTE = APPLICATION (SEE DPAPAPPL)                          *         
***********************************************************************         
T        USING JOBPASD,IOKEY                                                    
CDEFSJ   ST    RE,SAVERE                                                        
         XC    T.JOBPAS,T.JOBPAS                                                
         MVI   T.JOBPTYP,JOBPTYPQ                                               
         MVI   T.JOBPSUB,JOBPSUBQ                                               
         MVC   T.JOBPCPY,CUABIN   CONNECTED ID                                  
         MVC   T.JOBPAPPL,MYBYTE  APPLICATION                                   
         MVC   T.JOBPJOB,1(R2)                                                  
         MVC   SVIOKEY,IOKEY                                                    
         L     R1,=AL4(XIO4+XOACCDIR+XOHI)                                      
         B     *+8                                                              
CDEFSJ10 L     R1,=AL4(XIO4+XOACCDIR+XOSEQ)                                     
         GOTOR AIO                                                              
         CLC   SVIOKEY(JOBPPIDB-JOBPASD),IOKEY                                  
         BNE   CDEFSJOK                                                         
         CLC   SVPIDBIN,T.JOBPPIDB                                              
         BE    CDEFSJ10                                                         
         CLI   T.JOBPAPPL,JOBPATIM ONLY ONE APPROVER FOR TIME                   
         BE    CDEFERR                                                          
         CLI   T.JOBPAPPL,JOBPAEXP ONLY ONE APPROVER FOR EXPENSES               
         BE    CDEFERR                                                          
         TM    T.JOBPSTAT,JOBPDFLT ONLY ONE DEFAULT APPROVER FOR ORDERS         
         BZ    CDEFSJ10                        ESTIMATES AND JOBS               
         B     CDEFERR             DEFAULT EXISTS FOR ANOTHER APPROVER          
*                                                                               
CDEFSJOK L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
CDEFERR  GOTOX ('GETPID',AGROUTS),T.JOBPPIDB                                    
         MVC   FVXTRA(L'SVPID),BCWORK                                           
         B     OVROU1L             DEFAULT EXISTS FOR ANOTHER APPROVER          
*                                                                               
         DROP  T                                                                
         EJECT ,                                                                
***********************************************************************         
* VALIDATE SCHEME CODE                                                *         
* ON NTRY P1 CONTAINS SCHEME CODE                                     *         
*         P2 CONTAINS LENGTH OF CODE                                  *         
***********************************************************************         
         SPACE 1                                                                
VALSCH   L     R2,0(R1)                                                         
         SR    RE,RE                                                            
         IC    RE,0(R1)                                                         
         MVC   SCHCODE,BCSPACES                                                 
         MVC   SCHENAM,BCSPACES                                                 
         XC    IOKEY,IOKEY                                                      
         USING SCHRECD,IOKEY                                                    
         MVI   SCHKTYP,SCHKTYPQ                                                 
         MVI   SCHKSUB,SCHKSUBQ                                                 
         MVC   SCHKCPY,CUABIN                                                   
         MVC   SCHKUNT(2),PRODUL                                                
         MVC   SCHKCODE,0(R2)                                                   
*                                                                               
VALS1    L     R1,=AL4(XIO4+XOACCDIR+XOREAD)                                    
         GOTOR AIO                                                              
         BNE   OVROU1L                     INVALID SCHEME                       
         MVC   SCHCODE,SCHKCODE            SAVE SCHEME CODE                     
         L     R1,=AL4(XIO4+XOACCMST+XOGET)                                     
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('SCHELQ',AIO4),0                  
         CLI   12(R1),0                                                         
         BNE   OVROU1E                     NO SCHELD ELEMENT NO NAME            
         L     RF,12(,R1)                                                       
         MVC   SCHENAM(L'SCHNAME),SCHNAME-SCHELD(RF) EXTRACT SCHEME             
         B     OVROU1E                               NAME                       
***********************************************************************         
* CHECK HIGHER OR LOWER LEVEL ACCOUNT EXISTS                          *         
* NTRY   P1= LASTCODE                                                 *         
*        P2= SUPPLIER CODE BEING ADDED                                *         
***********************************************************************         
CHKLVL   L     R2,0(R1)         R2=LASTCODE                                     
         L     R3,4(R1)         R3=TLKSUC                                       
*                                                                               
CHKLVL2  XR    R0,R0                                                            
         XR    R4,R4                                                            
         LHI   R5,LENSVLNQ      CHECK LASTCODE/TLKSUC HAVE                      
         CLC   0(L'SVUL,R2),0(R3) SAME UNIT/LEDGER                              
         BNE   OVROU1E                                                          
         LA    RF,L'SVUL(R2)                                                    
         CLI   0(RF),C' '       CHECK WHETHER LASTCODE JUST UNIT LEDGER         
         BE    CHKLVL8                                                          
         CLC   SVUL,0(R2)       CHECK LASTCODE SV OR SX LEDGER                  
         BNE   CHKLVL4                                                          
         LA    R4,LENSVA                                                        
         B     CHKLVL6                                                          
*                                                                               
CHKLVL4  CLC   SXUL,0(R2)                                                       
         BE    *+6                                                              
         DC    H'0'             MUST BE SV OR SX                                
         LA    R4,LENSXA                                                        
*                               FIND LEDGER LEVEL OF LASTCODE                   
CHKLVL6  LA    RF,L'SVUL(R2)    BY COMPARING LENGTH WITH LEDGER LENGTH          
         IC    R0,0(R4)                                                         
         CHI   R0,L'LASTCODE-2  IS LEDGER LENGTH=LENGTH OF LASTCODE             
         BE    CHKLVL8                                                          
         AR    RF,R0                                                            
         CLI   0(RF),C' '                                                       
         BE    CHKLVL8                                                          
         AHI   R4,L'LENSVA                                                      
         BCT   R5,CHKLVL6                                                       
*                                                                               
CHKLVL8  LHI   R5,LENSVLNQ      FIND LEDGER LEVEL OF TLKSUC                     
         XR    RE,RE                                                            
         LA    RF,L'SVUL(R3)                                                    
         CLI   0(RF),C' '       CHECK WHETHER TLKSUC IS UNIT LEDGER             
         BE    CHKLVL14                                                         
         CLC   SVUL,0(R3)       CHECK TLKSUC SV OR SX LEDGER                    
         BNE   CHKLVL10                                                         
         LA    RE,LENSVA                                                        
         B     CHKLVL12                                                         
*                                                                               
CHKLVL10 CLC   SXUL,0(R3)                                                       
         BE    *+6                                                              
         DC    H'0'             MUST BE SV OR SX                                
         LA    RE,LENSXA                                                        
*                               FIND TLKSUC LEDGER LEVEL                        
CHKLVL12 LA    RF,L'SVUL(R3)    BY COMPARING TLKSUC LENGTH WITH                 
         IC    R0,0(RE)         LEDGER LENGTH                                   
         CHI   R0,L'TLKSUC-2    CHECK LEDGER LENGTH=LENGTH TLKSUC               
         BE    CHKLVL14                                                         
         AR    RF,R0                                                            
         CLI   0(RF),C' '                                                       
         BE    CHKLVL14                                                         
         AHI   RE,L'LENSVA                                                      
         BCT   R5,CHKLVL12                                                      
*                                                                               
CHKLVL14 CR    R4,RE            CHECK LASTCODE/TLKSUP SAME LEVEL                
         BE    CHKLVLX                                                          
         XR    RE,RE            NOT SAME LEVEL                                  
         LA    RE,L'LASTCODE    COMPARE TLKSUC AND LASTCODE                     
         LA    RF,L'LASTCODE-1(R2)                                              
CHKLVL16 CLI   0(RF),C' '                                                       
         BNE   CHKLVL18                                                         
         SHI   RF,1                                                             
         BCT   RE,CHKLVL16      RE=ACTUAL LENGTH OF LASTCODE                    
         DC    H'0'                                                             
*                                                                               
CHKLVL18 LA    R5,L'TLKSUC                                                      
         LA    RF,L'TLKSUC-1(R3)                                                
*                                                                               
CHKLVL20 CLI   0(RF),C' '                                                       
         BNE   CHKLVL22                                                         
         SHI   RF,1                                                             
         BCT   R5,CHKLVL20      R5=ACTUAL LENGTH OF TLKSUC                      
         DC    H'0'                                                             
*                                                                               
CHKLVL22 CR    RE,R5            IS TLKSUC OR LASTCODE SHORTER?                  
         BH    CHKLVL24                                                         
         SHI   RE,1                                                             
         EXCLC RE,0(R2),0(R3)   CHECK WHETHER DIFFERENT HIGH LEVEL              
         BNE   CHKLVLX          ACCOUNT                                         
         B     CHKLVLL                                                          
*                                                                               
CHKLVL24 SHI   R5,1                                                             
         EXCLC RE,0(R2),0(R3)                                                   
         BNE   CHKLVLX                                                          
         B     CHKLVLL                                                          
*                                                                               
CHKLVLX  B     OVROU1E          SAME LENGTH                                     
*                                                                               
CHKLVLL  B     OVROU1L          NOT SAME LENGTH                                 
***********************************************************************         
* ADD LLS RECORD                                                      *         
*                                                                     *         
* NTRY - P1     = A(CURRENT LLS RECORD)                               *         
*      - BOELEM = CURRENT LIMIT LIST ELEMENT                          *         
* EXIT - ADDSEQ# : NEXT RECORD SEQUENCE NUMBER                        *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R2                                                       
ADDLLS   L     R2,0(R1)            A(CURRENCT APPROVER RECORD)                  
*                                                                               
         SR    RF,RF                                                            
         ICM   RF,3,LLSRLEN                                                     
         CHI   RF,IOMAXLNQ         GREATER MAX RECORD ALLOWED ?                 
         BNH   ADDLLS06            NO - ADD IT INTO CURRENT RECORD              
*                                                                               
         CLI   ADDSEQ#,0           MAIN LLS RECORD ?                            
         BE    ADDLLS02                                                         
         GOTO1 AUPDLLS             NO - ADD SAVED LLS REC IN AIO5               
*                                                                               
ADDLLS02 L     R2,AIO5             R2=A(NEW LLS SUB-RECORD)                     
         L     RF,AIOREC                                                        
         MVC   LLSKEY(LLSRLNK-LLSRECD),0(RF)                                    
         LLC   RF,ADDSEQ#                                                       
         AHI   RF,1                                                             
         STC   RF,ADDSEQ#          NEXT SEQUENCE NUMBER                         
         STC   RF,LLSKSEQ                                                       
*                                                                               
         MVC   IOKEY(L'LLSKEY),LLSKEY                                           
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO5)                                   
         GOTOR AIO                                                              
         BE    ADDLLS04                                                         
         TM    IOERR,IOEDEL                                                     
         BNZ   ADDLLS04                                                         
         TM    IOERR,IOERNF                                                     
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LA    RF,LLSRFST                                                       
         MVI   0(RF),0                                                          
         SR    RF,R2                                                            
         AHI   RF,1                                                             
         STCM  RF,3,LLSRLEN        LENGTH OF LLS RECORD                         
         MVC   LLSKSTA,GSRECSTA    COPY STATUS FROM MAIN RECORD                 
         MVI   RECFLAG,1                                                        
         MVC   IOKEY(L'LLSKEY),LLSKEY                                           
         B     ADDLLS06                                                         
*                                                                               
ADDLLS04 MVI   RECFLAG,0                                                        
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO5)                                  
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                BAD MASTER RECORD                            
*                                                                               
         L     R2,AIO5                                                          
         SR    RF,RF               CHECK ENOUGH ROMM ON THIS RECORD             
         ICM   RF,3,LLSRLEN                                                     
         CHI   RF,IOMAXLNQ                                                      
         BH    ADDLLS02            ELSE TRY NEXT                                
*                                                                               
ADDLLS06 MVC   LLSRSTA,GSRECSTA    COPY STATUS FROM MAIN RECORD                 
         GOTO1 AADDEL,BOPARM,(R2)  ADD NEW LIDEL ELEMENT                        
         BE    ADDLLS08                                                         
         MVC   FVMSGNO,=AL2(AE$TMILS)                                           
         B     OVROU1L             SOMETHING WRONG!!!                           
*                                                                               
ADDLLS08 CLI   ADDSEQ#,0           HAVE WE SEQUENTIALS?                         
         BE    OVROU1E                                                          
         GOTO1 AUPDLLS             THEN SAVE IT                                 
         B     OVROU1E                                                          
         DROP  R2                                                               
***********************************************************************         
* ROUTINE TO UPDATE LLS RECORD                                        *         
* ETRY - AIO5: A(CURRENT RECORD) TO BE UPDATED                        *         
***********************************************************************         
         SPACE 1                                                                
T        USING LLSRECD,R4                                                       
UPDLLS   DS    0H                                                               
         LA    R4,IOKEY                                                         
         L     RF,AIO5                                                          
         MVC   T.LLSKEY,0(RF)                                                   
         CLI   RECFLAG,0           EXISTING OR NEW RECORD?                      
         BNE   UPDLLS04                                                         
*                                                                               
         L     R1,=AL4(XORDUPD+XOACCDIR+XIO5)                                   
         GOTOR AIO                                                              
         BE    UPDLLS02            REREAD FOR UPDATE (AVOID OCCASIONAL          
         TM    IOERR,IOEDEL        DUMP)                                        
         BNZ   UPDLLS02                                                         
         DC    H'0'                                                             
*                                                                               
UPDLLS02 L     R0,AIO7             MOVE RECORD ASIDE                            
         LA    R1,IOAREALN                                                      
         L     RE,AIO5                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         L     R1,=AL4(XOGETRUP+XOACCMST+XIO5)                                  
         GOTOR AIO                                                              
         BE    *+6                 REREAD FOR UPDATE (AVOID OCCASIONAL          
         DC    H'0'                DUMP)                                        
         L     R0,AIO5             AND MOVE BACK IN FOR UPDATE                  
         LA    R1,IOAREALN                                                      
         L     RE,AIO7                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
*                                                                               
         MVI   RECFLAG,0                                                        
*                                                                               
         L     R1,=AL4(XOPUTREC+XOACCMST+XIO5) CHANGE LLS RECORD                
         GOTOR AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         NI    T.LLSKSTAT,FF-LLSSDELT    CHANGE NON DELETED LLS DIR             
         L     R1,=AL4(XOWRITE+XOACCDIR+XIO5)                                   
         GOTOR AIO                                                              
         BE    UPDLLS06                                                         
         DC    H'0'                                                             
*                                                                               
UPDLLS04 L     R1,=AL4(XOADDREC+XOACCMST+XIO5) ADD DIR + FILE RECORDS           
         GOTOR AIO                                                              
         BE    UPDLLS06                                                         
         DC    H'0'                                                             
*                                                                               
UPDLLS06 L     RF,AIOREC           RESTORE IOADDR FOR CONTROLLER                
         ST    RF,IOADDR                                                        
         B     OVROU1E                                                          
         DROP  T                                                                
***********************************************************************         
* ROUTINE TO READ THE NEXT LLS RECORD                                 *         
* EXIT - READSEQ# : NEXT RECORD SEQUENCE NUMBER                       *         
*      - MNTDISP: DISPLACEMENT TO THE FIRST ELEMENT                   *         
***********************************************************************         
         SPACE 1                                                                
T        USING LLSRECD,IOKEY                                                    
READNXT  DS    0H                                                               
         L     RF,AIOREC           A(CURRENCT LLS RECORD)                       
         MVC   T.LLSKEY,0(RF)                                                   
READN02  LLC   RF,READSEQ#                                                      
         AHI   RF,1                                                             
         STC   RF,READSEQ#                                                      
         STC   RF,T.LLSKSEQ                                                     
         L     R1,=AL4(XORDD+XOACCMST+XIO5)                                     
         GOTOR AIO                                                              
         BE    READN04                                                          
         TM    IOERR,IOEDEL                                                     
         BNZ   READN02                                                          
         B     OVROU1L                                                          
READN04  LA    RF,LLSRFST-LLSRECD                                               
         STH   RF,MNTDISP                                                       
         B     OVROU1E                                                          
         DROP  T                                                                
***********************************************************************         
* READ NEXT LIMLIST RECORD                                            *         
*                                                                     *         
* NTRY - P1  = A(MAIN LIMLIST RECORD)                                 *         
* EXIT - IO5      : NEXT LIMLIST RECORD                               *         
*      - READSEQ# : NEXT RECORD SEQUENCE NUMBER                       *         
***********************************************************************         
         SPACE 1                                                                
T        USING LLSRECD,IOKEY                                                    
NXTLLS   L     RF,0(R1)            R1=A(CURRENCT LLS RECORD)                    
         MVC   T.LLSKEY,0(RF)                                                   
NXTLLS02 LLC   RF,READSEQ#                                                      
         AHI   RF,1                                                             
         STC   RF,READSEQ#                                                      
         STC   RF,T.LLSKSEQ                                                     
         L     R1,=AL4(XOHID+XOACCMST+XIO5)                                     
         GOTOR AIO                                                              
         BE    NXTLLS04                                                         
         TM    IOERR,IOEDEL                                                     
         BZ    OVROU1L                                                          
         B     NXTLLS02                                                         
NXTLLS04 CLC   IOKEY(LLSKSEQ-LLSRECD),IOKEYSAV                                  
         BE    OVROU1E                                                          
         B     OVROU1L                                                          
         DROP  T                                                                
***********************************************************************         
* CODE TO ADD RSTELD DATA IF MISSING FROM RECORD                      *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R2                                                       
RSTADD   LR    R2,R1                                                            
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',LLSRECD),0               
         CLI   12(R1),0            RSTEL ON RECORD?                             
         BE    OVROU1E                                                          
         GOTO1 AADDRST,LLSRECD     ADD A RSTEL IF IT DOESN`T EXIST              
         BNE   OVROU1L             SOMETHING WRONG                              
*                                                                               
         USING RSTELD,R5                                                        
         GOTO1 VHELLO,BOPARM,(C'G',GCFILNAM),('RSTELQ',LLSRECD),0               
         L     R5,12(R1)                                                        
         MVC   RSTBDATE,BCTODAYP   SET TO TODAY                                 
         MVC   RSTTDATE,BCTODAYP                                                
         MVI   RSTACST1,0                                                       
         MVI   RSTACST2,0                                                       
         CLI   CSACT,A#ADD                                                      
         BE    RSTADD2                                                          
         CLI   CSACT,A#CPY                                                      
         BNE   RSTADDX                                                          
*                                                                               
RSTADD2  OC    LLSKOFF,LLSKOFF     ANY OFFICE (GROUP)?                          
         BZ    RSTADD4                                                          
         USING OFFRECD,RE                                                       
         LA    RE,IOKEY                                                         
         XC    OFFKEY,OFFKEY                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,LLSKOFF                                                  
         LHI   R1,XOREAD+XOACCMST+XIO9                                          
         GOTO1 AIO                                                              
         BNE   RSTADD4                                                          
         GOTO1 VHELLO,BOPARM,(C'G',=C'ACCMST'),('CPXELQ',AIO9),0                
         CLI   12(R1),0                                                         
         BE    RSTADD6                                                          
         DROP  RE                                                               
*                                                                               
         USING CPYRECD,RE                                                       
RSTADD4  LA    RE,IOKEY                                                         
         MVC   CPYKEY,BCSPACES                                                  
         MVC   CPYKCPY,CUABIN                                                   
         LHI   R1,XOREAD+XOACCMST+XIO9                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         GOTO1 VHELLO,BOPARM,(C'G',=C'ACCMST'),('CPXELQ',AIO9),0                
         CLI   12(R1),0                                                         
         BNE   RSTADDX                                                          
         DROP  RE                                                               
*                                                                               
         USING CPXELD,R1                                                        
RSTADD6  L     R1,12(R1)                                                        
         TM    CPXSTAT3,CPXAJOBS   SET EACH FLAG DEPENDING ON SET UP            
         BZ    *+8                                                              
         OI    RSTACST1,RSTAJOBS                                                
         TM    CPXSTAT3,CPXAMED                                                 
         BZ    *+8                                                              
         OI    RSTACST1,RSTAMED                                                 
         TM    CPXSTAT3,CPXAETYP                                                
         BZ    *+8                                                              
         OI    RSTACST1,RSTAETYP                                                
         TM    CPXSTAT3,CPXA1NAC                                                
         BZ    *+8                                                              
         OI    RSTACST1,RSTA1NAC                                                
         TM    CPXSTAT3,CPXASTAF                                                
         BZ    *+8                                                              
         OI    RSTACST1,RSTASTAF                                                
         TM    CPXSTAT3,CPXAWC                                                  
         BZ    *+8                                                              
         OI    RSTACST1,RSTAWC                                                  
         TM    CPXSTAT3,CPXAREPF                                                
         BZ    *+8                                                              
         OI    RSTACST1,RSTAREPF                                                
         TM    CPXSTAT3,CPXASCHM                                                
         BZ    *+8                                                              
         OI    RSTACST1,RSTASCHM                                                
         TM    CPXSTAT4,CPXASUPP                                                
         BZ    *+8                                                              
         OI    RSTACST2,RSTASUPP                                                
*                                                                               
RSTADDX  B     OVROU1E                                                          
         DROP  R1,R2,R5                                                         
***********************************************************************         
* CHECK OFFICE CODE FOR THE PID IF AGENCY IS ON LIMIT ACCESSS OFFICE  *         
*                                                                     *         
* NTRY - P1  = PERSON CODE                                            *         
* EXIT - CC EQUAL = OK                                                *         
*      - CC NOT EQUAL = SECURITY LOCK                                 *         
***********************************************************************         
         SPACE 1                                                                
T        USING PIDRECD,IOKEY                                                    
CHKPID   L     R4,0(R1)                                                         
         XC    BCFULL,BCFULL                                                    
         MVC   SVIOKEY,IOKEY       SAVE IOKEY                                   
         XC    T.PIDKEY,T.PIDKEY   BUILD KEY TO READ                            
         MVI   T.PIDKTYP,PIDKTYPQ                                               
         MVI   T.PIDKSUB,PIDKSUBQ                                               
         MVC   T.PIDKCPY,CUABIN                                                 
         MVC   T.PIDKPID,0(R4)                                                  
         MVI   T.PIDKSTYP,PIDKPERQ                                              
         LHI   R1,XOHIGH+XOACCMST+XIO2                                          
         GOTO1 AIO                                                              
*&&UK*&& BNE   CHKPIDL                                                          
*&&US*&& BNE   CHKPIDE                                                          
         CLC   T.PIDKEY(PIDKPER-PIDRECD),IOKEYSAV                               
*&&UK*&& BNE   CHKPIDL                                                          
*&&US*&& BNE   CHKPIDE                                                          
         DROP  T                                                                
*                                                                               
         L     R4,AIO2                                                          
         LA    R4,PERRFST-PERRECD(R4)                                           
         USING LOCELD,R4                                                        
         SR    R0,R0                                                            
         B     *+10                                                             
CHKPID04 IC    R0,LOCLN                                                         
         AR    R4,R0                                                            
         CLI   LOCEL,0             END OF RECORD                                
*&&UK*&& BE    CHKPIDL             ACTIVE LOCATION NOT FOUND                    
*&&US*&& BE    CHKPID06                                                         
         CLI   LOCEL,LOCELQ                                                     
         BNE   CHKPID04                                                         
*&&US                                                                           
         ST    R4,BCFULL           SAVE ELEM ADDR AND KEEP READING              
         B     CHKPID04                                                         
*        CLI   LOCSTAT,LOCSTRM     TEST TERMINATED                              
*        BE    CHKPID06            NO - GET NEXT                                
*        CLI   LOCSTAT,LOCSLOA     TEST LOA                                     
*        BE    CHKPID06            NO - GET NEXT                                
*&&                                                                             
         CLI   LOCSTAT,LOCSACT     TEST ACTIVE LOCATION                         
         BNE   CHKPID04            NO - GET NEXT                                
CHKPID06 DS    0H                                                               
*&&US                                                                           
         ICM   R4,15,BCFULL        LOAD MOST CURRENT LOCATION ELEM              
         BZ    CHKPIDL             EXIT IF NO LOC FOUND                         
*&&                                                                             
         GOTO1 ATSTOFF,LOCOFF      TEST OFFICE                                  
         BE    CHKPIDE                                                          
*                                                                               
CHKPIDL  MVC   FVXTRA,BCSPACES                                                  
         XC    FVMSGNO,FVMSGNO                                                  
         MVC   IOKEY,SVIOKEY                                                    
         B     OVROU1L                                                          
*                                                                               
CHKPIDE  MVC   IOKEY,SVIOKEY                                                    
         B     OVROU1E                                                          
         DROP  R4                                                               
         EJECT ,                                                                
*&&UK                                                                           
***********************************************************************         
* UPDATE LIMLIST PASSIVES                                             *         
*                                                                     *         
* NTRY - P1  = LIMLIST RECORD                                         *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R3                                                       
ADDPAS   L     R3,0(R1)                                                         
*                                                                               
         CLI   LLSKSEQ,0                                                        
         BNE   ADDPAS10            NO - IODA HAS ALREADY BEEN SET               
         MVC   IOKEY,0(R3)         GET IODA FOR PADDLE                          
         LHI   R1,XOREAD+XOACCDIR+XIO2                                          
         GOTO1 AIO                                                              
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
ADDPAS10 XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'A',(R3)),(C'A',T.CPTRBLK),IODA,0,ACOM          
         B     OVROU1E                                                          
         DROP  T,R3                                                             
         EJECT ,                                                                
***********************************************************************         
* DELETE LIMIST PASSIVES                                              *         
*                                                                     *         
* NTRY - P1  = A(LLSREC)                                              *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R3                                                       
DELPAS   L     R3,0(R1)            GET RECORD ADDRESS                           
*                                                                               
T        USING CPTRBLK,CPTRWRK                                                  
         XC    T.CPTRBLK(CPTRBLKL),T.CPTRBLK                                    
         GOTO1 VPADDLE,BODMCB,(C'D',(R3)),(C'K',T.CPTRBLK),0,0,ACOM             
         B     OVROU1E                                                          
         DROP  T,R3                                                             
         EJECT ,                                                                
*&&                                                                             
***********************************************************************         
* CODE FROM RFADD MOVED HERE FOR ADDRESSIBILITY                       *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R2                                                       
MYRFADD  CLI   CSACT,A#ADD                                                      
         BNE   MYRFA02                                                          
         CLI   GSSMPAGE,1          PAGE 1?                                      
         BNE   OVROU1H             NO - INVALID ACTION FOR THIS SCREEN          
         OC    LLSKGRP,LLSKGRP     DOES ADD CONTAIN GROUP?                      
         BZ    MYRFA20             NO                                           
         MVC   FVMSGNO,=AL2(AE$UGLPD) USE GRPLIST TO ADD PID TO GROUP           
         B     OVROU1L                                                          
*                                                                               
MYRFA02  CLI   CSACT,A#CPY         IS IT COPY                                   
         BNE   MYRFA20             NO - CHECK RECORD ISN'T AS GROUP             
         OC    LLSKGRP,LLSKGRP     DOES COPY CONTAIN GROUP?                     
         BZ    MYRFA04             NO                                           
         MVC   FVMSGNO,=AL2(AE$UGLPD) USE GRPLIST TO ADD PID TO GROUP           
         B     OVROU1L                                                          
*                                                                               
MYRFA04  LA    R5,LLSRFST          START OF RECORD ELEMENTS                     
         XR    RE,RE                                                            
         USING LIDELD,R5                                                        
MYRFA06  CLI   LIDEL,0             RECORD END?                                  
         BE    MYRFA18             YES                                          
         CLI   LIDEL,NAMELQ        NAMEL?                                       
         BE    MYRFA08             YES                                          
         CLI   LIDEL,LIDELQ        LIDEL?                                       
         BNE   MYRFA16             NO                                           
         CLI   LIDTYPE,LIDTPID     IS IT PID LIST                               
         BNE   MYRFA16                                                          
MYRFA08  MVI   LIDEL,X'FF'         SET FOR DELETE                               
*                                                                               
MYRFA16  IC    RE,LIDLN            GET NEXT ELEMENT                             
         LA    R5,0(RE,R5)                                                      
         B     MYRFA06                                                          
*                                                                               
MYRFA18  GOTO1 VHELLO,BOPARM,(C'D',GCFILNAM),(X'FF',LLSRECD),0                  
*                                                                               
PAS      USING LLSRECD,R4                                                       
MYRFA20  LA    R4,IOKEY                                                         
         XC    PAS.LLSKEY,PAS.LLSKEY                                            
         MVC   PAS.LLSKPIDB,LLSKPIDB                                            
         MVI   PAS.LLSKTYP,LLSKTYPQ                                             
         MVI   PAS.LLSKSUB,LLSKSUBQ                                             
         MVC   PAS.LLSKCPY,CUABIN      CONNECTED ID                             
         MVC   SVIOKEY,PAS.LLSKEY                                               
         L     R1,=AL4(XOHI+XOACCDIR+XIO2)                                      
         GOTOR AIO                                                              
         CLC   SVIOKEY(LLSKGRP-LLSKEY),PAS.LLSKEY                               
         BNE   OVROU1E                                                          
         OC    PAS.LLSKGRP,PAS.LLSKGRP                                          
         BZ    OVROU1E                                                          
         MVC   FVMSGNO,=AL2(AE$PIDGP) PID EXISTS ON A GRPLIST RECORD            
         B     OVROU1L                                                          
         DROP  PAS                                                              
         DROP  R2                                                               
         SPACE 1                                                                
***********************************************************************         
* CODE FROM RFWRT MOVED HERE FOR ADDRESSIBILITY                       *         
***********************************************************************         
         SPACE 1                                                                
         USING LLSRECD,R2                                                       
MYRFWRT  CLI   LLSKSUB,LLSKSUBQ                                                 
         BE    OVROU1E                                                          
         MVC   FVMSGNO,=AL2(AE$MUGLS) MAINTAIN THROUGH GRPLIST                  
         NI    LSLTIND1,FF-LSLTIBLD                                             
         XC    GCLASKEY,GCLASKEY                                                
         NI    GSINDSL1,FF-GSIXMNT                                              
         B     OVROU1L                                                          
         DROP  R2                                                               
         EJECT                                                                  
***********************************************************************         
* INCLUDED BOOKS                                                      *         
***********************************************************************         
         SPACE 1                                                                
* ACFILWORK                                                                     
         PRINT OFF                                                              
*                                                                               
* NEEDS NEW ACFILWORK TO HAVE NEW ACGENFILEC                                    
*                                                                               
       ++INCLUDE ACFILWORK                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* SEACSFILE                                                                     
         PRINT OFF                                                              
       ++INCLUDE SEACSFILE                                                      
         PRINT ON                                                               
         SPACE 2                                                                
* ACSCRDSECT                   - FOR STYELD IN SCRIBE RECORDS                   
         PRINT OFF                                                              
       ++INCLUDE ACSCRDSECT                                                     
         PRINT ON                                                               
         SPACE 2                                                                
         EJECT ,                                                                
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
SAVERE   DS    A                  SAVED RE                                      
ACURIO   DS    A                  CURRENT IO AREA FOR SEQUENTIALS               
*                                                                               
OVROUT1  DS    0A                                                               
AVALCPJ  DS    A                  A(VALCPJ)                                     
AVALEXP  DS    A                  A(VALEXP)                                     
AVALWC   DS    A                  A(VALWC)                                      
AVALNCL  DS    A                  A(VALNCL)                                     
AVALMED  DS    A                  A(VALMED)                                     
AVALC1R  DS    A                  A(VALC1R)                                     
AVALFORM DS    A                  A(VALFORM)                                    
ADOFLT   DS    A                  A(DOFLT)                                      
AGETNLE  DS    A                  A(GETNLE)                                     
AGETLIT  DS    A                  A(GETLIT)                                     
AVALSUP  DS    A                  A(VALSUP)                                     
AVALSCH  DS    A                  A(VALSCH)                                     
ACHKLVL  DS    A                  A(CHKLVL)                                     
AADDLLS  DS    A                  A(ADDLLS)                                     
AUPDLLS  DS    A                  A(UPDLLS)                                     
AREADNXT DS    A                  A(READNXT)                                    
ANXTLLS  DS    A                  A(NXTLLS)                                     
AMYRFADD DS    A                  A(MYRFADD)                                    
AMYRFWRT DS    A                  A(MYRFWRT)                                    
ARSTADD  DS    A                  A(ARSTADD                                     
ACHKPID  DS    A                  A(CHKPID)                                     
AADDPAS  DS    A                  A(ADDPAS)                                     
ADELPAS  DS    A                  A(DELPAS)                                     
OVROUT1N EQU   (*-OVROUT1)/L'OVROUT1                                            
         SPACE 1                                                                
APIDFLD  DS    A                   A(PID FIELD)                                 
ACPJFLD  DS    A                   A(CLIENT FIELD)                              
ANCLFLD  DS    A                   A(NON-CLIENT FIELD)                          
AWRKFLD  DS    A                   A(WORD CODE FIELD)                           
AEXPFLD  DS    A                   A(EXPENDITURE FIELD)                         
AMEDFLD  DS    A                   A(MEDIA FIELD)                               
A1RAFLD  DS    A                   A(1R ACCOUNT FIELD)                          
AFORFLD  DS    A                   A(FORMAT CODE FIELD)                         
*                                                                               
MNTDISPD DS    0H                  CURRENT DISPLACEMENT INTO RECORD/            
MNTCPJ   DS    H                   ELEMENT PAIR                                 
CURCPJ   DS    H                                                                
MNTEXPD  DS    H                                                                
CUREXP   DS    H                                                                
MNTWC    DS    H                                                                
CURWC    DS    H                                                                
MNTNCLC  DS    H                                                                
CURNCLC  DS    H                                                                
MNTMED   DS    H                                                                
CURMED   DS    H                                                                
MNTAP1R  DS    H                                                                
CURAP1R  DS    H                                                                
MNTSCRC  DS    H                                                                
CURSCRC  DS    H                                                                
MNTESCH  DS    H                                                                
CURESCH  DS    H                                                                
MNTSUPP  DS    H                                                                
CURSUP   DS    H                                                                
MNTDISPL EQU   *-MNTDISPD                                                       
*                                                                               
MNTDISP  DS    H                   MOVE TO SAVED STORAGE                        
CURDISP  DS    H                   CURRENT DISPLACENT INTO ELEMENT              
DATALEN  DS    H                   LENGTH OF DATA                               
TOTELLN  DS    H                   TOTAL LENGTH OF ELEMENT                      
*                                                                               
LLSSAVDA DS    XL4                                                              
READSEQ# DS    XL(L'LLSKSEQ)                                                    
ADDSEQ#  DS    XL(L'LLSKSEQ)                                                    
RECFLAG  DS    XL1                                                              
LSRIND   DS    XL1                                                              
SVIOKEY  DS    XL42                                                             
SESNL    DS    XL1                                                              
ANYLINES DS    CL1                                                              
ANYERROR DS    CL1                                                              
MYWORK   DS    XL64                                                             
POFFICE  DS    CL2                                                              
CURAPPLS DS    XL2                                                              
*                                                                               
DSLISTU  DS    0F                  DDICT ENTRIES                                
UC@SRCV  DS    CL6                                                              
UC@SINC  DS    CL6                                                              
UC@PAY   DS    CL6                                                              
UC@EXP   DS    CL6                                                              
UC@497   DS    CL6                                                              
UC@498   DS    CL6                                                              
UC@540   DS    CL6                                                              
UC@544   DS    CL6                                                              
UC@GLG   DS    CL6                                                              
UC@MED   DS    CL6                                                              
UC@FI    DS    CL6'FI'                                                          
UC@M2    DS    CL6'M2'                                                          
AC@SUP   DS    CL8                                                              
*                                                                               
MYBYTE   DS    XL1                                                              
MYBYTE2  DS    XL1                                                              
MYBYTE3  DS    XL1                                                              
LIMINDS  DS    XL1                                                              
LIMIFCPJ EQU   X'80'               CLI/PRO/JOB CODE FOUND                       
LIMIFETY EQU   X'40'               EXPENDITURE CODE FOUND                       
LIMIFWRK EQU   X'20'               WORK CODE FOUND                              
LIMIFNCL EQU   X'10'               NON CLIENT CODE FOUND                        
LIMIFMED EQU   X'08'               MEDIA FOUND                                  
LIMIF1RA EQU   X'04'               1R ACCOUNT CODE FOUND                        
LIMIFFOR EQU   X'02'               SCRIBE FORMAT CODE FOUND                     
LIMIFSCH EQU   X'01'               SCHEME CODE FOUND                            
LIMIND2  DS    XL1                 SECOND SEARCH FILTER INDICATOR               
LIMIFSUC EQU   X'80'               SUPPLIER ACCOUNT CODE FOUND                  
CLILEN   DS    XL1                 LENGTH OF CLIENT                             
PROLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT                     
JOBLEN   DS    XL1                 LENGTH OF CLIENT+PRODUCT+JOB                 
L11RLEN  DS    XL1                 LENGTH OF 1R LEVEL 1                         
L21RLEN  DS    XL1                 LENGTH OF 1R LEVEL 2                         
L31RLEN  DS    XL1                 LENGTH OF 1R LEVEL 3                         
L41RLEN  DS    XL1                 LENGTH OF 1R LEVEL 4                         
TOTNAML  DS    XL1                 LENGTH OF CLIENT NAME                        
NOSX     DS    XL1                 NO SX LEDGER                                 
*                                                                               
ACONST   DS    0X                 CONSTANTS                                     
EXTYP    DS    CL2                C'ET'                                         
WC       DS    CL2                C'WC'                                         
PRODUL   DS    CL2                C'SJ'                                         
MED      DS    CL5                C'MEDIA'                                      
AC1R     DS    CL2                C'1R'                                         
NCTUL    DS    CL2                C'1N'                                         
SVUL     DS    CL2                C'SV'                                         
SXUL     DS    CL2                C'SX'                                         
NONOS    DS    CL9                C'NNNNNNNNN'                                  
         DS    CL30               SPARE                                         
*                                                                               
*&&UK                                                                           
CPTRWRK  DS    XL128                                                            
*&&                                                                             
OVERWRKN EQU   *-OVERWRKD                                                       
         EJECT ,                                                                
***********************************************************************         
* DSECT                                                               *         
***********************************************************************         
         SPACE 2                                                                
SAVED    DSECT                                                                  
SVPID    DS    CL(L'SAPALPID)      8 CHARACTER PERSONAL ID                      
SVGRPC   DS    CL(L'GLSKGRP)       8 CHARACTER GROUP CODE                       
SVGRPCDE DS    CL(L'GLSKGRP)       8 GROUP CODE                                 
SVPIDBIN DS    XL(L'SAPWDNUM)      2 BYTE BINARY PERSONAL ID                    
SVPIDLNM DS    CL(L'SANAME)        PERSONAL ID LAST NAME                        
SVPIDMNM DS    CL(L'SANAME)        PERSONAL ID MIDDLE NAME                      
SVPIDFNM DS    CL(L'SANAME)        PERSONAL ID FIRST NAME                       
CPJINDS  DS    XL1                                                              
CPJIVCPJ EQU   X'40'               VALCPJ HAS BEEN CALLED                       
CPJINCLI EQU   X'80'               NOT CLIENT                                   
CPJICCPJ EQU   X'20'               CLI/PRO/JOB HAS BEEN CHANGED                 
CPJNAME  DS    CL(NAMFLDLQ)        CLI/PRO/JOB NAME                             
CPJCODE  DS    CL(L'ACTKACT)       CLI/PRO/JOB CODE                             
CPJOFF   DS    CL(L'PPRGAOFF)      CLI/PRO/JOB EFFECTIVE OFFICE                 
CPJCOFF  DS    CL200               CLI/PRO/JOB CLIENT OFFICES                   
EXPCODE  DS    CL(L'ETYKCODE)      EXPENDITURE TYPE CODE                        
EXPNAME  DS    CL(L'NAMEREC)       EXPENDITURE TYPE NAME                        
WRKCODE  DS    CL(L'WCOKWRK)       WORK CODE                                    
WRKNAME  DS    CL(L'NAMEREC)       WORK CODE NAME                               
WRKDESC  DS    CL(L'WCODESC)       WORK CODE DESCRIPTION                        
NCLCODE  DS    CL(L'ACTKACT)       NON CLIENT ACCOUNT CODE                      
NCLNAME  DS    CL(L'NAMEREC)       NON CLIENT ACCOUNT NAME                      
MEDCODE  DS    CL(L'PMDKMED)       MEDIA CODE                                   
MEDNAME  DS    CL(L'PMDDESC)       MEDIA NAME                                   
C1RCODE  DS    CL(L'ACTKACT)       COSTING 1R ACCOUNT CODE                      
C1RNAME  DS    CL(L'NAMEREC)       COSTING 1R ACCOUNT NAME                      
FORMCODE DS    CL(L'RESKFORM)      SCRIBE FORMAT CODE                           
FORMNAME DS    CL(L'NAMEREC)       SCRIBE FORMAT NAME                           
REPCODE  DS    CL1                 SCRIBE REPORT TYPE                           
TRANTYPE DS    CL(L'ACTKACT)       SCRIBE TRANSMISSION TYPE                     
SCHCODE  DS    CL8                 SCHEME CODE                                  
SCHENAM  DS    CL(L'CADTNAM)       SCHEME NAME                                  
SUPCODE  DS    CL(L'ACTKACT+2)     SV OR SX SUPPLIER ACCOUNT CODE               
SUPNAME  DS    CL(L'NAMEREC)       SUPPLIER ACCOUNT NAME                        
*                                                                               
DWNINDS  DS    XL1                 DOWNLOAD INDICATOR                           
DWNGDATA EQU   X'80'               GET DATA INTO TSAR                           
DWNNOALL EQU   X'7F'               GET ALL RECORDS                              
DWNNOCPJ EQU   X'40'                                                            
DWNNOETC EQU   X'20'               NO MORE EXPENDITURE CODE                     
DWNNOWC  EQU   X'10'               NO MORE WORK CODE                            
DWNNONCC EQU   X'08'               NO MORE NON-CLIENT CODE                      
DWNNOMC  EQU   X'04'               NO MORE MEDIA CODE                           
DWNNOC1R EQU   X'02'               NO MORE COSTING 1R CODE                      
DWNNOSCR EQU   X'01'               NO MORE SCRIBE FORMAT CODE                   
DWNIND2  DS    XL1                 SECOND DOWNLOAD INDICATOR                    
DWNGDAT2 EQU   DWNGDATA            GET DATA INTO TSAR                           
DWNNOAL2 EQU   X'60'               GET ALL RECORDS                              
DWNNOSCH EQU   X'40'               NO ESTIMATE SCHEME                           
DWNNOSUP EQU   X'20'               NO SUPPLIER CODE                             
*                                                                               
ERRIND   DS    XL1                 ERROR INDICATOR                              
EREOFIN  EQU   X'40'               ERROR EXISTING OFFICE IS INVALID             
ERCOFIN  EQU   X'20'               ERROR CURRENT OFFICE IS INVALID              
*                                                                               
*                                                                               
SVFLTS   DS    0F                                                               
SVPIDFL  DS    CL8                 CHARACTER PID                                
SVPIDFLN DS    CL1                 LENGTH OF CHARACTER PID                      
SVCPJXLN DS    XL1                 LENGTH OF INPUT CLI/PRO/JOB - 1              
SVCPJ    DS    CL(L'ACTKACT)       FILTER ON CLI/PRO/JOB                        
SVETY    DS    CL(L'ETYKCODE)      FILTER ON EXPENDITURE TYPE CODE              
SVWC     DS    CL(L'WCOKWRK)       FILTER ON WORKCODE                           
SVNCL    DS    CL(L'ACTKACT)       FILTER ON NON CLIENT ACCOUNT CODE            
SVMED    DS    CL(L'PMDKMED)       FILTER ON MEDIA CODE                         
SV1RA    DS    CL(L'ACTKACT)       FILTER ON 1R ACCOUNT CODE                    
SVFORM   DS    CL(L'RESKFORM)      FILTER ON FORMAT CODE                        
SVFSUC   DS    CL(L'TLKSUC)        FILTER ON SUPPLIER CODE                      
SVFSCH   DS    CL(L'SCHKCODE)      FILTER ON SCHEME CODE                        
SVFLTLQ  EQU   *-SVFLTS                                                         
*                                                                               
LENSVA   DS    XL1                 LENGTH OF LEVEL 1 FOR SV                     
LENSVB   DS    XL1                 LENGTH OF LEVEL 2 FOR SV                     
LENSVC   DS    XL1                 LENGTH OF LEVEL 3 FOR SV                     
LENSVD   DS    XL1                 LENGTH OF LEVEL 4 FOR SV                     
LENSVLNQ EQU  *-LENSVA                                                          
LDGSVN   DS    CL(L'NAMEREC)       NAME OF SV LEDGER                            
LENSXA   DS    XL1                 LENGTH OF LEVEL 1 FOR SX                     
LENSXB   DS    XL1                 LENGTH OF LEVEL 2 FOR SX                     
LENSXC   DS    XL1                 LENGTH OF LEVEL 3 FOR SX                     
LENSXD   DS    XL1                 LENGTH OF LEVEL 4 FOR SX                     
LENSXLNQ EQU   *-LENSXA                                                         
LDGSXN   DS    CL(L'NAMEREC)       NAME OF SX LEDGER                            
*                                                                               
LASTCODE DS    CL12                PREVIOUS CLI/PRO/JOB OR 1R CODE              
LASTOFF  DS    CL2                 PREVIOUS OFFICE ON CLI/PRO/JOB PAGE          
SAVUL    DS    CL(L'SVUL)          SAVED UNIT/LEDGER CODE                       
         EJECT ,                                                                
***********************************************************************         
* REPORT TYPE DSECT                                                   *         
***********************************************************************         
         SPACE 2                                                                
REPTYPD  DSECT                                                                  
REPTYLLN DS    XL1                 LENGTH                                       
REPNAME  DS    CL2                 REPORT TYPE NAME ADDRESS                     
REPCODES DS    0X                  CODES FOR WHICH NAME APPLIES                 
REP#RCV  EQU   C'R'                  RECEIVABLES REPORT          (SR)           
REP#ADV  EQU   C'A'                  ADVANCES REPORT             (SA)           
REP#BAL  EQU   C'B'                  BALANCE SHEET/MISC. REPORT  (SB)           
*                                                                               
REP#INC  EQU   C'I'                  INCOME REPORT               (SI)           
REP#SUP  EQU   C'K'                  INCOME SUSPENDED            (SK)           
REP#ICST EQU   C'1'                  INCOME COST REPORT          (1C)           
*                                                                               
REP#PAY  EQU   C'P'                  PAYABLES                    (SP)           
REP#PAYQ EQU   C'Q'                  PAYABLES                    (SQ)           
REP#PAYS EQU   C'S'                  PAYABLES                    (SS)           
REP#PAYT EQU   C'T'                  PAYABLES                    (ST)           
REP#PAYU EQU   C'U'                  PAYABLES                    (SU)           
REP#PAYV EQU   C'V'                  PAYABLES                    (SV)           
REP#PAYW EQU   C'W'                  PAYABLES                    (SW)           
REP#PAYX EQU   C'X'                  PAYABLES                    (SX)           
REP#PAYY EQU   C'Y'                  PAYABLES                    (SY)           
REP#PAYC EQU   C'C'                  PAYABLES                    (2C)           
REP#PAYF EQU   C'F'                  PAYABLES            UK ONLY (SF)           
*                                                                               
REP#EXP  EQU   C'E'                  EXPENSE REPORT              (SE)           
REP#EXPF EQU   C'F'                  EXPENSE REPORT              (SF)           
REP#EXPL EQU   C'L'                  EXPENSE REPORT              (SL)           
REP#EXPD EQU   C'D'                  EXPENSE REPORT              (2D)           
REP#EXPB EQU   C'B'                  EXPENSE REPORT              (SB)           
REP#EXPP EQU   C'H'                  EXPENSE REPORT              (2P)           
*                                                                               
REP#PROD EQU   C'J'                  PRODUCTION REPORT           (SJ)           
*                                                                               
REP#CST  EQU   C'0'                  MANPOWER REPORT             (1R)           
*                                                                               
REP#CASH EQU   C'7'                  CASH REPORT                 (SC)           
*                                                                               
REP#PNL  EQU   C'9'                  PNL  REPORT                 (1C)           
*                                                                               
REP#GNL  EQU   C'G'                  G/L  REPORT                 (GB)           
REP#GNLP EQU   C'8'                  G/L  REPORT                 (GP)           
*                                                                               
REP#MEDA EQU   C'Z'                  MEDIA REPORT                (SZ)           
*                                                                               
REP#FI   EQU   C'I'                  APG  FINANCIAL REPORT                      
REP#M2   EQU   C'2'                  APG  MANPOWER  REPORT                      
REP#IV   EQU   C'3'                  APG  INVOICE   REPORT                      
         EJECT ,                                                                
***********************************************************************         
* TSAR DSECT                                                          *         
***********************************************************************         
         SPACE 2                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT+L'TLKSRT-2                                                
TLKAPPLS DS    0XL2                APPLICATION FILTER TO THE END                
TLKAPPL1 DS    XL1                                                              
TLKAPPL2 DS    XL1                                                              
         ORG   TLKSRT                                                           
TLKELEM  DS    0CL14                                                            
TLKPID   DS    CL8                 PERSONAL ID                                  
         DS    XL6                                                              
         ORG   TLKPID                                                           
TLKCPJC  DS    CL12                CLI/PRO/JOB CODE                             
TLKCPJPO DS    CL2                 CLI/PRO/JOB OFFICE                           
         ORG   TLKCPJC                                                          
TLKEXPC  DS    CL3                 EXPENDITURE TYPE CODE                        
         DS    XL11                                                             
         ORG   TLKEXPC                                                          
TLKWCC   DS    CL2                 WORK CODE                                    
         DS    CL12                                                             
         ORG   TLKWCC                                                           
TLKNCLC  DS    CL12                NON CLIENT CODE                              
         DS    CL2                                                              
         ORG   TLKNCLC                                                          
TLKMEDC  DS    CL1                 MEDIA CODE                                   
         DS    CL13                                                             
         ORG   TLKMEDC                                                          
*TLKMEDC  DS    CL(L'PMDKMED)       MEDIA CODE                                  
*        ORG   TLKMEDC                                                          
TLK1RAC  DS    CL(L'ACTKACT)       1R ACCOUNT CODE                              
         DS    CL2                                                              
         ORG   TLK1RAC                                                          
TLKFORM  DS    CL8                 FORMAT CODE                                  
         DS    CL6                                                              
         ORG   TLKFORM                                                          
TLKSUC   DS    CL(L'ACTKACT+2)     SUPPLIER ACCOUNT CODE                        
         ORG   TLKSUC                                                           
TLKSCHCD DS    CL8                 SCHEME CODE                                  
         DS    CL6                                                              
         ORG   TLUSER                                                           
TLKCPJNM DS    CL(NAMFLDLQ)        CLI/PRO/JOB NAME                             
         ORG   TLKCPJNM                                                         
TLKEXPNM DS    CL(L'NAMEREC)       EXPENDITURE NAME                             
TLKLOCKD DS    CL1                 LOCKED                                       
         DS    XL27                                                             
         ORG   TLKEXPNM                                                         
TLKWCNM  DS    CL(L'NAMEREC)       WORK CODE NAME                               
TLKWCDES DS    CL(L'WCODESC)       WORK CODE DESCRIPTION                        
         ORG   TLKWCNM                                                          
TLKNCLNM DS    CL(L'NAMEREC)       NON CLIENT ACCOUNT NAME                      
         ORG   TLKNCLNM                                                         
TLKMEDNM DS    CL(L'PMDDESC)       MEDIA NAME                                   
         ORG   TLKMEDNM                                                         
TLKC1RNM DS    CL(L'NAMEREC)       COSTING 1R NAME                              
         ORG   TLKC1RNM                                                         
TLKRPTY  DS    CL1                 REPORT TYPE                                  
TLKTRAN  DS    CL(L'ACTKACT)       TRANSMISSION TYPE                            
TLKFRNM  DS    CL(L'NAMEREC)       FORMAT NAME                                  
         ORG   TLKRPTY                                                          
TLKGRPC  DS    CL8                 GROUP CODE                                   
         DS    XL56                                                             
         ORG   TLKGRPC                                                          
TLKSCHNM DS    CL(L'CADTNAM)       SCHEME NAME                                  
         DS    XL44                                                             
         ORG   TLKSCHNM                                                         
TLSUPNM  DS    CL(L'NAMEREC)       SUPPLIER NAME                                
         DS    XL27                                                             
TLLNQ    EQU   *-TLSTD                                                          
*                                                                               
         ORG   TLUSER              **TSAR DOWNLOAD REPORT**                     
TLDLDAT  DS    0C                                                               
TLDCPJ   DS    CL(L'ACTKACT)       CLI/PRO/JOB CODE                             
TLDCPJOF DS    CL(L'TRNOFFC)       CLI/PRO OFFICE                               
TLDCPJAP DS    XL2                 APPLICATIONS                                 
TLDEXP   DS    CL(L'ETYKCODE)      EXPENDITURE TYPE CODE                        
TLDEXPAP DS    XL2                 APPLICATIONS                                 
TLDWC    DS    CL(L'WCOKWRK)       WORK CODE                                    
TLDWCDAP DS    XL2                 APPLICATIONS                                 
TLDNCC   DS    CL(L'ACTKACT)       NON-CLIENT CODE                              
TLDNCCAP DS    XL2                 APPLICATIONS                                 
TLDMED   DS    CL(L'PMDKMED)       MEDIA CODE                                   
TLDMEDAP DS    XL2                 APPLICATIONS                                 
TLD1RC   DS    CL(L'ACTKACT)       1R ACCOUNT CODE                              
TLD1RCAP DS    XL2                 APPLICATIONS                                 
TLQFC    DS    CL(L'RESKFORM)      SCRIBE FORMAT CODE                           
TLDFCDAP DS    XL2                 APPLICATIONS                                 
TLQNM    DS    CL(L'NAMEREC)       SCRIBE FORMAT NAME                           
TLQRT    DS    CL1                 SCRIBE REPORT TYPE                           
TLQTT    DS    CL(L'ACTKACT)       SCRIBE TRANSMISSION TYPE                     
TLDSCHCD DS    CL8                 SCHEME CODE                                  
TLDSCHAP DS    XL2                 APPLICATIONS                                 
TLDSUC   DS    CL14                SUPPLIER ACCOUNT CODE                        
TLDSUCAP DS    XL2                 APPLICATIONS                                 
TLDDEFAD DS    XL2                 DEFAULT ACCESS                               
TLDLDATL EQU   *-TLDLDAT                                                        
TLDLLNQ  EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'031ACFIL46   11/20/18'                                      
         END                                                                    
