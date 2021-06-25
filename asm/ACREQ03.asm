*          DATA SET ACREQ03    AT LEVEL 136 AS OF 01/11/21                      
*PHASE T60403B                                                                  
*INCLUDE PUBVAL                                                                 
*INCLUDE SQUASHER                                                               
*--------------------------------------------------------------------*          
* PID  LVL DATE    COMMENTS                                                     
* ---------------------------------                                             
* VGUP 132 31JUL19 SPEC-37489 UPDATED LDGTAB FOR NEW VE REPORT                  
* JSHA 133 15JAN20 SPEC-41535 CHECK DELIVERY FLAG                               
*----------------------------------------------------------------------         
         TITLE 'ACREQ03 - REQUEST - VALIDATE DATA FIELDS - PART 1'              
T60403   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 LWSX-LWS,T60403,RA,R7,RR=R5                                      
         USING LWS,RC              RC=A(LOCAL WORKING STORAGE)                  
         L     R9,0(R1)                                                         
         USING GWS,R9              R9=A(GLOBAL WORKING STORAGE)                 
         LA    R8,RCARDS                                                        
         USING ACQD,R8             R8=A(REQUEST CARDS)                          
         L     R3,ASAVE                                                         
         USING TWAD,R3             R3=A(TWA)                                    
         ST    R5,RELO                                                          
         ST    RC,SAVERC                                                        
*                                                                               
         LA    R1,ADCONS                                                        
         LA    RF,ATYPE3                                                        
         LA    R0,NADCONS                                                       
RELOLOOP L     RE,0(R1)                                                         
         A     RE,RELO                                                          
         ST    RE,0(RF)                                                         
         LA    R1,4(R1)                                                         
         LA    RF,4(RF)                                                         
         BCT   R0,RELOLOOP                                                      
*                                                                               
         L     RF,=V(SQUASHER)                                                  
         A     RF,RELO                                                          
         ST    RF,VSQUASH                                                       
*                                                                               
         L     R1,FLDHADR          R1=A(FLD HDR TO BE VALIDATED)                
         ST    R1,FADR                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,ROUTNUM          RF=ROUTINE NUM REQUIRED                      
         SLL   RF,2                                                             
         L     RF,ROUTADRT(RF)                                                  
         A     RF,RELO             RF=A(VALIDATION ROUTINE)                     
         BASR  RE,RF                                                            
         XMOD1 1                                                                
         EJECT                                                                  
***********************************************************************         
*              VALIDATE UNIT                                          *         
***********************************************************************         
*                                                                               
UNITVAL  NTR1                      UNIT - 02=ALL 04=X                           
         GOTO1 AINITV                                                           
         BNE   UNITVO              UNIT = ALL OR MISSING                        
         CLI   IFLDH+5,1                                                        
         BE    UNITV5              ONE CHAR MUST BE UNIT                        
         CLI   IFLD,C'+'           CHK FOR LIST                                 
         BE    LISTV02             GO TO LIST EDIT                              
         CLI   IFLD,C'-'                                                        
         BE    LISTV02                                                          
*                                                                               
UNITVE   MVC   FERN,=AL2(INVINPT)  UNIT INVALID                                 
         B     EXIT                                                             
*                                                                               
UNITV5   CLC   ACQPROG,=C'TX'                                                   
         BNE   UNITV7                                                           
         CLI   IFLD,C'S'           UNIT CAN ONLY BE 1 OR S                      
         BE    UNITV7                                                           
         CLI   IFLD,C'1'                                                        
         BNE   UNITVE                                                           
*                                                                               
UNITV7   MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(1),IFLD                                                    
         MVC   ACQUNT,IFLD                                                      
         GOTO1 AIOREAD             READ UNIT RECORD                             
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) UNIT NOT ON FILE                             
         B     UNITVO                                                           
         OI    FIND,FIVAL          UNIT = X                                     
*                                                                               
UNITVO   BRAS  RE,DISPNAME                                                      
*                                                                               
*                                                                               
EXIT     XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
*              VALIDATE LEDGER                                        *         
***********************************************************************         
*                                                                               
LDGVAL   NTR1                      LEDGER - 02=ALL 04=X                         
         MVC   NAMEX,SPACES                                                     
         GOTO1 AINITV                                                           
         BE    LDG10               LEDGER = ALL OR MISSING                      
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    LDGX                                                             
         CLI   TWAACCS,C'*'        SEE IF LIMIT ACCESS                          
         BNE   LDGX                                                             
         CLC   ACQPROG,=C'CM'      ** NO LEDGER SECURITY FOR                    
         BE    LDGX                ** TIMESHEET EDIT REPORT - ACCM              
         MVC   FERN,=AL2(SECLOCK)  SECURITY  LOCKOUT                            
         OI    FIND,FIINP          MUST FOOL 01 PHASE                           
         B     LDGX                LEDGER REQUIRED                              
*                                                                               
LDG10    CLI   ACQUNT,C' '                                                      
         BE    LDGERR              LEDGER SPECIFIC & UNIT NOT                   
         CLI   IFLDH+5,1                                                        
         BH    LDGERR                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(1),ACQUNT                                                  
         MVC   KEY+2(1),IFLD                                                    
         CLC   ACQPROG,=C'TX'                                                   
         BNE   LDG15                                                            
         CLC   KEY+1(2),=C'SJ'                                                  
         BE    LDG15                                                            
         CLC   KEY+1(2),=C'1N'                                                  
         BNE   LDGERR                                                           
*                                                                               
LDG15    MVC   ACQLDG,KEY+2                                                     
         GOTO1 AIOREAD             READ LEDGER RECORD                           
         MVC   NAMEX,NAME                                                       
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) LEDGER NOT ON FILE                           
         B     LDGX                                                             
         CLI   REQNUM,229          FOR ACTT (1099S) U/L MUST BE 2C              
         BNE   LDG20                                                            
***********************************                                             
         CLC   TWAAGY(2),=C'JW'    HARDCODE FOR JWNY                            
         BNE   *+18                                                             
         CLC   ACQUNT(2),=C'SV'    U/L MUST BE SV                               
         BE    LDG20                                                            
         B     LDGERR                                                           
***********************************                                             
         CLC   ACQUNT(2),=C'2C'                                                 
         BNE   LDGERR                                                           
*                                                                               
LDG20    DS    0H                                                               
         LA    RF,LDGTAB           TABLE OF REQS WITH SPECIFIC LEDGERS          
LDG30    SR    R1,R1                                                            
         ICM   R1,1,0(RF)                                                       
         BZ    LDG60                                                            
         CLC   1(1,RF),REQNUM                                                   
         BE    LDG40                                                            
         AR    RF,R1                                                            
         B     LDG30                                                            
*                                                                               
LDG40    SH    R1,=H'2'                                                         
LDG50    CLC   2(1,RF),ACQLDG                                                   
         BE    LDG60               VALID                                        
         LA    RF,1(RF)                                                         
         BCT   R1,LDG50                                                         
         B     LDGERR              INVALID LEDGER                               
*                                                                               
LDG60    DS    0H                                                               
         OI    FIND,FIVAL          LEDGER=X                                     
         USING LDGELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,LDGELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   *+10                                                             
         MVC   LOFFPOS,LDGOPOS     SAVE LEDGER INFO                             
*                                                                               
         CLC   ACQPROG,=C'CM'      ** NO LEDGER SECURITY FOR                    
         BE    LDG70               ** TIMESHEET EDIT REPORT - ACCM              
*                                                                               
         USING RSTELD,R6                                                        
         L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   LDG70                                                            
         CLC   TWAAUTH+1(1),RSTSECY+1                                           
         BNL   LDG70                                                            
         MVC   FERN,=AL2(SECLOCK)  SECURITY LOCK OUT                            
         B     LDGX                                                             
*                                                                               
* LEDGER / CHECK AUTHORIZATION RECORD FOR CHECKS(55)                            
*                                                                               
LDG70    DS    0H                                                               
         CLI   REQNUM,55                                                        
         BNE   LDGX                                                             
*                                                                               
         USING CHARECD,RF                                                       
         LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' CHECK AUTHORIZATION REC                
         MVC   CHAKCULA,ACQCPY                                                  
         MVC   LKEY,KEY                                                         
         GOTO1 DATAMGR,DMCB,(0,DMRDHI),=CL8'ACCOUNT',KEY,AIO1                   
         L     R6,AIO1                                                          
         CLC   KEY(4),0(R6)                                                     
         BNE   LDG100                                                           
*                                                                               
LDG80    L     R6,AIO1             MUST HAVE X'54' ELEM FOR THIS ID TO          
         AH    R6,DATADISP         REQUEST CHECKS                               
LDG90    CLI   0(R6),X'54'                                                      
         BE    LDG120                                                           
         CLI   0(R6),0                                                          
         BNE   LDG110                                                           
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'DMRSEQ',=CL8'ACCOUNT',AIO1,AIO1                
         L     R6,AIO1                                                          
         CLC   LKEY(4),0(R6)                                                    
         BE    LDG80                                                            
*                                                                               
LDG100   MVC   FERN,=AL2(SECLOCK)                                               
         B     LDGX                                                             
LDG110   SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     LDG90                                                            
*                                                                               
         USING OCNELD,R6                                                        
LDG120   CLC   OCNOFFID,TWAUSRID                                                
         BNE   LDG110                                                           
         TM    CMPSTAT,CMPNOFF+CMPEMUL                                          
         BNO   LDG130                                                           
         CLC   OCNPOFF,SPACES      POSTING OFFICE REQUIRED IF NEW               
         BH    LDG130              OFFICE AND NEW FILE                          
         MVC   FERN,=AL2(NOPOST)                                                
         B     LDGX                                                             
*                                                                               
LDG130   MVC   LOFFCHK,OCNSTAT                                                  
*                                                                               
* AS PER PCAS - AUTHORIZATION IS ALWAYS REQUIRED 4/10                           
*        TM    OCNSTAT,X'08'       AUTHORIZATION REQUIRED                       
*        BZ    LDG140                                                           
         TM    TWAAUTH,X'01'                                                    
         BO    LDG140                                                           
         MVC   FERN,=AL2(NOTAUTH)  TERMINAL AUTHORIZATION LOCKOUT               
         B     LDGX                                                             
*                                                                               
LDG140   CLI   OCNLN,OCNLNQ                                                     
         BNH   LDG180                                                           
         CLI   OCNLASR,OCNWSP                                                   
         BNE   LDG170                                                           
*                                                                               
         TM    OUTSTAT,SOONQ      FOR OVERNIGHT REQUESTS THE FOLLOWING          
         BZ    LDG170             IS DONE IN EOD (SORT AND WRAP)                
*                                                                               
         USING CTIREC,RF                                                        
         LA    RF,KEY              READ ORIGIN ID REC                           
         XC    CTIKEY,CTIKEY                                                    
         MVI   CTIKEY,C'I'                                                      
         MVC   CTIKID,SPACES                                                    
         MVC   CTIKID(5),=C'LASER'                                              
*                                                                               
         GOTO1 DATAMGR,DMCB,(0,DMREAD),CTFILE,KEY,AIO2                          
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     RF,AIO2                                                          
         LA    R2,CTIDATA                                                       
LDG150   CLI   0(R2),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         CLI   0(R2),CTDSCELQ      ID ELEMENT                                   
         BE    LDG160                                                           
         SR    R1,R1                                                            
         IC    R1,1(R2)                                                         
         AR    R2,R1                                                            
         B     LDG150                                                           
*                                                                               
         USING CTDSCD,R2                                                        
LDG160   MVC   REQDEST,CTDSC       FORCE DESTINATION TO LASER ID                
         DROP  R2,RF                                                            
*                                                                               
LDG170   OC    OUTSTAT,OUTSTAT     NO VALIDATION FOR 55-STACK CHECKS            
         BNZ   LDG180                                                           
         OC    OCNDPLR,OCNDPLR     CHECK IF A LOCAL REGISTER PENDING            
         BZ    *+14                                                             
         MVC   FERN,=AL2(LREGPEND)                                              
         B     LDGX                                                             
         OC    OCNDPSR,OCNDPSR                                                  
         BZ    LDG180                                                           
         CLC   OCNDPSR,TODAY2      CHECK IF A SOON REGISTER PENDING             
         BNL   *+14                FROM YESTERDAY                               
         MVC   FERN,=AL2(SREGPEND)                                              
         B     LDGX                                                             
*                                                                               
LDG180   CLI   OCNFILT,C'*'        SEE IF BY OFFICE                             
         BNE   LDGX                NOT BY OFFICE                                
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    LDGX                YES-EXIT RIGHT NOW                           
         LA    R0,MAXFLD           I'VE FOUND A 54 ELEM FOR THIS ID             
         LA    R1,LREQMAP          LOOK AHEAD TO SEE IF OFFICE WAS              
*                                  INPUT,IF NOT OR * MOVE OFFICE                
LDG190   CLI   0(R1),123           FIND OFFICE (FOR CHECKS)                     
         BE    LDG200                                                           
         CLI   0(R1),LREQMAPX                                                   
         BNE   *+6                                                              
         DC    H'0'                MUST FIND OFFICE                             
         LA    R1,3(R1)                                                         
         BCT   R0,LDG190                                                        
         DC    H'0'                MUST FIND OFFICE                             
*                                                                               
LDG200   MVC   HALF,1(R1)                                                       
         LR    R5,R3                                                            
         AH    R5,HALF             BUMP TO OFFICE                               
         CLI   5(R5),1             TEST MORE THAN 1 BYTE INPUT                  
         BH    LDGX                                                             
         CLI   8(R5),C'*'          CHANGE * TO LEDGER OFFICE                    
         BE    LDG210                                                           
         CLI   5(R5),0             SEE IF ANY OTHER INPUT                       
         BNE   LDGX                ERRORS WILL BE CAUGHT IN 02 PHASE            
*                                                                               
LDG210   TM    OUTSTAT,RUNQ                                                     
         BO    LDGX                                                             
         FOUT  (R5),OCNFILT+1,1                                                 
         OI    4(R5),X'80'         SET INPUT THIS TIME                          
         MVI   5(R5),1             SET INPUT LENGHT                             
         B     LDGX                                                             
*                                                                               
LDGERR   MVC   FERN,=AL2(INVINPT)  LEDGER INVALID                               
LDGX     MVC   NAME,NAMEX                                                       
         BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
*                                                                               
*              XL1=LENGTH,XL1=REQNUM,XLN=LEDGERS                                
*                                                                               
LDGTAB   DS    0C                                                               
         DC    AL1(07,20),C'STPQU'                                              
         DC    AL1(04,26),C'BP'                                                 
         DC    AL1(11,55),C'SPVXTQWYU'                                          
         DC    AL1(11,58),C'SPVXTQWYU'                                          
         DC    AL1(11,122),C'SPVXTQWYU'                                         
LDGTABX  DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
*              VALIDATE CLIENT                                        *         
***********************************************************************         
*                                                                               
CLI3VAL  NTR1                      CHECK CLIENT                                 
         GOTO1 AINITV                                                           
         BNE   CLIVLO              CLIENT = ALL OR MISSING                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),LREQJOB                                                 
         MVC   KEY+3(3),IFLD                                                    
         GOTO1 AIOREAD             READ CLIENT RECORD                           
         BNE   CLI3ERR             DISK ERROR                                   
         OI    FIND,FIVAL          CLIENT = X THRU XXXXXX                       
         MVC   ACQCNTRA+2(3),IFLD                                               
         B     CLI3X                                                            
*                                                                               
CLI3ERR  MVC   FERN,=AL2(INVINPT)  CLIENT INVALID                               
         B     EXIT                                                             
*                                                                               
CLI3X    BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE BILLING GROUP                                 *         
***********************************************************************         
*                                                                               
BGRPVAL  NTR1               BILLING GROUP - 02=ALL 04=XXXOR *XXX                
         GOTO1 AINITV                                                           
         BNE   BGRPVO              BGRP = ALL OR MISSING                        
         CLC   ACQACT,SPACES                                                    
         BNE   BGRPVE              BGRP SPECIFIC & ACCOUNT SPECIFIC             
         CLI   IFLD,C'*'                                                        
         BE    BGRPV1              EXCLUDE BGRP                                 
         CLI   IFLDH+5,3                                                        
         BH    BGRPVE                                                           
         MVC   ACQBILGP,IFLD                                                    
         B     BGRPVO                                                           
*                                                                               
BGRPV1   CLI   IFLDH+5,1                                                        
         BNH   BGRPVE                                                           
         MVC   ACQBILGP,IFLD+1                                                  
         NI    ACQBILGP,X'BF'      TURN OFF X'40' BIT                           
         B     BGRPVO                                                           
*                                                                               
BGRPVE   MVC   FERN,=AL2(INVINPT) BGRP INVALID                                  
         B     BGRPVO2                                                          
*                                                                               
BGRPVO   OI    FIND,FIVAL          BGRP = X THRU XXX                            
BGRPVO2  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              FILTERS#1-4, MEDIA FILTER & BILLTYPE FILTER LOGIC      *         
***********************************************************************         
*                                                                               
FTR1VAL  NTR1                                                                   
         LA    R6,ACQACTF1         ** FILTER #1 **                              
         B     FTRVAL                                                           
*                                                                               
FTR2VAL  NTR1                                                                   
         LA    R6,ACQACTF2         ** FILTER #2 **                              
         B     FTRVAL                                                           
*                                                                               
FTR3VAL  NTR1                                                                   
         LA    R6,ACQACTF3         ** FILTER #3 **                              
         B     FTRVAL                                                           
*                                                                               
FTR4VAL  NTR1                                                                   
         LA    R6,ACQACTF4         ** FILTER #4 **                              
         B     FTRVAL                                                           
*                                                                               
FTR5VAL  NTR1                                                                   
         LA    R6,ACQACTF5         ** FILTER #5 **                              
         B     FTRVAL                                                           
*                                                                               
MEDFTR   NTR1                                                                   
         LA    R6,ACQMEDFL         ** MEDIA FILTER **                           
         B     FTRVAL                                                           
*                                                                               
BILFTR   NTR1                                                                   
         LA    R6,ACQBILTY         ** BILL TYPE FILTER **                       
*                                                                               
FTRVAL   GOTO1 AINITV                                                           
         BL    FTRVO               FILTER MISSING                               
         CLI   IFLDH+5,1                                                        
         BH    FTRV1                                                            
         MVC   0(1,R6),IFLD                                                     
         OI    FIND,FIVAL          FILTER = X                                   
         B     FTRVO                                                            
*                                                                               
FTRV1    CLI   IFLD,C'*'           NEGATIVE FILTER                              
         BNE   FTRVE                                                            
         MVC   0(1,R6),IFLD+1                                                   
         NI    0(R6),X'BF'         TURN OFF X'40' BIT                           
         OI    FIND,FIVAL          FILTER=-X                                    
         B     FTRVO                                                            
*                                                                               
FTRVE    MVC   FERN,=AL2(INVINPT)  FILTER INVALID                               
*                                                                               
FTRVO    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              START DATE                                             *         
***********************************************************************         
         SPACE 1                                                                
STARTVAL NTR1                      START DATE - 04=YYMMDD 08=YYMM               
         MVC   FERN,=AL2(FF)                                                    
         GOTO1 AINITV                                                           
         BL    STRVX               START DATE MISSING                           
         BH    STRV10                                                           
*                                                                               
* CODE TO FIX SOFTDATE/RFP PROBLEM WITH KEYWORD 'TODAY'                         
*                                                                               
         TM    RFPSTAT,RFPINUSE                                                 
         BZ    STRV05                                                           
         CLI   IFLD,C'T'                                                        
         BNE   STRV05                                                           
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IFLD(0),=C'TODAY'                                                
         BE    STRV10                                                           
         MVC   FERN,=AL2(INVDATEF) START DATE INVALID                           
         B     STRVX                                                            
*                                                                               
STRV05   DS    0H                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM    FIRST CALL YYMM                              
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE                              
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD1+SOFOTSPC                                       
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQSTART                                                      
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   *+12                                                             
         OI    FIND,FIALT          START DATE = YYMM                            
         B     STRVX                                                            
*                                                                               
         MVI   SOFITYPE,SOFITYMD   SECOND CALL YYMMDD                           
         MVI   SOFOTYPE,SOFOTSD2                                                
         GOTO1 (RF),(R1)                                                        
         BNZ   STRV10                                                           
         OI    FIND,FIVAL          START DATE = YYMMDD                          
         B     STRVX                                                            
*                                                                               
STRV10   MVC   FERN,=AL2(INVDATEF) START DATE INVALID                           
         TM    RFPSTAT,RFPINUSE    $RFP INTERFACE IN USE?                       
         BZ    STRVX                                                            
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         NI    FIND,X'FF'-(FIVAL+FIALT)                                         
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         B     STRVX                                                            
*                                                                               
         CLC   QRFPDICT,=Y(E#START)     START DATE (YYMMDD)                     
         BNE   *+12                                                             
         OI    FIND,FIVAL                                                       
         B     STRV20                                                           
*                                                                               
         CLC   QRFPDICT,=Y(E#STARTM)    START DATE (YYMM)                       
         BNE   *+12                                                             
         OI    FIND,FIALT                                                       
         B     STRV20                                                           
*                                                                               
         CLC   QRFPDICT,=Y(E#BDATE)     BILL DATE (YYMMDD)                      
         BNE   *+12                                                             
         OI    FIND,FIVAL                                                       
         B     STRV20                                                           
*                                                                               
         CLC   QRFPDICT,=Y(E#TODAY)    TODAY (YYMMDD)                           
         BNE   *+12                                                             
         OI    FIND,FIVAL                                                       
         B     STRV20                                                           
*                                                                               
         MVC   FERN,=AL2(ISYMBFLD)      INVALID SYMBOLIC EQUATE                 
         B     STRVX                                                            
*                                                                               
STRV20   MVC   ACQSTART,SPACES          STORE ESCAPE SEQ IN REQCARD             
         MVC   ACQSTART(L'QRFPESC),QRFPESC                                      
         MVI   ACQSTART+3,L'ACQSTART                                            
         MVC   FERN,=AL2(FF)                                                    
STRVX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*              END DATE                                               *         
***********************************************************************         
         SPACE 1                                                                
ENDVAL   NTR1                      END DATE - 04=YYMMDD 08=YYMM                 
         MVC   FERN,=AL2(FF)                                                    
         GOTO1 AINITV                                                           
         BL    ENDVX               END DATE MISSING                             
         BH    ENDV20                                                           
*                                                                               
* CODE TO FIX SOFTDATE/RFP PROBLEM WITH KEYWORD 'TODAY'                         
*                                                                               
         TM    RFPSTAT,RFPINUSE                                                 
         BZ    ENDV05                                                           
         CLI   IFLD,C'T'                                                        
         BNE   ENDV05                                                           
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   IFLD(0),=C'TODAY'                                                
         BE    ENDV20                                                           
         MVC   FERN,=AL2(INVDATEF) START DATE INVALID                           
         B     ENDVX                                                            
*                                                                               
ENDV05   DS    0H                                                               
*                                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM    FIRST CALL YYMM                              
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE                              
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD1+SOFOTSPC                                       
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQEND                                                        
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   *+12                                                             
         OI    FIND,FIALT          START DATE = YYMM                            
         B     ENDV10                                                           
*                                                                               
         MVI   SOFITYPE,SOFITYMD   SECOND CALL YYMMDD                           
         MVI   SOFOTYPE,SOFOTSD2                                                
         GOTO1 (RF),(R1)                                                        
         BNZ   ENDV20                                                           
         OI    FIND,FIVAL          START DATE = YYMMDD                          
*                                                                               
ENDV10   TM    SOFOINDS,SOFOIHRD   TEST HARD DATE INPUT                         
         BZ    ENDVX                                                            
         CLI   ACQSTART,C' '                                                    
         BE    ENDVX                                                            
         CLC   ACQSTART,ACQEND     CHECK START LE END                           
         BNH   ENDVX                                                            
         MVC   FERN,=AL2(INVDATEF) END DATE INVALID                             
         B     ENDVX                                                            
*                                                                               
ENDV20   MVC   FERN,=AL2(INVDATEF) END DATE INVALID                             
         TM    RFPSTAT,RFPINUSE    $RFP INTERFACE IN USE?                       
         BZ    ENDVX                                                            
*                                                                               
*              $RFP - VALIDATE SYMBOLIC EQUATE                                  
*                                                                               
         NI    FIND,X'FF'-(FIVAL+FIALT)                                         
         XC    QRFPBLK(QRFPBLKQ),QRFPBLK                                        
         MVI   QRFPMODE,QRFPSYMB                                                
         MVC   QRFPWORK,IFLD                                                    
         OC    QRFPWORK,SPACES                                                  
         GOTO1 ARFP,DMCB                                                        
         CLI   QRFPMODE,QRFPOK                                                  
         BE    *+14                                                             
         MVC   FERN,=AL2(ISYMBEQU)      INVALID SYMBOLIC EQUATE                 
         B     EXIT                                                             
*                                                                               
         CLC   QRFPDICT,=Y(E#END)       END DATE (YYMMDD)                       
         BNE   *+12                                                             
         OI    FIND,FIVAL                                                       
         B     ENDV30                                                           
*                                                                               
         CLC   QRFPDICT,=Y(E#ENDM)      END DATE (YYMM)                         
         BNE   *+12                                                             
         OI    FIND,FIALT                                                       
         B     ENDV30                                                           
*                                                                               
         CLC   QRFPDICT,=Y(E#TODAY)     TODAY'S DATE (YYMMDD)                   
         BNE   *+12                                                             
         OI    FIND,FIVAL                                                       
         B     ENDV30                                                           
*                                                                               
         MVC   FERN,=AL2(ISYMBFLD)      INVALID SYMBOLIC EQUATE                 
         B     EXIT                                                             
*                                                                               
ENDV30   MVC   ACQEND,SPACES            STORE ESCAPE SEQ IN REQCARD             
         MVC   ACQEND(L'QRFPESC),QRFPESC                                        
         MVI   ACQEND+3,L'ACQEND                                                
         MVC   FERN,=AL2(FF)                                                    
ENDVX    B     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
*              VALIDATE OPTIONS                                       *         
***********************************************************************         
*                                                                               
OPT1VAL  NTR1                      OPTION#N - 04=X                              
         L     R6,AOPT1VTB                                                      
         LA    R2,ACQOPT1                                                       
         B     OPTNVAL                                                          
*                                                                               
OPT2VAL  NTR1                                                                   
         L     R6,AOPT2VTB                                                      
         LA    R2,ACQOPT2                                                       
         B     OPTNVAL                                                          
*                                                                               
OPT3VAL  NTR1                                                                   
         L     R6,AOPT3VTB                                                      
         LA    R2,ACQOPT3                                                       
         B     OPTNVAL                                                          
*                                                                               
OPT4VAL  NTR1                                                                   
         L     R6,AOPT4VTB                                                      
         LA    R2,ACQOPT4                                                       
         B     OPTNVAL                                                          
*                                                                               
OPT5VAL  NTR1                                                                   
         L     R6,AOPT5VTB                                                      
         LA    R2,ACQOPT5                                                       
         B     OPTNVAL                                                          
*                                                                               
OPT6VAL  NTR1                                                                   
         L     R6,AOPT6VTB                                                      
         LA    R2,ACQOPT6                                                       
         CLC   ACQPROG,=C'81'                                                   
         BNE   OPTNVAL                                                          
         CLI   ACQCPY,X'F5'                                                     
         BE    OPTNVAL                                                          
         CLI   ACQCPY,X'DB'                                                     
         BE    OPTNVAL                                                          
         GOTO1 AINITV                                                           
         B     OPTNVE                                                           
*                                                                               
OPT7VAL  NTR1                                                                   
         L     R6,AOPT7VTB                                                      
         LA    R2,ACQOPT7                                                       
         B     OPTNVAL                                                          
*                                                                               
OPT8VAL  NTR1                                                                   
         L     R6,AOPT8VTB                                                      
         LA    R2,ACQOPT8                                                       
         B     OPTNVAL                                                          
*                                                                               
OPT9VAL  NTR1                                                                   
         L     R6,AOPT9VTB                                                      
         LA    R2,ACQOPT9                                                       
         B     OPTNVAL                                                          
*                                                                               
OPTAVAL  NTR1                                                                   
         MVI   ACQOPT10,C'Y'       DEFAULT IS YES - SHOW SALARY                 
         CLC   ACQPROG,=C'92'      SPECIAL CODE FOR 92                          
         BNE   OPTA20                                                           
*                                                                               
         LA    R1,OPT1TAB          SALARY ONLY RPT TABLE                        
OPTA10   CLI   0(R1),X'FF'         END OF TABLE?                                
         BE    OPTA20              YES-NOT AN SAL ONLY RPT-CHK SEC              
         CLC   ACQOPT1,0(R1)       IF OPT 1 MATCH AN ENTRY IN TABLE             
         BE    OPTA30              CONTINUE                                     
         LA    R1,1(R1)                                                         
         B     OPTA10                                                           
*                                                                               
OPTA20   TM    CMPSTAT,CMPNSWT+CMPNSRD  IF EITHER READ/WRITE IS ON              
         BNZ   *+8                                                              
         MVI   ACQOPT10,C'N'            DEFAULT IS 'NO'                         
*                                                                               
OPTA30   L     R6,AOPTAVTB                                                      
         LA    R2,ACQOPT10                                                      
         B     OPTNVAL                                                          
*                                                                               
SORTVAL  NTR1                                                                   
         L     R6,ASORTVTB                                                      
         LA    R2,ACQSORT                                                       
         B     OPTNVAL                                                          
*                                                                               
OPTNVAL  GOTO1 AINITV                                                           
         BL    OPTNVO              OPTION MISSING                               
         CLI   IFLDH+5,1                                                        
         BH    OPTNVE                                                           
*                                                                               
OPTNV1   SR    R4,R4               FIND REQ NUM ENTRY                           
         ICM   R4,1,0(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         CLC   1(1,R6),REQNUM                                                   
         BE    *+10                                                             
         AR    R6,R4                                                            
         B     OPTNV1                                                           
         SH    R4,=H'2'                                                         
*                                                                               
OPTNV3   CLI   2(R6),255           ANY VALUE                                    
         BE    OPTNV4                                                           
         CLI   2(R6),254           ANY ALPHA-NUMERIC                            
         BE    OPTNV6                                                           
         CLI   2(R6),253           ANY ALPHA                                    
         BE    OPTNV8                                                           
         CLC   2(1,R6),IFLD        CHECK LIST OF VALUES FOR REQ NUM             
         BE    OPTNV4                                                           
         LA    R6,1(R6)                                                         
         BCT   R4,OPTNV3                                                        
         B     OPTNVE                                                           
*                                                                               
OPTNV4   MVC   0(1,R2),IFLD                                                     
         OI    FIND,FIVAL          OPTION = X                                   
         B     OPTNVO                                                           
*                                                                               
OPTNV6   CLI   IFLD,C'A'           ALPHA-NUMERIC - 254                          
         BL    OPTNVE                                                           
         CLI   IFLD,C'9'                                                        
         BH    OPTNVE                                                           
         MVC   0(1,R2),IFLD                                                     
         OI    FIND,FIVAL          OPTION = X                                   
         B     OPTNVO                                                           
*                                                                               
OPTNV8   CLI   IFLD,C'A'           ALPHA   -253                                 
         BL    OPTNVE                                                           
         CLI   IFLD,C'Z'                                                        
         BH    OPTNVE                                                           
         MVC   0(1,R2),IFLD                                                     
         OI    FIND,FIVAL          OPTION = X                                   
         B     OPTNVO                                                           
*                                                                               
OPTNVE   MVC   FERN,=AL2(INVOPT)   OPTION INVALID                               
*                                                                               
OPTNVO   B     EXIT                                                             
*                                                                               
* OPTIONS TABLE FOR 92 REPORT                                                   
*                                                                               
OPT1TAB  DS    0C                    RPTS THAT SHOW SALARY ONLY                 
         DC    C'7'                                                             
         DC    C'8'                                                             
         DC    C'9'                                                             
         DC    C'A'                                                             
         DC    C'D'                                                             
         DC    C'E'                                                             
         DC    C'H'                                                             
         DC    C'I'                                                             
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
*              BUCKET TYPE                                            *         
***********************************************************************         
*                                                                               
BUCKET   NTR1                                                                   
         GOTO1 AINITV                                                           
         BL    BUCKETX                                                          
         CLI   IFLDH+5,1                                                        
         BH    BUCKETE                                                          
         MVC   ACQSRTAR+2(1),IFLD                                               
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
BUCKETE  MVC   FERN,=AL2(INVINPT)                                               
BUCKETX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              SPARE ROUTINE                                          *         
***********************************************************************         
         SPACE 1                                                                
SPARE    NTR1                                                                   
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* EFFECTIVE DATE VALIDATION                                           *         
***********************************************************************         
         SPACE 1                                                                
RECDVAL  NTR1                      EFF DTE-04=YYMMDD 08-YYMM/ALL                
         MVC   FERN,=AL2(FF)                                                    
         GOTO1 AINITV                                                           
         BL    RECDVX              START DATE MISSING                           
         BNH   *+14                                                             
         MVC   ACQSEL(3),IFLD                                                   
         B     RECDVX                                                           
*                                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM    FIRST CALL YYMM                              
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT+SOFIIONE                              
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         MVI   SOFOTYPE,SOFOTSD1+SOFOTSPC                                       
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQSEL                                                        
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   *+12                                                             
         OI    FIND,FIALT          START DATE = YYMM                            
         B     RECDVX                                                           
*                                                                               
         MVI   SOFITYPE,SOFITYMD   SECOND CALL YYMMDD                           
         MVI   SOFOTYPE,SOFOTSD2                                                
         GOTO1 (RF),(R1)                                                        
         BNZ   *+12                                                             
         OI    FIND,FIVAL          START DATE = YYMMDD                          
         B     RECDVX                                                           
*                                                                               
         MVC   FERN,=AL2(INVDATEF) DATE INVALID                                 
RECDVX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PERCENTAGE                                             *         
***********************************************************************         
*                                                                               
PCTVAL   NTR1                      PERCENTAGE - 04=XNNN%                        
         DC    H'0'                NOT USED, DUMP IF ENTERED                    
         GOTO1 AINITV                                                           
         BL    PCTVO               PCT NOT INPUT                                
         CLI   IFLDH+5,6                                                        
         BH    PCTVE                                                            
         CLI   IFLD,C'O'           TEST 1ST CHR FOR OVER/UNDER CODE             
         BE    PCTV1                                                            
         CLI   IFLD,C'+'                                                        
         BE    PCTV1                                                            
         CLI   IFLD,C'U'                                                        
         BE    PCTV2                                                            
         CLI   IFLD,C'-' '                                                      
         BE    PCTV2                                                            
         MVI   ACQOPT2,C' '        ELSE SET SPACE                               
         LA    R6,IFLD                                                          
         B     PCTV3                                                            
*                                                                               
PCTV1    MVI   ACQOPT2,C'O'                                                     
         LA    R6,IFLD+1                                                        
         B     PCTV3                                                            
*                                                                               
PCTV2    MVI   ACQOPT2,C'U'                                                     
         LA    R6,IFLD+1                                                        
*                                                                               
PCTV3    CLI   0(R6),C','          ALLOW , BEFORE NUMERIC                       
         BNE   *+8                                                              
         LA    R6,1(R6)            R6=A(1ST NUMERIC)                            
         LA    RF,IFLD-1(R5)                                                    
         CLI   0(RF),C'%'          ALLOW % AFTER NUMERIC                        
         BNE   *+6                                                              
         BCTR  RF,0                RF=A(LAST NUMERIC)                           
         LR    R4,R6                                                            
         SH    R4,=H'8'            SET R4=A(NUMERIC FLD)-8                      
         LR    R5,RF                                                            
         SR    R5,R6                                                            
         BM    PCTVE                                                            
         LA    R5,1(R5)            SET R5=L'NUMERIC FLD                         
         GOTO1 ARJN,DMCB,(R5),(R4)     RT JUST AT TEMP+1(3)                     
         CLC   FERN,=AL2(FF)                                                    
         BL    PCTVE                                                            
         MVC   ACQOPT3(3),TEMP+1                                                
         OI    FIND,FIVAL          PCT EXPRESSION VALID                         
         B     PCTVO                                                            
PCTVE    MVC   FERN,=AL2(INVINPT)  PCT EXPRESSION INVALID                       
PCTVO    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE CLIENT                                        *         
***********************************************************************         
*                                                                               
CLIVAL   NTR1                      CLIENT - 02=ALL 04=XXXXXX                    
         GOTO1 AINITV                                                           
         BNE   CLIVLO              CLIENT = ALL OR MISSING                      
         CLI   IFLDH+5,6                                                        
         BH    CLIVLE                                                           
         CLI   LREQLEVA,0                                                       
         BE    CLIVLE              ERROR NO PRODUCTION LEDGER                   
         CLI   ACQLDG,C' '         MUST BE LEDGER SPECIFIC                      
         BE    CLIVLE                                                           
         GOTO1 AGRPVAL             GROUP ACCOUNT VALIDATION                     
         CLI   FERN,X'FF'          ANY ERRORS?                                  
         BNE   CLIVLO              YES                                          
         TM    FIND,FIVAL                                                       
         BNZ   CLIVLO                                                           
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),ACQUNT                                                  
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),IFLD                                                    
         GOTO1 AIOREAD             READ CLIENT RECORD                           
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) CLIENT NOT ON FILE                           
         B     CLIVLO                                                           
         OI    FIND,FIVAL          CLIENT = X THRU XXXXXX                       
         MVC   ACQACT(3),IFLD                                                   
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   CLIVLO                                                           
*                                                                               
         USING PPRELD,R6                                                        
         MVC   SOFFICE,PPRGAOFF                                                 
*                                                                               
         CLC   ACQPROG,=C'64'      THESE PROGRAMS DO NOT HAVE A PRODUCT         
         BE    CLIVL2                                                           
         CLC   ACQPROG,=C'71'                                                   
         BE    CLIVL2                                                           
         CLC   ACQPROG,=C'P8'                                                   
         BNE   CLIVLO              THE REST, VALIDATE AT PRODUCT                
*                                                                               
         USING OFFALD,R1                                                        
CLIVL2   L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,SOFFICE                                                 
         L     R0,AIO1                                                          
         ST    R0,OFFAREC                                                       
         MVC   OFFAOPOS,LOFFPOS                                                 
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BE    CLIVLO                                                           
         MVC   FERN,=AL2(SECLOCK)                                               
         B     CLIVLO                                                           
*                                                                               
CLIVLE   MVC   FERN,=AL2(INVINPT)  CLIENT INVALID                               
*                                                                               
CLIVLO   BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PRODUCT                                       *         
***********************************************************************         
*                                                                               
PROVAL   NTR1                      PRODUCT - 02=ALL 04=XXXXXX                   
         GOTO1 AINITV                                                           
         BNE   PROVL4              PRODUCT = ALL OR MISSING                     
         CLI   IFLDH+5,6                                                        
         BH    PROVLE                                                           
         CLC   ACQACT(3),SPACES                                                 
         BE    PROVLE              PRODUCT SPECIFIC & CLIENT NOT                
         CLI   LREQLEVA,0                                                       
         BE    PROVLE              NO PRODUCTION LEDGER                         
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(5),ACQUNT     C/U/L/CLI                                    
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+6(0),IFLD                                                    
         GOTO1 AIOREAD             READ PRODUCT RECORD                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) CLIENT NOT ON FILE                           
         B     PROVLO                                                           
         MVC   ACQACT+3(3),IFLD                                                 
         OI    FIND,FIVAL          PRODUCT = X THRU XXXXXX                      
*                                                                               
         L     R6,AIO1                                                          
         MVI   ELCODE,X'24'                                                     
         BAS   RE,GETEL                                                         
         BNE   PROVLO                                                           
*                                                                               
         USING PPRELD,R6                                                        
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,PPRGAOFF                                                
         CLC   OFFAOFFC,SPACES                                                  
         BNH   PROVL6                                                           
*                                                                               
PROVL2   L     R0,AIO1                                                          
         ST    R0,OFFAREC                                                       
         MVC   OFFAOPOS,LOFFPOS                                                 
         MVI   OFFAACT,OFFATST                                                  
         GOTO1 OFFAL                                                            
         BE    PROVLO                                                           
         MVC   FERN,=AL2(SECLOCK)                                               
         OI    FIND,FIINP          MAKE SURE YOU INDICATE INPUT                 
         B     PROVLO                                                           
*                                                                               
PROVL4   CLC   ACQACT,SPACES       NO PRODUCT, DO WE HAVE A CLIENT?             
         BE    PROVLO              NO, DONE                                     
*                                                                               
PROVL6   L     R1,AOFFBLK                                                       
         MVC   OFFAOFFC,SOFFICE                                                 
         B     PROVL2              YES, CALL OFFAL WITH THAT OFFICE             
*                                                                               
PROVLE   MVC   FERN,=AL2(INVINPT)  PRODUCT INVALID                              
*                                                                               
PROVLO   BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE JOB                                           *         
***********************************************************************         
*                                                                               
JOBVAL   NTR1                      JOB - 02=ALL 04=XXXXXX                       
         GOTO1 AINITV                                                           
         BNE   JOBVLO              JOB = ALL OR MISSING                         
         CLI   LREQLEVA,0                                                       
         BE    JOBVLE              NO PRODUCTION LEDGER                         
         CLC   ACQACT(6),SPACES                                                 
         BE    PROVLE              JOB SPECIFIC REQUIRES CLI/PRD                
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(8),ACQUNT       C/U/L/CLI/PRD                              
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY+9(0),IFLD                                                    
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) JOB NOT ON FILE                              
         B     JOBVLO                                                           
         MVC   ACQACT+6(6),IFLD                                                 
         OI    FIND,FIVAL          JOB = X THRU XXXXXX                          
         B     JOBVLO                                                           
*                                                                               
JOBVLE   MVC   FERN,=AL2(INVINPT)  JOB INVALID                                  
*                                                                               
JOBVLO   BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE FRENCH FIELD                                  *         
***********************************************************************         
*                                                                               
FRNCHVAL NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   EXIT                                                             
         CLI   IFLD,C'Y'                                                        
         BNE   FRNCHER                                                          
         MVI   ACQLANG,LANGFRE     SET LANGUAGE TO FRENCH                       
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
FRNCHER  MVC   FERN,=AL2(INVINPT)  INVALID INPUT                                
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CLIENT VALIDATION                                      *         
***********************************************************************         
*                                                                               
CLIBVAL  NTR1                      CLI/BNK - 02=ALL 04=XXXXXX                   
         GOTO1 AINITV                                                           
         BNE   CLIBVO              CLIENT = ALL OR MISSING                      
         CLI   IFLD,C'+'           CHK FOR LIST                                 
         BE    LISTV04             GO TO LISTVAL                                
         CLI   IFLD,C'-'                                                        
         BE    LISTV04             GO TO LISTVAL                                
         CLI   IFLDH+5,4                                                        
         BH    CLIBVE                                                           
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),LREQJOB                                                 
         MVC   KEY+3(3),IFLD                                                    
         CLI   IFLD,C'*'           NEGATIVE CLIENT FILTER                       
         BNE   *+10                                                             
         MVC   KEY+3(3),IFLD+1                                                  
         CLC   KEY+3(3),SPACES                                                  
         BE    CLIBVE                                                           
         GOTO1 AIOREAD             READ CLIENT RECORD                           
         BE    CLIB5                                                            
         CLC   ACQPROG,=C'86'      MAY NOT FIND REC FOR 86                      
         BNE   CLIB3                                                            
         MVC   FERN,=AL2(FF)       SET TO FOUND                                 
         B     CLIB5                                                            
*                                                                               
CLIB3    MVC   FERN,=AL2(NTONFILE) CLIENT NOT ON FILE                           
         B     CLIBVO                                                           
*                                                                               
CLIB5    OI    FIND,FIVAL          CLIENT = X THRU XXXXXX                       
         MVC   ACQSEL(3),IFLD                                                   
         CLI   IFLD,C'*'           NEGATIVE CLIENT FILTER                       
         BNE   CLIBVO                                                           
         MVC   ACQSEL(3),IFLD+1                                                 
         CLC   ACQSEL(3),SPACES                                                 
         BE    CLIBVE                                                           
         NI    ACQSEL,X'BF'        SET OFF X'40'  BIT                           
         B     CLIBVO                                                           
*                                                                               
CLIBVE   MVC   FERN,=AL2(INVINPT)  CLIENT INVALID                               
*                                                                               
CLIBVO   BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CHECK DATE VALIDATION                                  *         
***********************************************************************         
*                                                                               
CKDVAL   NTR1                      CHECK DATE - 08=YYMMDD                       
         LA    R5,0                BACKUP 0 DAYS                                
         GOTO1 AINITV                                                           
         BL    CKDVO               MISSING                                      
         BH    CKDVE                                                            
         CLC   IFLD(2),=C'B*'      WANT TO BACK DATE CHECKS?                    
         BNE   CKDVAL10                                                         
         MVC   IFLD(L'IFLD-2),IFLD+2                                            
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'2'                                                         
         STC   R1,IFLDH+5                                                       
         LH    R5,=H'-10'          BACKUP 10 DAYS                               
*                                                                               
CKDVAL10 GOTO1 DATVAL,PLIST,(0,IFLD),ACQSTART                                   
         OC    PLIST(4),PLIST                                                   
         BE    CKDVE                                                            
*                                  BACKDATE UPTO 10 DAYS                        
         MVC   TEMP(6),TODAY                                                    
         GOTO1 ADDAY,PLIST,TEMP,TEMP+6,(R5)                                     
         CLC   ACQSTART,TEMP+6                                                  
         BL    CKDVE                                                            
*                                  FORWARD DATE UPTO 30 DAYS                    
         MVC   TEMP(6),TODAY                                                    
         GOTO1 ADDAY,PLIST,TEMP,TEMP+6,30                                       
         CLC   ACQSTART,TEMP+6                                                  
         BH    CKDVE                                                            
         OI    FIND,FIALT          CHECK DATE = YYMMDD                          
         B     CKDVO                                                            
*                                                                               
CKDVE    MVC   FERN,=AL2(INVINPT) INVALID DATE                                  
*                                                                               
CKDVO    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              SCHEME VALIDATION                                      *         
***********************************************************************         
*                                                                               
SCHVAL   NTR1                      SCHEME - 04=XX                               
         GOTO1 AINITV                                                           
         BNE   SCHVO                                                            
         CLI   IFLDH+5,2                                                        
         BNE   SCHVE                                                            
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(1),ACQCPY                                                  
         MVC   KEY+2(2),ACQUNT                                                  
         MVC   KEY+4(2),IFLD                                                    
         MVC   ACQOPTS(2),IFLD                                                  
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
         B     SCHVO                                                            
         OI    FIND,FIVAL          SET VALID BIT                                
         B     SCHVO                                                            
*                                                                               
SCHVE    MVC   FERN,=AL2(INVINPT)  SCHEME INVALID                               
SCHVO    MVC   KEY(1),ACQCPY       RESET COMP,UNIT,ADV  IN KEY                  
         MVC   KEY+1(2),ACQUNT                                                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              INCREMENT VALIDATION                                   *         
***********************************************************************         
*                                                                               
INCVAL   NTR1                      INCREMENT - 04=NN                            
         GOTO1 AINITV                                                           
         BNE   INCVO                                                            
         CLI   IFLDH+5,1                                                        
         BNE   INCV10                                                           
         CLI   IFLD,C'A'                                                        
         BL    INCVE                                                            
         CLI   IFLD,C'C'                                                        
         BH    INCV10                                                           
         OI    FIND,FIALT          08= A,B,C                                    
         MVC   TEMP+2(1),IFLD                                                   
         MVI   TEMP+3,C' '                                                      
         B     INCV30                                                           
*                                                                               
INCV10   DS    0H                                                               
         SR    R5,R5                                                            
         IC    R5,IFLDH+5                                                       
         L     R4,FADR                                                          
         GOTO1 ARJN,DMCB,(R5),(R4)                                              
         CLC   FERN,=AL2(FF)                                                    
         BL    INCVE                                                            
         CLC   TEMP+2(2),=C'99'                                                 
         BH    INCVE                                                            
         OI    FIND,FIVAL                                                       
*                                                                               
INCV30   LA    R4,ACQOPT3          DEFAULT IN OPTIONS 3/4                       
         CLC   ACQPROG,=C'58'      AC58,                                        
         BE    INCV32                                                           
         CLC   ACQPROG,=C'83'      AC83 - INCREMENT IN                          
         BNE   *+12                                                             
INCV32   LA    R4,ACQSRTAR         SORT AREA                                    
         B     INCV40                                                           
         CLC   ACQPROG,=C'C1'      ACC1 - DAY RANGE IN                          
         BNE   *+8                                                              
         LA    R4,ACQOPT1          OPTS. 1 AND 2                                
*                                                                               
INCV40   MVC   0(2,R4),TEMP+2                                                   
         B     INCVO                                                            
*                                                                               
INCVE    MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
INCVO    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              TRANSACTION TYPE FILTER                                *         
***********************************************************************         
*                                                                               
TNTYPVAL NTR1                      TRANSACTION TYPE FILTER                      
         GOTO1 AINITV                                                           
         BNE   TNTYPVO                                                          
         L     R4,FADR             R4 = A(FIELD HEADER)                         
         SR    R5,R5                                                            
         IC    R5,IFLDH+5          R5 = INPUT LENGTH                            
         CLI   IFLD,C'*'           CHECK FOR EXCLUDE                            
         BNE   TNTYPV2                                                          
         MVC   8(3,R4),IFLD+1                                                   
         SH    R5,=H'1'                                                         
*                                                                               
TNTYPV2  MVC   TEMP+2(3),8(R4)                                                  
         OC    TEMP+2(3),=C'   '                                                
         CLC   TEMP+2(3),=C'M  '     'M' IS ONLY ALLOWABLE CHARACTER            
         BE    TNTYPV4                                                          
         GOTO1 ARJN,DMCB,(R5),(R4)                                              
         CLC   FERN,=AL2(FF)                                                    
         BL    TNTYPVE                                                          
*                                                                               
TNTYPV4  MVC   ACQTTYPE,TEMP+1                                                  
         CLI   IFLD,C'*'           IF EXLUDING                                  
         BNE   *+14                                                             
         NI    ACQTTYPE,X'FF'-X'40'   TURN OFF X'40' BIT IN TYPE NUMBER         
         MVC   8(3,R4),IFLD        RESTORE I/P                                  
         OI    FIND,FIVAL                                                       
         B     TNTYPVO                                                          
*                                                                               
TNTYPVE  MVC   FERN,=AL2(INVINPT)                                               
*                                                                               
TNTYPVO  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              EMPLOYEE VALIDATION                                    *         
***********************************************************************         
*                                                                               
EMPEVAL  NTR1                      EMPLOYEE                                     
         GOTO1 AINITV                                                           
         BNE   EMPEVO              ALL OR MISSING                               
         MVC   KEY+6(9),IFLD                                                    
         MVC   ACQACT+3(9),IFLD                                                 
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
         B     EMPEVO                                                           
         OI    FIND,FIVAL          X'04'=XXXXXXXXX                              
*                                                                               
EMPEVO   BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              LIST VALIDATION                                        *         
***********************************************************************         
*                                                                               
LISTVAL  NTR1                      LIST   04=-XXXXX  08=+XXXXX                  
         GOTO1 AINITV                                                           
         BNE   LISTVO                                                           
LISTV02  MVI   BYTE2,C'N'          UNIT VAL COMES HERE                          
         B     *+8                                                              
LISTV04  MVI   BYTE2,C'Y'          CLIBVAL COMES HERE                           
         CLI   IFLDH+5,2                                                        
         BL    LISTVE                                                           
         CLI   IFLD,C'+'           MUST START WITH + OR -                       
         BE    LISTV06                                                          
         CLI   IFLD,C'-'                                                        
         BNE   LISTVE                                                           
*                                                                               
LISTV06  MVC   KEY,SPACES          INITIALIZE KEY TO SPACES                     
         MVI   KEY,X'1D'                                                        
         MVC   KEY+1(1),ACQCPY                                                  
         MVC   KEY+2(5),IFLD+1                                                  
         OC    KEY+2(5),SPACES                                                  
         MVC   ACQSEL+1(5),KEY+2                                                
         MVC   ACQSEL(1),IFLD                                                   
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
         B     LISTVO                                                           
*                                                                               
         L     R6,AIO1             FIND EXPIRATION DATE                         
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   LISTVE                                                           
*                                                                               
         USING LITELD,R6                                                        
LISTV08  CLC   LITEDAT,TODAYB                                                   
         BL    LISTVE              LIST PAST EXPIRATION DATE                    
         CLC   ACQPROG,=C'P1'                                                   
         BE    *+14                                                             
         CLC   ACQPROG,=C'P8'                                                   
         BNE   LISTV10                                                          
         CLI   LITTYPE,C'W'        MUST BE WORK-CODE TYPE                       
         BNE   LISTVE                                                           
         B     LISTV12                                                          
*                                                                               
LISTV10  CLC   ACQPROG,=C'98'                                                   
         BE    LISTV14                                                          
         CLI   BYTE2,C'Y'          FORCE ACCOUNT TYPE IF CLIB                   
         BNE   LISTV12                                                          
         CLI   LITTYPE,C'A'        MUST BE ACCOUNT TYPE                         
         BNE   LISTVE                                                           
*                                                                               
LISTV12  CLI   IFLD,C'+'           SET VALID BIT                                
         BNE   *+12                                                             
         OI    FIND,FIALT                                                       
         B     *+8                                                              
         OI    FIND,FIVAL          IT MUST BE EXCLUDE                           
         BRAS  RE,DISPNAME                                                      
         B     LISTVO                                                           
*                                                                               
LISTV14  XC    LASTLED,LASTLED     CLEAR LAST LEDGER                            
         MVC   TYPE,LITTYPE        SAVE THE TYPE                                
         CLI   TYPE,LITTLDG        MUST BE LEDGER TYPE                          
         BNE   LISTVE                                                           
*                                                                               
         L     R6,AIO1             LOOK FOR SJ LEDGER                           
         MVI   ELCODE,LIDELQ                                                    
         BAS   RE,GETEL                                                         
         BNE   LISTVE                                                           
*                                                                               
         USING LIDELD,R6                                                        
LISTV16  CLI   LIDEL,0                                                          
         BE    LISTV12             ALL DONE                                     
         CLI   LIDEL,LIDELQ                                                     
         BE    LISTV20                                                          
*                                                                               
LISTV18  XR    RF,RF               KEEP LOOKING                                 
         IC    RF,LIDLN                                                         
         AR    R6,RF                                                            
         B     LISTV16                                                          
*                                                                               
LISTV20  CLI   TYPE,LITTACT        IS THIS AN ACCOUNT TYPE?                     
         BNE   LISTV22             NO                                           
         CLC   LIDDLEDG,=C'SJ'     YES, CHECK FOR SJ LEDGER                     
         MVC   LASTLED,LIDDLEDG    SAVE IT FOR ACREQ02                          
         BNE   LISTV12             FINISH IF OTHER THAN SJ FOUND                
         B     LISTV18             SEE IF ANY MORE ELEMENTS                     
*                                                                               
LISTV22  XR    R0,R0                                                            
         LA    RF,LIDDATA-LIDELD                                                
         XR    R1,R1                                                            
         IC    R1,LIDLN                                                         
         SR    R1,RF               R1=LENGTH OF LIDDATA                         
         XR    RF,RF                                                            
         IC    RF,LIDITLN                                                       
         DR    R0,RF               R1=NO. OF OFFICE CODES IN LIDDATA            
         LR    R3,R1                                                            
         LA    R4,LIDDATA          R4 POINTS TO THE LEDGER CODES                
*                                                                               
LISTV24  CLC   0(2,R4),=C'SJ'                                                   
         MVC   LASTLED,0(R4)       SAVE FOR FINAL VALIDATION?                   
         BNE   LISTV12                                                          
         LA    R4,L'LIDDLEDG(R4)                                                
         BCT   R3,LISTV24                                                       
         B     LISTV12                                                          
*                                                                               
LISTVE   MVC   FERN,=AL2(INVINPT)  LIST INVALID                                 
LISTVO   MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),ACQUNT     RESET COMP,UNIT,ADV  IN KEY                  
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE WIGROUP RECORDS LEDGER                        *         
***********************************************************************         
         SPACE 1                                                                
VALGRP   NTR1                      LEDGER                                       
         MVC   ACQOPT8,SPACES                                                   
         GOTO1 AINITV                                                           
         BNE   VALGX               IF NOT ENTERED - EXIT                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVI   KEY+1,C'F'          UNIT ALWAYS 'F'                              
         MVC   KEY+2(1),IFLD                                                    
         GOTO1 AIOREAD             READ LEDGER RECORD                           
         BNE   VALGXERR                                                         
         MVC   ACQOPT8,IFLD        PUT LEDGER IN QOPT8 FOR PROCESSING           
         MVI   FIND,FIVAL          SET FIELD TO FOUND                           
         B     VALGX                                                            
*                                                                               
VALGXERR MVC   FERN,=AL2(INVLDG)   INVALID LEDGER                               
VALGX    B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
*                                                                               
CONVAL   NTR1                      CONTRA U/L - 04=XX                           
         BAS   RE,CONVLALL                                                      
         TM    FIND,FIVAL                                                       
         BZ    *+10                                                             
         MVC   ACQCNTRA(2),KEY+1   USE HEX LEDGER                               
         B     EXIT                                                             
*                                                                               
CONVAL2  NTR1                      CONTRA U/L - 04=XX                           
         BAS   RE,CONVLALL                                                      
         TM    FIND,FIVAL                                                       
         BZ    *+10                                                             
         MVC   ACQSEL+2(2),IFLD                                                 
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE WI U/L FOR SINGLE LEDGER REQUEST              *         
***********************************************************************         
         SPACE 1                                                                
VALWIUL  NTR1                      UNIT/LEDGER                                  
         MVC   ACQSEL,SPACES                                                    
         GOTO1 AINITV                                                           
         BNE   VALWX               IF NOT ENTERED - EXIT                        
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY       COMPANY                                      
         MVC   KEY+1(2),IFLD       UNIT/LEDGER ENTERED ON SCREEN                
         GOTO1 AIOREAD             READ LEDGER RECORD                           
         BNE   VALWXERR                                                         
*                                                                               
         LA    R0,ULTABQ           # OF U/L IN TABLE                            
         LA    R1,ULTAB            CHECK IF U/L ENTERED IS VALID FOR WI         
VALW10   CLC   IFLD(2),0(R1)                                                    
         BNE   *+18                                                             
         MVC   ACQSEL(2),IFLD                                                   
         MVI   FIND,FIVAL          SET FIELD TO FOUND                           
         B     VALWX                                                            
         LA    R1,L'ULTAB(R1)                                                   
         BCT   R0,VALW10                                                        
*                                                                               
         MVC   FERN,=AL2(INVWIUL)  INVALID LEDGER                               
         B     VALWX                                                            
*                                                                               
VALWXERR MVC   FERN,=AL2(INVLDG)   INVALID LEDGER                               
VALWX    B     EXIT                                                             
*                                                                               
* U/L TABLE                                                                     
*                                                                               
ULTAB    DS    0CL2                                                             
         DC    C'SJ'               PRODUCTION                                   
         DC    C'SP'               PRINT PAYEES                                 
         DC    C'SR'               RECEIVABLES                                  
         DC    C'SS'               SPOT PAYEES                                  
         DC    C'SU'               NETWORK PAYEES                               
         DC    C'SV'               PROD VENDORS                                 
         DC    C'SZ'               MEDIA CONTROL                                
ULTABQ   EQU   (*-ULTAB)/L'ULTAB                                                
         EJECT                                                                  
***********************************************************************         
*              BATCH GROUP VALIDATION                                 *         
***********************************************************************         
*                                                                               
BGROUP   NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   BGROUPX                                                          
         CLI   IFLD,C'P'                                                        
         BE    BGROUP10                                                         
         CLI   IFLD,C'G'                                                        
         BE    BGROUP10                                                         
         MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
*                                                                               
BGROUP10 MVC   ACQCOMNT(1),IFLD                                                 
*                                                                               
BGROUPX  OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              TIMESHEET # VALIDATION                                 *         
***********************************************************************         
*                                                                               
TSVAL    NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   TSX                                                              
*                                                                               
         XC    BLOCK,BLOCK                                                      
         GOTO1 SCANNER,DMCB,(0,IFLDH),(2,BLOCK),C',=- '                         
         CLI   DMCB+4,2                                                         
         BNE   TSE                 MUST HAVE 2 INPUTS                           
         TM    BLOCK+2,X'80'       FIRST PART MUST BE NUMERIC                   
         BZ    TSE                                                              
         TM    BLOCK+34,X'80'      SECOND PART MUST BE NUMERIC                  
         BZ    TSE                                                              
         CLC   BLOCK+4(4),BLOCK+36 1ST # MUST BE <= 2ND #                       
         BH    TSE                                                              
         CLC   BLOCK+4(4),=F'1'    MUST BE IN RANGE 1-99                        
         BL    TSE                                                              
         CLC   BLOCK+36(4),=F'99'                                               
         BH    TSE                                                              
         LA    R4,ACQSEL                                                        
         EDIT  (B4,BLOCK+4),(2,(R4)),ZERO=NOBLANK,FILL=0                        
         EDIT  (B4,BLOCK+36),(2,2(R4)),ZERO=NOBLANK,FILL=0                      
         OI    FIND,FIVAL                                                       
TSX      B     EXIT                                                             
*                                                                               
TSE      MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              CONTRA U/L                                             *         
***********************************************************************         
*                                                                               
CONVLALL NTR1                      VALIDATE CONTRA U/L                          
         GOTO1 AINITV                                                           
         BNE   CONVO                                                            
         CLI   IFLDH+5,2           MUST HAVE BOTH UNIT & LEDGER                 
         BL    CONVE                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(1),IFLD       VALIDATE UNIT                                
         GOTO1 AIOREAD                                                          
         XC    NAME,NAME                                                        
         BE    *+14                                                             
         MVC   FERN,=AL2(INVUNIT)                                               
         B     CONVO                                                            
*                                                                               
         MVC   KEY+1(2),IFLD       VALIDATE LEDGER                              
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   FERN,=AL2(INVLDG)                                                
         B     CONVO                                                            
         OI    FIND,FIVAL                                                       
         B     CONVO                                                            
*                                                                               
CONVE    MVC   FERN,=AL2(INVINPT)  CONTRA U/L INVALID                           
*                                                                               
CONVO    BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
*                                                                               
TRMTHVAL NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   TRMTHX                                                           
         TM    IFLDH+4,X'08'       MUST BE NUMERIC                              
         BZ    TRMTHE                                                           
         CLI   IFLDH+5,1                                                        
         BH    *+14                                                             
         MVC   IFLD+1(1),IFLD      CHANGE X TO 0X                               
         MVI   IFLD,C'0'                                                        
*                                                                               
         CLC   IFLD(2),=C'01'      MINIMUM                                      
         BL    TRMTHE2                                                          
         CLC   IFLD(2),=C'15'      MAXIMUM                                      
         BH    TRMTHE2                                                          
         MVC   ACQSEL(2),IFLD                                                   
         MVI   ACQSEL+2,C'/'                                                    
         OI    FIND,FIVAL                                                       
         B     TRMTHX                                                           
*                                                                               
TRMTHE   MVC   FERN,=AL2(FLDNNUM)                                               
         B     EXIT                                                             
TRMTHE2  MVC   FERN,=AL2(INVINPT)                                               
TRMTHX   B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*                                                                     *         
***********************************************************************         
*                                                                               
TRYRVAL  NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   TRMTHX                                                           
         TM    IFLDH+4,X'08'       MUST BE NUMERIC                              
         BZ    TRMTHE                                                           
         CLI   IFLDH+5,2                                                        
         BNE   TRMTHE2                                                          
         MVC   ACQSEL+3(2),IFLD                                                 
         OI    FIND,FIVAL                                                       
         B     TRMTHX                                                           
         EJECT                                                                  
***********************************************************************         
* CALENDAR PERIOD VALIDATION                                          *         
***********************************************************************         
         SPACE 1                                                                
CALPERD  NTR1                                                                   
         GOTO1 AINITV              04=MMM/YY(-MMM/YY)                           
         BNE   CALVALX             08=(MMM/YY)-MMM/YY  HAS END DATE             
         CLC   =C'TX',ACQPROG                                                   
         BNE   *+12                                                             
         CLI   IFLDH+5,6           MUST HAVE AT LEAST ONE DATE                  
         BL    CALPE                                                            
*                                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM    FIRST CALL YYMM                              
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT                                       
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         CLC   =C'R6',ACQPROG                                                   
         BE    *+8                                                              
         OI    SOFIINDS,SOFIIF1O   SET START OPTIONAL IF NOT R6 RPT             
         MVI   SOFOTYPE,SOFOTSD1+SOFOTSPC                                       
         MVC   SOFAINP,FADR                                                     
         LA    R0,WORK                                                          
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   CALP10                                                           
         OI    FIND,FIALT          START DATE = YYMM                            
         MVC   ACQDTSTR(12),SPACES    CLEAR BOTH START AND END FIELDS           
         MVC   ACQDTSTR(4),WORK                                                 
         MVC   ACQDTSTR+4(2),SPACES                                             
         MVC   ACQDTEND,WORK+4                                                  
         MVC   ACQDTEND+4(2),SPACES                                             
         B     CALP20                                                           
*                                                                               
CALP10   MVI   SOFITYPE,SOFITYMD   SECOND CALL YYMMDD                           
         MVI   SOFOTYPE,SOFOTSD2                                                
         GOTO1 (RF),(R1)                                                        
         BNZ   CALPE                                                            
         OI    FIND,FIVAL          START DATE = YYMMDD                          
         MVC   ACQDTSTR(12),SPACES    CLEAR BOTH START AND END FIELDS           
         MVC   ACQDTSTR(12),WORK      FILL IN STR/END DATES FROM SOFDAT         
*                                                                               
CALP20   MVI   ACQTYP1,ACQDATE                                                  
         MVI   ACQDTTYP,ACQDTPER   CALENDAR PERIOD                              
*        OI    FIND,FIVAL+FIALT    SET VALID BIT                                
         B     CALVALX                                                          
*                                                                               
CALPE    MVC   FERN,=AL2(INVDATEF) INVALID DATE                                 
CALVALX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              BATCH TYPE                                             *         
***********************************************************************         
         SPACE 1                                                                
BTYPVAL  NTR1                                                                   
         GOTO1 AINITV              04=XX                                        
         BNE   EXIT                                                             
         TM    IFLDH+4,X'08'       MUST BE NUMERIC                              
         BNZ   *+14                                                             
         MVC   FERN,=AL2(FLDNNUM)                                               
         B     EXIT                                                             
*                                                                               
         CLI   IFLDH+5,1                                                        
         BH    *+14                                                             
         MVC   IFLD+1(1),IFLD      CHANGE X TO 0X                               
         MVI   IFLD,C'0'                                                        
         MVC   ACQCOMNT+1(2),IFLD                                               
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              PERSON VALIDATION                                      *         
***********************************************************************         
*                                                                               
PERVAL   NTR1                                                                   
         GOTO1 AINITV              04=XX                                        
         BNE   EXIT                                                             
         CLC   ACQPROG,=C'IO'                                                   
         BE    PERVAL04                                                         
         CLC   ACQPROG,=C'IJ'                                                   
         BNE   PERVAL10                                                         
*                                                                               
PERVAL04 MVC   ACQAPPL(8),SPACES                                                
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACQAPPL(0),IFLD                                                  
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
*                                                                               
         USING PERRECD,R4                                                       
PERVAL10 LA    R4,KEY                                                           
         MVC   PERKEY,SPACES                                                    
         MVI   PERKTYP,PERKTYPQ    X'0F' RECORD                                 
         MVC   PERKCPY,ACQCPY      COMPANY CODE                                 
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'1'                                                         
         BM    EXIT                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PERKCODE(0),IFLD                                                 
         OC    PERKCODE,SPACES                                                  
         GOTO1 AIOREAD             READ PERSON RECORD                           
         BE    *+14                                                             
         MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
*                                                                               
         MVC   NAMES,SPACES                                                     
         L     R4,AIO1                                                          
         AH    R4,DATADISP                                                      
PERVAL20 CLI   0(R4),0                                                          
         BE    PERVAL40                                                         
         CLI   0(R4),X'5A'         GENERAL NAME ELEMENT                         
         BE    PERVAL30                                                         
PERVAL25 SR    R1,R1                                                            
         IC    R1,1(R4)                                                         
         AR    R4,R1                                                            
         B     PERVAL20                                                         
*                                                                               
         USING GPNELD,R4                                                        
PERVAL30 LA    RF,NAMEFRST                                                      
         CLI   GPNTYP,GPNTFST      FIRST OR LAST NAME                           
         BE    *+8                                                              
         LA    RF,NAMELAST                                                      
         MVC   1(36,RF),SPACES                                                  
         SR    R1,R1                                                            
         IC    R1,GPNLN                                                         
         SH    R1,=Y(GPNLNQ)                                                    
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),GPNNME      STORE NAME                                   
         OC    1(36,RF),SPACES                                                  
         CLI   GPNTYP,GPNTFST      FIRST OR LAST NAME                           
         BE    PERVAL25                                                         
         LA    R1,NAMELAST+2(R1)                                                
         MVI   0(R1),C','                                                       
         B     PERVAL25                                                         
*                                                                               
PERVAL40 GOTO1 VSQUASH,DMCB,NAMES,L'NAMES                                       
         MVC   NAME,NAMES                                                       
         MVI   ACQTYP1,ACQPRSN     PERSON FILTER                                
         MVC   ACQFLT1(8),IFLD                                                  
         OC    ACQFLT1(8),SPACES                                                
         OI    FIND,FIVAL                                                       
*        GOTO1 DISPNAME                                                         
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              BATCH REFERENCE                                        *         
***********************************************************************         
*                                                                               
BREFVAL  NTR1                                                                   
         GOTO1 AINITV              04=XX                                        
         BNE   EXIT                                                             
         MVC   ACQAPPL+8(4),SPACES                                              
         SR    R1,R1                                                            
         IC    R1,IFLDH+5                                                       
         SH    R1,=H'1'                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACQAPPL+8(0),IFLD                                                
         OI    FIND,FIVAL                                                       
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              OFFICE VALIDATION                                      *         
***********************************************************************         
*                                                                               
OFFVAL   NTR1                                                                   
         CLC   ACQPROG,=C'C8'      FOR THE ACC8 THE OFFICE HAS TO GO            
         BNE   OFFVAL1             SOMEWHERE ELSE.                              
         GOTO1 AINITV                                                           
         BNE   OFFVALO                                                          
         CLC   IFLDH+5(1),SV1RLEV1                                              
         BNH   *+14                                                             
         MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),=C'1R'     VALIDATE 1R OFFICE LEVEL                     
         MVC   KEY+3(2),IFLD                                                    
         GOTO1 AIOREAD             READ CLIENT RECORD                           
         BE    *+14                                                             
         MVC   FERN,=AL2(INVINPT)                                               
         B     EXIT                                                             
*                                                                               
         MVC   ACQAPPL(2),IFLD                                                  
         MVC   ACQOFFFL,SPACES                                                  
         OI    FIND,FIVAL                                                       
         B     OFFVALO                                                          
*                                                                               
OFFVAL1  DS    0H                                                               
         CLC   ACQPROG,=C'AA'                                                   
         BE    *+14                                                             
         CLC   ACQPROG,=C'AB'                                                   
         BNE   OFFVAL1C                                                         
*                                                                               
         GOTO1 AINITV                                                           
*                                                                               
         USING PROFKD,R4                                                        
         LA    R4,WORK             READ AA PROFILE RECORD                       
         XC    WORK,WORK                                                        
         MVI   PROFKSYS,C'A'         ACCOUNT SYSTEM                             
         MVC   PROFKPGM+1(2),=C'AA'  AA PROGRAM                                 
         MVC   PROFKAGY,TWAAGY     ALPHA ID                                     
                                                                                
         XC    DMCB(24),DMCB                                                    
         GOTO1 GETPROF,DMCB,PROFKEY,WORK+L'PROFKEY,DATAMGR,0                    
         OC    WORK+L'PROFKEY(L'PROFKEY),WORK+L'PROFKEY ANY PROFILES?           
         BZ    OFFVAL1A            NO, DON'T ALLOW OFFICE FOR AA/AB             
         CLI   WORK+L'PROFKEY+9,C'Y'                                            
         BE    OFFVAL1D                                                         
*                                                                               
OFFVAL1A MVC   FERN,=AL2(INVOPT2)  OPTION INVALID                               
         B     OFFVALO                                                          
*                                                                               
OFFVAL1C DS    0H                                                               
         GOTO1 AINITV                                                           
*                                                                               
OFFVAL1D DS    0H                                                               
         L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         TM    OFFACST4,X'01'      TEST NEW OFFICES                             
         BO    OFFVAL10            YES                                          
                                                                                
         CLI   FIND,FIINP          TEST IF OFFICE INPUT                         
         BNE   OFFVALO             NO                                           
         CLI   TWAACCS,C'*'        TEST SINGLE OFFICE ACCESS CONTROL            
         BNE   OFFVAL2             NO                                           
         CLI   LOFFPOS,0           TEST OFFICE IN KEY LEDGER                    
         BE    OFFVAL2             NO                                           
         CLI   LOFFPOS,12                                                       
         BH    OFFVAL2                                                          
         B     OFFVAL3             OFFICE IS ALREADY IN ACCOUNT                 
*                                                                               
OFFVAL2  CLI   IFLDH+5,1           TEST FOR ONE BYTE INPUT                      
         BE    OFFVAL4                                                          
         CLI   IFLDH+5,2           TEST FOR TWO BYTES NOW                       
         BH    OFFVAL3             TOO MANY                                     
         CLI   IFLD,C'*'           FILTERING BY OFFICE?                         
         BE    OFFVAL4                                                          
*                                                                               
OFFVAL3  MVC   FERN,=AL2(INVINPT)                                               
         B     OFFVALO                                                          
*                                                                               
OFFVAL4  MVC   OFFAOFFC,IFLD                                                    
         CLI   IFLDH+5,1           TEST FOR ONE BYTE INPUT                      
         BE    OFFVAL5                                                          
         CLI   IFLD,C'*'           IF FIRST BYTE IS * TO FILTER                 
         BNE   *+10                                                             
         MVC   OFFAOFFC,IFLD+1     THEN OFFICE IS IN 2ND BYTE                   
*                                                                               
OFFVAL5  MVI   OFFAACT,OFFAPST                                                  
         GOTO1 OFFAL                                                            
         BE    OFFVAL6                                                          
         MVC   FERN,=AL2(SECLOCK)                                               
         B     OFFVALO                                                          
*                                                                               
OFFVAL6  MVC   ACQOFFFL(1),IFLD    SET IN TRANSACTION FILTER                    
         CLI   IFLDH+5,1           TEST FOR ONE BYTE INPUT                      
         BE    OFFVAL8                                                          
         CLI   IFLD,C'*'           THEN BUMP TO OFFICE                          
         BNE   OFFVAL8                                                          
         MVC   ACQOFFFL(1),IFLD+1  SET IN TRANSACTION FILTER                    
         NI    ACQOFFFL,X'BF'      TURN OFF 1011111 BIT                         
*                                                                               
OFFVAL8  OI    FIND,FIVAL                                                       
         B     OFFVALO                                                          
*                                                                               
OFFVAL10 CLI   FIND,FIINP                                                       
         BNE   OFFVALO                                                          
         MVC   ACQOFFFL,IFLD       SET IN TRANSACTION FILTER                    
         CLI   IFLDH+5,2           TEST FOR TWO BYTE INPUT                      
         BE    OFFVAL11                                                         
*                                                                               
         CLI   IFLD,C'*'           EXCLUDE OFFICE OR LIST?                      
         BNE   OFFVAL11            NO                                           
*                                                                               
         MVC   ACQOFFFL,IFLD+1     SET IN TRANSACTION FILTER                    
         NI    ACQOFFFL,X'BF'      TURN OFF X'40' BIT                           
*                                                                               
OFFVAL11 MVC   OFFAREQL,AREQOFFL                                                
         MVI   OFFAACT,OFFAREQ                                                  
         MVC   OFFAOFFC,ACQOFFFL                                                
         NI    OFFAINDS,X'FF'-OFFAIXOL                                          
         TM    ACQOFFFL,X'40'      EXCLUDING?                                   
         BO    *+12                NO                                           
         OI    OFFAOFFC,X'40'      YES, PUT IT BACK FOR OFFAL                   
         OI    OFFAINDS,OFFAIXOL   SET EXCLUDE OPTION ON                        
         GOTO1 OFFAL                                                            
         BE    OFFVAL12                                                         
         MVC   FERN,=AL2(SECLOCK)                                               
         CLI   OFFAERR,OFFAESEC    TEST SECURITY ERROR                          
         BE    *+10                                                             
         MVC   FERN,=AL2(NTONFILE)                                              
         B     OFFVALO                                                          
*                                                                               
OFFVAL12 OI    FIND,FIVAL                                                       
*                                                                               
OFFVALO  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              FR DEPARTMENT VALIDATION                               *         
***********************************************************************         
*                                                                               
DEPTFR   NTR1                                                                   
         GOTO1 AINITV                                                           
         BNE   DEPTFRX                                                          
         OC    IFLD,SPACES                                                      
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),=C'FR'     READ FOR DEPARTMENT                          
*                                                                               
         MVC   KEY+3(2),IFLD                                                    
         CLI   IFLD,C'*'                                                        
         BNE   *+10                                                             
         MVC   KEY+3(2),IFLD+1                                                  
*                                                                               
         GOTO1 AIOREAD                                                          
         BNE   DEPTFRE                                                          
         OI    FIND,FIVAL                                                       
*                                                                               
         MVC   ACQAPPL(2),IFLD                                                  
         CLI   IFLD,C'*'           NEGATIVE FILTER?                             
         BNE   DEPTFRX             NO, EXIT                                     
         MVC   ACQAPPL(2),IFLD+1                                                
         CLC   ACQAPPL(2),SPACES                                                
         BE    DEPTFRE                                                          
         NI    ACQAPPL,X'BF'                                                    
         B     DEPTFRX                                                          
*                                                                               
DEPTFRE  MVC   FERN,=AL2(INVINPT)  INVALID INPUT FIELD                          
         B     EXIT                                                             
*                                                                               
DEPTFRX  BRAS  RE,DISPNAME                                                      
         B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              VALIDATE PRODUCT                                       *         
***********************************************************************         
*                                                                               
PROBVAL  NTR1                      PRODUCT - 02=ALL 04=XXXXXX                   
         GOTO1 AINITV                                                           
         BNE   PROBV90              PRODUCT = ALL OR MISSING                    
         CLI   IFLDH+5,6                                                        
         BH    PROBV70                                                          
         CLC   ACQSEL(3),SPACES                                                 
         BNE   *+14                                                             
         MVC   FERN,=AL2(CLIREQ)                                                
         B     PROBV80             PRODUCT SPECIFIC & CLIENT NOT                
*                                                                               
         CLI   ACQSEL,C'+'         NOT VALID WITH LIST                          
         BE    PROBV70                                                          
         CLI   ACQSEL,C'-'                                                      
         BE    PROBV70                                                          
         CLI   IFLD,C'+'           CHK FOR LIST                                 
         BE    PROBV10             GO CHECK LIST                                
         CLI   IFLD,C'-'                                                        
         BE    PROBV10             GO CHECK LIST                                
         TM    ACQSEL,X'40'        NEGATIVE CLIENT?                             
         BO    *+12                NO                                           
         CLI   IFLD,C'*'           YES, NEGATIVE PRODUCT?                       
         BE    PROBV70             YES, ERROR                                   
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),LREQJOB    U/L                                          
         MVC   KEY+3(3),ACQSEL     CLIENT                                       
         OI    KEY+3,X'40'         TURN ON IN CASE NEGATIVE CLIENT              
         MVC   KEY+6(3),IFLD       PRODUCT                                      
         CLI   IFLD,C'*'           NEGATIVE PRODUCT?                            
         BNE   *+10                NO                                           
         MVC   KEY+6(3),IFLD+1                                                  
         CLC   KEY+6(3),SPACES                                                  
         BZ    PROBV70                                                          
*                                                                               
         GOTO1 AIOREAD             READ PRODUCT RECORD                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) NOT ON FILE                                  
         B     PROBV90                                                          
*                                                                               
         MVC   ACQSEL+3(3),IFLD                                                 
         OI    FIND,FIVAL          PRODUCT = X THRU XXXXXX                      
         CLI   IFLD,C'*'           NEGATIVE PRODUCT?                            
         BNE   PROBV90             NO                                           
         MVC   ACQSEL+3(3),IFLD+1                                               
         NI    ACQSEL+3,X'BF'      TIURN OF X'40' BIT                           
         B     PROBV90                                                          
*                                                                               
PROBV10  CLC   ACQPROG,=C'55'      LIST ONLY VALID FOR AC55                     
         BNE   PROBV70                                                          
         MVC   KEY,SPACES          INITIALIZE KEY TO SPACES                     
         MVI   KEY,X'1D'                                                        
         MVC   KEY+1(1),ACQCPY                                                  
         MVC   KEY+2(5),IFLD+1                                                  
         OC    KEY+2(5),SPACES                                                  
         MVC   ACQSRTAR(6),IFLD                                                 
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
         B     PROBV80                                                          
*                                                                               
         L     R6,AIO1             FIND EXPIRATION DATE                         
         MVI   ELCODE,X'1E'        LIST TYPE ELEMENT                            
         BAS   RE,GETEL                                                         
         BNE   PROBV70                                                          
*                                                                               
         USING LITELD,R6                                                        
         CLC   LITEDAT,TODAYB                                                   
         BL    PROBV70             LIST PAST EXPIRATION DATE                    
         CLI   LITTYPE,C'A'        MUST BE ACCOUNT TYPE                         
         BNE   PROBV70                                                          
*                                                                               
         MVI   FLAG,0                                                           
         MVI   ELCODE,X'1F'        LIST DATA ELEMENT                            
PROBV20  BAS   RE,NEXTEL                                                        
         BE    PROBV30                                                          
         TM    FLAG,FLG1F          DID WE GET A LIST DATA ELM                   
         BO    PROBV60                                                          
         B     PROBV70             HAS TO BE A LIST DATA ELEM                   
*                                                                               
         USING LIDELD,R6                                                        
PROBV30  CLC   LIDDLEDG,LREQJOB    IS THIS FOR SJ?                              
         BNE   PROBV20                                                          
         OI    FLAG,FLG1F                                                       
         CLI   LIDDALVL,2                                                       
         BL    PROBV20                                                          
         OI    FLAG,FLGLEV         GOT AT LEAST THE LEVEL                       
*                                                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         IC    RF,LIDLN            FIND OUT THE # OF ENTRIES IN LIST            
         SHI   RF,LIDDACCS-LIDELD                                               
         SR    R2,R2                                                            
         IC    R2,LIDITLN                                                       
         LTR   R2,R2                                                            
         BZ    PROBV70                                                          
         DR    RE,R2                                                            
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         LR    R0,RF               NUMBER OF ENTRIES IN LIST                    
*                                                                               
         LA    RE,LIDDACCS         POINT TO ACCOUNTS                            
PROBV40  DS    0H                                                               
*                                                                               
         NI    FLAG,X'FF'-FLGCLI2  TURN OFF PRD WITH MATCHING CLI               
         LR    RF,RE                                                            
         SR    R1,R1                                                            
         IC    R1,LREQLEVA                                                      
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   ACQSEL(0),0(RE)                                                  
         BNE   *+8                                                              
         OI    FLAG,FLGCLI+FLGCLI2                                              
         AHI   R1,1                                                             
*                                                                               
         LR    R2,R1                                                            
         AR    RF,R2                                                            
         SR    R1,R1                                                            
         IC    R1,LREQLEVB                                                      
         SR    R1,R2                                                            
         AHI   R1,-1                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   0(0,RF),SPACES                                                   
         BNH   PROBV50                                                          
         OI    FLAG,FLGPRD         SHOW THAT WE GOT A PRODUCT                   
         TM    FLAG,FLGCLI2        IS THIS A CLI/PRD MATCH?                     
         BNO   *+12                                                             
         OI    FLAG,FLGPRD2                                                     
         B     PROBV60                                                          
*                                                                               
PROBV50  SR    R1,R1                                                            
         IC    R1,LIDITLN                                                       
         AR    RE,R1               BUMP TO NEXT ENTRY                           
         BCT   R0,PROBV40                                                       
         B     PROBV20                                                          
*                                                                               
PROBV60  DS    0H                                                               
         TM    FLAG,FLGLEV         DID WE GET A PROD LEVEL?                     
         BO    *+14                                                             
         MVC   FERN,=AL2(NOTPRDLS) NOT DEFINED AT PRD LEV                       
         B     PROBV80                                                          
         TM    FLAG,FLGCLI         DID WE MATCH A CLIENT IN LIST?               
         BO    *+14                                                             
         MVC   FERN,=AL2(NOCLILIS) CLIENT NOT IN LIST                           
         B     PROBV80                                                          
         TM    FLAG,FLGPRD         DID WE GET A PROD?                           
         BO    *+14                                                             
         MVC   FERN,=AL2(INVPRDLS) NO PRDS IN LIST                              
         B     PROBV80                                                          
         TM    FLAG,FLGPRD2        DID WE GET A CLI/PRD MATCH?                  
         BO    *+14                                                             
         MVC   FERN,=AL2(INVCLIPR) NO CLI MATCH AT PRD LEVEL                    
         B     PROBV80                                                          
*                                                                               
         CLI   IFLD,C'+'           SET VALID BIT                                
         BNE   *+12                                                             
         OI    FIND,FIALT                                                       
         B     *+8                                                              
         OI    FIND,FIVAL          IT MUST BE EXCLUDE                           
         BRAS  RE,DISPNAME                                                      
         B     PROBV80                                                          
*                                                                               
PROBV70  MVC   FERN,=AL2(INVINPT)  PRODUCT INVALID                              
PROBV80  MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),ACQUNT     RESET COMP,UNIT,ADV  IN KEY                  
         B     PROBVLX                                                          
*                                                                               
PROBV90  BRAS  RE,DISPNAME                                                      
PROBVLX  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
* VALIDATE CLIENT IN U/L=1F FOR ACREPFA02                             *         
***********************************************************************         
         SPACE 1                                                                
FACLI    NTR1                      CHECK CLIENT                                 
         GOTO1 AINITV                                                           
         BNE   FACLIVO             CLIENT = ALL OR MISSING                      
         MVC   KEY,SPACES                                                       
         MVC   KEY(3),ACQCPY                                                    
         MVC   KEY+3(12),IFLD                                                   
         GOTO1 AIOREAD             READ CLIENT RECORD                           
         BNE   FACLIERR            DISK ERROR                                   
         OI    FIND,FIVAL          CLIENT = X THRU XXXXXX                       
         MVC   ACQACT,IFLD                                                      
         B     FACLIX                                                           
*                                                                               
FACLIERR MVC   FERN,=AL2(INVINPT)  CLIENT INVALID                               
         B     EXIT                                                             
*                                                                               
FACLIX   BRAS  RE,DISPNAME                                                      
FACLIVO  B     EXIT                                                             
         EJECT                                                                  
***********************************************************************         
*              GETELS                                                 *         
***********************************************************************         
         SPACE 1                                                                
         GETEL R6,DATADISP,ELCODE                                               
         EJECT                                                                  
***********************************************************************         
*              CONSTANTS/LITERALS                                     *         
***********************************************************************         
         SPACE 1                                                                
DATADISP DC    H'49'                                                            
DMREAD   DC    CL8'DMREAD'                                                      
DMRDHI   DC    CL8'DMRDHI'                                                      
CTFILE   DC    CL8'CTFILE'                                                      
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              VALIDATION ROUTINE TABLE                               *         
***********************************************************************         
*                                                                               
ROUTADRT DC    F'0'                00 - N/D                                     
         DC    A(UNITVAL)          01 - UNIT                                    
         DC    A(LDGVAL)           02 - LEDGER                                  
         DC    A(CLI3VAL)          03 - CLIENT CHECK                            
         DC    A(BGRPVAL)          04 - BILLING GROUP                           
         DC    A(FTR1VAL)          05 - FILTER#1                                
         DC    A(FTR2VAL)          06 - FILTER#2                                
         DC    A(STARTVAL)         07 - START DATE                              
         DC    A(ENDVAL)           08 - END DATE                                
         DC    A(SORTVAL)          09 - SORT OPTION                             
         DC    A(OPT1VAL)          10 - OPTION#1                                
         DC    A(RECDVAL)          11 - RECORD DATE VALIDATE                    
         DC    A(CLIVAL)           12 - CLIENT                                  
         DC    A(PROVAL)           13 - PRODUCT                                 
         DC    A(JOBVAL)           14 - JOB                                     
         DC    A(FACLI)            15 - CLIENT FIELD (U/L=1F)                   
         DC    A(FRNCHVAL)         16 - FRENCH FIELD                            
         DC    A(OPT2VAL)          17 - OPTION#2                                
         DC    A(OPT3VAL)          18 - OPTION#3                                
         DC    A(BUCKET)           19 - BUCKET TYPE IN QSRTAREA+2               
         DC    A(MEDFTR)           20 - MEDIA FILTER                            
         DC    A(BILFTR)           21 - BILLING FILTER                          
         DC    A(BTYPVAL)          22 - BATCH TYPE                              
         DC    A(PERVAL)           23 - PERSON                                  
         DC    A(BREFVAL)          24 - BATCH REFERENCE                         
         DC    A(CLIBVAL)          25 - CLIENT(BANK)                            
         DC    A(CKDVAL)           26 - CHECK DATE                              
         DC    A(FTR5VAL)          27 - FILTER#5                                
         DC    A(FTR4VAL)          28 - FILTER#4                                
         DC    A(OPT4VAL)          29 - OPTION#4                                
         DC    A(OPT5VAL)          30 - OPTION#5                                
         DC    A(SCHVAL)           31 - SCHEME                                  
         DC    A(INCVAL)           32 - INCREMENT                               
         DC    A(OPT6VAL)          33 - OPTION#6                                
         DC    A(TNTYPVAL)         34 - TRANSACTION TYPE FILTER                 
         DC    A(FTR3VAL)          35 - FILTER#3                                
         DC    A(EMPEVAL)          36 - EMPLOYEE                                
         DC    A(LISTVAL)          37 - LISTVAL                                 
         DC    A(OPT7VAL)          38 - OPTION 7                                
         DC    A(CONVAL)           39 - CONTRA U/L IN ACQCNTRA                  
         DC    A(BGROUP)           40 - BATCH GROUP                             
         DC    A(TSVAL)            41 - TIME SHEET # VALIDATION                 
         DC    A(OPT8VAL)          42 - OPTION 8                                
         DC    A(OPT9VAL)          43 - OPTION 9                                
         DC    A(OPTAVAL)          44 - OPTION 10                               
         DC    A(TRMTHVAL)         45 - TRANSMISSION MONTH IN QSELECT           
         DC    A(TRYRVAL)          46 - TRANSMISSION YEAR IN QSELECT+3          
         DC    A(CALPERD)          47 - VALIDATE CALENDAR PERIOD                
         DC    A(DEPTFR)           48 - FR DEPARTMENT IN QAPPL                  
         DC    A(PROBVAL)          49 - PRODUCT IN QSELECT+3                    
         DC    A(VALGRP)           50 - VALIDATE WIGROUP RECORDS LEDGER         
         DC    A(CONVAL2)          51 - CONTRA U/L IN QSELECT+2                 
         DC    A(VALWIUL)          52 - VALIDATE WI U/L                         
         DC    A(OFFVAL)           53 - OFFICE IN ACQOFFFL                      
         DC    A(OCVAL)            54 - OFFICE FOR 55 (CHECKS)                  
         DC    A(CKCLIVAL)         55 - CLIENT VALIDATION FOR AC55              
         DC    A(TMONVAL)          56 - FVFLTX2 MOA ACTX                        
         DC    A(TWRKVAL)          57 - FVFLTX3 WORK FOR ACTX                   
         DC    A(TUNTVAL)          58 - FVFLTX4 TO ACCOUNT FOR ACTX             
         DC    A(TLDGVAL)          59 - FVFLTX4 TO ACCOUNT FOR ACTX             
         DC    A(TACTVAL)          60 - FVFLTX4 TO ACCOUNT FOR ACTX             
         DC    A(FHRSVAL)          61 - FVFLTX6 HOUR FOR ACTX                   
         DC    A(REVVAL)           62 - FVFLTX8 REVERSAL REF FOR ACTX           
         DC    A(NARVAL)           63 - FVNARR NARRATIVE FOR ACTX               
         DC    A(VNDLVAL)          64 - VENDOR LEVEL                            
         DC    A(NAABVAL)          65 - N/A INVALID FOR AB                      
         DC    X'FF'                                                            
*                                                                               
ADCONS   DC    A(OPT1VTBL)                                                      
         DC    A(OPT2VTBL)                                                      
         DC    A(OPT3VTBL)                                                      
         DC    A(OPT4VTBL)                                                      
         DC    A(OPT5VTBL)                                                      
         DC    A(OPT6VTBL)                                                      
         DC    A(OPT7VTBL)                                                      
         DC    A(OPT8VTBL)                                                      
         DC    A(OPT9VTBL)                                                      
         DC    A(OPTAVTBL)                                                      
NADCONS  EQU   (*-ADCONS)/4                                                     
         EJECT                                                                  
***********************************************************************         
*              OFFICE VALIDATION FOR CHECKS                           *         
***********************************************************************         
*                                                                               
OCVAL    NMOD1 0,**OCHK                                                         
         GOTO1 AINITV                                                           
         TM    OUTSTAT,RUNQ                                                     
         BO    OCVALX                                                           
         TM    COMPSTA4,X'01'      TEST NEW OFFICES                             
         BO    OCVAL10             YES                                          
*                                                                               
OCVAL1   CLI   FIND,FIINP          TEST IF FIELD INPUT                          
         BE    OCVAL2              YES                                          
         CLI   TWAACCS,C'*'        TEST SINGLE ACCESS                           
         BE    *+12                YES                                          
         TM    LOFFCHK,X'40'       TEST OFFICE CHECK REQUIRES OFFICE            
         BZ    OCVALX              NO-CAN EXIT NOW                              
         CLI   LOFFPOS,0           TEST OFFICE IN KEY LEDGER                    
         BE    *+12                                                             
         CLI   LOFFPOS,12                                                       
         BNH   OCVALX              YES-OK FOR NO INPUT                          
         MVC   FERN,=AL2(FLDMIS)   MISSING INP FLD                              
         MVI   FIND,FIINP          FORCE MISSING INP MESSAGE                    
         B     OCVALX                                                           
*                                                                               
OCVAL2   CLI   TWAACCS,C'*'        TEST SINGLE OFFICE ACCESS                    
         BNE   OCVAL3              NO                                           
         CLI   LOFFPOS,0           TEST OFFICE IN KEY LEDGER                    
         BE    OCVAL3              NO                                           
         CLI   LOFFPOS,12                                                       
         BH    OCVAL3              YES-CAN EXIT NOW                             
         B     OCVAL4                                                           
*                                                                               
OCVAL3   CLI   IFLDH+5,1           TEST ONE BYTE INPUT                          
         BE    OCVAL5                                                           
         CLI   IFLDH+5,2           TEST TWO BYTE INPUT                          
         BH    OCVAL4              TOO MANY                                     
         CLI   IFLD,C'*'           EXCLUDING AN OFFICE?                         
         B     OCVAL5              YES                                          
*                                                                               
OCVAL4   MVC   FERN,=AL2(INVINPT)                                               
         B     OCVALX                                                           
*                                                                               
OCVAL5   L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAOFFC,IFLD                                                    
         CLI   IFLD,C'*'           EXCLUDING?                                   
         BNE   *+10                                                             
         MVC   OFFAOFFC,IFLD+1                                                  
         MVI   OFFAACT,OFFAPST                                                  
         GOTO1 OFFAL                                                            
         BE    OCVAL6                                                           
         MVC   FERN,=AL2(SECLOCK)                                               
         B     OCVALX                                                           
*                                                                               
OCVAL6   MVC   ACQOFFFL,SPACES                                                  
         MVC   ACQOFFFL(1),OFFAOFFC                                             
         CLI   IFLD,C'*'                                                        
         BNE   OCVAL7                                                           
         NI    ACQOFFFL,X'BF'                                                   
*                                                                               
OCVAL7   OI    FIND,FIVAL          NOTE IT WAS INPUT                            
         B     OCVALX                                                           
*                                                                               
* TWO BYTE OFFICE VALIDATION                                                    
*                                                                               
         USING CHARECD,RF                                                       
OCVAL10  LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' CHECK AUTHORIZATION REC                
         MVC   CHAKCULA,ACQCPY                                                  
         MVC   LKEY,KEY                                                         
         GOTO1 DATAMGR,DMCB,DMREAD,=CL8'ACCOUNT',KEY,AIO1                       
         CLI   DMCB+8,0                                                         
         BNE   OCVAL20                                                          
*                                                                               
OCVAL11  L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
*                                                                               
OCVAL12  CLI   0(R6),0             TEST EOR                                     
         BE    OCVAL14A            YES                                          
         CLI   0(R6),X'54'         TEST OFFICE CHECK ELEMENT                    
         BE    OCVAL15                                                          
OCVAL14  SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     OCVAL12                                                          
*                                                                               
OCVAL14A GOTO1 DATAMGR,DMCB,=CL8'DMRSEQ',=CL8'ACCOUNT',AIO1,AIO1                
         L     R6,AIO1                                                          
         CLC   LKEY(4),0(R6)                                                    
         BE    OCVAL11                                                          
         B     OCVAL20                                                          
*                                                                               
         USING OCNELD,R6                                                        
OCVAL15  CLC   OCNOFFID,TWAUSRID   TEST FOR MATCH ON USER ID                    
         BNE   OCVAL14             NO                                           
         CLI   OCNFILT,C'*'        TEST FOR AN OFFICE FILTER                    
         BNE   OCVAL14             NO                                           
         CLI   FIND,FIINP          TEST FOR INPUT                               
         BNE   OCVAL16                                                          
         MVC   FERN,=AL2(INVINPT)                                               
         B     OCVALX                                                           
*                                                                               
OCVAL16  MVC   IFLD(2),OCNFILT+1   EXTRACT OFFICE FILTER                        
         OC    IFLD(2),SPACES                                                   
         L     R4,FADR             TREAT IT AS THOUGH IT WAS INPUT              
         MVC   8(2,R4),IFLD        RETURN IT TO SCREEN                          
         MVI   5(R4),1                                                          
         CLI   IFLD+1,C' '                                                      
         BE    *+8                                                              
         MVI   5(R4),2                                                          
         OI    6(R4),X'80'         XMIT BACK                                    
         MVC   IFLDH+5(1),5(R4)    SET INPUT LENGTH                             
         OI    FIND,FIINP          TURN ON INPUT BIT IF ERROR OCCURS            
*                                                                               
OCVAL20  CLI   IFLDH+5,0           TEST FOR ANY INPUT                           
         BE    OCVALX                                                           
*                                                                               
         MVC   ACQOFFFL,IFLD       MOVE OFFICE TO REQUEST CARD                  
         CLI   IFLDH+5,2           TWO BYTES ENTERED?                           
         BE    OCVAL22             YES                                          
*                                                                               
         CLI   IFLD,C'*'           EXCLUDE OFFICE OR LIST?                      
         BNE   OCVAL22             NO                                           
*                                                                               
         MVC   ACQOFFFL,IFLD+1     SHIFT OFFICE                                 
         NI    ACQOFFFL,X'BF'      TUEN OFF X'40' BIT                           
*                                                                               
OCVAL22  L     R1,AOFFBLK                                                       
         USING OFFALD,R1                                                        
         MVC   OFFAREQL,AREQOFFL                                                
         MVI   OFFAACT,OFFAREQ                                                  
         MVC   OFFAOFFC,ACQOFFFL                                                
         NI    OFFAINDS,X'FF'-OFFAIXOL                                          
         TM    ACQOFFFL,X'40'      EXCLUDING?                                   
         BO    *+12                NO                                           
         OI    OFFAOFFC,X'40'      YES, PUT BIT ON FOR OFFAL                    
         OI    OFFAINDS,OFFAIXOL   SET EXLUDE OPTION ON                         
         GOTO1 OFFAL                                                            
         BE    OCVAL24                                                          
         MVC   FERN,=AL2(SECLOCK)                                               
         CLI   OFFAERR,OFFAESEC    TEST FOR SECURITY ERROR                      
         BE    *+10                                                             
         MVC   FERN,=AL2(NTONFILE)                                              
         B     OCVALX                                                           
*                                                                               
OCVAL24  OI    FIND,FIVAL                                                       
*                                                                               
OCVALX   XMOD1 1                                                                
         DROP  R1,R6                                                            
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              CLIENT VALIDATION                                      *         
***********************************************************************         
         SPACE 1                                                                
CKCLIVAL NMOD1 0,*CKCL*                                                         
         GOTO1 AINITV                                                           
         TM    OUTSTAT,RUNQ                                                     
         BO    OCVALX                                                           
*                                                                               
         L     R1,FADR                                                          
         CLI   5(R1),0                                                          
         BNE   CKCLI60             CLIENT = ALL OR MISSING                      
*                                                                               
         MVC   FERN,=AL2(FF)                                                    
*                                                                               
         USING CHARECD,RF                                                       
         LA    RF,KEY                                                           
         XC    CHAKEY,CHAKEY                                                    
         MVI   CHAKTYP,CHAKTYPQ    X'10' CHECK AUTHORIZATION REC                
         MVC   CHAKCULA,ACQCPY                                                  
         MVC   LKEY,KEY                                                         
         GOTO1 DATAMGR,DMCB,DMREAD,=CL8'ACCOUNT',KEY,AIO1                       
         CLI   DMCB+8,0                                                         
         BNE   CKCLI60                                                          
*                                                                               
CKCLI10  DS    0H                                                               
         L     R6,AIO1                                                          
         AH    R6,DATADISP                                                      
*                                                                               
CKCLI20  DS    0H                                                               
         CLI   0(R6),X'54'         TEST OFFICE CHECK ELEMENT                    
         BE    CKCLI40                                                          
*                                                                               
         CLI   0(R6),0             TEST EOR                                     
         BNE   CKCLI30             YES                                          
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'DMRSEQ',=CL8'ACCOUNT',AIO1,AIO1                
         L     R6,AIO1                                                          
         CLC   LKEY(4),0(R6)                                                    
         BE    CKCLI10                                                          
         B     CKCLI60                                                          
*                                                                               
CKCLI30  DS    0H                                                               
         SR    R1,R1                                                            
         IC    R1,1(R6)                                                         
         AR    R6,R1                                                            
         B     CKCLI20                                                          
*                                                                               
         USING OCNELD,R6                                                        
CKCLI40  DS    0H                                                               
         CLC   OCNOFFID,TWAUSRID   TEST FOR MATCH ON USER ID                    
         BNE   CKCLI30             NO                                           
         CLI   OCNFILT,C'*'        TEST FOR AN OFFICE FILTER                    
         BE    CKCLI30             SKIP IF OFFICE FILTER                        
         CLC   OCNFILT,SPACES      DO WE HAVE ANYTHING?                         
         BNH   CKCLI30             NO SKIP                                      
         CLI   FIND,FIINP          TEST FOR INPUT                               
         BNE   CKCLI50                                                          
         MVC   FERN,=AL2(INVINPT)                                               
         B     CKCLIX                                                           
*                                                                               
CKCLI50  DS    0H                                                               
         MVC   IFLD(3),OCNFILT     EXTRACT OFFICE FILTER                        
         OC    IFLD(3),SPACES                                                   
         L     R4,FADR             TREAT IT AS THOUGH IT WAS INPUT              
         MVC   8(3,R4),IFLD        RETURN IT TO SCREEN                          
         MVI   5(R4),2                                                          
         CLI   IFLD+2,C' '                                                      
         BE    *+8                                                              
         MVI   5(R4),3                                                          
         OI    6(R4),X'80'         TRANSMIT TO SCREEN                           
         MVC   IFLDH,0(R4)         COPY HEADER INTO TEMP HEADER                 
         OI    FIND,FIINP          TURN ON INPUT BIT IF ERROR OCCURS            
*                                                                               
CKCLI60  DS    0H                                                               
         CLI   IFLD,C'+'           CHK FOR LIST                                 
         BE    CKCLI100            GO CHECK LIST                                
         CLI   IFLD,C'-'                                                        
         BE    CKCLI100            GO CHECK LIST                                
         CLI   IFLDH+5,4                                                        
         BH    CKCLI80             CLIENT IS INVALID                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),LREQJOB                                                 
         MVC   KEY+3(3),IFLD                                                    
         CLI   IFLD,C'*'           NEGATIVE CLIENT FILTER                       
         BNE   *+10                                                             
         MVC   KEY+3(3),IFLD+1                                                  
         CLC   KEY+3(3),SPACES                                                  
         BE    CKCLI80                                                          
         GOTO1 AIOREAD             READ CLIENT RECORD                           
         BE    CKCLI70                                                          
         CLC   ACQPROG,=C'86'      MAY NOT FIND REC FOR 86                      
         BE    CKCLI70                                                          
         MVC   FERN,=AL2(NTONFILE) CLIENT NOT ON FILE                           
         B     CKCLIX                                                           
*                                                                               
CKCLI70  DS    0H                                                               
         OI    FIND,FIVAL          CLIENT = X THRU XXXXXX                       
         MVC   ACQSEL(3),IFLD                                                   
         CLI   IFLD,C'*'           NEGATIVE CLIENT FILTER                       
         BNE   CKCLI90                                                          
         MVC   ACQSEL(3),IFLD+1                                                 
         CLC   ACQSEL(3),SPACES                                                 
         BE    CKCLI80                                                          
         NI    ACQSEL,X'BF'        SET OFF X'40'  BIT                           
         B     CKCLI90                                                          
*                                                                               
CKCLI80  DS    0H                                                               
         MVC   FERN,=AL2(INVINPT)  CLIENT INVALID                               
*                                                                               
CKCLI90  DS    0H                                                               
         BRAS  RE,DISPNAME                                                      
         B     CKCLIX                                                           
*                                                                               
* CLIENT LIST VALIDATION                                                        
*                                                                               
CKCLI100 CLI   IFLDH+5,2                                                        
         BL    CKCLI150                                                         
         CLI   IFLD,C'+'           MUST START WITH + OR -                       
         BE    CKCLI110                                                         
         CLI   IFLD,C'-'                                                        
         BNE   CKCLI150                                                         
*                                                                               
CKCLI110 MVC   KEY,SPACES          INITIALIZE KEY TO SPACES                     
         MVI   KEY,X'1D'                                                        
         MVC   KEY+1(1),ACQCPY                                                  
         MVC   KEY+2(5),IFLD+1                                                  
         OC    KEY+2(5),SPACES                                                  
         MVC   ACQSEL+1(5),KEY+2                                                
         MVC   ACQSEL(1),IFLD                                                   
         GOTO1 AIOREAD                                                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) NOT FOUND                                    
         B     CKCLI160                                                         
*                                                                               
         L     R6,AIO1             FIND EXPIRATION DATE                         
         MVI   ELCODE,X'1E'                                                     
         BAS   RE,GETEL                                                         
         BNE   CKCLI150                                                         
*                                                                               
         USING LITELD,R6                                                        
         CLC   LITEDAT,TODAYB                                                   
         BL    CKCLI150            LIST PAST EXPIRATION DATE                    
         CLC   ACQPROG,=C'P1'                                                   
         BE    *+14                                                             
         CLC   ACQPROG,=C'P8'                                                   
         BNE   CKCLI120                                                         
         CLI   LITTYPE,C'W'        MUST BE WORK-CODE TYPE                       
         BNE   CKCLI150                                                         
         B     CKCLI140                                                         
*                                                                               
CKCLI120 CLI   LITTYPE,C'A'        MUST BE ACCOUNT TYPE                         
         BNE   CKCLI150                                                         
*                                                                               
         MVI   FLAG,0                                                           
         MVI   ELCODE,X'1F'        LIST DATA ELEMENT                            
CKCLI130 BAS   RE,NEXTEL                                                        
         BNE   CKCLI150                                                         
*                                                                               
         USING LIDELD,R6                                                        
         CLC   LIDDLEDG,LREQJOB    IS THIS FOR SJ?                              
         BNE   CKCLI130                                                         
         CLI   LIDDALVL,1                                                       
         BNE   CKCLI130                                                         
*                                                                               
CKCLI140 CLI   IFLD,C'+'           SET VALID BIT                                
         BNE   *+12                                                             
         OI    FIND,FIALT                                                       
         B     *+8                                                              
         OI    FIND,FIVAL          IT MUST BE EXCLUDE                           
         BRAS  RE,DISPNAME                                                      
         B     CKCLI160                                                         
*                                                                               
CKCLI150 MVC   FERN,=AL2(INVINPT)  LIST INVALID                                 
CKCLI160 MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(2),ACQUNT     RESET COMP,UNIT,ADV  IN KEY                  
CKCLIX   XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              MOA VALIDATION  (FILTER #2 FOR ACTX)                   *         
***********************************************************************         
*                                                                               
TMONVAL  NMOD1 0,*TOMV*                                                         
         GOTO1 AINITV                                                           
         BNE   TMOVX                                                            
*                                                                               
         LA    R1,TEMP                                                          
         USING SOFDATD,R1                                                       
         XC    SOFDATD(SOFDATL),SOFDATD                                         
         MVI   SOFITYPE,SOFITYM                                                 
         MVI   SOFIINDS,SOFIIANY+SOFIIOUT                                       
         TM    RFPSTAT,RFPINUSE                                                 
         BNZ   *+8                                                              
         OI    SOFIINDS,SOFIIRES   NOT RFP - RESOLVE DATES                      
         OI    SOFIINDS,SOFIIF1O   SET START OPTIONAL IF NOT R6 RPT             
         MVI   SOFOTYPE,SOFOTSD1                                                
         MVC   SOFAINP,FADR                                                     
         LA    R0,ACQFLT2                                                       
         ST    R0,SOFAOUT                                                       
         MVC   SOFACOM,ACOMFACS                                                 
         MVI   SOFSYSN,6           SET ACCPAK SYSTEM                            
         MVI   SOFLANG,2                                                        
         MVC   SOFACFST,COMPMOSX                                                
         GOTO1 VSOFDAT                                                          
         BNZ   TMOVE                                                            
*                                                                               
         XC    WORK,WORK                                                        
         GOTO1 ABMONVAL,DMCB,(6,IFLD),(49,ACOMFACS),(0,WORK),(ACQCPY,0)         
         LA    R1,WORK                                                          
         USING BMONVALD,R1                                                      
         CLI   BMOERR,BMOEOKQ      EVERYTHING OK?                               
         BE    TMOV2                                                            
         MVC   FERN,=AL2(MOSLOCK)                                               
         B     TMOVX                                                            
*                                                                               
TMOV2    OI    FIND,FIVAL          SET VALID BIT                                
         MVI   ACQTYP2,C'2'        SET INDICATOR FOR ACTX                       
         B     TMOVX                                                            
*                                                                               
TMOVE    MVC   FERN,=AL2(INVINPT)  INVALID FIELD                                
*                                                                               
TMOVX    XMOD1                                                                  
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
*                                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              WORK VALIDATION (FILTER #3 FOR ACTX)                   *         
***********************************************************************         
         USING WCOELD,R6                                                        
TWRKVAL  NMOD1 0,*TWRV*                                                         
         GOTO1 AINITV                                                           
         BL    TWRVX               MISSING                                      
         CLI   IFLDH+5,2                                                        
         BH    TWRVE                                                            
         MVC   KEY,SPACES          BUILD KEY FOR WORK-CODE RECORD               
         MVI   KEY,X'0A'                                                        
         MVC   KEY+1(L'ACQCPY),ACQCPY                                           
         MVC   KEY+2(2),ACQFLT4                                                 
         MVC   KEY+4(2),IFLD                                                    
         GOTO1 AIOREAD                                                          
         BNE   TWRVE               WORK-CODE NOT FOUND                          
         L     R6,AIO1                                                          
         MVI   ELCODE,X'12'        GET ANALYSIS ELEMENT                         
         BAS   RE,GETEL                                                         
         BNE   TWRVE                                                            
         MVC   ACQFLT3(2),IFLD                                                  
         MVI   ACQTYP3,C'3'        SET INDICATOR FOR ACTX                       
         OI    FIND,FIVAL          FILTER3 = X                                  
         B     TWRVX                                                            
*                                                                               
TWRVE    MVC   FERN,=AL2(INVWKCD)  WORK-CODE INVALID                            
TWRVX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              TO UNIT VALIDATION (FILTER #4 FOR ACTX)                *         
***********************************************************************         
*                                                                               
TUNTVAL  NMOD1 0,*TUNV*                                                         
         GOTO1 AINITV                                                           
         BNE   TUNVX                                                            
         CLI   IFLDH+5,1                                                        
         BNE   TUNVE               ONE CHAR MUST BE UNIT                        
         CLI   IFLD,C'S'           UNIT CAN ONLY BE 1 OR S                      
         BE    TUNV2                                                            
         CLI   IFLD,C'1'                                                        
         BNE   TUNVE                                                            
*                                                                               
TUNV2    MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(1),IFLD                                                    
         MVC   ACQFLT4(1),IFLD                                                  
         GOTO1 AIOREAD             READ UNIT RECORD                             
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) UNIT NOT ON FILE                             
         B     TUNVO                                                            
         OI    FIND,FIVAL          UNIT = X                                     
         MVI   ACQTYP4,C'4'        SET INDICATOR FOR ACTX                       
         B     TUNVO                                                            
*                                                                               
TUNVE    MVC   FERN,=AL2(INVINPT)  UNIT INVALID                                 
         B     TUNVX                                                            
*                                                                               
TUNVO    BRAS  RE,DISPNAME                                                      
*                                                                               
TUNVX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              TO LEDGER VALIDATION (FILTER #4 FOR ACTX)              *         
***********************************************************************         
*                                                                               
TLDGVAL  NMOD1 0,*TLDV*                                                         
         MVC   NAMEX,SPACES                                                     
         GOTO1 AINITV                                                           
         BNE   TLDVX                                                            
         CLI   IFLDH+5,1                                                        
         BNE   TLDVE                                                            
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(1),ACQFLT4                                                 
         MVC   KEY+2(1),IFLD                                                    
         MVC   ACQFLT4+1(1),KEY+2                                               
         CLC   ACQFLT4(2),=C'SJ'                                                
         BE    TLDV2                                                            
         CLC   ACQFLT4(2),=C'1N'                                                
         BNE   TLDVE                                                            
*                                                                               
TLDV2    GOTO1 AIOREAD             READ LEDGER RECORD                           
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) LEDGER NOT ON FILE                           
         B     TLDVO                                                            
         OI    FIND,FIVAL          FILTER4 = X                                  
         MVC   NAMEX,NAME                                                       
         B     TLDVO                                                            
*                                                                               
TLDVE    MVC   FERN,=AL2(INVINPT)  LEDGER INVALID                               
         B     TLDVX                                                            
*                                                                               
TLDVO    BRAS  RE,DISPNAME                                                      
*                                                                               
TLDVX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              TO ACCOUNT VALIDATION (FILTER #4 FOR ACTX)             *         
***********************************************************************         
TACTVAL  NMOD1 0,*TACV*                                                         
         GOTO1 AINITV                                                           
         BNE   TACVX                                                            
*                                                                               
         MVC   ACQFLT4+2(12),IFLD                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),ACQCPY                                                    
         MVC   KEY+1(14),ACQFLT4                                                
         GOTO1 AIOREAD             READ ACCOUNT RECORD                          
         BE    *+14                                                             
         MVC   FERN,=AL2(NTONFILE) ACCOUNT NOT ON FILE                          
         B     TACVO                                                            
         OI    FIND,FIVAL          ACCOUNT = X THRU XXXXXXXXXXXX                
*                                                                               
         USING OFFALD,R1                                                        
         L     R1,AOFFBLK                                                       
         TM    OFFACST4,X'01'      TEST FOR NEW OFFICES                         
         BO    TACSEC              YES-JUST DO SECURITY NUMBER CHECK            
         CLI   LOFFPOS,0                                                        
         BE    TACSEC              NO OFFICE CHK                                
         CLI   LOFFPOS,12                                                       
         BH    TACSEC              OFFICE POSITION NOT IN ACNO                  
         L     R0,AIO1                                                          
         ST    R0,OFFAREC                                                       
         MVC   OFFAOPOS,LOFFPOS                                                 
         MVI   OFFAACT,OFFATST     TEST SECURITY                                
         GOTO1 OFFAL                                                            
         BE    TACSEC                                                           
         MVC   FERN,=AL2(SECLOCK)                                               
         B     TACVO                                                            
*                                                                               
TACSEC   L     R6,AIO1                                                          
         MVI   ELCODE,X'30'                                                     
         BAS   RE,GETEL                                                         
         BNE   TACVO                                                            
         USING RSTELD,R6                                                        
         TM    RSTSTAT1,RSTSACIC                                                
         BZ    *+14                                                             
         MVC   FERN,=AL2(ACCCLOS)  ACCOUNT IS CLOSED                            
         B     TACVO                                                            
         TM    RSTSTAT1,RSTSACIL                                                
         BZ    *+14                                                             
         MVC   FERN,=AL2(ACCLOCK)  ACCOUNT IS LOCKED                            
         B     TACVO                                                            
         CLC   TWAAUTH+1(1),RSTSECY+1                                           
         BNL   TACVO                                                            
         MVC   FERN,=AL2(SECLOCK)  SECURITY LOCK OUT                            
         B     TACVO                                                            
         DROP  R6                                                               
*                                                                               
TACVE    MVC   FERN,=AL2(INVINPT)  ACCOUNT INVALID                              
TACVO    BRAS  RE,DISPNAME                                                      
         OI    FIND,FIVAL          FILTER4 = X                                  
TACVX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              HOUR VALIDATION (FILTER #6 FOR ACTX)                   *         
***********************************************************************         
FHRSVAL  NMOD1 0,*FHRV*                                                         
         GOTO1 AINITV                                                           
         BNE   FHRVX                                                            
*                                                                               
         TM    IFLDH+4,X'08'       IS FIELD NUMERIC                             
         BNZ   *+14                                                             
         MVC   FERN,=AL2(FLDNNUM)  NUMERIC DATA REQUIRED                        
         B     FHRVX                                                            
*                                                                               
         SR    R5,R5                                                            
         IC    R5,IFLDH+5          YES, GET LENGTH                              
         GOTO1 CASHVAL,DMCB,(2,IFLD),(R5)                                       
         CLI   DMCB,X'FF'                                                       
         BE    FHRVE                                                            
*                                                                               
         L     R1,DMCB+4           MUST BE LESS THAN 1000.00                    
         C     R1,=F'0'            AND GREATER THAN 0                           
         BL    FHRVX                                                            
         CVD   R1,DUB                                                           
*                                                                               
         UNPK  ACQFLT6(6),DUB+5(3)                                              
         OI    ACQFLT6+5,X'F0'                                                  
         MVI   ACQTYP6,C'6'        SET INDICATOR FOR ACTX                       
         OI    FIND,FIVAL          FILTER6 = X                                  
         B     FHRVX                                                            
*                                                                               
FHRVE    MVC   FERN,=AL2(INVAMNT)  INVALID AMOUNT                               
*                                                                               
FHRVX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              REVERSAL REFERENCE VALIDATION (FILTER #8 FOR ACTX)     *         
***********************************************************************         
REVVAL   NMOD1 0,*REVV*                                                         
         GOTO1 AINITV                                                           
         BNE   REVVX                                                            
         MVC   ACQFLT8(6),IFLD                                                  
         MVI   ACQTYP8,C'8'        SET INDICATOR FOR ACTX                       
         OI    FIND,FIVAL          FILTER8 = X                                  
REVVX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              NARRATIVE (ACQCARD5 FOR ACTX)                          *         
***********************************************************************         
NARVAL   NMOD1 0,*NARV*                                                         
         GOTO1 AINITV                                                           
         BNE   NARVX                                                            
         MVC   ACQCARD5,SPACES                                                  
         MVC   ACQCARD5(60),IFLD                                                
         OI    FIND,FIVAL          NARRATIVE INPUT                              
NARVX    XMOD1                                                                  
         EJECT                                                                  
***********************************************************************         
*              VENDOR LEVEL                                           *         
***********************************************************************         
VNDLVAL  NMOD1 0,*VNDL*                                                         
         GOTO1 AINITV                                                           
         CLI   IFLDH+5,0           OPTIONAL FIELD                               
         BE    VNDLV200            READ PROFILE                                 
         CLI   IFLDH+5,1                                                        
         BH    VNDLERR                                                          
         OI    FIND,FIINP                                                       
         CLI   IFLD,C'N'           Y/N ACCEPTED ONLY                            
         BE    VNDLVX                                                           
         CLI   IFLD,C'Y'                                                        
         BNE   VNDLERR                                                          
         MVC   ACQPROG,=C'AB'                                                   
         B     VNDLVX                                                           
                                                                                
         USING PROFKD,R4                                                        
VNDLV200 DS    0H                                                               
         LA    R4,PROFDATA         READ LK PROFILE RECORD                       
         XC    PROFDATA,PROFDATA                                                
         XC    PROFREC,PROFREC                                                  
         MVI   PROFKSYS,C'A'         ACCOUNT SYSTEM                             
         MVC   PROFKPGM+1(2),=C'AA'  AA PROGRAM                                 
         MVC   PROFKAGY,TWAAGY     ALPHA ID                                     
                                                                                
         XC    DMCB(24),DMCB                                                    
         GOTO1 GETPROF,DMCB,PROFKEY,PROFREC,DATAMGR,0                           
         OC    PROFREC,PROFREC     DID WE FIND ANY PROFILE ?                    
         BZ    VNDLVX              NO, NO NEED TO CHECK                         
         CLI   PROFREC+8,C'Y'      BY VENDOR?                                   
         BNE   VNDLVX                                                           
         MVC   ACQPROG,=C'AB'                                                   
                                                                                
VNDLVX   CLI   IFLDH+5,0           OPTIONAL FIELD                               
         BE    VNDLVX2                                                          
         OI    FIND,FIVAL                                                       
VNDLVX2  XMOD1                                                                  
                                                                                
VNDLERR  MVC   FERN,=AL2(INVINPT)  CLIENT INVALID                               
         B     VNDLVX2                                                          
                                                                                
PROFDATA DS    CL16                DATA TO PASS TO GETPROF                      
PROFREC  DS    CL16                PROFILE                                      
                                                                                
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              INVALID REQUEST FOR AB                                 *         
***********************************************************************         
NAABVAL  NMOD1 0,*VNDL*                                                         
         CLI   AGYCTRY,CTRYUSA                                                  
         BNE   NAABVAL5                                                         
         MVC   FERN,=AL2(INVNAAB)  INVALID REQUEST, PLEASE RQST AS AA           
         B     NAABVALX                                                         
NAABVAL5 MVC   FERN,=AL2(INVR4CAN) INVALID REQUEST FOR CANADA                   
NAABVALX XMOD1                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              DISPLAY NAME                                           *         
***********************************************************************         
          SPACE 1                                                               
DISPNAME NTR1  BASE=*,LABEL=*                                                   
         CLC   NAME,SPACES                                                      
         BNH   DISPNX                                                           
         L     R6,FLDHADR                                                       
         SR    R1,R1                                                            
         IC    R1,IFLDH                                                         
         AR    R6,R1                                                            
         FOUT  (R6),NAME                                                        
                                                                                
DISPNX   XIT1                                                                   
         EJECT                                                                  
***********************************************************************         
* LITERALS                                                            *         
***********************************************************************         
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
*              OPTIONS VALUE TABLES                                   *         
***********************************************************************         
*                                                                               
*              XL1 = LENGTH                                                     
*              XL1 = REQUEST NUMBER                                             
*              XLN = VALID VALUES                                               
*                                                                               
*              SPECIAL VALUES                                                   
*              255 = ANY VALUE                                                  
*              254 = ANY ALPHA-NUMERIC                                          
*              253 = ANY ALPHA                                                  
*                                                                               
OPT1VTBL DS    0C                                                               
         DC    AL1(04,13),C'YN'                                                 
         DC    AL1(04,14),C'YN'                                                 
         DC    AL1(03,15),C'A'                                                  
         DC    AL1(03,16),C'Y'                                                  
         DC    AL1(03,17),C'S'                                                  
         DC    AL1(05,18),C'YNR'                                                
         DC    AL1(04,19),C'YN'                                                 
         DC    AL1(04,20),C'YN'                                                 
         DC    AL1(03,21),C'S'                                                  
         DC    AL1(03,22),C'S'                                                  
         DC    AL1(03,23),C'Y'                                                  
         DC    AL1(03,24),C'Y'                                                  
         DC    AL1(03,25),C'C'                                                  
         DC    AL1(04,27),C'MP'                                                 
         DC    AL1(04,28),C'MP'                                                 
         DC    AL1(03,29),C'Y'                                                  
         DC    AL1(03,30),C'Y'                                                  
         DC    AL1(03,31),C'S'                                                  
         DC    AL1(03,32),C'Y'                                                  
         DC    AL1(03,34),C'Y'                                                  
         DC    AL1(07,35),C'NRISP'                                              
         DC    AL1(04,36),C'PU'                                                 
         DC    AL1(06,37),C'ABMU'                                               
         DC    AL1(04,38),C'YN'                                                 
         DC    AL1(04,39),C'YN'                                                 
         DC    AL1(03,40),C'Y'                                                  
         DC    AL1(04,41),C'PX'                                                 
         DC    AL1(03,48),C'R'                                                  
         DC    AL1(06,51),C'BLHU'                                               
         DC    AL1(05,52),C'BRT'                                                
         DC    AL1(04,55),C'UY'                                                 
         DC    AL1(05,57),C'ACU'                                                
         DC    AL1(03,58),C'U'                                                  
         DC    AL1(04,59),C'NG'                                                 
         DC    AL1(03,61),C'S'                                                  
         DC    AL1(03,62),C'Y'                                                  
         DC    AL1(04,64),C'SL'                                                 
         DC    AL1(07,66),C'CP123'                                              
         DC    AL1(07,67),C'SMDWP'                                              
         DC    AL1(03,71),C'S'                                                  
         DC    AL1(09,73),C'123J4AB'                                            
         DC    AL1(05,75),C'12J'                                                
         DC    AL1(04,77),C'SL'                                                 
         DC    AL1(04,79),C'YN'                                                 
         DC    AL1(03,81),C'S'                                                  
         DC    AL1(03,82),C'Y'                                                  
         DC    AL1(07,83),C'SD123'                                              
         DC    AL1(05,85),C'BCD'                                                
         DC    AL1(04,86),C'BC'                                                 
         DC    AL1(05,87),C'BCD'                                                
         DC    AL1(06,88),C'S123'                                               
         DC    AL1(05,89),C'CDB'                                                
         DC    AL1(03,90),C'D'                                                  
         DC    AL1(04,91),C'RP'                                                 
         DC    AL1(03,92,255)                                                   
         DC    AL1(04,93),C'SL'                                                 
         DC    AL1(03,94,255)                                                   
         DC    AL1(03,95,255)                                                   
         DC    AL1(14,96),C'12345678ABCD'                                       
         DC    AL1(03,97),C'N'                                                  
         DC    AL1(03,98),C'D'                                                  
         DC    AL1(21,100),C'123456789ABCDEFGHIJ'                               
         DC    AL1(03,101),C'P'                                                 
         DC    AL1(03,102),C'P'                                                 
         DC    AL1(04,103),C'LE'                                                
         DC    AL1(12,104),C'0123456789'                                        
         DC    AL1(04,106),C'DT'                                                
         DC    AL1(03,107),C'B'                                                 
         DC    AL1(03,110),C'D'                                                 
         DC    AL1(06,111),C'MQHY'                                              
         DC    AL1(07,112),C'123CA'                                             
         DC    AL1(07,113),C'123CA'                                             
         DC    AL1(08,114),C'ACDGTM'                                            
         DC    AL1(04,115),C'YN'                                                
         DC    AL1(04,116),C'ON'                                                
         DC    AL1(05,118),C'RBN'                                               
         DC    AL1(06,120),C'ABMT'                                              
         DC    AL1(05,127),C'123'                                               
         DC    AL1(03,129),C'Y'                                                 
         DC    AL1(06,132),C'TYMP'                                              
         DC    AL1(07,135),C'12345'                                             
         DC    AL1(03,137),C'N'                                                 
         DC    AL1(03,138),C'S'                                                 
         DC    AL1(06,142),C'1234'                                              
         DC    AL1(05,145),C'ABM'                                               
         DC    AL1(03,148),C'Y'                                                 
         DC    AL1(05,149),C'BEA'                                               
         DC    AL1(04,150),C'YN'                                                
         DC    AL1(05,151),C'123'                                               
         DC    AL1(06,152),C'1234'                                              
         DC    AL1(03,153),C'S'                                                 
         DC    AL1(07,154),C'SPARI'                                             
         DC    AL1(03,160),C'N'                                                 
         DC    AL1(03,161),C'Y'                                                 
         DC    AL1(03,162,255)     ANY VALUE                                    
         DC    AL1(03,163,255)     ANY VALUE                                    
*        DC    AL1(05,164),C'YND'    TEST TO SEE WHO DOWNLOADS                  
         DC    AL1(04,164),C'YN'                                                
         DC    AL1(04,165),C'YN'                                                
         DC    AL1(04,166),C'YN'                                                
         DC    AL1(04,167),C'YN'                                                
         DC    AL1(03,168),C'Y'                                                 
         DC    AL1(04,169),C'YN'                                                
         DC    AL1(03,170,254)     ANY ALPHA NUMERIC                            
         DC    AL1(04,171),C'YN'                                                
         DC    AL1(04,172),C'YN'                                                
         DC    AL1(04,173),C'YN'                                                
         DC    AL1(04,174),C'YN'                                                
         DC    AL1(03,175),C'P'                                                 
         DC    AL1(04,176),C'YN'                                                
         DC    AL1(06,177),C'1234'                                              
         DC    AL1(03,178),C'Y'                                                 
         DC    AL1(03,179),C'Y'                                                 
         DC    AL1(03,180),C'Y'                                                 
         DC    AL1(04,181),C'BU'                                                
         DC    AL1(03,182),C'Y'                                                 
         DC    AL1(06,183),C'SDUA'                                              
         DC    AL1(03,185),C'Y'                                                 
         DC    AL1(03,187),C'Y'                                                 
         DC    AL1(07,189),C'1234S'                                             
         DC    AL1(05,191),C'CPE'                                               
         DC    AL1(07,192),C'ABIOP'                                             
         DC    AL1(05,193),C'CPJ'                                               
         DC    AL1(04,194),C'AB'                                                
         DC    AL1(05,195),C'IOB'                                               
         DC    AL1(03,196),C'D'                                                 
         DC    AL1(15,197),C'ABCDEFGHIJKLM'                                     
         DC    AL1(07,198),C'NBRCA'                                             
         DC    AL1(03,199),C'L'                                                 
         DC    AL1(05,201),C'YTN'                                               
         DC    AL1(04,203),C'OU'                                                
         DC    AL1(04,206),C'YN'                                                
         DC    AL1(04,209),C'CG'                                                
         DC    AL1(05,210),C'345'                                               
         DC    AL1(04,211),C'SN'                                                
         DC    AL1(03,212),C'Y'                                                 
         DC    AL1(03,213),C'Y'                                                 
         DC    AL1(03,217),C'L'                                                 
         DC    AL1(03,218),C'Y'                                                 
         DC    AL1(03,219),C'Y'                                                 
         DC    AL1(04,220),C'YN'                                                
         DC    AL1(05,221),C'PNW'                                               
         DC    AL1(05,223),C'NAP'                                               
         DC    AL1(04,226),C'EM'                                                
         DC    AL1(04,227),C'TP'                                                
         DC    AL1(04,228),C'DL'                                                
         DC    AL1(05,229),C'YNR'                                               
         DC    AL1(03,230),C'Y'                                                 
         DC    AL1(03,231),C'L'                                                 
         DC    AL1(04,232),C'TA'                                                
         DC    AL1(04,235),C'YN'                                                
         DC    AL1(06,236),C'1234'                                              
         DC    AL1(03,237),C'D'                                                 
         DC    AL1(04,238),C'LD'                                                
         DC    AL1(04,239),C'CP'                                                
         DC    AL1(04,240),C'MT'                                                
         DC    AL1(04,241),C'OP'                                                
         DC    AL1(03,243),C'Y'                                                 
         DC    AL1(05,244),C'CPE'                                               
         DC    AL1(0)                                                           
*                                                                               
OPT2VTBL DS    0C                                                               
         DC    AL1(03,13),C'Y'                                                  
         DC    AL1(04,14),C'SC'                                                 
         DC    AL1(03,15),C'C'                                                  
         DC    AL1(03,16),C'Y'                                                  
         DC    AL1(03,21),C'Y'                                                  
         DC    AL1(03,22),C'Y'                                                  
         DC    AL1(04,25),C'DL'                                                 
         DC    AL1(04,27),C'YP'                                                 
         DC    AL1(04,28),C'YP'                                                 
         DC    AL1(04,31),C'YN'                                                 
         DC    AL1(03,32),C'Y'                                                  
         DC    AL1(03,33),C'Y'                                                  
         DC    AL1(03,34),C'Y'                                                  
         DC    AL1(05,35),C'YNO'                                                
         DC    AL1(03,36),C'Y'                                                  
         DC    AL1(04,37),C'YN'                                                 
         DC    AL1(03,38),C'S'                                                  
         DC    AL1(04,39),C'YN'                                                 
         DC    AL1(03,40),C'Y'                                                  
         DC    AL1(03,48,255)                                                   
         DC    AL1(03,55),C'Y'                                                  
         DC    AL1(04,57),C'NY'                                                 
         DC    AL1(03,58),C'Y'                                                  
         DC    AL1(28,59),C'Y123456789ABCDEFGHIJKLMNOP'                         
         DC    AL1(03,61),C'T'                                                  
         DC    AL1(04,64),C'CS'                                                 
         DC    AL1(03,66),C'Y'                                                  
         DC    AL1(08,67),C'CSTPHU'                                             
         DC    AL1(03,71),C'Y'                                                  
         DC    AL1(03,73),C'P'                                                  
         DC    AL1(05,81),C'123'                                                
         DC    AL1(03,82),C'Y'                                                  
         DC    AL1(05,83),C'MIC'                                                
         DC    AL1(03,86),C'Y'                                                  
         DC    AL1(11,87),C'123456789'                                          
         DC    AL1(03,88),C'Y'                                                  
         DC    AL1(04,92),C'DI'                                                 
         DC    AL1(03,93),C'C'                                                  
         DC    AL1(03,94),C'S'                                                  
         DC    AL1(05,96),C'S12'                                                
         DC    AL1(03,97),C'1'                                                  
         DC    AL1(03,98),C'C'                                                  
         DC    AL1(06,100),C'S123'                                              
         DC    AL1(03,101),C'D'                                                 
         DC    AL1(03,102),C'S'                                                 
         DC    AL1(03,103),C'S'                                                 
         DC    AL1(03,106),C'Y'                                                 
         DC    AL1(03,110),C'N'                                                 
         DC    AL1(14,111),C'123456789ABC'                                      
         DC    AL1(11,112),C'Y23456789'                                         
         DC    AL1(06,113),C'PVIB'                                              
         DC    AL1(03,114),C'L'                                                 
         DC    AL1(04,116),C'LM'                                                
         DC    AL1(04,127),C'SN'                                                
         DC    AL1(03,129),C'Y'                                                 
         DC    AL1(07,132),C'12345'                                             
         DC    AL1(03,138),C'S'                                                 
         DC    AL1(03,142),C'Y'                                                 
         DC    AL1(03,147),C'Y'                                                 
         DC    AL1(03,148),C'Y'                                                 
         DC    AL1(04,152),C'12'                                                
         DC    AL1(03,154),C'M'                                                 
         DC    AL1(03,162,255)    ANY VALUE                                     
         DC    AL1(03,163,255)    ANY VALUE                                     
         DC    AL1(04,164),C'YN'                                                
*        DC    AL1(03,164,255)    ANY VALUE                                     
         DC    AL1(05,166),C'OCB'                                               
         DC    AL1(05,167),C'OCB'                                               
         DC    AL1(05,168),C'ABM'                                               
         DC    AL1(04,169),C'NO'                                                
         DC    AL1(03,170,255)    ANY VALUE                                     
         DC    AL1(04,172),C'YN'                                                
         DC    AL1(06,177),C'1234'                                              
         DC    AL1(10,181),C'12345678'                                          
         DC    AL1(04,182),C'SC'                                                
         DC    AL1(04,183),C'PE'                                                
         DC    AL1(03,185),C'C'                                                 
         DC    AL1(03,188),C'G'                                                 
         DC    AL1(07,189),C'1234E'                                             
         DC    AL1(03,190,255)    ANY VALUE                                     
         DC    AL1(03,191),C'S'                                                 
         DC    AL1(03,192,255)    ANY VALUE                                     
         DC    AL1(04,193),C'EN'                                                
         DC    AL1(05,196),C'BCD'                                               
         DC    AL1(04,197),C'MT'                                                
         DC    AL1(05,198),C'JPC'                                               
         DC    AL1(03,199),C'J'                                                 
         DC    AL1(03,209),C'Y'                                                 
         DC    AL1(03,210),C'Y'                                                 
         DC    AL1(11,211),C'123456789'                                         
         DC    AL1(06,212),C'HCAM'                                              
         DC    AL1(03,213),C'Y'                                                 
         DC    AL1(04,219),C'YO'                                                
         DC    AL1(05,221),C'ANK'                                               
         DC    AL1(11,227),C'123456789'                                         
         DC    AL1(03,229),C'Y'                                                 
         DC    AL1(03,230),C'Y'                                                 
         DC    AL1(11,231),C'123456789'                                         
         DC    AL1(11,232),C'123456789'                                         
         DC    AL1(11,233),C'123456789'                                         
         DC    AL1(11,234),C'123456789'                                         
         DC    AL1(03,236),C'C'                                                 
         DC    AL1(03,237),C'W'                                                 
         DC    AL1(04,238),C'YN'                                                
         DC    AL1(06,240),C'1234'                                              
         DC    AL1(05,241),C'ATL'                                               
         DC    AL1(03,243),C'S'                                                 
         DC    AL1(03,244),C'S'                                                 
         DC    AL1(0)                                                           
*                                                                               
OPT3VTBL DS    0C                                                               
         DC    AL1(04,14),C'SL'                                                 
         DC    AL1(04,15),C'YN'                                                 
         DC    AL1(04,21),C'YR'                                                 
         DC    AL1(04,22),C'YR'                                                 
         DC    AL1(04,29),C'YN'                                                 
         DC    AL1(04,31),C'YT'                                                 
         DC    AL1(04,32),C'YN'                                                 
         DC    AL1(04,33),C'YN'                                                 
         DC    AL1(05,35),C'YNO'                                                
         DC    AL1(04,37),C'SL'                                                 
         DC    AL1(04,38),C'YN'                                                 
         DC    AL1(03,39),C'S'                                                  
         DC    AL1(04,47),C'NY'                                                 
         DC    AL1(03,55),C'A'                                                  
         DC    AL1(03,57),C'D'                                                  
         DC    AL1(03,58),C'Y'                                                  
         DC    AL1(04,59),C'SC'                                                 
         DC    AL1(06,61),C'AUNR'                                               
         DC    AL1(04,64),C'NG'                                                 
         DC    AL1(04,66),C'SL'                                                 
         DC    AL1(04,67),C'SL'                                                 
         DC    AL1(04,73),C'SC'                                                 
         DC    AL1(04,81),C'DC'                                                 
         DC    AL1(04,82),C'YN'                                                 
         DC    AL1(05,83),C'BDM'                                                
         DC    AL1(03,85,255)                                                   
         DC    AL1(03,86,253)              ANY ALPHA                            
         DC    AL1(03,87,255)                                                   
         DC    AL1(03,92),C'Y'                                                  
         DC    AL1(03,94),C'Y'                                                  
         DC    AL1(03,95),C'Y'                                                  
         DC    AL1(04,96),C'YN'                                                 
         DC    AL1(04,100),C'YN'                                                
         DC    AL1(07,101),C'12345'                                             
         DC    AL1(03,102),C'S'                                                 
         DC    AL1(03,103),C'S'                                                 
         DC    AL1(05,110),C'SAR'                                               
         DC    AL1(06,111),C'123S'                                              
         DC    AL1(03,112),C'Y'                                                 
         DC    AL1(11,114),C'123456789'                                         
         DC    AL1(04,115),C'YN'                                                
         DC    AL1(04,118),C'YN'                                                
         DC    AL1(03,127),C'S'                                                 
         DC    AL1(03,131,255)                                                  
         DC    AL1(04,132),C'SL'                                                
         DC    AL1(03,142),C'Y'                                                 
         DC    AL1(03,147),C'Y'                                                 
         DC    AL1(03,149),C'S'                                                 
         DC    AL1(03,160),C'N'                                                 
         DC    AL1(03,162,255)                                                  
         DC    AL1(03,163,255)                                                  
         DC    AL1(03,168),C'U'                                                 
         DC    AL1(03,170,255)                                                  
         DC    AL1(06,177),C'1234'                                              
         DC    AL1(03,181),C'G'                                                 
         DC    AL1(04,182),C'SC'                                                
         DC    AL1(04,183),C'IZ'                                                
         DC    AL1(05,189),C'123'                                               
         DC    AL1(03,191),C'1'                                                 
         DC    AL1(04,192),C'BR'                                                
         DC    AL1(04,193),C'TN'                                                
         DC    AL1(05,196),C'NPS'                                               
         DC    AL1(03,197),C'Y'                                                 
         DC    AL1(04,198),C'TN'                                                
         DC    AL1(05,199),C'AIW'                                               
         DC    AL1(03,203),C'U'                                                 
         DC    AL1(04,205),C'UO'                                                
         DC    AL1(03,209),C'S'                                                 
         DC    AL1(04,210),C'YA'                                                
         DC    AL1(03,211),C'S'                                                 
         DC    AL1(03,221),C'Y'                                                 
         DC    AL1(05,229),C'TCR'                                               
         DC    AL1(03,232),C'S'                                                 
         DC    AL1(04,236),C'YN'                                                
         DC    AL1(06,240),C'DOCP'                                              
         DC    AL1(03,241),C'Y'                                                 
         DC    AL1(04,243),C'YN'                                                
         DC    AL1(03,244),C'1'                                                 
         DC    AL1(0)                                                           
*                                                                               
OPT4VTBL DS    0C                                                               
         DC    AL1(04,14),C'YN'                                                 
         DC    AL1(04,15),C'YN'                                                 
         DC    AL1(04,20),C'YN'                                                 
         DC    AL1(03,21),C'Y'                                                  
         DC    AL1(03,22),C'Y'                                                  
         DC    AL1(03,31),C'S'                                                  
         DC    AL1(03,32),C'Y'                                                  
         DC    AL1(03,33),C'Y'                                                  
         DC    AL1(05,35),C'YNO'                                                
         DC    AL1(06,39),C'1234'                                               
         DC    AL1(03,55),C'Y'                                                  
         DC    AL1(03,57),C'Y'                                                  
         DC    AL1(03,58),C'A'                                                  
         DC    AL1(04,59),C'SL'                                                 
         DC    AL1(04,61),C'DC'                                                 
         DC    AL1(04,64),C'YN'                                                 
         DC    AL1(04,66),C'SC'                                                 
         DC    AL1(03,67),C'S'                                                  
         DC    AL1(04,73),C'SL'                                                 
         DC    AL1(03,81),C'C'                                                  
         DC    AL1(05,83),C'YNA'                                                
         DC    AL1(05,86),C'123'                                                
         DC    AL1(04,87),C'YN'                                                 
         DC    AL1(05,92),C'HTB'                                                
         DC    AL1(03,96),C'Y'                                                  
         DC    AL1(03,100),C'Y'                                                 
         DC    AL1(03,101),C'Y'                                                 
         DC    AL1(03,102),C'S'                                                 
         DC    AL1(03,103),C'S'                                                 
         DC    AL1(06,111),C'0123'                                              
         DC    AL1(03,114),C'Y'                                                 
         DC    AL1(04,115),C'YN'                                                
         DC    AL1(05,118),C'RBN'                                               
         DC    AL1(04,127),C'MP'                                                
         DC    AL1(03,131),C'Y'                                                 
         DC    AL1(04,132),C'DW'                                                
         DC    AL1(03,142),C'Y'                                                 
         DC    AL1(03,149),C'Y'                                                 
         DC    AL1(03,162,255)                                                  
         DC    AL1(03,168),C'Y'                                                 
         DC    AL1(03,170,255)                                                  
         DC    AL1(06,177),C'1234'                                              
         DC    AL1(04,181),C'JP'                                                
         DC    AL1(04,192),C'CO'                                                
         DC    AL1(03,197),C'Y'                                                 
         DC    AL1(03,203),C'C'                                                 
         DC    AL1(03,209),C'Y'                                                 
         DC    AL1(03,210),C'Y'                                                 
         DC    AL1(03,221,255)                                                  
         DC    AL1(03,225),C'C'                                                 
         DC    AL1(03,227),C'C'                                                 
         DC    AL1(03,232),C'C'                                                 
         DC    AL1(03,236),C'S'                                                 
         DC    AL1(03,241),C'Y'                                                 
         DC    AL1(04,243),C'AC'                                                
         DC    AL1(0)                                                           
*                                                                               
OPT5VTBL DS    0C                                                               
         DC    AL1(03,14),C'Y'                                                  
         DC    AL1(03,15),C'Y'                                                  
         DC    AL1(03,21),C'Y'                                                  
         DC    AL1(03,22),C'Y'                                                  
         DC    AL1(03,32),C'Y'                                                  
         DC    AL1(03,33),C'Y'                                                  
         DC    AL1(06,39),C'1234'                                               
         DC    AL1(05,55),C'ADT'                                                
         DC    AL1(04,57),C'NV'                                                 
         DC    AL1(03,58,255)                                                   
         DC    AL1(03,59),C'Y'                                                  
         DC    AL1(04,61),C'TY'                                                 
         DC    AL1(03,64),C'Y'                                                  
         DC    AL1(04,66),C'NG'                                                 
         DC    AL1(06,67),C'SBAU'                                               
         DC    AL1(03,73),C'Y'                                                  
         DC    AL1(03,81),C'Y'                                                  
         DC    AL1(03,83),C'N'                                                  
         DC    AL1(03,85),C'Y'                                                  
         DC    AL1(04,86),C'YN'                                                 
         DC    AL1(03,87),C'Y'                                                  
         DC    AL1(03,103),C'F'                                                 
         DC    AL1(03,111),C'Y'                                                 
         DC    AL1(04,115),C'YN'                                                
         DC    AL1(04,131),C'AU'                                                
         DC    AL1(03,132),C'Y'                                                 
         DC    AL1(04,145),C'NA'                                                
*        DC    AL1(14,168),C'LMSNAGYCKBZU'                                      
         DC    AL1(06,177),C'1234'                                              
         DC    AL1(03,181),C'S'                                                 
         DC    AL1(04,197),C'BR'                                                
         DC    AL1(03,204),C'Y'                                                 
         DC    AL1(03,210),C'Y'                                                 
         DC    AL1(03,221),C'Y'                                                 
         DC    AL1(03,225),C'Y'                                                 
         DC    AL1(03,227),C'C'                                                 
         DC    AL1(03,241),C'Y'                                                 
         DC    AL1(0)                                                           
*                                                                               
OPT6VTBL DS    0C                                                               
         DC    AL1(04,21),C'MP'                                                 
         DC    AL1(04,22),C'MP'                                                 
         DC    AL1(03,57),C'A'                                                  
         DC    AL1(03,58),C'Y'                                                  
         DC    AL1(04,61),C'YN'                                                 
         DC    AL1(04,64),C'YN'                                                 
         DC    AL1(03,66),C'S'                                                  
         DC    AL1(04,67),C'ZS'                                                 
         DC    AL1(03,73),C'Y'                                                  
         DC    AL1(03,81),C'C'                                                  
         DC    AL1(04,83),C'YC'                                                 
         DC    AL1(03,103),C'L'                                                 
         DC    AL1(03,112),C'Y'                                                 
         DC    AL1(04,115),C'YN'                                                
         DC    AL1(04,118),C'AP'                                                
         DC    AL1(04,132),C'YN'                                                
         DC    AL1(04,162),C'SN'                                                
         DC    AL1(04,163),C'SN'                                                
         DC    AL1(04,168),C'AB'                                                
         DC    AL1(04,170),C'SN'                                                
         DC    AL1(11,197),C'123456789'                                         
         DC    AL1(05,210),C'Y1A'                                               
         DC    AL1(03,241),C'Y'                                                 
         DC    AL1(0)                                                           
*                                                                               
OPT7VTBL DS    0C                                                               
         DC    AL1(03,21),C'A'                                                  
         DC    AL1(03,22),C'A'                                                  
         DC    AL1(03,23),C'A'                                                  
         DC    AL1(03,24),C'A'                                                  
         DC    AL1(03,35),C'Y'                                                  
*        DC    AL1(03,57),C'N'                                                  
         DC    AL1(07,61),C'AFOUN'                                              
         DC    AL1(06,64),C'AUNR'                                               
         DC    AL1(03,66),C'I'                                                  
         DC    AL1(08,67),C'YPBUAN'                                             
         DC    AL1(03,73),C'S'                                                  
         DC    AL1(03,81),C'Y'                                                  
         DC    AL1(04,83),C'YC'                                                 
         DC    AL1(03,103),C'B'                                                 
         DC    AL1(04,115),C'YN'                                                
         DC    AL1(04,118),C'YN'                                                
         DC    AL1(03,131),C'Y'                                                 
         DC    AL1(04,132),C'TB'                                                
         DC    AL1(11,197),C'123456789'                                         
         DC    AL1(03,154),C'Y'                                                 
         DC    AL1(05,210),C'CEA'                                               
         DC    AL1(0)                                                           
*                                                                               
OPT8VTBL DS    0C                                                               
         DC    AL1(05,21),C'YPG'                                                
         DC    AL1(05,22),C'YPG'                                                
         DC    AL1(05,55),C'124'                                                
         DC    AL1(03,66),C'Y'                                                  
         DC    AL1(04,115),C'YO'                                                
         DC    AL1(05,197),C'PBU'                                               
         DC    AL1(03,210,255)                                                  
         DC    AL1(0)                                                           
*                                                                               
OPT9VTBL DS    0C                                                               
         DC    AL1(03,66),C'Y'                                                  
         DC    AL1(04,20),C'YN'                                                 
         DC    AL1(0)                                                           
*                                                                               
OPTAVTBL DS    0C                                                               
         DC    AL1(03,66),C'Y'                                                  
         DC    AL1(04,162),C'YN'                                                
         DC    AL1(04,191),C'YN'                                                
         DC    AL1(04,192),C'YN'                                                
         DC    AL1(04,193),C'YN'                                                
         DC    AL1(04,198),C'YN'                                                
         DC    AL1(0)                                                           
         EJECT                                                                  
***********************************************************************         
*              WORKING STORAGE                                        *         
***********************************************************************         
*                                                                               
LWS      DSECT                                                                  
RELO     DS    F                                                                
SAVERC   DS    F                                                                
VSQUASH  DS    A                                                                
VMONVAL  DS    A                                                                
BLOCK    DS    CL100                                                            
*                                                                               
NAMEX    DS    0CL36                                                            
NAMES    DS    0CL72                                                            
NAMELAST DS    CL36                                                             
NAMEFRST DS    CL36                                                             
*                                                                               
FLAG     DS    X                                                                
FLG1F    EQU   X'80'               GOT A LIST DATA ELEMENT                      
FLGLEV   EQU   X'40'               GOT A RIGHT LEVEL                            
FLGPRD   EQU   X'20'               GOT A PRODUCT                                
FLGPRD2  EQU   X'10'               GOT A PRODUCT WITH MATCHING CLIENT           
FLGCLI   EQU   X'08'               MATCH CLIENT AND LIST ENTRY                  
FLGCLI2  EQU   X'04'               TEMP CLI FLAG                                
*                                                                               
TYPE     DS    CL1                 SAVE LIST TYPE                               
                                                                                
WORK     DS    CL60                                                             
                                                                                
LWSX     EQU   *                                                                
                                                                                
***********************************************************************         
*              GETPROFS KEY DSECT                                     *         
***********************************************************************         
*                                                                               
PROFKD   DSECT                                                                  
PROFKEY  DS    0CL16                                                            
PROFKSYS DS    CL1                                                              
PROFKPGM DS    CL3                                                              
         DS    CL1                                                              
PROFKUNL DS    CL2                                                              
PROFKACC DS    CL3                                                              
PROFKAST DS    CL1                                                              
PROFKOFF DS    CL1                                                              
PROFKAGY DS    CL2                                                              
PROFKOFC DS    CL2                 NEW OFFICE                                   
         EJECT                                                                  
                                                                                
       ++INCLUDE ACREQWORK                                                      
* ACBMONVALD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBMONVALD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'136ACREQ03   01/11/21'                                      
         END                                                                    
