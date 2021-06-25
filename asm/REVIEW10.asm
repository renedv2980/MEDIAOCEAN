*          DATA SET REVIEW10   AT LEVEL 024 AS OF 08/10/11                      
*          DATA SET REVIEW10   AT LEVEL 022 AS OF 11/18/99                      
*PHASE T81710C                                                                  
VIEW10   TITLE 'REPORT VIEWER BASE REPORT SCREEN'                               
VIEW10   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REVIEW10,R7,RR=RE                                              
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
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(EXITH)                                  
         DC    AL1(ORECH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OBUILD),AL1(0,0,0),AL4(EXITH)                                
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(SUB)                                 
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(OPFK),AL1(0,0,0),AL4(EXITH)                                  
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                      *         
***********************************************************************         
         SPACE 1                                                                
INIT     GOTOX VDICTAT,BOPARM,C'LU  ',DCLIST,DSLISTU                            
         GOTOX (RF),(R1),C'LL  ',,DSLISTL                                       
         B     EXITOK                                                           
         SPACE 2                                                                
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
         USING FSRRECD,R2                                                       
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
         BR    RF                                                               
         SPACE 1                                                                
*                                                                               
DTATABL  DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* TABLE OF KNOWN RECORD OBJECTS                                       *         
***********************************************************************         
         SPACE 1                                                                
KNOWTAB  DC    AL2(00020),AL4(DSCDTA)       DISPLAY DESCRIPTION                 
         DC    AL2(00021),AL4(REPDTA)       DISPLAY REPORT CODE                 
         DC    AL2(00022),AL4(PERDTA)       DISPLAY REPORT PERIOD               
         DC    AL2(00023),AL4(FRMDTA)       DISPLAY REPORT FORMAT               
         DC    AL2(00024),AL4(RNKDTA)       DISPLAY REPORT RANK                 
         DC    AL2(EOT)                                                         
         SPACE 1                                                                
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
VIEW10   CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR DESCRIPTION                                         *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
DSCDTA   LA    RF,DSCTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
DSCTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSC)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD DESCRIPTION                                        *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISDSC   MVC   FVIFLD(L'TLDESC),TLDESC                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR PERIOD                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
PERDTA   LA    RF,PERTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
PERTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPER)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD PERIOD                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISPER   MVC   FVIFLD(L'TLPERD),TLPERD                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR FORMAT                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
FRMDTA   LA    RF,FRMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
FRMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFRM)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD FORMAT                                             *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISFRM   MVC   FVIFLD(L'TLFORM),TLFORM                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR RANK/SORT                                           *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
RNKDTA   LA    RF,RNKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
RNKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISRNK)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A RECORD RANK/SORT                                          *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISRNK   MVC   FVIFLD(L'TLRANK),TLRANK                                          
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* DATA OBJECT FOR REPORT                                              *         
*                                                                     *         
* R1 HOLDS EQUATED VERB                                               *         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                 *         
***********************************************************************         
         SPACE 1                                                                
REPDTA   LA    RF,REPTBL           TABLE OF KNOWN VERBS                         
         B     ITER                                                             
*                                                                               
REPTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISREP)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* DISPLAY A REPORT CODE                                               *         
***********************************************************************         
         SPACE 1                                                                
         USING TLSTD,R2                                                         
DISREP   MVC   FVIFLD(L'TLREP),TLREP                                            
         B     EXITOK                                                           
***********************************************************************         
* SUB ACTION OBJECT                                                   *         
* -----------------                                                   *         
* P3 = A(SUB-ACTION FIELD)                                            *         
***********************************************************************         
         SPACE 1                                                                
SUB      LM    R0,R3,SVPARMS                                                    
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
         L     R2,SVPARMS3         R2=A(SUB-ACTION FIELD)                       
         USING FHD,R2                                                           
         L     R4,ATLST            R4=A(TSAR RECORD)                            
         USING TLSTD,R4                                                         
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
         MVC   LSSUBFRA+FRADICT-FRAELD(L'LSSUBFRA),DCLIST+1                     
         OI    FHII,FHIIVA         SET FIELD VALID                              
*                                                                               
         ICM   RF,15,SNGL.SUBRTN   PROCESSING ROUTINE                           
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  R3                                                               
*                                                                               
SVAL18   B     EXITOK              FINISHED                                     
*                                                                               
SVALL    B     EXITL                                                            
         DROP  R2,R4                                                            
*                                                                               
SUBACTS  DS    0H              *** TABLE OF VALID SUB-ACTIONS ***               
         DC    AL2(FU@SEL-OVERWRKD,FL@SEL-OVERWRKD),AL4(SUB_SEL)                
         DC    AL2(EOT)                                                         
*                                                                               
SUBACTSD DSECT                                                                  
SUBUPR   DS    AL2                 DISPLACEMENT TO UPPERCASE NAME               
SUBLWR   DS    AL2                 DISPLACEMENT TO LOWER CASE NAME              
SUBRTN   DS    AL4                 DISPLACEMENT TO VALIDATION ROUTINE           
SUBACTLQ EQU   *-SUBACTSD                                                       
*                                                                               
VIEW10   CSECT                                                                  
         SPACE 2                                                                
***********************************************************************         
* ROUTINE TO NTRSES INTO THE SELECTED REPORT                          *         
***********************************************************************         
         SPACE 1                                                                
SUB_SEL  L     RF,ATLST                                                         
         USING TLSTD,RF                                                         
*                                                                               
N        USING SSAVD,NSSAV                                                      
         MVC   N.SREC,TLOVR                                                     
         MVI   N.SACT,A#LST                                                     
         GOTOX AGEN,BOPARM,OSES,SNTR                                            
         DROP  N,RF                                                             
         SPACE 2                                                                
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
         DC    AL1(LLSTFRST),AL1(0,0,0),AL4(FTFLST)                             
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(BLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(BLST)                               
         DC    AL1(LTSARDIR),AL1(0,0,0),AL4(TSARDIR)                            
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALISE LIST                                                     *         
***********************************************************************         
         SPACE 1                                                                
ILST     MVI   LSSUBLEN,3          SUB-ACTION FIELD LENGTH IS 3                 
*        MVC   LSNUMHED,=AL2(1)    ONE HEADLINE FIELD                           
         OI    LSSTAT1,LSSBALL     BUILD WHOLE LIST                             
         OI    LSSTAT1,LSSTSAR     IMAGINARY RECORDS                            
         NI    LSSTAT1,FF-LSSENTK                                               
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST TIME FOR LIST                                                 *         
***********************************************************************         
         SPACE 1                                                                
FTFLST   XC    LINENOW,LINENOW     RESTART CURRENT LINE                         
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR RECORD PAGE                                               *         
***********************************************************************         
         SPACE 1                                                                
BLST     LH    RF,LINENOW          REACHED END OF LIST?                         
         MH    RF,=Y(RPOTABLQ)                                                  
         CLC   CUAALF,=C'MR'       KTVNY HAVE DIFFERENT MENU                    
         BNE   *+12                                                             
         LA    RF,RPOTABMR(RF)                                                  
         B     BLST02                                                           
         CLC   CUAALF,=C'KH'       REPDEMO HAS DIFFERENT MENU                   
         BNE   *+12                                                             
         LA    RF,RPOTABKH(RF)                                                  
         B     BLST02                                                           
         CLC   CUAALF,=C'SZ'       SELTEL HAS DIFFERENT MENU                    
         BNE   *+12                                                             
         LA    RF,RPOTABSZ(RF)                                                  
         B     BLST02                                                           
*                                                                               
         LA    RF,RPOTAB(RF)                                                    
*                                                                               
BLST02   CLI   0(RF),0                                                          
         BE    EXITL                                                            
*                                                                               
         LH    RF,LINENOW                                                       
         LA    RF,1(RF)                                                         
         STH   RF,LINENOW                                                       
         B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* SET UP TSAR RECORD                                                  *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR  L     R3,SVPARMS4         TSAR RECORD                                  
         USING TLSTD,R3                                                         
         LH    RF,LINENOW          GET CURRENT LIST LINE                        
         BCTR  RF,0                                                             
         MH    RF,=Y(RPOTABLQ)                                                  
         CLC   CUAALF,=C'MR'       KTVNY HAVE DIFFERENT MENU                    
         BNE   *+12                                                             
         LA    RF,RPOTABMR(RF)                                                  
         B     TDIR02                                                           
         CLC   CUAALF,=C'KH'       REPDEMO HAS DIFFERENT MENU                   
         BNE   *+12                                                             
         LA    RF,RPOTABKH(RF)                                                  
         B     TDIR02                                                           
         CLC   CUAALF,=C'SZ'       SELTEL HAS DIFFERENT MENU                    
         BNE   *+12                                                             
         LA    RF,RPOTABSZ(RF)                                                  
         B     TDIR02                                                           
*                                                                               
         LA    RF,RPOTAB(RF)                                                    
         USING RPOTABD,RF                                                       
*                                                                               
TDIR02   MVC   TLRLEN,=AL2(TLLNQ)                                               
         MVC   TLREP,RPOREP                                                     
         MVC   TLDESC,RPODESC                                                   
         MVC   TLPERD,RPOPERD                                                   
         MVC   TLFORM,RPOFORM                                                   
         MVC   TLRANK,RPORANK                                                   
         MVC   TLOVR,RPOOVR                                                     
         B     EXITOK                                                           
         DROP  R3,RF                                                            
         SPACE 2                                                                
***********************************************************************         
* LITERALS & CONSTANTS                                                *         
***********************************************************************         
         SPACE 1                                                                
*                12       1234567890123456789012345678901234567890              
*POTAB   DC    C'K1',CL40'K1 Description'                                       
*        DC    CL8'????',CL1'?',CL8'???????',X'11'                              
*        DC    C'K2',CL40'K2 Description'                                       
*        DC    CL8'????',CL1'?',CL8'???????',X'12'                              
*POTAB   DC    C'K3',CL40'Station Office Advertiser Summary Ranked'             
*        DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'13'                              
RPOTAB   DC    C'K4',CL40'Station Advertiser Summary Ranked'                    
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'14'                              
*        DC    C'K5',CL40'Office Station Advertiser Summary Ranked'             
*        DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'15'                              
* This fucking report is in and out more than John Holmes...                    
*        DC    C'K6',CL40'Office Advertiser Summary Ranked'                     
*        DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'16'                              
*        DC    C'K7',CL40'Category Billing Report (Spot Only)'                  
*        DC    CL8'Yrly',CL1'A',CL8'Alpha',X'17'                                
*        DC    C'K8',CL40'Market/Office Summary Report'                         
*        DC    CL8'Yrly',CL1'Q',CL8'Alpha',X'18'                                
*        DC    C'K9',CL40'Office/Station Ranker'                                
*        DC    CL8'Yrly',CL1'Q',CL8'PriFin',X'19'                               
*        DC    C'K0',CL40'Station Office Ranker'                                
*        DC    CL8'Yrly',CL1'Q',CL8'PriFin',X'20'                               
*        DC    C'KA',CL40'Station/Office Advertiser Blg Report'                 
*        DC    CL8'1 Qtr',CL1'A',CL8'PriFin',X'1A'                              
*        DC    C'KB',CL40'Station Advertiser Blg Report'                        
*        DC    CL8'1 Qtr',CL1'A',CL8'PriFin',X'1B'                              
*        DC    C'KC',CL40'Station Office Agency Ranker'                         
*        DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'1C'                              
         DC    C'KD',CL40'Station Office Agency/Advertiser Ranker'              
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'1D'                              
         DC    X'00'                                                            
*                                                                               
RPOTABKH DC    C'K3',CL40'Station Office Advertiser Summary Ranked'             
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'13'                              
         DC    C'K4',CL40'Station Advertiser Summary Ranked'                    
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'14'                              
         DC    C'K7',CL40'Category Billing Report (Spot Only)'                  
         DC    CL8'Yrly',CL1'A',CL8'Alpha',X'17'                                
         DC    C'K9',CL40'Office/Station Ranker'                                
         DC    CL8'Yrly',CL1'Q',CL8'PriFin',X'19'                               
         DC    C'KC',CL40'Station Office Agency Ranker'                         
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'1C'                              
         DC    C'KD',CL40'Station Office Agency/Advertiser Ranker'              
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'1D'                              
         DC    X'00'                                                            
*                                                                               
RPOTABMR DC    X'00'                                                            
*POTABMR DC    C'K7',CL40'Category Billing Report (Spot Only)'                  
*        DC    CL8'Yrly',CL1'A',CL8'Alpha',X'17'                                
*        DC    C'K9',CL40'Office/Station Ranker'                                
*        DC    CL8'Yrly',CL1'Q',CL8'PriFin',X'19'                               
*        DC    C'K0',CL40'Station Office Ranker'                                
*        DC    CL8'Yrly',CL1'Q',CL8'PriFin',X'20'                               
*        DC    X'00'                                                            
*                                                                               
RPOTABSZ DC    C'K3',CL40'Station Office Advertiser Summary Ranked'             
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'13'                              
         DC    C'K4',CL40'Station Advertiser Summary Ranked'                    
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'14'                              
         DC    C'K6',CL40'Office Advertiser Summary Ranked'                     
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'16'                              
         DC    C'K7',CL40'Category Billing Report (Spot Only)'                  
         DC    CL8'Yrly',CL1'A',CL8'Alpha',X'17'                                
         DC    C'K8',CL40'Market/Office Summary Report'                         
         DC    CL8'Yrly',CL1'Q',CL8'Alpha',X'18'                                
         DC    C'K9',CL40'Office/Station Ranker'                                
         DC    CL8'Yrly',CL1'Q',CL8'PriFin',X'19'                               
         DC    C'K0',CL40'Station Office Ranker'                                
         DC    CL8'Yrly',CL1'Q',CL8'PriFin',X'20'                               
         DC    C'KA',CL40'Station/Office Advertiser Blg Report'                 
         DC    CL8'1 Qtr',CL1'A',CL8'PriFin',X'1A'                              
         DC    C'KB',CL40'Station Advertiser Blg Report'                        
         DC    CL8'1 Qtr',CL1'A',CL8'PriFin',X'1B'                              
         DC    C'KC',CL40'Station Office Agency Ranker'                         
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'1C'                              
         DC    C'KD',CL40'Station Office Agency/Advertiser Ranker'              
         DC    CL8'Yrly',CL1'Q',CL8'CurrBlg',X'1D'                              
         DC    C'W1',CL40'Pacing by Station/Group                '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'21'                                  
         DC    C'W4',CL40'Pacing Capitols div by Station (*CAPL) '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'24'                                  
         DC    C'W7',CL40'Pacing Republics div by Station (*REPL)'              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'27'                                  
         DC    C'WA',CL40'Pacing/Voyagers - Lee Winikoff (*VOYL) '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'2A'                                  
*gone    DC    C'WG',CL40'Pacing/Voyagers - Jeff Roberts (*VOYB) '              
*gone    DC    CL8'Yrly',CL1'M',CL8'N/A',X'2B'                                  
         DC    C'WC',CL40'Pacing/Voyagers - Jeff Roberts (*VOYR) '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'2C'                                  
         DC    C'WD',CL40'Pacing by Station/Group (New York)     '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'2D'                                  
         DC    C'WE',CL40'Pacing by Station/Group (*STAR/NY)     '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'2E'                                  
         DC    C'WF',CL40'Pacing by Station/Group (*ROCK/NY)     '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'31'                                  
*TEMP    DC    C'WO',CL40'*TOM R1 Station Report                 '              
*        DC    CL8'Yrly',CL1'M',CL8'N/A',X'2F'                                  
*        DC    C'WT',CL40'*TOM R3 Station Report                 '              
*TEMP    DC    CL8'Yrly',CL1'M',CL8'N/A',X'30'                                  
         DC    C'W2',CL40'Pacing by Station by Office            '              
         DC    CL8'Yrly',CL1'Q',CL8'N/A',X'22'                                  
         DC    C'W5',CL40'Pacing Capitols div by Office (*CAPL)  '              
         DC    CL8'Yrly',CL1'Q',CL8'N/A',X'25'                                  
         DC    C'W8',CL40'Pacing Republics div by Office (*REPL) '              
         DC    CL8'Yrly',CL1'Q',CL8'N/A',X'28'                                  
         DC    C'W3',CL40'Pacing by Office/National Group        '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'23'                                  
         DC    C'WH',CL40'Pacing by Office/Team Group Summary    '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'32'                                  
         DC    C'WJ',CL40'Pacing by Statio/Group (BB)            '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'33'                                  
         DC    C'W6',CL40'Pacing Capitols div by Off/Team (*CAPL)'              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'26'                                  
         DC    C'W9',CL40'Pacing Republics div by Off/Team (*REPL)'             
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'29'                                  
         DC    C'WK',CL40'Pacing/NY Senators A (*NYSA)           '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'34'                                  
         DC    C'WL',CL40'Pacing/NY Senators B (*NYSB)           '              
         DC    CL8'Yrly',CL1'M',CL8'N/A',X'35'                                  
         DC    X'00'                                                            
*                                                                               
RPOTABD  DSECT                                                                  
RPOREP   DS    CL2                                                              
RPODESC  DS    CL40                                                             
RPOPERD  DS    CL8                                                              
RPOFORM  DS    CL1                                                              
RPORANK  DS    CL8                                                              
RPOOVR   DS    XL1                                                              
RPOTABLQ EQU   *-RPOTABD                                                        
*                                                                               
VIEW10   CSECT                                                                  
*                                                                               
FF       EQU   X'FF'                                                            
         SPACE 1                                                                
DCLIST   DS    0D                                                               
         DCDDL GE#SEL,3,L                                                       
         DC    X'00'                                                            
         SPACE 1                                                                
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                             *         
***********************************************************************         
         SPACE 1                                                                
MYSAVED  DSECT                                                                  
LSNTRY   DS    XL(SUBACTLQ)        SINGLE ENTRY ACTION                          
LMNTRY   DS    XL(SUBACTLQ)        MULTIPLE ENTRY ACTION                        
LMCOUNT  DS    XL(SUBACTLQ)        REPEAT COUNT FOR MULTIPLES                   
*                                                                               
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
LINENOW  DS    H                                                                
*                                                                               
DSLISTU  DS    0C                                                               
FU@SEL   DS    XL3                                                              
*                                                                               
DSLISTL  DS    0C                                                               
FL@SEL   DS    XL3                                                              
         SPACE 2                                                                
*        REVIEWWRK                                                              
         PRINT OFF                                                              
       ++INCLUDE REVIEWWRK                                                      
         PRINT ON                                                               
*                                                                               
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLREP    DS    CL(L'RPOREP)                                                     
         ORG   TLRDATA                                                          
TLDESC   DS    CL(L'RPODESC)                                                    
TLPERD   DS    CL(L'RPOPERD)                                                    
TLFORM   DS    CL(L'RPOFORM)                                                    
TLRANK   DS    CL(L'RPORANK)                                                    
TLOVR    DS    CL(L'RPOOVR)                                                     
TLLNQ    EQU   *-TLSTD                                                          
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'024REVIEW10  08/10/11'                                      
         END                                                                    
