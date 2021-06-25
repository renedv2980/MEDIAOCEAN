*          DATA SET REPRO15S   AT LEVEL 010 AS OF 06/25/97                      
*&&      SET   NOP=N                                                            
*PHASE T80A15A                                                                  
*                                                                               
T80A15   TITLE 'REPRO15 - OBJECT VERSION OF TEXT RECORDS'                       
PRO15    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO15*,R7,RR=RE                                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
*                                                                               
         L     R8,AGWORK                                                        
         USING GWORKD,R8                                                        
         L     RC,AOVERWRK                                                      
         USING OVERWRKD,RC                                                      
         ST    RE,BORELO                                                        
         MVC   SVPARMS,0(R1)                                                    
         ST    R1,CALLR1                                                        
*                                                                               
         LR    R4,RA                                                            
         AH    R4,=Y(R15TWRK-TWAD)                                              
         USING R15TWRK,R4                                                       
*                                                                               
         SRL   RF,24                                                            
         CLM   RF,1,=AL1(ROUTSN)                                                
         BNH   *+6                 UNKNOWN ROUTINE                              
         DC    H'0'                                                             
         SLL   RF,2                                                             
         B     ROUTS(RF)                                                        
*                                                                               
ROUTS    DS    0XL4                                                             
         B     OBJECT              OBJECT INVOKER                               
         B     INIT                INITIALIZATION CALL                          
*                                                                               
ROUTSN   EQU   (*-ROUTS)/4                                                      
         EJECT                                                                  
A#R15LST EQU   34                                                               
A#R15SEL EQU   2                                                                
LST#SCR1 EQU   C'L'                LIST SCREEN 1                                
LST#SCR2 EQU   C'X'                LIST SCREEN 2                                
SEL#SCR1 EQU   0                   SELECT SCREEN 1                              
SEL#SCR2 EQU   C'S'                SELECT SCREEN 2                              
***********************************************************************         
* TABLE ITERATION ROUTINE  - EXPECTS RF TO HOLD A(TABLE)                        
*                          - EXPECTS R1 TO HOLD VERB                            
***********************************************************************         
         USING OBJTABD,RF                                                       
ITER     CLI   OBJVERB,EOT         E.O.T.                                       
         BE    EXITOK           ** NEED TO SET HIGH IF NOT OVERRIDE             
         CLM   R1,1,OBJVERB        R1 HOLDS EQUATED VERB                        
         BE    ITER02              MATCHED                                      
         LA    RF,OBJTABL(RF)                                                   
         B     ITER                ITERATE THIS TABLE                           
*                                                                               
ITER02   LR    RE,RF               @@ DEBUG  @@                                 
         ICM   RF,15,OBJADR        ROUTINE TO HANDLE THE VERB                   
         LA    R1,SVPARMS                                                       
         A     RF,BORELO                                                        
         BR    RF                                                               
         DROP  RF                                                               
***********************************************************************         
* OBJECT CONTROLLER - INTERFACES TO ALL USER OBJECTS                            
*                                                                               
* P1 HOLDS EQUATED VERB                                                         
***********************************************************************         
OBJECT   L     R1,SVPARMS                                                       
         LA    RF,TABLEOO          KNOWN OBJECTS                                
         B     ITER                                                             
*                                                                               
TABLEOO  DC    AL1(OKEY),AL1(0,0,0),AL4(KEY)                                    
         DC    AL1(ORECH),AL1(0,0,0),AL4(RECRD)                                 
         DC    AL1(ODATA),AL1(0,0,0),AL4(DATA)                                  
         DC    AL1(OSES),AL1(0,0,0),AL4(NTRSES)                                 
         DC    AL1(OSCRN),AL1(0,0,0),AL4(SCRN)                                  
         DC    AL1(OLIST),AL1(0,0,0),AL4(LIST)                                  
         DC    AL1(OPFK),AL1(0,0,0),AL4(PFKEY)                                  
         DC    AL1(OSUBACT),AL1(0,0,0),AL4(EXITH)                               
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             UNPROT ON NTRSES                     
         OI    GSINDSL1,GSINOIO    WE'LL DO THE IO'S                            
         OI    GSINDSL1,GSIXKEY    NO ENTER KEY MESG                            
         OI    LSSTAT1,LSSBALL     BUILD ALL THE LIST AT ONCE                   
         OI    LSSTAT1,LSSMAIN     LIST PART OF MAINT. SCREEN                   
         OI    LSSTAT1,LSSTSAR     TSAR ONLY LIST                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* KEY OBJECT                                                                    
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
KEY      LM    R0,R2,SVPARMS                                                    
         LA    RF,KEYTABL                                                       
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
KEYTABL  DC    AL1(KFIRST),AL1(0,0,0),AL4(KEYFRST)                              
         DC    AL1(KLAST),AL1(0,0,0),AL4(KEYLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE KEY FIELDS ON THE SCREEN                         
***********************************************************************         
KEYFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KFTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KFKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KFKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE KEY FIELDS                                              
***********************************************************************         
KFKDIS   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
KFKVAL   DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG   DON'T KNOW IF KEY CHANGED YET          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE KEY FIELDS ON THE SCREEN                   
***********************************************************************         
KEYLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,KLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
KLTABL   DC    AL1(KDIS),AL1(0,0,0),AL4(KLKDIS)      DISPLAY                    
         DC    AL1(KVAL),AL1(0,0,0),AL4(KLKVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER DISPLAYING THE KEY FIELDS                                               
***********************************************************************         
KLKDIS   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* AFTER VALIDATING THE KEY FIELDS                                               
***********************************************************************         
KLKVAL   DS    0H                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
*                                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         BZ    KLKVX                                                            
*                                                                               
         CLI   CSACT,A#R15LST                                                   
         BNE   KLKVX                                                            
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    KLKVX                                                            
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'R15KMEL),R15KMEL    DETAIL ELEMENT                     
         BAS   RE,MINIORD                    READ IT                            
         BE    *+6                 SOMEONE PASSED A BOGUS DETAIL                
         DC    H'0'                                                             
         BAS   RE,INTOAIO5         FOR RECUP                                    
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   ELCODE,RPRITELQ     INVENTORY TEXT ELEMENT                       
         BAS   RE,FIRSTEL                                                       
         BNE   KLKVX               NOTHING TO DELETE                            
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),(R6)                        
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'R15KMEL),R15KMEL    DETAIL ELEMENT                     
         BAS   RE,MINIORD                    READ IT                            
         BE    *+6                 SOMEONE PASSED A BOGUS DETAIL                
         DC    H'0'                                                             
*                                                                               
         BAS   RE,FROMAIO5         FOR RECUP                                    
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,MINIOCLS                                                      
*                                                                               
KLKVX    B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RECORD OBJECT                                                                 
*                                                                               
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 A(KEY OR WHERE TO BUILD THE KEY)                                           
* P4 HOLDS SUB-ACTION                                                           
***********************************************************************         
RECRD    LM    R0,R2,SVPARMS                                                    
         LA    RF,RECRDTBL                                                      
         B     ITER                ITERATE KEY TABLE                            
*                                                                               
RECRDTBL DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* AFTER THE CONTROLLER CALLS THE I/O ACTION                                     
***********************************************************************         
RECLAST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RLTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RLTABL   DS    0H                                                               
         DC    AL1(RWRT),AL1(0,0,0),AL4(RLRWRT)      WRITE                      
         DC    AL1(EOT)                                                         
***********************************************************************         
* AFTER THE I/O CALL TO WRITE THE RECORD                                        
***********************************************************************         
RLRWRT   DS    0H                                                               
         BAS   RE,MINIOCLS                                                      
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECTS FOR KEY DATA OR RECORD DATA                                      
*                                                                               
* P1 HOLDS EQUATED OBJECT IDENTIFIER                                            
* P2 HOLDS EQUATED DATA IDENTIFIER OR 0 IF GLOBAL DATA ACTION                   
* P3 BYTE  0   HOLDS EQUATED DATA VERB IF P2 IS ZERO                            
* P3 BYTES 1-3 HOLDS EQUATED ACTION VERB                                        
* P4 HOLDS A(RECORD AT CORRECT LEVEL)                                           
* P5 HOLDS A(FIELD TABLE ENTRY) OR ZERO IF P2 IS ZERO                           
*                                                                               
* FIELD DATA IS EXTRACTED/OUTPUT INTO FVIFLD                                    
***********************************************************************         
DATA     ICM   R1,15,SVPARMS2      DOING ACTION ON SPECIFIC DATA OBJ?           
         BNZ   DATA10              YES                                          
***********************************************************************         
************** DOING A GLOBAL ACTION ON ENTIRE RECORD *****************         
***********************************************************************         
         L     R2,SVPARMS4         R2 = A(RECORD)                               
         SR    R1,R1                                                            
         IC    R1,SVPARMS3         R1 = GLOBAL ACTION                           
         LA    RF,DTATABL          TABLE OF GLOBAL VERBS                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DTATABL  DC    AL1(DFIRST),AL1(0,0,0),AL4(DTAFRST)                              
         DC    AL1(DLAST),AL1(0,0,0),AL4(DTALAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE WE DO ANYTHING TO THE DATA FIELDS ON THE SCREEN                        
***********************************************************************         
DTAFRST  DS    0H                                                               
         L     R1,SVPARMS3         VERB                                         
         LA    RF,DFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
DFTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DFDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DFDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE DISPLAYING THE DATA FIELDS                                             
***********************************************************************         
DFDDIS   DS    0H                                                               
         LR    RE,RA                                                            
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)     ANY CONTRACT NUMBER?                  
         BZ    DFDDISX                    NO                                    
         CLI   SVPRONUM-SVCONNUM(RE),0    ANY PROPOSAL NUMBER?                  
         BZ    DFDDISX                    NO                                    
*                                                                               
         BAS   RE,READDTEL         READ DETAIL ELEMENT                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
***************************                                                     
** BUILD A LIST OF BOOKS **                                                     
***************************                                                     
DFDDBK0  XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRBKELQ    BOOK ELEMENT                                 
         BAS   RE,MINIOHI                                                       
         BE    *+6                  1 REQ'D                                     
         DC    H'0'                                                             
*                                                                               
         LA    R3,SAVBOOKS                                                      
         L     R6,MINELEM                                                       
         USING RPRBKELD,R6                                                      
DFDDBK5  CLI   0(R6),RPRBKELQ                                                   
         BNE   DFDDBKX             ALL DONE                                     
         CLI   RPRBKLEN,RPRBKOVQ   USER DEFINED?                                
         BNE   DFDDBK10            YES                                          
*                                                                               
         MVC   0(3,R3),RPRBKSTT                                                 
         MVC   3(1,R3),RPRBKFIL                                                 
         MVC   4(1,R3),RPRBKBKT                                                 
         LA    R3,L'SAVBOOK(R3)                                                 
         LA    RE,SAVBOOKS+L'SAVBOOKS                                           
         CR    R3,RE                                                            
         BNL   DFDDBKX             SAVBOOKS FULL                                
*                                                                               
DFDDBK10 BAS   RE,MINIOSEQ                                                      
         BE    DFDDBK5                                                          
*                                                                               
DFDDBKX  DS    0H                                                               
***************************                                                     
** BUILD A LIST OF DEMOS **                                                     
***************************                                                     
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDMELQ    DEMO ELEMENT                                 
         BAS   RE,MINIOHI                                                       
         BE    *+6                  1 REQ'D                                     
         DC    H'0'                                                             
*                                                                               
         LA    R3,SAVDEMOS                                                      
         USING RPRDMELD,R6                                                      
DFDDDM5  CLI   0(R6),RPRDMELQ                                                   
         BNE   DFDDDMX             ALL DONE                                     
         MVC   0(L'SAVDEMO,R3),RPRDMBY1                                         
         LA    R3,L'SAVDEMO(R3)                                                 
         LA    RE,SAVDEMOS+L'SAVDEMOS                                           
         CR    R3,RE                                                            
         BNL   DFDDDMX             SAVDEMOS FULL                                
*                                                                               
         BAS   RE,MINIOSEQ                                                      
         BE    DFDDDM5                                                          
*                                                                               
DFDDDMX  DS    0H                                                               
         DROP  R5,R6                                                            
*                                                                               
DFDDISX  B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
         BAS   RE,READDTEL         READ DETAIL ELEMENT                          
         BE    EXITOK                                                           
         DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* READ DATA FROM DETAIL ELEMENT                                                 
***********************************************************************         
READDTEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,MINELEM                                                       
*                                                                               
         USING RPRDTELD,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'R15KMEL),R15KMEL    DETAIL ELEMENT                     
         BAS   RE,MINIOHI                    READ IT                            
         BNE   EXITL                         MINIO ERROR                        
*                                                                               
         CLI   0(R6),RPRDTELQ                                                   
         BNE   EXITL                         BOGUS DETAIL                       
         CLC   2(L'R15KMEL-1,R6),R15KMEL+1                                      
         BNE   EXITL                         BOGUS DETAIL                       
*                                                                               
         MVC   R15STACD,RPRDTSTA                                                
         MVC   R15DPT,RPRDTDPT                                                  
         MVC   R15DAY,RPRDTDAY                                                  
         MVC   R15TIM,RPRDTTIM                                                  
         MVC   R15SEQ#,RPRDTSEQ                                                 
         MVC   R15STM,RPRDTSTM                                                  
         MVC   R15ETM,RPRDTETM                                                  
         MVC   R15INV#,RPRDTINM                                                 
         MVC   R15EFST,RPRDTEFF                                                 
         MVC   R15EFEN,RPRDTEEF                                                 
         MVC   R15PRGCD,RPRDTPRG                                                
*                                                                               
         USING RPRSTELD,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ    STATION ELEMENT                              
         MVC   MINEKEY+1(1),R15STACD                                            
         BAS   RE,MINIORD                                                       
         BE    *+6                 THE STATION IS BOGUS                         
         DC    H'0'                                                             
         MVC   R15STATX,RPRSTSTA                                                
         TM    RPRSTFLG,RPRSTSTL              SATELLITE REQUEST?                
         BZ    *+8                            NO                                
         MVI   R15STATX+4,C'1'                                                  
*                                                                               
         USING RPRTXELD,R6                                                      
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRTXELQ         TEXT ELEMENT                            
         MVI   MINEKEY+1,RPRTXPRQ       PROGRAM TEXT                            
         MVC   MINEKEY+6(2),R15PRGCD                                            
         BAS   RE,MINIORD                                                       
         BE    *+6                 THE TEXT # IS BOGUS                          
         DC    H'0'                                                             
*                                                                               
         MVC   R15PRGTX,BCSPACES                                                
         ZIC   RE,RPRTXLEN                                                      
         SH    RE,=Y(RPRTXOVQ+1)   EX LENGTH                                    
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   R15PRGTX(0),RPRTXTXT                                             
         B     EXITOK                                                           
         DROP  R5,R6                                                            
         EJECT                                                                  
***********************************************************************         
* AFTER WE'VE DONE EVERYTHING TO THE DATA FIELDS ON THE SCREEN                  
***********************************************************************         
DTALAST  DS    0H                                                               
         L     R1,SVPARMS3         VERB IN R1                                   
         LA    RF,DLTABL                                                        
         B     ITER                ITERATE TABLE                                
*                                                                               
DLTABL   DC    AL1(DDIS),AL1(0,0,0),AL4(DLDDIS)      DISPLAY                    
         DC    AL1(DVAL),AL1(0,0,0),AL4(DLDVAL)      VALIDATE                   
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* AFTER DISPLAYING THE DATA FIELDS                                              
***********************************************************************         
DLDDIS   DS    0H                                                               
         B     EXITOK                                                           
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLDVAL   DS    0H                                                               
*                                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LA    RF,KNOWTAB          TABLE OF KNOWN OBJECTS                       
         USING KNOWTABD,RF                                                      
*                                                                               
DATA20   CLC   KNOWID,=AL2(EOT)    REACH END - NOT A KNOWN DATA TYPE            
         BE    EXITH                                                            
         CLM   R1,3,KNOWID         IS THIS A KNOWN TYPE?                        
         BE    DATA30              YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     DATA20                                                           
***********************************                                             
* WE KNOW OF THIS DATA OBJECT                                                   
***********************************                                             
DATA30   ICM   RF,15,KNOWADD       A(KNOWN OBJECT)                              
         A     RF,BORELO           RELOCATE IT                                  
         DROP  RF                                                               
*                                                                               
         LM    R1,R2,SVPARMS3      R1 HOLDS VERB                                
         USING FRRRECD,R2          R2 HOLDS A(RECORD)                           
         L     R3,AFRREL                                                        
         USING FRRELD,R3           R3=A(FRREL ON RECORD)                        
         BR    RF                                                               
***********************************************************************         
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY PORTION                                                                   
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
         DC    AL2(00003),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(00004),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(00005),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(00006),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(00007),AL4(BYRDTA)    BUYER                                  
         DC    AL2(00008),AL4(STADTA)    STATION                                
         DC    AL2(00009),AL4(FLTDTA)    FLIGHT DATES                           
         DC    AL2(00010),AL4(DSKDTA)    DISK ADDRESS                           
         DC    AL2(00026),AL4(DVSDTA)    DEVELOPMENT SALESPERSON                
         DC    AL2(00027),AL4(DVTDTA)    DEVELOPMENT CONTRACT TYPE              
* DETAIL CLUSTER KEY                                                            
         DC    AL2(00012),AL4(DPTDTA)    DAYPART                                
         DC    AL2(00013),AL4(DTMDTA)    DAY/TIME                               
         DC    AL2(00025),AL4(SEQDTA)    SEQUENCE #                             
* DETAIL CLUSTER DATA                                                           
         DC    AL2(00021),AL4(PRGDTA)    PROGRAM                                
         DC    AL2(00022),AL4(INVDTA)    INVENTORY NUMBER                       
         DC    AL2(00024),AL4(EFFDTA)    EFFECTIVE DATE                         
* SELECT SCREEN DATA FIELDS                                                     
         DC    AL2(00023),AL4(TXTSNDTA) TEXT NUMBER                             
* 'LIST' COLUMNS                                                                
         DC    AL2(00016),AL4(TXTNMDTA)  TEXT NUMBER                            
         DC    AL2(00018),AL4(PRTDTA)    PRINT OPT                              
         DC    AL2(00017),AL4(DESDTA)    DESCRIPTION                            
* SELECT SCREEN FIELDS                                                          
         DC    AL2(00011),AL4(TXTDTA)    TEXT                                   
         DC    AL2(00028),AL4(PRNTDTA)   PRINT                                  
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
PRO15    CSECT                                                                  
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR CONTRACT NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
CONDTA   DS    0H                                                               
         LA    RF,CONTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
CONTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISCON)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALCON)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRCON)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
DISCON   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY CONTRACT NUMBER?              
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)                                           
         BZ    NTRCONX             NO                                           
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         ZAP   BOWORK1(5),=P'99999999'                                          
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  FVIFLD(8),BOWORK1(5)                                             
*                                                                               
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A CONTRACT FIELD                                                     
***********************************************************************         
VALCON   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
NTRCON   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY CONTRACT NUMBER?              
         AH    RE,=Y(SVCONNUM-TWAD)                                             
         OC    0(L'SVCONNUM,RE),0(RE)                                           
         BZ    NTRCONX             NO                                           
*                                                                               
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),0(4,RE)                                            
         ZAP   BOWORK1(5),=P'99999999'                                          
         SP    BOWORK1(5),BOWORK1+10(5)                                         
         OI    BOWORK1+4,X'0F'                                                  
         UNPK  FVIFLD(8),BOWORK1(5)                                             
         OI    FVIIND,FVIVAL       VALIDATED                                    
NTRCONX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROPOSAL NUMBER                                               
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRODTA   DS    0H                                                               
         LA    RF,PROTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PROTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRO)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRO)                                 
         DC    AL1(DNTR),AL1(0,0,0),AL4(NTRPRO)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A PROPOSAL FIELD                                                      
***********************************************************************         
DISPRO   DS    0H                                                               
         USING RPROKEY,R2                                                       
         ZIC   RE,RPROKPRO                                                      
         LA    R0,X'FF'                                                         
         SR    R0,RE                                                            
         EDIT  (R0),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1                
         DROP  R2                                                               
*                                                                               
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE A PROPOSAL FIELD                                                     
***********************************************************************         
VALPRO   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   FVILEN,0            ANY DATA IN THIS FIELD?                      
         BE    EXITNO                                                           
*                                                                               
         GOTOX (VALPROQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
         MVC   RPROKPRO,BPRONUM                                                 
         DROP  R2                                                               
*                                                                               
         GOTOX (MNIOINQ,AREPRO01),BOPARM INITIALIZE MINIO                       
         OI    FVIIND,FVIVAL       VALIDATED                                    
*                                                                               
         TM    MISCFLG1,MF1NTRIN   PARAMETERS PASSED?                           
         BZ    VALPROX             NO                                           
         BAS   RE,READDTEL         YES - READ THE ELEMENT                       
         BE    VALPROX                                                          
*                                                                               
         MVC   FVMSGNO,=AL2(564)                                                
         B     EXITL                                                            
*                                                                               
VALPROX  B     EXITOK                                                           
***********************************************************************         
* PASS PROPOSAL NUMBER TO NEXT SESSION                                          
***********************************************************************         
NTRPRO   DS    0H                                                               
         LR    RE,RA               DO WE HAVE ANY PROPOSAL NUMBER?              
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         CLI   0(RE),0                                                          
         BE    NTRPROX             NO                                           
*                                                                               
         MVC   BOBYTE1,0(RE)                                                    
         XI    BOBYTE1,X'FF'                                                    
         EDIT  (B1,BOBYTE1),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,         X        
               DUB=BODUB1                                                       
         OI    FVIIND,FVIVAL       VALIDATED                                    
NTRPROX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR AGENCY                                                        
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
AGYDTA   DS    0H                                                               
         LA    RF,AGYTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
AGYTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISAGY)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY AGENCY FIELD                                                          
***********************************************************************         
DISAGY   DS    0H                                                               
         MVC   FVIFLD(L'EAGYNAM1),EAGYNAM1                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR ADVERTISER                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
ADVDTA   DS    0H                                                               
         LA    RF,ADVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
ADVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISADV)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY ADVERTISER FIELD                                                      
***********************************************************************         
DISADV   DS    0H                                                               
         MVC   FVIFLD(L'EADVNAME),EADVNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRODUCT                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRDDTA   DS    0H                                                               
         LA    RF,PRDTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRDTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRD)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY PRODUCT FIELD                                                         
***********************************************************************         
DISPRD   DS    0H                                                               
         MVC   FVIFLD(L'EPRDNAME),EPRDNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SALESPERSON                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SALDTA   DS    0H                                                               
         LA    RF,SALTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SALTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSAL)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SALESPERSON FIELD                                                     
***********************************************************************         
DISSAL   DS    0H                                                               
         MVC   FVIFLD(L'ESALNAME),ESALNAME                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR BUYER                                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
BYRDTA   DS    0H                                                               
         LA    RF,BYRTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
BYRTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISBYR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY BUYER FIELD                                                           
***********************************************************************         
DISBYR   DS    0H                                                               
         MVC   FVIFLD(L'ECONBUYR),ECONBUYR                                      
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR STATION                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
STADTA   DS    0H                                                               
         LA    RF,STATBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
STATBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSTA)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSTA)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* VALIDATE STATION FIELD                                                        
***********************************************************************         
VALSTA   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         TM    MISCFLG1,MF1NTRIN                                                
         BNZ   DISSTA              DISPLAY INSTEAD                              
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         L     R5,AIO7              FIND THE STATION ELEMENT                    
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRSTELQ                                                 
         L     R6,MINELEM                                                       
         USING RPRSTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BNE   EXITNV                                                           
*                                                                               
VALSTA5  CLI   0(R6),RPRSTELQ                                                   
         BNE   EXITNV                                                           
*                                                                               
         MVC   BODUB1(L'RPRSTSTA),RPRSTSTA                                      
         TM    RPRSTFLG,RPRSTSTL             SATELLITE STATION?                 
         BZ    *+8                                                              
         MVI   BODUB1+L'RPRSTSTA-1,C'1'       YES                               
*                                                                               
         CLC   BODUB1(L'RPRSTSTA),FVIFLD                                        
         BE    VALSTA10                                                         
         BAS   RE,MINIOSEQ                                                      
         BE    VALSTA5                                                          
         B     EXITNV                                                           
*                                                                               
VALSTA10 MVC   R15STATX,FVIFLD                                                  
         MVC   R15STACD,RPRSTICD                                                
         TM    RPRSTFLG,RPRSTSTL              SATELLITE REQUEST?                
         BZ    *+8                            NO                                
         MVI   R15STATX+4,C'1'                                                  
         DROP  R6,R5                                                            
*                                                                               
VALSTAX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* DISPLAY STATION FIELD                                                         
***********************************************************************         
DISSTA   DS    0H                                                               
         MVC   FVIFLD(L'R15STATX),R15STATX                                      
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR FLIGHT DATES                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
FLTDTA   DS    0H                                                               
         LA    RF,FLTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
FLTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISFLT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY FLIGHT FIELD                                                          
***********************************************************************         
DISFLT   DS    0H                                                               
         GOTO1 VDATCON,BODMCB,(3,CCONDAT),(5,FVIFLD)                            
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,CCONDAT+3),(5,FVIFLD+9)                             
         OI    FVATRB,FVAPROT+FVAHIGH      PROTECT AND HIGHLIGHT                
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DISK ADDRESS                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DSKDTA   DS    0H                                                               
         LA    RF,DSKTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DSKTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDSK)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DISK ADDRESS                                                          
***********************************************************************         
DISDSK   DS    0H                                                               
         GOTO1 VHEXOUT,BODMCB,GSRECDA,FVIFLD,L'GSRECDA                          
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT SALESPERSON                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVSDTA   DS    0H                                                               
         LA    RF,DVSTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVSTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDVS)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT SALESPERSON FIELD                                         
***********************************************************************         
DISDVS   DS    0H                                                               
         MVC   FVIFLD(L'EDVSNAME),EDVSNAME                                      
         B     DISDVSX                                                          
*                                                                               
DISDVS5  MVC   FVIFLD(L'CCONDVS),CCONDVS                                        
*                                                                               
DISDVSX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DEVELOPMENT TYPE                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DVTDTA   DS    0H                                                               
         LA    RF,DVTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DVTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDVT)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DEVELOPMENT TYPE                                                      
***********************************************************************         
DISDVT   DS    0H                                                               
         MVC   FVIFLD(L'CCONDVT),CCONDVT                                        
*                                                                               
DISDVTX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DAYPART                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DPTDTA   DS    0H                                                               
         LA    RF,DPTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DPTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDPT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDPT)                                 
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY DAYPART                                                               
**********************************************************************          
DISDPT   DS    0H                                                               
         MVC   FVIFLD(L'R15DPT),R15DPT                                          
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
**********************************************************************          
* VALIDATE DAYPART                                                              
**********************************************************************          
VALDPT   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         TM    MISCFLG1,MF1NTRIN                                                
         BNZ   DISDPT              DISPLAY INSTEAD                              
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         L     R5,AIO7              FIND A DETAIL ON DAYPART                    
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         MVC   MINEKEY+1(1),R15STACD                                            
         MVC   MINEKEY+2(1),FVIFLD                                              
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BNE   EXITNV                                                           
*                                                                               
         CLI   0(R6),RPRDTELQ                                                   
         BNE   EXITRCNF                                                         
         CLC   RPRDTSTA,R15STACD                                                
         BNE   EXITRCNF                                                         
         CLC   RPRDTDPT,FVIFLD                                                  
         BNE   EXITRCNF                                                         
*                                                                               
         MVC   R15DPT,FVIFLD                                                    
         DROP  R6,R5                                                            
*                                                                               
VALDPTX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
               EJECT                                                            
***********************************************************************         
* DATA OBJECT FOR DAYS/TIMES                                                    
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DTMDTA   DS    0H                                                               
         LA    RF,DTMTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DTMTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDTM)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALDTM)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY DAYS/TIMES FIELD                                                      
***********************************************************************         
DISDTM   DS    0H                                                               
*                                                                               
         CLI   R15DAY,0                       ANY DAYS?                         
         BE    DISDTM50                       NO                                
         GOTO1 VDAYUNPK,BODMCB,R15DAY,FVIFLD   YES                              
*                                                                               
         LA    RE,FVIFLD                                                        
DISDTM10 CLI   0(RE),C' '          CONVERT '/'S TO ','S                         
         BNH   DISDTM20                                                         
         CLI   0(RE),C'/'                                                       
         BNE   *+8                                                              
         MVI   0(RE),C','                                                       
         LA    RE,1(RE)                                                         
         B     DISDTM10                                                         
*                                                                               
DISDTM20 LA    R3,FVIFLD+9         FIND WHERE TO PUT THE TIMES                  
DISDTM30 CLI   0(R3),C' '                                                       
         BH    DISDTM40                                                         
         BCTR  R3,0                                                             
         B     DISDTM30                                                         
DISDTM40 LA    R3,1(R3)                                                         
*                                                                               
DISDTM50 OC    R15STM,R15STM       START TIME?                                  
         BNZ   DISDTM55            YES                                          
         OC    R15STM,R15ETM       END TIME?                                    
         BZ    DISDTMX             NO                                           
*                                                                               
DISDTM55 MVI   0(R3),C'/'          SEPARATE DAYS AND TIMES WITH A C'/'          
         LA    R3,1(R3)                                                         
*                                                                               
         GOTO1 VUNTIME,BODMCB,R15STM,0(R3)                                      
*                                                                               
DISDTMX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE DAYS/TIMES FIELD                                                     
***********************************************************************         
VALDTM   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         TM    MISCFLG1,MF1NTRIN                                                
         BNZ   DISDTM              DISPLAY INSTEAD                              
*                                                                               
         CLI   FVILEN,0                                                         
         BZ    EXITNO                                                           
*                                                                               
         L     RE,AIO4                                                          
         LA    RF,480                                                           
         XCEFL                                                                  
*                                                                               
         GOTO1 VSCANNER,BODMCB,FVIHDR,(X'83',AIO4),C',=/='                      
         CLI   4(R1),2             SHOULD HAVE 2 COMPONENTS                     
         BNE   EXITNV                NO MORE, NO LESS                           
*                                                                               
         L     R3,AIO4                                                          
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BE    EXITNO                                                           
         GOTO1 VDAYVAL,BODMCB,(0(R3),12(R3)),R15DAY,BOBYTE1                     
         CLI   0(R3),0             DAYVAL COMPLAINS IF INPUT > 11               
         BE    EXITNV                                                           
*                                                                               
VALDTM10 LA    R3,32(R3)                                                        
*                                                                               
         CLI   1(R3),0                                                          
         BNE   EXITNV                                                           
         CLI   0(R3),0                                                          
         BNE   VALDTM20                                                         
         MVC   FVERRNDX,4(R3)     WE NEED THE TIME PORTION ALSO                 
         B     EXITNO                                                           
*                                                                               
VALDTM20 GOTO1 VTIMVAL,BODMCB,(0(R3),12(R3)),BOWORK1                            
*                                                                               
         CLI   0(R1),X'FF'                                                      
         BNE   *+14                                                             
         MVC   FVERRNDX,4(R3)     POINT TO THE TIME PORTION                     
         B     EXITNV                 THAT'S IN ERROR                           
*                                                                               
         MVC   R15STM,BOWORK1                                                   
         MVC   R15ETM,BOWORK1+2                                                 
         GOTOX (PCKTIMQ,AREPRO01),BODMCB,BOWORK1,BOWORK1+2,R15TIM               
*                                                                               
         L     R5,AIO7              FIND A DETAIL ON DAYPART/DTM                
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         MVC   MINEKEY+1(1),R15STACD                                            
         MVC   MINEKEY+2(1),R15DPT                                              
         MVC   MINEKEY+3(1),R15DAY                                              
         MVC   MINEKEY+4(2),R15TIM                                              
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BNE   EXITRCNF                                                         
*                                                                               
         CLI   0(R6),RPRDTELQ                                                   
         BNE   EXITRCNF                                                         
         CLC   RPRDTSTA,R15STACD                                                
         BNE   EXITRCNF                                                         
         CLC   RPRDTDPT,R15DPT                                                  
         BNE   EXITRCNF                                                         
         CLC   RPRDTDAY,R15DAY                                                  
         BNE   EXITRCNF                                                         
         CLC   RPRDTTIM,R15TIM                                                  
         BNE   EXITRCNF                                                         
*                                                                               
         DROP  R6,R5                                                            
VALDTMX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR SEQUENCE FIELD                                                
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
SEQDTA   DS    0H                                                               
         LA    RF,SEQTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
SEQTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISSEQ)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALSEQ)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY SEQUENCE FIELD                                                        
***********************************************************************         
DISSEQ   DS    0H                                                               
         EDIT  R15SEQ#,(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1,   X        
               ZERO=NOBLANK                                                     
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE SEQUENCE FIELD                                                       
***********************************************************************         
VALSEQ   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         TM    MISCFLG1,MF1NTRIN                                                
         BNZ   DISSEQ              DISPLAY INSTEAD                              
*                                                                               
         CLI   FVILEN,0                                                         
         BNZ   VALSEQ5                                                          
*                                                                               
         MVI   FVIFLD,C'0'                                                      
         XC    BCFULL,BCFULL                                                    
         B     VALSEQ10                                                         
*                                                                               
VALSEQ5  TM    FVIIND,FVINUM       VALID NUMERIC FIELD?                         
         BZ    EXITNOTN            NO                                           
VALSEQ10 L     R1,BCFULL           THIS IS THE BINARY FOR THE NUMBER            
         CH    R1,=H'255'                                                       
         BH    EXITNV                                                           
         STC   R1,R15SEQ#          SEQUENCE NUMBER ENTERED BY USER              
*                                                                               
         L     R5,AIO7              FIND A DETAIL ON DAYPART/DTM                
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVI   MINEKEY,RPRDTELQ                                                 
         MVC   MINEKEY+1(1),R15STACD                                            
         MVC   MINEKEY+2(1),R15DPT                                              
         MVC   MINEKEY+3(1),R15DAY                                              
         MVC   MINEKEY+4(2),R15TIM                                              
         MVC   MINEKEY+6(1),R15SEQ#                                             
         MVC   R15KMEL,MINEKEY                                                  
*                                                                               
         L     R6,MINELEM                                                       
         USING RPRDTELD,R6                                                      
         BAS   RE,MINIOHI                                                       
         BNE   EXITRCNF                                                         
*                                                                               
         CLI   0(R6),RPRDTELQ                                                   
         BNE   EXITRCNF                                                         
         CLC   RPRDTSTA,R15STACD                                                
         BNE   EXITRCNF                                                         
         CLC   RPRDTDPT,R15DPT                                                  
         BNE   EXITRCNF                                                         
         CLC   RPRDTDAY,R15DAY                                                  
         BNE   EXITRCNF                                                         
         CLC   RPRDTTIM,R15TIM                                                  
         BNE   EXITRCNF                                                         
         CLC   RPRDTSEQ,R15SEQ#                                                 
         BNE   EXITRCNF                                                         
         DROP  R6,R5                                                            
*                                                                               
VALSEQX  OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR INVENTORY NUMBER                                              
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
INVDTA   DS    0H                                                               
         LA    RF,INVTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
INVTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISINV)                                 
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY INVENTORY NUMBER                                                      
**********************************************************************          
DISINV   DS    0H                                                               
         MVC   FVIFLD(L'R15INV#),R15INV#                                        
         B     EXITOK                                                           
               EJECT                                                            
*          DATA SET REPRO13    AT LEVEL 068 AS OF 02/08/96                      
***********************************************************************         
* DATA OBJECT FOR EFFECTIVE DATES FIELD                                         
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
EFFDTA   DS    0H                                                               
         LA    RF,EFFTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
EFFTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISEFF)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY EFFECTIVE DATES FIELD                                                 
***********************************************************************         
DISEFF   DS    0H                                                               
         OC    R15EFST,R15EFST                 EFFECTIVE DATE START?            
         BZ    DISEFFX                         NONE                             
*                                                                               
         GOTO1 VDATCON,BODMCB,(8,R15EFST),(17,FVIFLD)                           
*                                                                               
         OC    R15EFEN,R15EFEN                 EFFECTIVE DATE END?              
         BZ    DISEFFX                         NONE                             
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 VDATCON,BODMCB,(8,R15EFEN),(17,FVIFLD+9)                         
*                                                                               
DISEFFX  B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PROGRAM                                                       
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRGDTA   DS    0H                                                               
         LA    RF,PRGTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRGTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRG)                                 
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY PROGRAM                                                               
**********************************************************************          
DISPRG   DS    0H                                                               
         MVC   FVIFLD(L'R15PRGTX),R15PRGTX                                      
         B     EXITOK                                                           
               EJECT                                                            
***********************************************************************         
* DATA OBJECT FOR TEXT NUMBER                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TXTNMDTA DS    0H                                                               
         LA    RF,TXTNMTBL         TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TXTNMTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXTNM)                               
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY TEXT NUMBER                                                           
**********************************************************************          
DISTXTNM DS    0H                                                               
         CLI   CSACT,A#R15LST      ACTION LIST?                                 
         BNE   DISTNX              NO                                           
*                                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         EDIT  TLTXTNUM,(6,FVIFLD),WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT            
*                                                                               
DISTNX   B     EXITOK                                                           
         DROP  R6                                                               
               EJECT                                                            
***********************************************************************         
* DATA OBJECT FOR PRINT OPTION                                                  
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRTDTA   DS    0H                                                               
         LA    RF,PRTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRT)                                 
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRT)                                 
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY PRINT OPTION                                                          
**********************************************************************          
DISPRT   DS    0H                                                               
         CLI   CSACT,A#R15LST      ACTION LIST?                                 
         BNE   DISPRTX             NO                                           
*                                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         MVC   FVIFLD(L'TLPRNT),TLPRNT                                          
         DROP  R6                                                               
*                                                                               
DISPRTX  B     EXITOK                                                           
**********************************************************************          
* VALIDATE PRINT OPTION                                                         
**********************************************************************          
VALPRT   DS    0H                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         CLI   FVILEN,0                                                         
         BNE   *+8                                                              
         MVI   FVIFLD,C'N'                                                      
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BE    VALPRTX                                                          
         CLI   FVIFLD,C'N'                                                      
         BNE   EXITNV                                                           
*                                                                               
VALPRTX  OI    FVIIND,FVIVAL                                                    
         MVC   TLPRNT,FVIFLD                                                    
         B     EXITOK                                                           
         DROP  R6                                                               
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR PRINT OPTION ON SELECT SCREEN                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
PRNTDTA  DS    0H                                                               
         LA    RF,PRNTTBL          TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
PRNTTBL  DC    AL1(DDIS),AL1(0,0,0),AL4(DISPRNT)                                
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALPRNT)                                
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY PRINT OPTION ON SELECT SCREEN                                         
**********************************************************************          
DISPRNT  DS    0H                                                               
         CLI   CSACT,A#R15SEL      ACTION SELECT?                               
         BNE   DISPRNTX            NO                                           
*                                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         MVC   FVIFLD(1),R15PRNT                                                
         DROP  R6                                                               
*                                                                               
DISPRNTX B     EXITOK                                                           
**********************************************************************          
* VALIDATE PRINT OPTION                                                         
**********************************************************************          
VALPRNT  DS    0H                                                               
         CLI   FVILEN,0                                                         
         BNE   *+8                                                              
         MVI   FVIFLD,C'N'                                                      
*                                                                               
         CLI   FVIFLD,C'Y'                                                      
         BE    VALPRNTX                                                         
         CLI   FVIFLD,C'N'                                                      
         BNE   EXITNV                                                           
*                                                                               
VALPRNTX DS    0H                                                               
         MVC   R15PRNT,FVIFLD                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR DESCRIPTION(1ST LINE OF TEXT)                                 
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
DESDTA   DS    0H                                                               
         LA    RF,DESTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
DESTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISDES)                                 
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY DESCRIPTION                                                           
**********************************************************************          
DISDES   DS    0H                                                               
         CLI   CSACT,A#R15LST      ACTION LIST?                                 
         BNE   DISDESX             NO                                           
*                                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         MVC   FVIFLD(L'TLDESC),TLDESC                                          
         DROP  R6                                                               
*                                                                               
DISDESX  B     EXITOK                                                           
               EJECT                                                            
***********************************************************************         
* DATA OBJECT FOR TEXT NUMBER                                                   
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TXTSNDTA DS    0H                                                               
         LA    RF,TXTSNTBL         TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TXTSNTBL DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXTSN)                               
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALTXTSN)                               
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY TEXT NUMBER                                                           
**********************************************************************          
DISTXTSN DS    0H                                                               
         EDIT  R15TXT#,(6,FVIFLD),WRK=BOWORK1,DUB=BODUB1,ALIGN=LEFT             
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
**********************************************************************          
* VALIDATE TEXT NUMBER                                                          
**********************************************************************          
VALTXTSN DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         TM    MISCFLG1,MF1NTRIN                                                
         BNZ   DISTXTSN                                                         
*                                                                               
         CLI   FVILEN,0                                                         
         BE    EXITNO                                                           
*                                                                               
         TM    FVIIND,FVINUM                                                    
         BZ    EXITNOTN                                                         
*                                                                               
         MVC   R15TXT#,BCFULL+2                                                 
*                                                                               
VALTXTSX DS    0H                                                               
         OI    FVIIND,FVIVAL                                                    
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR TEXT                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
TXTDTA   DS    0H                                                               
         LA    RF,TXTTBL           TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
TXTTBL   DC    AL1(DDIS),AL1(0,0,0),AL4(DISTXT)                                 
         DC    AL1(EOT)                                                         
**********************************************************************          
* DISPLAY TEXT                                                                  
**********************************************************************          
DISTXT   DS    0H                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         MVC   FVIFLD(70),TLTXLINE                                              
         DROP  R6                                                               
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
         SPACE 1                                                                
LIST     DS    0H                                                               
         LM    R0,R3,SVPARMS                                                    
         LA    RF,LISTABL                                                       
         B     ITER                                                             
*                                                                               
LISTABL  DS    0H                                                               
         DC    AL1(LINIT),AL1(0,0,1),AL4(INITL)                                 
         DC    AL1(LGETFRST),AL1(0,0,0),AL4(FLST)                               
         DC    AL1(LGETNEXT),AL1(0,0,0),AL4(NLST)                               
         DC    AL1(LTSARFIL),AL1(0,0,0),AL4(EXITOK)                             
         DC    AL1(LTSARDIR),AL1(0,0,1),AL4(TSARDIR)                            
         DC    AL1(LDEFCLM),AL1(0,0,1),AL4(DEFCLM)                              
         DC    AL1(LUPDFRST),AL1(0,0,1),AL4(UPDFRST)                            
         DC    AL1(LUPDLAST),AL1(0,0,1),AL4(UPDLAST)                            
         DC    AL1(LUPDREC),AL1(0,0,1),AL4(UPDREC)                              
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZE LIST                                                               
***********************************************************************         
INITL    DS    0H                                                               
         NI    LSSTAT1,FF-LSSSEL                                                
         NI    LSSTAT2,FF-LSSIUPD                                               
*                                                                               
         CLI   CSACT,A#R15LST      ACTION LIST?                                 
         BNE   INITLX                                                           
*                                                                               
         OI    LSSTAT1,LSSSEL                                                   
         OI    LSSTAT2,LSSIUPD                                                  
         MVI   LSSUBLEN,3          SELECT FIELD LENGTH                          
         NI    LSSTAT2,FF-LSSADD                                                
*                                                                               
INITLX   B     EXITOK                                                           
***********************************************************************         
* BUILD DEFAULT COLUMN LIST                                           *         
***********************************************************************         
DEFCLM   DS    0H                                                               
         CLI   CSACT,A#R15LST      ACTION LIST?                                 
         BE    DEFCLML             YES                                          
*                                                                               
*****************                                                               
** SEL. SCREEN **                                                               
*****************                                                               
         LA    RF,LSFIXCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
         MVC   DCTFLD#,=AL2(11)    TEXT FIELD                                   
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         DROP  RF                                                               
         MVC   LSFIXNUM,=AL2(1)    NUMBER OF FIXED = 1                          
         MVC   LSVARNUM,=AL2(0)    NUMBER OF VARIABLE = 0                       
         B     DEFCLMX                                                          
*****************                                                               
** LIST SCREEN **                                                               
*****************                                                               
DEFCLML  LA    RF,LSFIXCLM         RF = A(1ST FIXED COLUMN)                     
         USING DCTABD,RF                                                        
         MVC   DCTFLD#,=AL2(18)    PRINT OPTION FIELD                           
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    *+8                                                              
         MVI   DCTINDS1,DCTIOPEN    - USER CAN CHANGE                           
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         MVC   DCTFLD#,=AL2(16)    TEXT # FIELD                                 
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         MVC   DCTFLD#,=AL2(17)    DESCRIPTION FIELD(1ST TEXT LINE)             
         LA    RF,DCTABL(RF)                                                    
*                                                                               
         DROP  RF                                                               
         MVC   LSFIXNUM,=AL2(3)    NUMBER OF FIXED = 3                          
         MVC   LSVARNUM,=AL2(0)    NUMBER OF VARIABLE = 0                       
*                                                                               
DEFCLMX  B     EXITOK                                                           
         SPACE 2                                                                
***********************************************************************         
* FIRST FOR LIST -  DO THE FETCH AND ALL THE ADDS HERE                *         
***********************************************************************         
FLST     DS    0H                                                               
         CLC   R15INV#,BCSPACES       INVENTORY #                               
         BNH   EXITL                  DAY/TIME ONLY, NOTHING TO DO              
*                                                                               
FLST0    LA    R2,FETCHBLK                                                      
         LA    R3,L'FETCHBLK                                                    
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R2,RE                                                            
*                                                                               
         LA    R3,FETCHBLK                                                      
         USING RFTBLKD,R3                                                       
         MVC   RFTACOM,ACOM                   A(COMFACS)                        
         MVC   RFTAIO1,AIO1                   A(2K IO AREA)                     
         MVC   RFTAIO2,AIO6                   A(2K IO AREA)                     
         MVC   RFTAWRK,AIO2                   A(6K WORK AREA)                   
*                                              USES AIO2,AIO3, & AIO4           
         LA    RE,FTCHHOOK                                                      
         STCM  RE,15,RFTHOOKA                 HOOK ROUTINE                      
         MVI   RFTAMODE,RFTAMSTQ              FETCH MODE                        
         MVI   RFTCNTL,RFTCTXTQ               DATA FLAGS                        
         MVC   RFTCREP,CUAALF                 REP CODE                          
         MVC   RFTCSTAT,R15STATX              STATION CALL LETTERS              
         CLI   RFTCSTAT+4,C' '                NEED 'T' SET?                     
         BNE   *+8                            NO                                
         MVI   RFTCSTAT+4,C'T'                                                  
         MVI   RFTCSRC,C'N'                   DEMO SOURCE                       
         MVI   RFTCDCTL,RFTCDC1Q              FIRST DAYPART FETCH?              
         MVC   RFTCINV,R15INV#                INVENTORY #                       
         OC    R15EFST,R15EFST                                                  
         BZ    FLST1                                                            
         GOTO1 VDATCON,BODMCB,(8,R15EFST),(2,RFTCEFST)     EFF START &          
FLST1    OC    R15EFEN,R15EFEN                                                  
         BZ    FLST3                                                            
         GOTO1 VDATCON,BODMCB,(8,R15EFEN),(2,RFTCEFEN)     END DATES            
FLST3    DS    0H                                                               
****     MVC   RFTCDTDP,R15DPT                DAYPART                           
         MVC   RFTCDEMS(L'SAVDEMOS),SAVDEMOS                                    
         MVC   RFTCBKS(L'SAVBOOKS),SAVBOOKS                                     
         MVI   RFTCTXTT,RFTCTXIQ              GET INVENTORY TEXT                
*                                                                               
         CLI   CSACT,A#R15LST      ACTION LIST?                                 
         BE    FLSTL               YES                                          
********************************                                                
** BUILD LIST FOR TEXT SELECT **                                                
********************************                                                
         MVI   RFTAMODE,C'T'                  FETCH MODE                        
         MVI   RFTCTXTW,70          TEXT WIDTH                                  
         MVC   RFTCTXT#,R15TXT#    TEXT #                                       
*                                                                               
*********************                                                           
** BUILD TEXT LIST **                                                           
*********************                                                           
FLSTL    XC    IOKEY,IOKEY                                                      
*                                                                               
***      GOTOX ('SAVVAL',AGROUTS)  SAVE TIA - INCASE OF OVERRUN                 
*                                                                               
         GOTO1 VFETCH,BODMCB,FETCHBLK                                           
*                                                                               
***      BAS   RE,RSTVAL           RESTORE TIA - INCASE OF OVERRUN              
*                                                                               
         B     EXITL                                                            
         DROP  R3                                                               
         SPACE 2                                                                
***********************************************************************         
* NEXT FOR LIST                                                       *         
***********************************************************************         
NLST     DS    0H                                                               
NLSTX    B     EXITL               PRETEND NOTHING TO FIND                      
***********************************************************************         
* TSAR DIRECTORY UPDATE                                               *         
***********************************************************************         
         SPACE 1                                                                
TSARDIR  DS    0H                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     R6,SVPARMS3                                                      
         USING RPROKEY,R6                                                       
         L     RF,MINBUFF                                                       
         MVC   RPROKEY(RPROKMEL-RPROKEY),0(RF)                                  
         MVC   RPROKCTL,GSRECSTA                                                
         MVC   RPROKDA,GSRECDA                                                  
         B     EXITOK                                                           
         DROP  R5,R6                                                            
***********************************************************************         
* FIRST FOR UPDATE FROM TSAR                                                    
***********************************************************************         
UPDFRST  DS    0H                                                               
         CLI   CSACT,A#R15LST                                                   
         BNE   EXITOK                                                           
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    EXITOK                                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'R15KMEL),R15KMEL    DETAIL ELEMENT                     
         BAS   RE,MINIORD                    READ IT                            
         BE    *+6                 SOMEONE PASSED A BOGUS DETAIL                
         DC    H'0'                                                             
         BAS   RE,INTOAIO5         FOR RECUP                                    
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
         MVI   ELCODE,RPRITELQ     INVENTORY TEXT ELEMENT                       
         BAS   RE,FIRSTEL                                                       
         BNE   UPDF5               NOTHING TO DELETE                            
*                                                                               
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),(R6)                        
*                                                                               
UPDF5    XC    BOELEM,BOELEM       CREATE SKEL ELEMENT                          
         MVI   BOELEM,RPRITELQ     ELEMENT CODE                                 
         MVI   BOELEM+1,RPRITOVQ   ELEMENT LENGTH                               
*                                                                               
UPDFRSTX B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* UPDATE FILE FROM TSAR RECORD                                                  
* P3 = A (FILE RECORD)   <==  NOT REALLY THE RECORD FOR US                      
* P4 = A (TSAR RECORD)                                                          
***********************************************************************         
UPDREC   DS    0H                                                               
         CLI   CSACT,A#R15LST                                                   
         BNE   EXITOK                                                           
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    EXITOK                                                           
*                                                                               
         L     R2,SVPARMS4                                                      
         USING TLSTD,R2                                                         
*                                                                               
         CLI   TLPRNT,C'Y'                                                      
         BNE   UPDRECX             NOTHING TO DO                                
*                                                                               
         LA    R6,BOELEM                                                        
         ZIC   RE,1(R6)            ELEMENT LENGTH                               
         AR    R6,RE               WHERE THIS # GOES                            
         LA    RE,L'TLTXTNUM(RE)   NEW ELEMENT LENGTH                           
         CH    RE,=H'255'          OH MY, AMBITIOUS USER                        
         BNH   *+6                                                              
         DC    H'0'                                                             
         STC   RE,BOELEM+1                                                      
         MVC   0(L'TLTXTNUM,R6),TLTXTNUM                                        
*                                                                               
UPDRECX  B     EXITOK                                                           
         DROP  R2                                                               
***********************************************************************         
* LAST FOR UPDATE FROM TSAR                                                     
***********************************************************************         
UPDLAST  DS    0H                                                               
         CLI   CSACT,A#R15LST                                                   
         BNE   EXITOK                                                           
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BE    EXITOK                                                           
*                                                                               
         CLI   BOELEM+1,RPRITOVQ   WORTH ADDING?                                
         BNH   UPDL15              NO                                           
*                                                                               
         L     R6,AIO5                                                          
         LA    R6,RCONELEM-RCONKEY(R6)                                          
UPDL5    DS    0H                                                               
         CLI   0(R6),0             END OF CLUSTER?                              
         BE    UPDL10                                                           
         CLI   0(R6),RPRITELQ      DOES THE ELEMENT GO HERE?                    
         BNL   UPDL10                                                           
         ZIC   RE,1(R6)                                                         
         AR    R6,RE                                                            
         B     UPDL5                                                            
*                                                                               
UPDL10   DS    0H                  ADD THE NEW IVENTORY TEXT ELEMENT            
         LA    R0,BOELEM                                                        
         GOTOX (RECUPQ,AREPRO01),BODMCB,(C'R',AIO5),(R0),(R6)                   
*                                                                               
UPDL15   L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(L'R15KMEL),R15KMEL    DETAIL ELEMENT                     
         BAS   RE,MINIORD                    READ IT                            
         BE    *+6                 SOMEONE PASSED A BOGUS DETAIL                
         DC    H'0'                                                             
*                                                                               
         BAS   RE,FROMAIO5                                                      
         BAS   RE,MINIOWRT                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    MISCFLG1,MF1PFRET                                                
         BZ    UPDLASTX                                                         
*                                                                               
         BAS   RE,MINIOCLS                                                      
*                                                                               
UPDLASTX B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* FETCH HOOK - NEED TO ADD THESE TO THE TSAR LIST                     *         
***********************************************************************         
FTCHHOOK NTR1                                                                   
         LA    R3,FETCHBLK                                                      
         USING RFTBLKD,R3                                                       
         CLI   RFTMODE,RFTNTXTQ                                                 
         BNE   FHOOKX                                                           
*                                                                               
         CLI   CSACT,A#R15SEL                                                   
         BNE   FHL0                                                             
*                                                                               
**************************                                                      
** HOOK FOR TEXT SELECT **                                                      
**************************                                                      
         SR    R2,R2                                                            
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         L     R5,RFTFTXTA                                                      
*                                                                               
FHS5     CLM   R2,1,RFTFTXTN                                                    
         BNL   FHOOKX                                                           
*                                                                               
         GOTOX AGENLST,BOPARM,OLIST,LTSARDIR,IOKEY                              
         STCM  R0,15,TLKSRT                                                     
         MVC   TLTXLINE,0(R5)                                                   
         MVC   TLRLEN,=AL2(TSARLN2Q)    MY TSAR RECORD LENGTH                   
         GOTOX AGENLST,BOPARM,OLIST,LTSARADD                                    
         LA    R2,1(R2)                                                         
         LA    R5,132(R5)                                                       
         B     FHS5                                                             
         DROP  R6                                                               
*                                                                               
************************                                                        
** HOOK FOR TEXT LIST **                                                        
************************                                                        
FHL0     L     R6,ATLST                                                         
         GOTOX AGENLST,BOPARM,OLIST,LTSARDIR,IOKEY                              
*                                                                               
         USING TLSTD,R6                                                         
         MVC   TLRLEN,=AL2(TSARLNQ)     MY TSAR RECORD LENGTH                   
         MVC   TLDPT,R15DPT                       DAYPART                       
         MVC   TLINVNUM,R15INV#                   INVENTORY #                   
         MVC   TLTXTNUM,RFTFTXT#                  TEXT #                        
         MVC   TLSRT#,RFTFTXT#                    TEXT #                        
         MVI   TLPRNT,C'N'                        DEFAULT = DON'T PRINT         
         CLI   TWAACCS,C'$'        STATION IS USER                              
         BNE   *+8                                                              
         MVI   TLPRNT,C'?'                                                      
         L     RE,RFTFTX1A                                                      
         MVC   TLDESC,0(RE)                       FIRST TEXT LINE               
*                                                                               
         GOTOX AGENLST,BOPARM,OLIST,LTSARADD                                    
         DROP  R6                                                               
*                                                                               
FHOOKX   B     EXITOK                                                           
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* NTRSES OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
* P3 HOLDS A(BLOCK) COVERED BY SSAVD                                            
***********************************************************************         
NTRSES   LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,NSSTABL                                                       
         B     ITER                                                             
*                                                                               
NSSTABL  DC    AL1(SNTROUT),AL1(0,0,0),AL4(NTROUT)                              
         DC    AL1(SXITOUT),AL1(0,0,0),AL4(NTRXOUT)                             
         DC    AL1(SNTRIN),AL1(0,0,0),AL4(NTRIN)                                
         DC    AL1(SXITIN),AL1(0,0,0),AL4(NTRXIN)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                                      
***********************************************************************         
         PUSH  USING                                                            
         USING FSRRECD,GSRECKEY                                                 
NTROUT   DS    0H                                                               
         CLI   SREC,O#MAX          CHECK FOR CONTROLLER                         
         BNH   EXITOK                                                           
*                                                                               
         OI    SNINDS1,SNIPARMS    SO WE CAN GET DNTR                           
*                                                                               
         XC    SDATA,SDATA                                                      
         MVC   SDSKMEL,R15KMEL                                                  
*                                                                               
         CLI   CSACT,A#R15SEL                                                   
         BE    NTROUTX                                                          
*                                                                               
         L     R6,ATLST                                                         
         USING TLSTD,R6                                                         
         MVC   SDSTXT#,TLTXTNUM                                                 
         MVC   SDSPRNT,TLPRNT                                                   
         DROP  R6                                                               
*                                                                               
NTROUTX  DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* BUILD PARAMETER LIST FOR NTRSES TRANSFER                                      
***********************************************************************         
         PUSH  USING                                                            
         USING FSRRECD,GSRECKEY                                                 
NTRXOUT  DS    0H                                                               
         XC    SDATA,SDATA                                                      
         MVC   SDSPRNT,R15PRNT                                                  
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                                    
***********************************************************************         
         PUSH  USING                                                            
NTRIN    DS    0H                                                               
*                                                                               
         OC    SDSKMEL+2(L'SDSKMEL-2),SDSKMEL+2                                 
         BZ    NTRINX                                                           
*                                                                               
         OI    MISCFLG1,MF1NTRIN                                                
         MVC   R15KMEL,SDSKMEL      DETAIL ELEMENT KEY                          
         MVC   R15TXT#,SDSTXT#                                                  
         MVC   R15PRNT,SDSPRNT                                                  
*                                                                               
NTRINX   B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         OI    MISCFLG1,MF1PFRET                                                
         L     RE,ATLST                                                         
         USING TLSTD,RE                                                         
         MVC   TLPRNT,SDSPRNT                                                   
         DROP  RE                                                               
         B     EXITOK                                                           
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* SCREEN OBJECT                                                                 
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
SCRN     LM    R0,R3,SVPARMS                                                    
         USING SSAVD,R2                                                         
         LA    RF,SCRNTBL                                                       
         B     ITER                                                             
*                                                                               
SCRNTBL  DC    AL1(SSET),AL1(0,0,0),AL4(SETSCR)                                 
         DC    AL1(SKSET),AL1(0,0,0),AL4(SETKSCR)                               
         DC    AL1(SMOD),AL1(0,0,0),AL4(MODSCR)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* SET THE SCREEN CODE                                                           
***********************************************************************         
SETSCR   DS    0H                                                               
*                                                                               
         TM    MISCFLG1,MF1KYCHG                                                
         BNZ   *+12                                                             
         TM    BCINDS1,BCINACT     NEW ACTION THIS TIME                         
         BZ    SETSCRX                                                          
*                                                                               
         CLI   CSACT,A#R15LST      IS THE ACTION 'LIST' ??                      
         BNE   SETSCR10            NO                                           
*                                                                               
         CLI   GSSMCODE,LST#SCR1                                                
         BE    *+12                                                             
         MVI   GSSMCODE,LST#SCR1                                                
         B     SETSCRX                                                          
         MVI   GSSMCODE,LST#SCR2                                                
         B     SETSCRX                                                          
*                                                                               
SETSCR10 CLI   GSSMCODE,SEL#SCR1                                                
         BE    *+12                                                             
         MVI   GSSMCODE,SEL#SCR1                                                
         B     SETSCRX                                                          
         MVI   GSSMCODE,SEL#SCR2                                                
         B     SETSCRX                                                          
*                                                                               
SETSCRX  B     EXITOK                                                           
***********************************************************************         
* SET THE KEY SCREEN CODE                                                       
***********************************************************************         
SETKSCR  DS    0H                                                               
         MVI   GSSKCODE,0                                                       
         CLI   CSACT,A#R15LST                                                   
         BNE   SETKSCRX                                                         
         MVI   GSSKCODE,C'L'       LIST KEY SCREEN                              
*                                                                               
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* MODIFY THE SCREEN FIELDS (PULL OUT THE KEYS FROM AKYFLD)                      
***********************************************************************         
MODSCR   DS    0H                                                               
         L     R5,ATWA             SCREEN                                       
         LA    R5,64(R5)           SKIP HEADER                                  
*                                                                               
MODSCR10 CLI   0(R5),0             END OF SCREEN?                               
         BE    MODSCRX             YES, WE'RE DONE                              
*                                                                               
         TM    1(R5),X'02'         EXTENDED HEADER?                             
         BZ    MODSCRNX            NO, SKIP TO NEXT FIELD                       
*                                                                               
MODSCR20 LR    RF,R5               RF = A(EXTENDED FIELD HDR)                   
         ZIC   R0,0(R5)                                                         
         AR    RF,R0                                                            
         SH    RF,=H'8'                                                         
         USING FVIXHDR,RF                                                       
         ZIC   RE,FVIXUS2          RE = FIELD #                                 
         DROP  RF                                                               
         BCTR  RE,0                MAKE IT ZERO-BASED                           
         LTR   RE,RE                                                            
         BM    MODSCRNX            SKIP FLUFF                                   
*                                                                               
         SLL   RE,2                MULTIPLY BY 4                                
         L     R6,AFDRADDR         START OF FORMATTED RECORD INFO.              
         LA    R6,0(RE,R6)         A(THIS FIELD ENTRY)                          
         L     RF,0(R6)            THIS FIELD ENTRY                             
         USING FDRELD,RF                                                        
         LTR   RF,RF                                                            
         BNZ   *+6                                                              
         DC    H'0'                NO ENTRY FOR THIS FIELD                      
         ZICM  RE,FDRNUM,2                                                      
         DROP  RF                                                               
*                                                                               
         LA    RF,KNOWTAB2                                                      
         USING KNOWTABD,RF                                                      
MODSCR30 CLC   KNOWID,=AL2(EOT)    REACH END OF KEY WANTED LIST?                
         BE    MODSCRNX            YES, CHECK NEXT SCREEN FIELD                 
         CLM   RE,3,KNOWID         IS THIS A WANTED KEY?                        
         BE    MODSCR40            YES                                          
         LA    RF,KNOWLQ(RF)                                                    
         B     MODSCR30                                                         
         DROP  RF                                                               
*                                                                               
MODSCR40 SR    R0,R0               SEE IF WE HAVE DATA FOR THIS FIELD           
         L     RF,AKYFLD                                                        
         ZICM  R1,0(RF),2          R1 = A(AFTER LAST KEY FIELD ENTRY)           
         AR    R1,RF                                                            
         LA    RF,2(RF)            RF = A(1ST ENTRY IN KEY FIELD TBL)           
         USING KEYELD,RF                                                        
MODSCR45 CLI   KEYEL,KEYELQ                                                     
         BNE   MODSCRNX            NO DATA FOR THIS KEY FIELD                   
*                                                                               
         CLM   RE,3,KEYNUM         MATCH ON THIS FIELD?                         
         BE    MODSCR50                                                         
         IC    R0,KEYLN            NO                                           
         AR    RF,R0                                                            
         CR    RF,R1                                                            
         BNL   MODSCRNX                                                         
         B     MODSCR45                                                         
*                                                                               
MODSCR50 ZIC   R1,KEYLN            COPY THE DATA OVER TO THE FIELD              
         SH    R1,=Y(KEYLN1Q+1)                                                 
         BM    MODSCRNX                                                         
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    8(0,R5),8(R5)       IS THERE ANYTHING HERE?                      
         BNZ   MODSCRNX            YES                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   8(0,R5),KEYDATA                                                  
         LA    R1,1(R1)                                                         
         STC   R1,5(R5)                                                         
         OI    6(R5),X'80'         AND TRANSMIT IT                              
         DROP  RF                                                               
*                                                                               
MODSCRNX ZIC   R0,0(R5)            BUMP TO NEXT SCREEN FIELD                    
         AR    R5,R0                                                            
         B     MODSCR10                                                         
*                                                                               
MODSCRX  B     EXITOK                                                           
***********************************************************************         
* TABLE OF WANTED KEY OBJECTS                                                   
***********************************************************************         
KNOWTAB2 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(00002),AL4(FLDPROT)   PROPOSAL                               
         DC    AL2(EOT)                                                         
*                                                                               
FLDPROT  DC    H'0'                DUMMY BRANCH                                 
         EJECT                                                                  
***********************************************************************         
* PFKEY OBJECT                                                                  
* -------------                                                                 
* P1 HOLDS EQUATED OBJECT                                                       
* P2 HOLDS EQUATED VERB                                                         
***********************************************************************         
PFKEY    LM    R0,R3,SVPARMS                                                    
         LA    RF,PFKYTBL                                                       
         B     ITER                                                             
*                                                                               
PFKYTBL  DC    AL1(PFREC),AL1(0,0,0),AL4(RECPFK)                                
         DC    AL1(PFACT),AL1(0,0,0),AL4(ACTPFK)                                
         DC    AL1(PFUSER),AL1(0,0,0),AL4(USRPFK)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* CAN SET THE RECORD FOR THE PFKEY                                              
***********************************************************************         
RECPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   0(RE),7                                                          
         BNL   NOTPFK              DON'T OUTPUT RECORD NAME                     
*                                                                               
RECPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE ACTION FOR THE PFKEY                                              
***********************************************************************         
ACTPFK   L     RE,8(R1)            R2 = A(PFKEY #)                              
*                                                                               
ACTPFKX  B     EXITOK                                                           
***********************************************************************         
* CAN SET THE USER NAME FOR THE PFKEY                                           
***********************************************************************         
USRPFK   DS    0H                                                               
         L     RE,8(R1)            R2 = A(PFKEY #)                              
         CLI   PSREC,R#WORK                                                     
         BNE   USRPFK10                                                         
*                                                                               
         CLI   0(RE),11                                                         
         BNE   USRPFK2                                                          
         CLI   PSACT,A#UPDATE     PREV ACTION WORK/UPDATE?                      
         BNE   NOTPFK              NO                                           
*                                                                               
USRPFK2  DS    0H                                                               
         B     USRPFKX                                                          
*                                                                               
USRPFK10 CLI   0(RE),11                                                         
         BNE   USRPFK12                                                         
         CLI   PSACT,A#R15LST     PREV ACTION LIST?                             
         BNE   NOTPFK              NO                                           
*                                                                               
USRPFK12 DS    0H                                                               
         B     USRPFKX                                                          
USRPFKX  B     EXITOK                                                           
***********************************************************************         
* PFKEY DEFINITION (RECORD, ACTION, OR USER) NOT WANTED                         
***********************************************************************         
NOTPFK   OI    SVPARMS3,X'80'                                                   
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* RESVAL CODE USED TO RESTORE A SAVED TIA PAGE                                  
*          DATA SET REPRO01A   AT LEVEL 120 AS OF 03/07/96                      
***********************************************************************         
RSTVAL   NTR1                                                                   
         ICM   R3,12,=C'L='        RESTORE TEMPSTR PAGE FOR FDRELDS             
         ICM   R3,3,=Y(TWAMAX)                                                  
         GOTO1 VDMGR,BODMCB,=CL8'DMREAD',=CL8'TEMPSTR',(4,0),ATIA,,(R3)         
         BE    *+6                                                              
         DC    H'0'                                                             
RESVAL04 XR    R0,R0                                                            
         ICM   R0,1,GS#FDR                                                      
         BZ    RESVALX                                                          
         L     R1,ATIA             RESTORE FDRBLK                               
         AH    R1,=Y(FDRDISPQ)                                                  
         USING FDRELD,R1                                                        
         L     RE,AFDRADDR                                                      
         XR    RF,RF                                                            
RESVAL06 IC    RF,FDRLN                                                         
         CLI   FDREL,FDRELQ                                                     
         BE    *+12                                                             
         CLI   FDREL,FLTRLQ                                                     
         BNE   *+12                                                             
         ST    R1,0(RE)                                                         
         LA    RE,L'FDRADDR(RE)                                                 
         AR    R1,RF                                                            
         BCT   R0,RESVAL06                                                      
         DROP  R1                                                               
RESVALX  B     EXITOK                                                           
         EJECT                                                                  
*          DATA SET REPRO24    AT LEVEL 174 AS OF 02/26/96                      
***********************************************************************         
* SETS UP AIO5 SO WE CAN USE RECUP TO ADD ELEMENTS, DELETE ELEMENTS, OR         
* CHANGE THE SIZE OF EXISTING ELEMENTS OF THE CLUSTER IN MINELEM                
***********************************************************************         
INTOAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         L     RF,MINBUFF                                                       
         MVC   0(RPROR1ST-RPROKEY,RE),0(RF)                                     
         LA    RE,RPROR1ST-RPROKEY(RE)                CLUSTER GOES HERE         
         LA    RF,IOAREALN-(RPROR1ST-RPROKEY)                                   
*                                                                               
         L     R0,MINELEM          COPY THE ENTIRE CLUSTER                      
         LH    R1,MINELEML                                                      
         MVCL  RE,R0                                                            
*                                                                               
         LH    R1,MINELEML                                                      
         AH    R1,=Y(RPROR1ST-RPROKEY)     L(FAKE RECORD)                       
         L     RE,AIO5             COPY KEY, LEN, STAT, & LINK                  
         STCM  R1,3,RPRORLEN-RPROKEY(RE)                                        
         B     EXITOK                                                           
         DROP  R5                                                               
***********************************************************************         
* SETS UP MINELEM FROM AIO5 BECAUSE WE NEEDED RECUP                             
***********************************************************************         
FROMAIO5 NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         L     RE,AIO5                                                          
         ZICM  RF,RPRORLEN-RPROKEY(RE),2                                        
         SH    RF,=Y(RPROR1ST-RPROKEY)     L'CLUSTER                            
         LA    RE,RPROR1ST-RPROKEY(RE)                                          
*                                                                               
         L     R0,MINELEM                                                       
         LH    R1,MINMAXEL                                                      
         MVCL  R0,RE               COPY CLUSTER                                 
*                                                                               
         B     EXITOK                                                           
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* THIS ROUTINE READS FOR A MINIO ELEMENT                                        
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIORD  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINRD',(R5))                                     
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS HIGH FOR A MINIO ELEMENT.                                  
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINEKEY             MINIO ELEMENT KEY SET BY CALLER              
***********************************************************************         
MINIOHI  NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINHI',(R5))                                     
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         B     EXITL               OTHERWISE RETURN 'NO'                        
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE READS SEQUENTIAL FOR A MINIO ELEMENT.                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOSEQ NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINSEQ',(R5))                                    
         CLI   MINERR,0            RETURN 'YES' IF NO ERRORS                    
         BE    EXITOK                                                           
         CLI   MINERR,MINEEOF      RETURN 'NO' IF END-OF-FILE                   
         BE    EXITL                                                            
         DC    H'0'                DIE ON ANY OTHER ERROR                       
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE WRITES OUT A MINIO ELEMENT.                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOWRT NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,BODMCB,('MINWRT',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE ADDS A MINIO ELEMENT.                                            
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
*              MINELEM             CONTAINS THE MINIO ELEMENT                   
***********************************************************************         
MINIOADD NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         L     RF,MINELEM                                                       
         XC    MINEKEY,MINEKEY                                                  
         MVC   MINEKEY(1),0(RF)                                                 
         MVC   MINEKEY+1(L'RPROKMEL-1),2(RF)                                    
*                                                                               
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,BODMCB,('MINADD',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         CLI   MINERR,MINEDUP      DUPLICATE KEY?                               
         BE    EXITL               YES, RETURN A NO                             
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE DELETES A MINIO ELEMENT.  CALLER IS RESPONSIBLE FOR              
* POINTING TO ELEMENT FIRST.                                                    
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIODEL NTR1                                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         OI    MNIOFLAG,MNIOCLSQ   REMEMBER TO CLOSE MINIO FILE                 
         GOTO1 VMINIO,BODMCB,('MINDEL',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
***********************************************************************         
* THIS ROUTINE CLOSES MINIO AND FLUSHES OUT THE BUFFERS TO THE MINIO            
* RECORDS.                                                                      
*                                                                               
* ON ENTRY:    AIO7                MINIO BLOCK                                  
***********************************************************************         
MINIOCLS NTR1                                                                   
         TM    MNIOFLAG,MNIOCLSQ   DO WE NEED TO?                               
         BZ    EXITOK              NO                                           
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
         GOTO1 VMINIO,BODMCB,('MINCLS',(R5))                                    
         CLI   MINERR,0                                                         
         BE    EXITOK                                                           
         DC    H'0'                DIE ON ANY ERROR                             
         DROP  R5                                                               
         EJECT                                                                  
***********************************************************************         
* EXITS                                                                         
***********************************************************************         
EXITH    CLI   *,0                 SET CC HIGH                                  
         B     EXIT                                                             
EXITL    CLI   *,FF                SET CC LOW                                   
         B     EXIT                                                             
EXITOK   CR    RB,RB               SET CC EQUAL                                 
         SPACE 1                                                                
EXIT     L     R1,CALLR1           RETURN PARAMETERS                            
         MVC   0(L'SVPARMS,R1),SVPARMS                                          
         XIT1  ,                   EXIT WITH CC SET                             
***************                                                                 
* INFO EXITS                                                                    
***************                                                                 
EXITENTR MVC   FVMSGNO,=AL2(GI$ENTER)                                           
         MVI   FVOMTYP,GTMINF                                                   
         B     EXITL               EXIT WITH ENTER DATA                         
***************                                                                 
* ERROR EXITS                                                                   
***************                                                                 
EXITNV   MVC   FVMSGNO,=AL2(FVFNOTV)                                            
         B     EXITL               EXIT WITH FIELD NOT VALID SET                
EXITNO   MVC   FVMSGNO,=AL2(FVFNONE)                                            
         B     EXITL               EXIT WITH FIELD NOT INPUT SET                
EXITNOTN MVC   FVMSGNO,=AL2(FVFNOTN)                                            
         B     EXITL               EXIT WITH FIELD NOT NUMERIC SET              
EXITRCNF MVC   FVMSGNO,=AL2(FVFERNF)                                            
         B     EXITL               EXIT WITH RECORD NOT ON FILE                 
EXITRCDL MVC   FVMSGNO,=AL2(FVFRDEL)                                            
         B     EXITL               EXIT WITH RECORD IS DELETED                  
EXITRCAE MVC   FVMSGNO,=AL2(FVFERAE)                                            
         B     EXITL               EXIT WITH RECORD ALREADY EXISTS              
EXITCRES MVC   FVMSGNO,=AL2(FVFXRES)                                            
         B     EXITL               EXIT WITH RECORD CAN'T BE RESTORED           
INVLUPGD MVC   FVMSGNO,=AL2(INVUPGRD)                                           
         B     EXITL               INVALID UPGRADE EXPRESSION                   
EXITNMOR MVC   FVMSGNO,=AL2(NMOREPRO)                                           
         B     EXITL               NO MORE PROPOSALS FOR THIS CONTRACT          
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
***********************************************************************         
* GETEL                                                                         
***********************************************************************         
         GETEL R6,=AL2(RCONELEM-RCONKEY),ELCODE                                 
*                                                                               
***********************************************************************         
* LITERALS & CONSTANTS                                                          
***********************************************************************         
         LTORG                                                                  
*                                                                               
         SPACE 1                                                                
FF       EQU   X'FF'                                                            
FFFF     EQU   X'FFFF'                                                          
         EJECT                                                                  
***********************************************************************         
* OVERLAY WORKING STORAGE                                                       
***********************************************************************         
OVERWRKD DSECT                                                                  
CALLR1   DS    A                                                                
SVPARMS  DS    0XL24                                                            
SVPARMS1 DS    A                                                                
SVPARMS2 DS    A                                                                
SVPARMS3 DS    A                                                                
SVPARMS4 DS    A                                                                
SVPARMS5 DS    A                                                                
SVPARMS6 DS    A                                                                
AFRREL   DS    A                                                                
AFVADDR  DS    A                                                                
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS                          
MF1KYCHG EQU   X'80'                - KEY FIELD CHANGED                         
MF1PFRET EQU   X'40'                - RETURNING FROM CALLED SESSION             
MF1NTRIN EQU   X'20'                - COMMING IN WITH DATA                      
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MNIOFLAG DS    XL1                 MINIO FLAG                                   
MNIOCLSQ EQU   X'80'               - A CHANGE WAS MADE, CLOSE MINIO             
*                                                                               
ELCODE   DS    XL1                                                              
*                                                                               
SAVBOOKS DS    0XL(7*(3+1+1))      SAVED 3-BYTE BOOK                            
SAVBOOK  DS    7XL(3+1+1)                1-BYTE BOOK SOURCE(I/T/P/4)            
*                                        1-BYTE SURVEY BOOK                     
*                                                                               
SAVDEMOS DS    0CL(7*3)            SAVED 3-BYTE DEMO                            
SAVDEMO  DS    7CL3                                                             
         DS    XL3                                                              
*                                                                               
R15DPT   DS    CL(L'RPRDTDPT)      DETAIL DAYPART                               
R15STACD DS    CL(L'RPRDTSTA)      DETAIL SATION INTERNAL CODE                  
R15STATX DS    CL(L'RPRSTSTA)      DETAIL STATION TEXT                          
R15DAY   DS    CL(L'RPRDTDAY)      DETAIL DAY                                   
R15INV#  DS    CL(L'RPRDTINM)      DETAIL INVETORY NUMBER                       
R15TXT#  DS    XL2                 TEXT RECORD #                                
R15SEQ#  DS    XL1                 DETAIL SEQUENCE #                            
R15STM   DS    CL(L'RPRDTSTM)      DETAIL START TIME                            
R15ETM   DS    CL(L'RPRDTETM)      DETAIL END TIME                              
R15TIM   DS    CL(L'RPRDTTIM)      DETAIL KEY TIME                              
R15PRGCD DS    CL(L'RPRDTPRG)      DETAIL PRORGAM TEXT #                        
R15EFST  DS    CL(L'RPRDTEFF)      DETAIL EFF. START DATE                       
R15EFEN  DS    CL(L'RPRDTEEF)      DETAIL EFF END DATE                          
R15PRGTX DS    CL30                DETAIL PROGRAM TEXT                          
*                                                                               
FETCHBLK DS    CL(RFTBLKL)         FETCH BLOCK                                  
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
         SPACE 1                                                                
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
R15TWRK  EQU   *                                                                
R15KMEL  DS    CL(L'RPROKMEL)      DETAIL ELEMENT KEY                           
R15PRNT  DS    C                                                                
*                                                                               
         SPACE 1                                                                
TLSTD    DSECT                                                                  
         ORG   TLKSRT                                                           
TLSRT#   DS    XL2                 TEXT #                                       
         ORG   TLUSER                                                           
TLDPT    DS    C                   DAYPART                                      
TLTXTNUM DS    XL2                 TEXT #                                       
TLINVNUM DS    CL4                 INVENTORY #                                  
TLPRNT   DS    C                   PRINT OPTION(Y/N)                            
TLDESC   DS    CL70                DESCRIPTION TEXT                             
TSARLNQ  EQU   *-TLSTD             SAR LENGTH EQUATE                            
         SPACE 2                                                                
         ORG   TLUSER                                                           
TLTXLINE DS    CL132                                                            
TSARLN2Q EQU   *-TLSTD             SAR LENGTH EQUATE                            
         SPACE 2                                                                
SSAVD    DSECT                                                                  
         ORG   SDATA               PASSED DATA                                  
SDSKEY   DS    CL6                 MIGHT NEED THIS                              
SDSKMEL  DS    XL(L'RPROKMEL)      DETAIL ELEMENT KEY                           
SDSEFST  DS    CL(L'RPRDTSTM)      DETAIL START TIME                            
SDSEFEN  DS    CL(L'RPRDTETM)      DETAIL END TIME                              
SDSTXT#  DS    XL2                 TEXT #                                       
SDSPRNT  DS    C                   PRINT OPTION                                 
         DS    XL(L'SDATA-(*-SDATA))                                            
         SPACE 2                                                                
*                                                                               
* REFETCHD                                                                      
         PRINT OFF                                                              
       ++INCLUDE REFETCHD                                                       
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FASYSLSTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASYSLSTD                                                      
         PRINT ON                                                               
* DDDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDDDEQUS                                                       
         PRINT ON                                                               
* CTMSGEQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE CTMSGEQUS                                                      
         PRINT ON                                                               
* FASELIST                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASELIST                                                       
         PRINT ON                                                               
* FASYSFAC                                                                      
         PRINT OFF                                                              
       ++INCLUDE FASYSFAC                                                       
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'010REPRO15S  06/25/97'                                      
         END                                                                    
