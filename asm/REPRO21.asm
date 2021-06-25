*          DATA SET REPRO21    AT LEVEL 006 AS OF 03/14/97                      
*&&      SET   NOP=N                                                            
*PHASE T80A21C                                                                  
T80A21   TITLE 'REPRO21 - PROPROSAL RECORDS - COPY ACTION OVERLAY'              
PRO21    CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,REPRO21*,R7,RR=RE                                              
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
*****************************                                                   
** KEY SCREEN CODE EQUATES **                                                   
*****************************                                                   
KYCPY    EQU   C'F'                                                             
KYDEL    EQU   C'E'                                                             
                                                                                
                                                                                
******************************                                                  
** DATA SCREEN CODE EQUATES **                                                  
******************************                                                  
DTCPY    EQU   C'C'                                                             
DTDEL    EQU   C'D'                                                             
DTRES    EQU   C'R'                                                             
                                                                                
                                                                                
A#MCPY   EQU   16                  MY COPY ACTION EQUATE                        
A#MDEL   EQU   17                  MY DELETE ACTION EQUATE                      
A#MRES   EQU   18                  MY RESTORE ACTION EQUATE                     
         EJECT                                                                  
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
         DC    AL1(OACTH),AL1(0,0,0),AL4(EXITH)                                 
         DC    AL1(EOT)                                                         
         SPACE 2                                                                
***********************************************************************         
* INITIALIZATION                                                                
***********************************************************************         
INIT     DS    0H                                                               
         OI    TWASRVH+1,X'01'     SERVICE REQ FLD IS ALWAYS MODIFIED           
         OI    TWASRVH+6,X'80'                                                  
         OI    GCINDS1,GCIPROT             NEVER PROTECT ON NTRSES              
         OI    GSINDSL1,GSINOIO+GSIXKEY    WE'LL DO THE IO'S                    
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
         MVI   CONLINE,0                                                        
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE KEY FIELDS                                              
***********************************************************************         
KFKVAL   DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG   DON'T KNOW IF KEY CHANGED YET          
         MVI   CONLINE,0                                                        
         LA    R0,CONBLKS                                                       
         LA    R1,L'CONBLKS                                                     
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
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
         LA    RE,SRCCON                                                        
         USING PROCOND,RE                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,PROCNUM                                                 
         MVC   RPROKPRO,PROBPRO                                                 
         DROP  R2,RE                                                            
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
RECRDTBL DC    AL1(RFIRST),AL1(0,0,0),AL4(RECFRST)                              
         DC    AL1(RLAST),AL1(0,0,0),AL4(RECLAST)                               
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE CONTROLLER CALLS THE I/O ACTION                                    
***********************************************************************         
RECFRST  DS    0H                                                               
         L     R1,SVPARMS4         R1=INVOKING ACTION                           
         LA    RF,RFTABL           TABLE OF KNOWN INVOKERS                      
         B     ITER                ITERATE TABLE                                
*                                                                               
RFTABL   DS    0H                                                               
         DC    AL1(RWRT),AL1(0,0,0),AL4(RFRWRT)      WRITE                      
         DC    AL1(EOT)                                                         
***********************************************************************         
* BEFORE THE I/O CALL TO WRITE THE RECORD                                       
***********************************************************************         
RFRWRT   DS    0H                                                               
DEST     USING PROCOND,DESTCON                                                  
SRC      USING PROCOND,SRCCON                                                   
*                                                                               
         CLI   CPYOK,C'Y'                                                       
         BNE   RFRWRTX                                                          
*                                                                               
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
*                                                                               
         CLI   CSACT,A#MCPY                                                     
         BNE   RFRW20                                                           
*                                                                               
**************************                                                      
** PROCESS COPY REQUEST **                                                      
**************************                                                      
         XC    MINEKEY,MINEKEY                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R6,MINEKEY                                                       
         USING RPROKEY,R6                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,DEST.PROCNUM                                            
         MVC   RPROKPRO,DEST.PROBPRO                                            
         MVC   IOKEY(L'RPROKEY),RPROKEY                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINCPY',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         ICM   R1,15,=AL4(XOREPDIR+XOHI+XIO4)                                   
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         CLC   IOKEY(L'RPROKMST),IOKEYSAV                                       
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R6,IOKEY                                                         
         USING RPROKEY,R6                                                       
         XC    RPROKEY,RPROKEY                                                  
         MVI   RPROPTYP,RPROPTYQ                                                
         MVI   RPROPSTY,RPROPSBQ                                                
         MVC   RPROPRCD,CUAALF                                                  
         MVC   RPROPSAL,DEST.PROCKSAL                                           
         MVC   RPROPSTA,DEST.PROCSTA                                            
         MVC   RPROPCON,DEST.PROCNUM                                            
         MVC   RPROPPRO,DEST.PROBPRO                                            
         DROP  R6                                                               
*                                                                               
         ICM   R1,15,=AL4(XOREPDIR+XOADD+XIO4)                                  
         GOTOX (XIO,AGROUTS)                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   CPYOK,0                                                          
         B     RFRWRTX                                                          
*                                                                               
RFRW20   CLI   CSACT,A#MDEL                                                     
         BNE   RFRW30                                                           
*                                                                               
****************************                                                    
** PROCESS DELETE REQUEST **                                                    
****************************                                                    
         XC    MINEKEY,MINEKEY                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R6,MINEKEY                                                       
         USING RPROKEY,R6                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,DEST.PROCNUM                                            
         MVC   RPROKPRO,DEST.PROBPRO                                            
         MVC   IOKEY(L'RPROKEY),RPROKEY                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINDLF',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     RFRWRTX                                                          
*                                                                               
RFRW30   DS    0H                                                               
*                                                                               
RFRWRTX  MVI   CPYOK,0                                                          
         B     EXITOK                                                           
         DROP  R5,SRC,DEST                                                      
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
         MVI   CONLINE,0                                                        
         B     EXITOK                                                           
***********************************************************************         
* BEFORE VALIDATING THE DATA FIELDS                                             
***********************************************************************         
DFDVAL   DS    0H                                                               
         MVI   CONLINE,0                                                        
         MVI   CPYOK,0                                                          
         B     EXITOK                                                           
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
         CLI   CSACT,A#MRES                                                     
         BNE   DLDDIS0                                                          
*                                                                               
*****************************                                                   
** PROCESS RESTORE REQUEST **                                                   
*****************************                                                   
         L     R5,AIO7                                                          
         USING MINBLKD,R5                                                       
DEST     USING PROCOND,DESTCON                                                  
SRC      USING PROCOND,SRCCON                                                   
*                                                                               
         XC    MINEKEY,MINEKEY                                                  
         XC    IOKEY,IOKEY                                                      
         LA    R6,MINEKEY                                                       
         USING RPROKEY,R6                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,SRC.PROCNUM                                             
         MVC   RPROKPRO,SRC.PROBPRO                                             
         MVC   IOKEY(L'RPROKEY),RPROKEY                                         
         DROP  R6                                                               
*                                                                               
         GOTO1 VMINIO,BODMCB,('MINRSF',(R5))                                    
         CLI   MINERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
***      MVC   FVADDR,AFVADDR                                                   
         MVC   FVMSGNO,=AL2(83)                                                 
         MVI   FVOMTYP,C'I'                                                     
         B     EXITL                                                            
*                                                                               
         DROP  R5,SRC,DEST                                                      
*                                                                               
*******************                                                             
** OTHER ACTIONS **                                                             
*******************                                                             
DLDDIS0  TM    MISCFLG1,MF1RDIS                                                 
         BNZ   DLDDISX                                                          
*                                                                               
         CLI   CSACT,A#MCPY                                                     
         BNE   DLDDIS2                                                          
*                                                                               
         MVC   FVADDR,AFVADDR                                                   
         MVC   FVMSGNO,=AL2(81)                                                 
         MVI   FVOMTYP,C'I'                                                     
         B     EXITL                                                            
*                                                                               
DLDDIS2  CLI   CSACT,A#MDEL                                                     
         BNE   DLDDISX                                                          
*                                                                               
         MVC   FVADDR,AFVADDR                                                   
         MVC   FVMSGNO,=AL2(82)                                                 
         MVI   FVOMTYP,C'I'                                                     
         B     EXITL                                                            
*                                                                               
DLDDISX  DS    0H                                                               
         NI    MISCFLG1,FF-MF1RDIS                                              
         MVI   CONLINE,0                                                        
         B     EXITOK                                                           
***********************************************************************         
* AFTER VALIDATING THE DATA FIELDS                                              
***********************************************************************         
DLDVAL   DS    0H                                                               
         MVI   CONLINE,0                                                        
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
************ DOING AN ACTION ON A SPECIFIC DATA OBJECT ****************         
***********************************************************************         
DATA10   DS    0H                                                               
         LR    RF,RB               TABLE OF KNOWN OBJECTS                       
         AH    RF,=Y(KNOWTAB-PRO21)                                             
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
CONTBL   DC    AL1(DVAL),AL1(0,0,0),AL4(VALCON)                                 
         DC    AL1(DDIS),AL1(0,0,0),AL4(DISCON)                                 
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A CONTRACT FIELD                                                      
***********************************************************************         
DISCON   DS    0H                                                               
         TM    MISCFLG1,MF1LDIS                                                 
         BO    DISCONX                                                          
*                                                                               
         USING RPROKMST,R2                                                      
         ZAP   BOWORK1+20(5),=P'99999999' EDIT USES 17 BYTES OF WORK            
         ZAP   BOWORK1+10(5),=P'0'                                              
         MVO   BOWORK1+10(5),RPROKCON                                           
         SP    BOWORK1+20(5),BOWORK1+10(5)                                      
         DROP  R2                                                               
         EDIT  (P5,BOWORK1+20),(8,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,      X        
               DUB=BODUB1                                                       
*                                                                               
DISCONX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE A CONTRACT FIELD                                                     
***********************************************************************         
VALCON   DS    0H                                                               
         CLI   TWAACCS,C'$'        STATION IS USER?                             
         BE    EXITSLCK            YES                                          
*                                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         ZIC   R1,CONLINE                                                       
         LA    R0,1(R1)                                                         
         STC   R0,CONLINE                                                       
*                                                                               
         LA    R0,CCONNUM                                                       
         LH    R1,=Y((ADDRCDA+100*4)-CCONNUM)                                   
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         CLI   FVILEN,0            THIS FIELD IS REQUIRED                       
         BE    EXITNO                                                           
*                                                                               
         GOTOX (VALCONQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
         CLI   CONLINE,2                                                        
         BNE   VALCONX                                                          
*                                                                               
SRC      USING PROCOND,SRCCON                                                   
         CLC   SRC.PROCSTA,CCONKSTA        SAME STATION?                        
         BE    *+14                                                             
         MVC   FVMSGNO,=AL2(596)                                                
         B     EXITL                                                            
         DROP  SRC                                                              
*                                                                               
         OI    FVIIND,FVIVAL       VALIDATED                                    
VALCONX  B     EXITOK                                                           
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY A PROPOSAL FIELD                                                      
***********************************************************************         
DISPRO   DS    0H                                                               
         TM    MISCFLG1,MF1LDIS                                                 
         BO    DISPROX                                                          
*                                                                               
         USING RPROKEY,R2                                                       
         ZIC   RE,RPROKPRO                                                      
         LA    R0,X'FF'                                                         
         SR    R0,RE                                                            
         EDIT  (R0),(3,FVIFLD),ALIGN=LEFT,WRK=BOWORK1,DUB=BODUB1                
         DROP  R2                                                               
         OI    MISCFLG1,MF1LDIS                                                 
DISPROX  B     EXITOK                                                           
***********************************************************************         
* VALIDATE A PROPOSAL FIELD                                                     
***********************************************************************         
VALPRO   DS    0H                                                               
         TM    FVIIND,FVIVAL       PREVIOUSLY VALIDATED?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   NO, THIS KEY FIELD WAS CHANGED               
*                                                                               
         CLI   CONLINE,1                                                        
         BE    VALPRO50                                                         
*                                                                               
*****************************                                                   
** DESTINATION  VALIDATION **                                                   
*****************************                                                   
         XC    IOKEY,IOKEY                                                      
         LA    R2,IOKEY                                                         
         USING RPROKEY,R2                                                       
         MVI   RPROKTYP,RPROKTYQ                                                
         MVI   RPROKSTY,RPROKSBQ                                                
         MVC   RPROKRCD,CUAALF                                                  
         MVC   RPROKCON,CCONNUM                                                 
*                                                                               
         L     R1,=AL4(XOREPDIR+XOHID+XIO4)                                     
         GOTOX (XIO,AGROUTS)                                                    
*                                                                               
         CLC   IOKEY(RPROKPRO-RPROKEY),IOKEYSAV   SAME UPTO PROPOSAL #?         
         BE    VALPRO10                           YES                           
         LA    R1,X'FF'                                                         
         B     VALPRO15                                                         
*                                                                               
VALPRO10 ZIC   R1,RPROKPRO                                                      
         CLI   RPROKPRO,0                                                       
         BE    EXITNMOR            NO MORE PROPOSAL FOR THIS CONTRACT           
*                                                                               
VALPRO15 BCTR  R1,0                                                             
         STC   R1,BPRONUM                                                       
*                                                                               
         STC   R1,BOBYTE1                                                       
         XI    BOBYTE1,X'FF'                                                    
*                                                                               
         CLI   FVILEN,0            USER INPUT?                                  
         BE    VALPRO20            NO - OUTPUT THE NEXT AVAIL. #                
*                                                                               
         CLC   BOBYTE1,BCFULL+3    IS THE INPUT < THE NEXT PRO #                
         BH    EXITRCAE            YES                                          
*                                                                               
         CLC   BOBYTE1,BCFULL+3    IS THE INPUT THE NEXT PRO #                  
         BNE   EXITNV              NO                                           
         B     VALPRO49                                                         
*                                                                               
VALPRO20 EDIT  (B1,BOBYTE1),(3,FVIFLD),WRK=BOWORK1,DUB=BODUB1,         X        
               ALIGN=LEFT                                                       
         STC   R0,FVILEN                                                        
         SH    R0,=H'1'                                                         
         STC   R0,FVXLEN                                                        
         OI    FVIIND,FVINUM                                                    
         DROP  R2                                                               
*                                                                               
VALPRO49 LA    R1,SVPARMS          RESET BECAUSE WE USED R1                     
         OI    FVIIND,FVIVAL       VALIDATED                                    
         MVC   BPRONUM,BOBYTE1                                                  
         XI    BPRONUM,X'FF'                                                    
*                                                                               
         LR    RE,RA               CLEAR SAVED PROPOSAL #                       
         AH    RE,=Y(SVPRONUM-TWAD)                                             
         XC    0(L'SVPRONUM,RE),0(RE)                                           
         B     VALPROX                                                          
*                                                                               
************************                                                        
** SOURCE VALIDATION **                                                         
************************                                                        
VALPRO50 CLI   FVILEN,0            ANY DATA IN THIS FIELD?                      
         BE    EXITNO              NO, NEED A NUMBER                            
*                                                                               
         GOTOX (VALPROQ,AREPRO01),BOPARM                                        
         BL    EXITL                                                            
*                                                                               
VALPROX  OI    FVIIND,FVIVAL       VALIDATED                                    
         B     SVCONINF                                                         
***********************************************************************         
* SAVE PROPOSALS CONTRACT INFORMATION                                           
***********************************************************************         
SVCONINF DS    0H                                                               
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   PROCNUM,CCONNUM                                                  
         MVC   PROBPRO,BPRONUM                                                  
         MVC   PROCSAL,ESALNAME                                                 
         MVC   PROCKSAL,CCONSAL                                                 
         MVC   PROCSTA,CCONKSTA                                                 
         MVC   PROCDAT,CCONDAT                                                  
         MVC   PROCDVS,EDVSNAME                                                 
         MVC   PROCDVT,CCONDVT                                                  
         MVC   PROCAGY,EAGYNAM1                                                 
         MVC   PROCADV,EADVNAME                                                 
         MVC   PROCPRD,EPRDNAME                                                 
         MVC   PROCBYR,ECONBUYR                                                 
         DROP  RE                                                               
*                                                                               
         B     EXITOK                                                           
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
         ZIC   RE,CONLINE                                                       
         LA    RE,1(RE)                                                         
         STC   RE,CONLINE                                                       
*                                                                               
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCAGY),PROCAGY                                        
         DROP  RE                                                               
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
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCADV),PROCADV                                        
         DROP  RE                                                               
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
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCPRD),PROCPRD                                        
         DROP  RE                                                               
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
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCSAL),PROCSAL                                        
         DROP  RE                                                               
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
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCBYR),PROCBYR                                        
         DROP  RE                                                               
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
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY STATION FIELD                                                         
***********************************************************************         
DISSTA   DS    0H                                                               
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCSTA),PROCSTA                                        
         DROP  RE                                                               
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
         ZIC   R3,CONLINE                                                       
         BCTR  R3,0                                                             
         MH    R3,=Y(PROCONLQ)                                                  
         LA    R3,CONBLKS(R3)                                                   
         USING PROCOND,R3                                                       
*                                                                               
         GOTO1 VDATCON,BODMCB,(3,PROCDAT),(5,FVIFLD)                            
         MVI   FVIFLD+8,C'-'                                                    
         GOTO1 (RF),(R1),(3,PROCDAT+3),(5,FVIFLD+9)                             
         DROP  R3                                                               
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
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCDVS),PROCDVS                                        
         DROP  RE                                                               
         B     EXITOK                                                           
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
         ZIC   RE,CONLINE                                                       
         BCTR  RE,0                                                             
         MH    RE,=Y(PROCONLQ)                                                  
         LA    RE,CONBLKS(RE)                                                   
         USING PROCOND,RE                                                       
*                                                                               
         MVC   FVIFLD(L'PROCDVT),PROCDVT                                        
         DROP  RE                                                               
         B     EXITOK                                                           
         EJECT                                                                  
***********************************************************************         
* DATA OBJECT FOR OKAY                                                          
*                                                                               
* R1 HOLDS EQUATED VERB                                                         
* R2 HOLDS A(RECORD AT CORRECT LEVEL)                                           
***********************************************************************         
OKDTA    DS    0H                                                               
         LA    RF,OKTBL            TABLE OF KNOWN VERBS                         
         B     ITER                ITERATE TABLE                                
*                                                                               
OKTBL    DC    AL1(DDIS),AL1(0,0,0),AL4(DISOK)                                  
         DC    AL1(DVAL),AL1(0,0,0),AL4(VALOK)                                  
         DC    AL1(EOT)                                                         
***********************************************************************         
* DISPLAY OKAY FIELD                                                            
***********************************************************************         
DISOK    DS    0H                                                               
         MVC   AFVADDR,FVADDR                                                   
         MVI   FVIFLD,C'N'                                                      
         CLI   CPYOK,0                                                          
         BE    *+8                                                              
         MVI   FVIFLD,C'Y'                                                      
         B     EXITOK                                                           
***********************************************************************         
* VALIDATE OKAY FIELD                                                           
***********************************************************************         
VALOK    DS    0H                                                               
         CLI   FVIFLD,C'N'                                                      
         BE    VALOKX                                                           
         CLI   FVIFLD,C'Y'                                                      
         BNE   EXITNV              INVALID IF NEITHER Y/N                       
         MVI   CPYOK,C'Y'                                                       
*                                                                               
VALOKX   DS    0H                                                               
         OI    MISCFLG1,MF1RDIS                                                 
         B     EXITOK                                                           
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
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS PARAMETER LIST FOR NTRSES TRANSFER                                    
***********************************************************************         
         PUSH  USING                                                            
NTRIN    DS    0H                                                               
         B     EXITOK                                                           
         POP   USING                                                            
***********************************************************************         
* PROCESS COMIMG BACK FROM CALLED SESSION                                       
***********************************************************************         
         PUSH  USING                                                            
NTRXIN   DS    0H                                                               
         OI    MISCFLG1,MF1PFRET                                                
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
* SET THE KEY SCREEN CODE                                                       
***********************************************************************         
SETKSCR  DS    0H                                                               
         MVI   GSSKCODE,KYCPY      SET TO COPY SCREEN CODE                      
         CLI   CSACT,A#MCPY                                                     
         BE    SETKSCRX                                                         
*                                                                               
         MVI   GSSKCODE,KYDEL      OTHERWISE SET TO DELETE SCREEN CODE          
*                                                                               
SETKSCRX B     EXITOK                                                           
***********************************************************************         
* SET THE DATA SCREEN CODE                                                      
***********************************************************************         
SETSCR   DS    0H                                                               
         MVI   GSSMCODE,DTCPY      SET TO COPY SCREEN CODE                      
         CLI   CSACT,A#MCPY                                                     
         BE    SETSCRX                                                          
*                                                                               
         MVI   GSSMCODE,DTDEL      SET TO DELETE SCREEN CODE                    
         CLI   CSACT,A#MDEL                                                     
         BE    SETSCRX                                                          
*                                                                               
         MVI   GSSMCODE,DTRES      OTHERWISE SET TO RESTORE SCREEN CODE         
*                                                                               
SETSCRX  B     EXITOK                                                           
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
         EJECT                                                                  
***********************************************************************         
* TABLE OF WANTED KEY OBJECTS                                                   
***********************************************************************         
KNOWTAB2 DS    0XL(KNOWLQ)                                                      
         DC    AL2(00001),AL4(FLDPROT)   CONTRACT                               
         DC    AL2(00002),AL4(FLDPROT)   PROPOSAL                               
         DC    AL2(EOT)                                                         
FLDPROT  DC    H'0'                DUMMY BRANCH                                 
*                                                                               
         EJECT                                                                  
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
EXITIOBK MVC   FVMSGNO,=AL2(INVOVBOK)                                           
         B     EXITL               INVALID OVERRIDE BOOK                        
EXITIBKE MVC   FVMSGNO,=Y(INVBKEXP)                                             
         B     EXITL               INVALID BOOK EXPRESSION                      
EXITNMOR MVC   FVMSGNO,=AL2(NMOREPRO)                                           
         B     EXITL               NO MORE PROPOSALS FOR THIS CONTRACT          
EXITDUPL MVC   FVMSGNO,=AL2(401)                                                
         B     EXITL               DUPLICATE ENTRY NOT ALLOWED                  
EXITSLCK MVC   FVMSGNO,=AL2(GE$SLOCK)                                           
         B     EXITL               EXIT WITH SECURITY LOCKOUT                   
*                                                                               
DIE      DC    H'0'                                                             
         EJECT                                                                  
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
* TABLE OF KNOWN DATA OBJECTS                                                   
***********************************************************************         
KNOWTAB  DS    0XL(KNOWLQ)                                                      
* KEY PORTION                                                                   
         DC    AL2(00001),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00002),AL4(PRODTA)    PROPOSAL                               
* SECOND KEY PORTION FOR PRO/COPY                                               
         DC    AL2(00071),AL4(CONDTA)    CONTRACT                               
         DC    AL2(00072),AL4(PRODTA)    PROPOSAL                               
* KEY (PROTECTED PORTION)                                                       
         DC    AL2(01002),AL4(AGYDTA)    AGENCY                                 
         DC    AL2(01003),AL4(ADVDTA)    ADVERTISER                             
         DC    AL2(01004),AL4(PRDDTA)    PRODUCT                                
         DC    AL2(01005),AL4(SALDTA)    SALESPERSON                            
         DC    AL2(01006),AL4(DVSDTA)    DEV SALESPERSON                        
         DC    AL2(01007),AL4(DVTDTA)    DEV CONTRACT TYPE                      
         DC    AL2(00007),AL4(STADTA)    STATION                                
         DC    AL2(01008),AL4(BYRDTA)    BUYER                                  
         DC    AL2(01009),AL4(FLTDTA)    FLIGHT DATES                           
* RECORD (INPUT PORTION)                                                        
         DC    AL2(00022),AL4(OKDTA)     OKAY FIELD                             
         DC    AL2(EOT)                                                         
*                                                                               
KNOWTABD DSECT                                                                  
KNOWID   DS    XL2                 IDENTIFIER                                   
KNOWADD  DS    AL4                 A(OBJECT)                                    
KNOWLQ   EQU   *-KNOWTABD                                                       
*                                                                               
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
MF1RDIS  EQU   X'20'                - REDISPLAY AFTER CONFIRM                   
MF1LDIS  EQU   X'10'                - DISPLAY FROM LIST COMPLETED               
MF1TMPBT EQU   X'01'                - TEMPORARY BIT (USED BY ANYONE)            
*                                                                               
MNIOFLAG DS    XL1                 MINIO FLAG                                   
MNIOCLSQ EQU   X'80'               - A CHANGE WAS MADE, CLOSE MINIO             
*                                                                               
CPYOK    DS    CL1                                                              
*                                                                               
CONLINE  DS    XL1                 WHICH CONTRACT LINE                          
*                                                                               
CONBLKS  DS    0CL(2*PROCONLQ)                                                  
SRCCON   DS    CL(PROCONLQ)                                                     
DESTCON  DS    CL(PROCONLQ)                                                     
*                                                                               
         EJECT                                                                  
PROCOND  DSECT                     PROPOSALS CONTRACT DATA                      
PROCNUM  DS    XL(L'CCONNUM)                                                    
PROBPRO  DS    XL(L'BPRONUM)                                                    
PROCSAL  DS    CL(L'ESALNAME)                                                   
PROCKSAL DS    CL(L'CCONSAL)                                                    
PROCSTA  DS    CL(L'CCONKSTA)                                                   
PROCDAT  DS    XL(L'CCONDAT)                                                    
PROCDVS  DS    CL(L'EDVSNAME)                                                   
PROCDVT  DS    CL(L'CCONDVT)                                                    
PROCAGY  DS    CL(L'EAGYNAM1)                                                   
PROCADV  DS    CL(L'EADVNAME)                                                   
PROCPRD  DS    CL(L'EPRDNAME)                                                   
PROCBYR  DS    CL(L'ECONBUYR)                                                   
PROCONLQ EQU   *-PROCOND                                                        
         EJECT                                                                  
* REPROWORK                                                                     
         PRINT OFF                                                              
       ++INCLUDE REPROWORK                                                      
         PRINT ON                                                               
TWAD     DSECT                                                                  
         ORG   TWUSER                                                           
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
**PAN#1  DC    CL21'006REPRO21   03/14/97'                                      
         END                                                                    
