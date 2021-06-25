*          DATA SET SPSFM96    AT LEVEL 004 AS OF 01/09/08                      
*PHASE T21796A                                                                  
*                                                                               
*INCLUDE DLFLD                                                                  
***********************************************************************         
*                                                                     *         
*  TITLE:        T21796  -- BILLING PERCENTAGE RECORD                 *         
*                                                                     *         
*  INPUTS:       SCREEN SCSFM3A (MAINT) & SCSFM39 (LIST)              *         
*                                                                     *         
*  REGISTERS:    R0 -- WORK                                           *         
*                R1 -- WORK                                           *         
*                R2 -- SCREEN FIELD HEADER                            *         
*                R3 -- WORK                                           *         
*                R4 -- WORK                                           *         
*                R5 -- WORK                                           *         
*                R6 -- GETEL REGISTER                                 *         
*                R7 -- WORK                                           *         
*                R8 -- SPOOL                                          *         
*                R9 -- SYSD                                           *         
*                RA -- TWA                                            *         
*                RB -- FIRST BASE                                     *         
*                RC -- GEND                                           *         
*                RD -- SYSTEM                                         *         
*                RE -- SYSTEM                                         *         
*                RF -- SYSTEM                                         *         
*                                                                     *         
***********************************************************************         
         TITLE 'T21796 - BILLING PERCENTAGE'                                    
T21796   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**1796**,RR=R3                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD          GENERAL PRINT AREAS                          
         USING SPOOLD,R8                                                        
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA      BASE SCREEN FOR SYSTEM + THIS PROG           
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         ST    R3,RELO                                                          
*                                                                               
         BRAS  RE,SETUP                                                         
*                                                                               
         CLI   MODE,VALKEY         VALIDATE RECORD KEY                          
         BE    VK                                                               
         CLI   MODE,SETFILE        SET FILES                                    
         BE    SF                                                               
         CLI   MODE,VALREC         VALIDATE RECORD                              
         BE    VR                                                               
         CLI   MODE,DISPKEY        DISPLAY KEY (FOR LIST)                       
         BE    DK                                                               
         CLI   MODE,DISPREC        DISPLAY RECORD                               
         BE    DR                                                               
         CLI   MODE,LISTRECS       LIST RECORDS                                 
         BE    LR                                                               
         CLI   MODE,PRINTREP       REPORT                                       
         BE    PR                                                               
         CLI   MODE,RECDEL         DELETE                                       
         BE    DELR                                                             
*                                                                               
EQXIT    CR    RB,RB                                                            
         J     XIT                                                              
NEQXIT   LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
***********************************************************************         
*                       SET FILES                                     *         
***********************************************************************         
SF       DS    0H                                                               
         BRAS  RE,SSV                                                           
         B     EQXIT                                                            
*                                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE KEY                                  *         
***********************************************************************         
VK       DS    0H                                                               
         NI    MISCFLG1,X'FF'-MF1KYCHG   ASSUME NO KEY FIELDS CHANGED           
*                                                                               
         BRAS  RE,RSV                                                           
*                                                                               
         XC    SVFLDS(SVFLDSLQ),SVFLDS                                          
*                                                                               
* CLEAR MEDIA, CLIENT, PRODUCT, AND ESTIMATE NAME FIELDS                        
*                                                                               
         MVC   BPCMEDE,SPACES                                                   
         OI    BPCMEDEH+6,X'80'                                                 
         MVC   BPCCLIE,SPACES                                                   
         OI    BPCCLIEH+6,X'80'                                                 
         MVC   BPCPRDE,SPACES                                                   
         OI    BPCPRDEH+6,X'80'                                                 
         MVC   BPCESTE,SPACES                                                   
         OI    BPCESTEH+6,X'80'                                                 
*                                                                               
* VALIDATE MEDIA                                                                
*                                                                               
         LA    R2,BPCMEDSH                                                      
         CLI   5(R2),0                                                          
         BE    ERRMIS                                                           
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALIMED                                                          
         MVC   BPCMEDE,MEDNM                                                    
         OI    BPCMEDEH+6,X'80'                                                 
         MVC   SVAM,BAGYMD                                                      
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
* VALIDATE CLIENT                                                               
*                                                                               
         LA    R2,BPCCLISH                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         BRAS  RE,ACTLRPR                                                       
         BNE   ERRMIS                                                           
         B     VKX                                                              
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         GOTO1 VALICLT                                                          
         MVC   BPCCLIE,CLTNM                                                    
         OI    BPCCLIEH+6,X'80'                                                 
         MVC   SVBCLT,BCLT                                                      
         L     R6,AIO                                                           
         USING CLTHDR,R6                                                        
         MVC   SVCPRF,CPROF        SAVE CLIENT PROFILE                          
         DROP  R6                                                               
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
* VALIDATE PRODUCT                                                              
*                                                                               
         LA    R2,BPCPRDSH                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         BRAS  RE,ACTLRPR                                                       
         BNE   ERRMIS                                                           
         B     VKX                                                              
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIPRD                                                          
         MVI   ERROPT,C'N'                                                      
         CLC   KEY(L'CKEY),KEYSAVE                                              
         BNE   ERRINVP                                                          
*                                                                               
         MVC   SVPRD,QPRD                                                       
         MVC   BPCPRDE,PRDNM                                                    
         OI    BPCPRDEH+6,X'80'                                                 
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
* VALIDATE ESTIMATE                                                             
*                                                                               
         LA    R2,BPCESTSH                                                      
*                                                                               
         CLI   5(R2),0                                                          
         BNE   *+16                                                             
         BRAS  RE,ACTLRPR                                                       
         BNE   ERRMIS                                                           
         B     VKX                                                              
*                                                                               
         TM    4(R2),X'20'         VALIDATED PREVIOUSLY?                        
         BNZ   *+8                                                              
         OI    MISCFLG1,MF1KYCHG   KEY FIELD HAS BEEN CHANGED                   
*                                                                               
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(0,8(R2)),(R5)                                      
         CLI   DMCB,X'00'                                                       
         BNE   ERRINVE                                                          
*                                                                               
         L     RF,4(R1)                                                         
         CHI   RF,100                                                           
         BL    ERRINVE                                                          
         CHI   RF,25500                                                         
         BH    ERRINVE                                                          
*                                                                               
         MVI   ERROPT,C'Y'                                                      
         GOTO1 VALIEST                                                          
         MVI   ERROPT,C'N'                                                      
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BNE   ERRINVE                                                          
*                                                                               
         L     R6,AIO                                                           
         USING ESTHDR,R6                                                        
         MVC   SVESTART,ESTART                                                  
         MVC   SVEEND,EEND                                                      
         DROP  R6                                                               
*                                                                               
         BRAS  RE,FINDDAT                                                       
*                                                                               
         MVC   SVEST,BEST                                                       
         MVC   BPCESTE,ESTNM                                                    
         OI    BPCESTEH+6,X'80'                                                 
         OI    4(R2),X'20'         VALIDATE THE FIELD                           
*                                                                               
VKX      DS    0H                                                               
         BRAS  RE,SSV                                                           
*                                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING BPCKEY,R3                                                        
         MVI   BPCKTYP,BPCKTYQQ                                                 
         MVI   BPCKSUB,BPCKSUBQ                                                 
         MVC   BPCKAM,SVAM                                                      
         MVC   BPCKCLT,SVBCLT                                                   
         MVC   BPCKPRD,SVPRD                                                    
         MVC   BPCKEST,SVEST                                                    
*                                                                               
         CLI   ACTNUM,ACTREP                                                    
         BNE   EQXIT                                                            
         MVC   BPKEY,KEY                                                        
*                                                                               
         B     EQXIT                                                            
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*                       VALIDATE RECORD                               *         
***********************************************************************         
VR       DS    0H                                                               
         CLI   ACTNUM,ACTADD                                                    
         BNE   *+10                                                             
         XC    SVMOSTAB(SVMTBLQ),SVMOSTAB                                       
*                                                                               
         LA    R3,BPCMOSH          A(1ST MONTH FIELD)                           
         LHI   R4,(BPCTAGH-BPCMOSH)/(BPCL2H-BPCMOSH) 12 LINES                   
*                                                                               
VR10     DS    0H                                                               
         LR    R2,R3               A(MONTH FIELD)                               
         CLI   5(R2),0             ANYTHING THERE?                              
         BE    VR100               NO - GO TO NEXT LINE                         
*                                                                               
* VALIDATE MONTH                                                                
*                                                                               
         GOTO1 DATVAL,DMCB,(2,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         BZ    ERRINV                                                           
*                                                                               
* CONVERT MONTH TO BINARY FORMAT                                                
         GOTO1 DATCON,DMCB,(0,WORK),(3,WORK+6)                                  
         MVC   SVMONTH,WORK+6                                                   
*                                                                               
* MAKE SURE THE MONTH IS WITHIN ESTIMATE DATES                                  
*                                                                               
         CLC   WORK(6),SVEDATS                                                  
         BL    *+14                                                             
         CLC   WORK(6),SVEDATS+6                                                
         BNH   VR13                                                             
*                                                                               
* IF MOS OUTSIDE EST RANGE, *AND* % IS 'DEL' - DELETE ELEMENT                   
*                                                                               
         LA    RF,BPCPCTH-BPCMOSH(R3)                                           
         CLI   5(RF),3                                                          
         BNE   ERRDTEST                                                         
         CLC   =C'DEL',8(RF)                                                    
         BNE   ERRDTEST                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PCTELEM,R6                                                       
         MVI   PCTELCD,PCTELQ                                                   
         MVI   PCTLEN,PCTLENQ                                                   
         MVC   PCTMONTH,SVMONTH                                                 
         GOTO1 HELLO,DMCB,(C'D',=C'XSPFIL'),(X'10',AIO),(2,SVMONTH),0           
         B     VR100                                                            
*                                                                               
*                                                                               
* SVMOSTAB HAS LIST OF MONTH ALREADY ON RECORD                                  
* SEARCH THE TABLE FOR THE MONTH CURRENTLY BEING VALIDATED                      
* IF FOUND, SEE WHETHER IT HAS BEEN PROCESSED ALREADY                           
* IF YES - THIS CURRENT ENTRY IS A DUPLICATE                                    
*                                                                               
VR13     DS    0H                                                               
         MVI   ADDING,C'N'                                                      
         XC    AMOSTAB,AMOSTAB                                                  
         LHI   RE,12                                                            
         LA    RF,SVMOSTAB                                                      
         USING SVMOSTBD,RF                                                      
*                                                                               
VR15     DS    0H                                                               
         OC    SVMMOS,SVMMOS                                                    
         BNZ   *+16                                                             
         ST    RF,AMOSTAB          SAVE A(NEXT EMPTY TABLE ENTRY)               
         MVI   ADDING,C'Y'                                                      
         B     VR30                                                             
*                                                                               
         CLC   SVMMOS,WORK+6       SAME MOS AS CURRENT?                         
         BE    *+16                YES - CHECK THE PROC FLAG                    
         LA    RF,SVMNTRLQ(RF)     ADVANCE THE MOS TABLE POINTER                
         BCT   RE,VR15                                                          
         B     ERRINV              TABLE FULL, AND NEW MOS IS VALID             
*                                                                               
         ST    RF,AMOSTAB          SAVE MOSTAB ENTRY                            
         CLI   SVMPROC,C'Y'        PROCESSED ALREADY?                           
         BE    ERRDUP              YES - DUPLICATE ERROR                        
*                                                                               
         DROP  RF                                                               
*                                                                               
* INITIALIZE ELEM                                                               
*                                                                               
VR30     DS    0H                                                               
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING PCTELEM,R6                                                       
         MVI   PCTELCD,PCTELQ                                                   
         MVI   PCTLEN,PCTLENQ                                                   
         MVC   PCTMONTH,SVMONTH                                                 
*                                                                               
* FIND THE ELEMENT FOR THIS MONTH IN THE RECORD                                 
*                                                                               
         MVI   BYTE,C'Y'                                                        
         GOTO1 HELLO,DMCB,(C'G',=C'XSPFIL'),(X'10',AIO),(2,WORK+6),0            
         CLI   DMCB+12,0           ERRORS?                                      
         BNE   *+12                YES - ELEM NOT FOUND, HAVE TO ADD IT         
         L     R6,DMCB+12          NO ERRORS, LOAD A(ELEMENT)                   
         MVI   BYTE,C'N'           INDICATE WE'RE NOT ADDING IT                 
*                                                                               
* CROSS-CHECK THE TABLE SEARCH RESULTS W. HELLO RESULTS                         
*                                                                               
         CLC   ADDING,BYTE                                                      
         BE    *+6                                                              
         DC    H'0'                BAD ERROR:                                   
*                                  EITHER MONTH IS IN TABLE, BUT NOT            
*                                  IN RECORD, OR THE OTHER WAY ROUND            
*                                                                               
* R6 HAS A(ELEMENT) FOR THE MOS CURRENTLY BEING VALIDATED                       
*                                                                               
* VALIDATE PERCENTAGE                                                           
*                                                                               
         XC    SVPCT,SVPCT                                                      
         LA    R2,BPCPCTH-BPCMOSH(R3)                                           
*                                                                               
         LA    R1,8(R2)                                                         
         ZIC   R0,5(R2)                                                         
*                                                                               
         LTR   R0,R0                                                            
         BZ    ERRMIS                                                           
*                                                                               
         CLI   5(R2),3                                                          
         BNE   VR40                                                             
         CLC   =C'DEL',8(R2)                                                    
         BE    VR50                                                             
*                                                                               
VR40     DS    0H                                                               
         ZIC   R5,5(R2)                                                         
         GOTO1 CASHVAL,DMCB,(1,8(R2)),(R5)                                      
         CLI   DMCB,X'00'                                                       
         BNE   ERRINV                                                           
*                                                                               
         L     RF,4(R1)                                                         
*                                                                               
         CHI   RF,0                                                             
         BL    ERRINV                                                           
         CHI   RF,1000                                                          
         BH    ERRINV                                                           
*                                                                               
         STCM  RF,3,SVPCT                                                       
*                                                                               
         OC    SVPCT,SVPCT                                                      
         BNZ   *+8                                                              
         OI    SVPCT,X'80'                                                      
*                                                                               
         CLC   SVPCT,PCTPCT                                                     
         BE    VR100                                                            
*                                                                               
VR50     DS    0H                                                               
         L     RF,AMOSTAB                                                       
         USING SVMOSTBD,RF                                                      
         MVC   SVMMOS,SVMONTH                                                   
         MVC   SVMPCT,SVPCT                                                     
         MVI   SVMPROC,C'Y'                                                     
         DROP  RF                                                               
*                                                                               
         MVC   PCT2PID,PCTCPID                                                  
         MVC   PCT2DATE,PCTCDATE                                                
         MVC   PCT2PPCT,PCTCPPCT                                                
*                                                                               
         MVC   PCTCPID,PCTAPID                                                  
         MVC   PCTCDATE,PCTADATE                                                
         MVC   PCTCPPCT,PCTPCT                                                  
*                                                                               
         MVC   PCTPCT,SVPCT                                                     
         GOTO1 DATCON,DMCB,(5,0),(3,PCTADATE)                                   
         MVC   PCTAPID,TWAORIG                                                  
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         TM    FATFLAG,X'08'       IS PASSWORD PROTECT ACTIVE                   
         BZ    *+10                                                             
         MVC   PCTAPID,FAPASSWD    YES SO USE THIS ID                           
         DROP  R1                                                               
*                                                                               
         CLI   ADDING,C'Y'         ARE WE ADDING NEW ELEM?                      
         BNE   VR100               NO - SKIP THE PUT CALL                       
*                                                                               
         GOTO1 HELLO,DMCB,(C'P',=C'XSPFIL'),AIO,(R6),0                          
         CLI   DMCB+12,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
VR100    DS    0H                                                               
         LA    R3,BPCL2H-BPCMOSH(R3)                                            
         BCT   R4,VR10                                                          
*                                                                               
VRX      DS    0H                                                               
         OI    GENSTAT2,RETEQSEL     PUT IT AND REDISPLAY                       
         B     DR                                                               
*                                                                               
***********************************************************************         
*                       DISPLAY KEY                                   *         
***********************************************************************         
DK       DS    0X                                                               
         L     R3,AIO                                                           
         USING BPCKEY,R3                                                        
*                                                                               
         BRAS  RE,RSV                                                           
         LA    R1,BPCKAM                                                        
         ICM   R1,8,=C'A'                                                       
         BRAS  RE,GETMED                                                        
         MVC   BPCMEDS,BYTE                                                     
         OI    BPCMEDSH+6,X'80'                                                 
*                                                                               
         LR    R1,R3                                                            
         BRAS  RE,DISBCLT                                                       
         MVC   BPCCLIS,WORK                                                     
         OI    BPCCLISH+6,X'80'                                                 
         MVC   BPCCLIE,WORK+3                                                   
         OI    BPCCLIEH+6,X'80'                                                 
*                                                                               
         LR    R1,R3                                                            
         BRAS  RE,DISPRD                                                        
         MVC   BPCPRDS,WORK                                                     
         OI    BPCPRDSH+6,X'80'                                                 
         MVC   BPCPRDE,WORK+3                                                   
         OI    BPCPRDEH+6,X'80'                                                 
*                                                                               
         LR    R1,R3                                                            
         BRAS  RE,DISEST                                                        
         MVC   BPCESTS,WORK                                                     
         OI    BPCESTSH+6,X'80'                                                 
         MVC   BPCESTE,WORK+3                                                   
         OI    BPCESTEH+6,X'80'                                                 
*                                                                               
*                                                                               
DKX      DS    0H                                                               
         BRAS  RE,SSV                                                           
         B     XIT                                                              
         DROP  R3                                                               
*                                                                               
***********************************************************************         
*                       DISPLAY RECORD                                *         
***********************************************************************         
DR       DS    0H                                                               
         BRAS  RE,CLR                                                           
*                                                                               
         XC    SVMOSTAB(SVMTBLQ),SVMOSTAB                                       
         LA    R3,BPCMOSH          A(1ST MONTH FIELD)                           
         LHI   R4,(BPCTAGH-BPCMOSH)/(BPCL2H-BPCMOSH) 12 LINES                   
*                                                                               
         LA    R5,SVMOSTAB                                                      
         USING SVMOSTBD,R5                                                      
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PCTELQ                                                    
         MVC   DATADISP,=AL2(42)                                                
         BRAS  RE,GETEL                                                         
         B     DR20                                                             
*                                                                               
DR10     DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
DR20     DS    0H                                                               
         BNE   DRX                                                              
         CLI   0(R6),PCTELQ                                                     
         BNE   DRX                                                              
*                                                                               
         LR    R2,R3               A(MOS) FIELD                                 
*                                                                               
* UPDATE SVMOSTAB                                                               
*                                                                               
         USING PCTELEM,R6                                                       
         MVC   SVMMOS,PCTMONTH                                                  
         MVC   SVMPCT,PCTPCT                                                    
         MVI   SVMPROC,C'N'                                                     
*                                                                               
* MOS FIELD                                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCTMONTH),(6,8(R2))                               
         OI    6(R2),X'80'                                                      
*                                                                               
* BILLING % FIELD                                                               
*                                                                               
         LA    R2,BPCPCTH-BPCMOSH(R3)                                           
*                                                                               
         MVC   HALF,PCTPCT                                                      
         BRAS  RE,DISPCT                                                        
         MVC   8(5,R2),WORK                                                     
         OI    6(R2),X'80'                                                      
*                                                                               
* LAST CHANGE DATE FIELD                                                        
*                                                                               
         LA    R2,BPCDTCH-BPCMOSH(R3)                                           
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCTADATE),(5,8(R2))                               
*                                                                               
         MVC   17(3,R2),=C'add'                                                 
         OC    PCTCPPCT,PCTCPPCT   LAST VALUE PRESENT?                          
         BZ    *+10                NO                                           
         MVC   17(3,R2),=C'chg'                                                 
*                                                                               
         OI    6(R2),X'80'                                                      
*                                                                               
* ID THAT MADE LAST CHANGE                                                      
*                                                                               
         LA    R2,BPCIDCH-BPCMOSH(R3)                                           
*                                                                               
         MVC   WORK(2),PCTAPID                                                  
         BRAS  RE,FINDPID                                                       
         MVC   8(8,R2),WORK                                                     
         OI    6(R2),X'80'                                                      
*                                                                               
* LAST VALUE                                                                    
*                                                                               
         LA    R2,BPCLVH-BPCMOSH(R3)                                            
*                                                                               
         MVC   HALF,PCTCPPCT                                                    
         BRAS  RE,DISPCT                                                        
         MVC   8(5,R2),WORK                                                     
         OI    6(R2),X'80'                                                      
*                                                                               
         LA    R3,BPCL2H-BPCMOSH(R3)                                            
         LA    R5,SVMNTRLQ(R5)                                                  
*                                                                               
         B     DR10                                                             
         DROP  R6,R5                                                            
*                                                                               
DRX      B     XIT                                                              
         BRAS  RE,SSV                                                           
*                                                                               
***********************************************************************         
*                       LIST RECORDS                                  *         
***********************************************************************         
LR       DS    0H                                                               
         MVI   NLISTS,12                                                        
         LA    R2,BPLTTLE                                                       
         USING LINED,R2                                                         
*                                                                               
         MVC   LCLT,=CL3'Clt'                                                   
         MVC   LPRD,=CL3'Prd'                                                   
         MVC   LEST,=CL3'Est'                                                   
         OI    BPLTTLEH+6,X'80'                                                 
*                                                                               
         LA    R2,BPLUND2                                                       
         MVC   LCLT,=CL3'---'                                                   
         MVC   LPRD,=CL3'---'                                                   
         MVC   LEST,=CL3'---'                                                   
         OI    BPLUND2H+6,X'80'                                                 
         DROP  R2                                                               
*                                                                               
         TM    MISCFLG1,MF1KYCHG   KEY CHANGED?                                 
         BNZ   LR05                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'BPCKEY),BPKEY                                              
         B     LR10                                                             
*                                                                               
* BUILD KEY                                                                     
*                                                                               
LR05     DS    0H                                                               
         XC    KEY,KEY                                                          
         LA    R3,KEY                                                           
         USING BPCKEY,R3                                                        
         MVI   BPCKTYP,BPCKTYQQ                                                 
         MVI   BPCKSUB,BPCKSUBQ                                                 
         MVC   BPCKAM,SVAM                                                      
         MVC   BPCKCLT,SVBCLT                                                   
         MVC   BPCKPRD,SVPRD                                                    
         MVC   BPCKEST,SVEST                                                    
         DROP  R3                                                               
*                                                                               
LR10     DS    0H                                                               
         GOTO1 HIGH                                                             
         B     LR20                                                             
*                                                                               
LR15     GOTO1 SEQ                                                              
*                                                                               
LR20     DS    0H                                                               
         LA    R3,KEY                                                           
         USING BPCKEY,R3                                                        
*                                                                               
         CLI   BPCKTYP,BPCKTYQQ                                                 
         BNE   LRX                 NO                                           
         CLI   BPCKSUB,BPCKSUBQ                                                 
         BNE   LRX                 NO                                           
         CLC   BPCKAM,BPCKAM-BPCKEY+KEYSAVE                                     
         BNE   LRX                 NO                                           
*                                                                               
         MVC   BPKEY,KEY                                                        
*                                                                               
         GOTO1 GETREC                                                           
         XC    LISTAR,LISTAR                                                    
         LA    R2,LISTAR                                                        
         USING LINED,R2                                                         
*                                                                               
         LA    R1,KEY                                                           
         BRAS  RE,DISBCLT                                                       
         MVC   DMDSKADD,BPCKDA                                                  
         MVC   LCLT,WORK                                                        
*                                                                               
         MVC   LPRD,BPCKPRD                                                     
*                                                                               
         EDIT  BPCKEST,(3,LEST)                                                 
         DROP  R2                                                               
*                                                                               
LR100    GOTO1 LISTMON                                                          
         B     LR15                                                             
*                                                                               
LRX      DS    0H                                                               
         NI    BPCMEDSH+4,X'FF'-X'20'  TAKE OFF PREV VALIDATED BIT              
         OI    BPCMEDSH+6,X'80'                                                 
         B     EQXIT                                                            
         LTORG                                                                  
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*                       REPORT                                        *         
***********************************************************************         
PR       DS    0H                                                               
         BRAS  RE,INITDL                                                        
*                                                                               
* SEND PAGE BREAK                                                               
*                                                                               
         GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
* SEND COLUMN HEADINGS                                                          
*                                                                               
         MVC   PMED,=CL3'Med'                                                   
         MVC   PCLT,=CL3'Clt'                                                   
         MVC   PPRD,=CL3'Prd'                                                   
         MVC   PEST,=CL3'Est'                                                   
         MVC   PMOS,=CL3'MOS'                                                   
         MVC   PPCT,=CL5'Pct'                                                   
         MVC   PDATC,=CL8'Chg Date'                                             
         MVC   PPIDC,=CL8'Chg PID'                                              
         MVC   PPCT1,=CL5'Prev%'                                                
         MVC   PDATC1,=CL8'Prev Dt'                                             
         MVC   PPIDC1,=CL8'Prev ID'                                             
         MVC   PPCT2,=CL5'2nd %'                                                
         MVC   PDATC2,=CL8'2nd Date'                                            
         MVC   PPIDC2,=CL8'2nd PID'                                             
*                                                                               
         BRAS  RE,FORMP                                                         
*                                                                               
         MVC   KEY,BPKEY                                                        
         LA    R3,KEY                                                           
         USING BPCKEY,R3                                                        
*                                                                               
PR10     GOTO1 HIGH                                                             
         B     PR20                                                             
*                                                                               
PR15     GOTO1 SEQ                                                              
PR20     CLC   KEY(2),KEYSAVE                                                   
         BNE   PRX                                                              
         CLC   BPCKAM,SVAM                                                      
         BNE   PRX                                                              
*                                                                               
         OC    SVBCLT,SVBCLT                                                    
         BZ    PR30                                                             
         CLC   BPCKCLT,SVBCLT                                                   
         BNE   PRX                                                              
*                                                                               
         OC    SVPRD,SVPRD                                                      
         BZ    PR30                                                             
         CLC   BPCKPRD,SVPRD                                                    
         BNE   PRX                                                              
*                                                                               
PR30     DS    0H                                                               
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,PCTELQ                                                    
         MVC   DATADISP,=AL2(42)                                                
         BRAS  RE,GETEL                                                         
         B     PR60                                                             
*                                                                               
PR50     DS    0H                                                               
         BRAS  RE,NEXTEL                                                        
*                                                                               
PR60     DS    0H                                                               
         BNE   PR15                                                             
         CLI   0(R6),PCTELQ                                                     
         BNE   PR15                                                             
*                                                                               
         USING PCTELEM,R6                                                       
*                                                                               
         MVC   PRTFLDS(PRTFLDLQ),SPACES                                         
*                                                                               
* MEDIA                                                                         
*                                                                               
         LA    R1,KEY+BPCKAM-BPCKEY                                             
         ICM   R1,8,=C'A'                                                       
         BRAS  RE,GETMED                                                        
         MVC   PMED,SPACES                                                      
         MVC   PMED(1),BYTE                                                     
*                                                                               
* CLIENT                                                                        
*                                                                               
         LA    R1,KEY                                                           
         BRAS  RE,DISBCLT                                                       
         MVC   PCLT,WORK                                                        
*                                                                               
* PRODUCT                                                                       
*                                                                               
         MVC   PPRD,BPCKPRD-BPCKEY+KEY                                          
*                                                                               
* ESTIMATE                                                                      
*                                                                               
         EDIT  (B1,BPCKEST-BPCKEY+KEY),(3,PEST)                                 
*                                                                               
* MOS FIELD                                                                     
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCTMONTH),(6,PMOS)                                
*                                                                               
* BILLING % FIELD                                                               
*                                                                               
         MVC   HALF,PCTPCT                                                      
         BRAS  RE,DISPCT                                                        
         MVC   PPCT,WORK                                                        
*                                                                               
* LAST CHANGE DATE FIELD                                                        
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCTADATE),(5,PDATC)                               
*                                                                               
* ID THAT MADE LAST CHANGE                                                      
*                                                                               
         MVC   WORK(2),PCTAPID                                                  
         BRAS  RE,FINDPID                                                       
         MVC   PPIDC,WORK                                                       
*                                                                               
         OC    PCTCDATE,PCTCDATE   WAS THERE A PREV. VALUE?                     
         BZ    PR80                                                             
*                                                                               
* LAST % VALUE                                                                  
*                                                                               
         MVC   HALF,PCTCPPCT                                                    
         BRAS  RE,DISPCT                                                        
         MVC   PPCT1,WORK                                                       
*                                                                               
* ID THAT ENTERED THE LAST % VALUE                                              
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCTCDATE),(5,PDATC1)                              
*                                                                               
* DATE WHEN LAST % VALUE WAS ENTERED                                            
*                                                                               
         MVC   WORK(2),PCTCPID                                                  
         BRAS  RE,FINDPID                                                       
         MVC   PPIDC1,WORK                                                      
*                                                                               
         OC    PCT2DATE,PCT2DATE   WAS THERE 2ND PREV VALUE?                    
         BZ    PR80                                                             
*                                                                               
* NEXT TO LAST % VALUE                                                          
*                                                                               
         MVC   HALF,PCT2PPCT                                                    
         BRAS  RE,DISPCT                                                        
         MVC   PPCT2,WORK                                                       
*                                                                               
* DATE WHEN NEXT TO LAST % VALUE WAS ENTERED                                    
*                                                                               
         GOTO1 DATCON,DMCB,(3,PCT2DATE),(5,PDATC2)                              
*                                                                               
* ID THAT ENTERED THE NEXT TO LAST % VALUE                                      
*                                                                               
         MVC   WORK(2),PCT2PID                                                  
         BRAS  RE,FINDPID                                                       
         MVC   PPIDC2,WORK                                                      
*                                                                               
PR80     DS    0H                                                               
         BRAS  RE,FORMP                                                         
*                                                                               
         B     PR50                                                             
         DROP  R6                                                               
*                                                                               
PR100    DS    0H                                                               
         B     PR15                                                             
*                                                                               
PRX      DS    0H                                                               
         MVC   P,SPACES                                                         
         MVI   DLCB+DLCBACT-DLCBD,C'R'                                          
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
         B     EQXIT                                                            
         LTORG                                                                  
*                                                                               
* AT DELREC CHECK WHETHER THERE WAS ANY BILLING GENERATED                       
*           IF THERE WAS - DO NOT DELETE                                        
*                                                                               
DELR     DS    0H                                                               
         L     R6,AIO                                                           
         MVC   SAVEKEY,0(R6)                                                    
*                                                                               
         LA    R3,SAVEKEY                                                       
         USING BPCKEY,R3                                                        
         XC    KEY,KEY                                                          
         LA    R4,KEY                                                           
         USING BILLREC,R4                                                       
*                                                                               
         MVC   BKEYAM,BPCKAM                                                    
         MVC   BKEYCLT,BPCKCLT                                                  
         MVC   BKEYPRD,BPCKPRD                                                  
         MVC   BKEYEST,BPCKEST                                                  
         MVC   BKEYEST,BPCKEST                                                  
         MVI   BKEYINV+1,X'01'                                                  
*                                                                               
         LA    R2,CONACTH                                                       
         BRAS  RE,RSV                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(BKEYYSRV-BKEY),KEYSAVE                                       
         BNE   DELR20                                                           
         OC    KEY+BKEYYSRV-BKEY(5),KEY+BKEYYSRV-BKEY                           
         BNZ   ERRBILL                                                          
*                                                                               
DELR20   DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         BRAS  RE,SSV                                                           
         GOTO1 HIGH                                                             
         CLC   KEY(L'BPCKEY),KEYSAVE                                            
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*                          ERROR MESSAGES                             *         
***********************************************************************         
*                                                                               
ERRINVP  MVI   ERROR,INVPRD                                                     
         B     VSFMERR                                                          
ERRINVE  MVI   ERROR,INVEST                                                     
         B     VSFMERR                                                          
ERRINV   MVI   ERROR,INVALID                                                    
         B     VSFMERR                                                          
ERRDUP   MVI   ERROR,DUPENTRY                                                   
         B     VSFMERR                                                          
ERRMIS   MVI   ERROR,MISSING                                                    
         B     VSFMERR                                                          
*                                                                               
ERRBILL  MVC   ERRNUM,=AL2(ERRBILLQ)                                            
         B     SPERREX                                                          
ERRDTEST MVC   ERRNUM,=AL2(DTESTERQ)                                            
         B     SPERREX                                                          
ERRNO100 MVC   ERRNUM,=AL2(ERR100Q)                                             
         B     SPERREX                                                          
ERRSEC2  MVC   ERRNUM,=AL2(NOTAUTH)                                             
         B     SPERREX                                                          
*                                                                               
MSGERR   MVI   ERROR,0                                                          
         GOTO1 ERREX2                                                           
*                                                                               
SPERREX  OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
VSFMERR  MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
         DROP  RF                                                               
*                                  SHORT DESP OF ERROR MSGS                     
INVPRD   EQU   63                  NOT AUTHORIZED FOR THIS FUNCTION             
NOTAUTH  EQU   175                 NOT AUTHORIZED FOR THIS FUNCTION             
ESTERR1  EQU   563                 ESTIMATE CODE MUST BE NUMERIC                
ESTERR2  EQU   564                 ESTIMATE CODE BETWEEN 1 - 255                
DELERR   EQU   997                 PRESS ENTER TO CONFIRM DELETE                
DTESTERQ EQU   372                 MOS OUTSIDE ESTIMATE RANGE                   
ERR100Q  EQU   238                 SUM OF PERCENTAGE ELEMENTS NOT 100           
ERRBILLQ EQU   1316                BILLS ON FILE - CAN'T DELETE BPCT            
*                                                                               
*                                                                               
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
SETUP    NTR1  BASE=*,LABEL=*                                                   
         OI    GENSTAT1,USKYMRG+NOSETEFH                                        
         OI    CONSERVH+1,X'01'    MODIFY SERVICE REQUEST                       
         OI    CONSERVH+6,X'80'    TRANSMIT TO GET CONTROL                      
         OI    GENSTAT4,NODELLST   NO DELETION ALLOWED                          
         OI    GENSTAT2,DISTHSPG                                                
         OI    GENSTAT3,OKVALSEL                                                
*                                                                               
         ICM   R0,15,=X'D9000A1D'                                               
         GOTO1 CALLOV,DMCB,0,(R0),0                                             
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   GETBROAD,DMCB                                                    
*                                                                               
         MVC   MAXLINE,=AL2(132)                                                
         MVI   DELIM,C' '         FIELD DELIMITER                               
         MVI   EOTCHR,C'"'        END OF TEXT FIELD DELIMITER                   
         MVI   EOTALT,C''''       END OF TEXT CHR ALTERNATE                     
         MVI   EOLCHR,X'5E'       END OF LINE CHAR - SEMI-COLON                 
         MVI   EORCHR,C':'        END OF REPORT CHR                             
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =         
* R1 EXPECTED TO ADDRESS EITHER A/M BYTE OR MEDIA CODE                          
* R1 HIGH-ORDER BYTE MUST HAVE INPUT TYPE C=CODE A=A/M                          
* ON EXIT, BYTE CONTAINS MEDIA CODE OR MEDIA NIBBLE FOR A/M                     
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =         
GETMED   NTR1  BASE=*,LABEL=*                                                   
         MVC   BYTE,0(R1)          INPUT                                        
*                                                                               
         CLM   R1,8,=C'A'          IS INPUT A/M BYTE?                           
         BNE   *+8                 NO                                           
         NI    BYTE,X'0F'          YES - TURN OFF AGENCY BITS                   
*                                                                               
         LA    R5,GMTAB                                                         
*                                                                               
GM10     DS    0H                                                               
         CLM   R1,8,=C'A'          IS INPUT A/M BYTE?                           
         BNE   GM15                NO                                           
*                                  YES                                          
         CLC   BYTE,1(R5)          COMPARE ON A/M BYTE COLUMN                   
         BE    GM20                                                             
         B     GM18                                                             
*                                                                               
GM15     CLC   BYTE,0(R5)          COMPARE ON MEDIA LETTER COLUMN               
         BE    GM20                                                             
*                                                                               
GM18     LA    R5,GMTABLQ(R5)      ADVANCE TO NEXT LINE IN THE TABLE            
         CLI   0(R5),X'FF'                                                      
         BNE   GM10                                                             
         J     NEQXIT                                                           
*                                                                               
GM20     DS    0H                                                               
         MVC   BYTE,0(R5)          COPY MEDIA CODE                              
         CLM   R1,8,=C'C'          INPUT = A/M BYTE?                            
         BNE   *+10                NO                                           
         MVC   BYTE,1(R5)          COPY A/M NIBBLE                              
*                                                                               
         J     EQXIT                                                            
*                                                                               
GMTAB    DS    0X                                                               
         DC    CL1'T',XL1'01'                                                   
GMTABLQ  EQU   *-GMTAB                                                          
         DC    CL1'R',XL1'02'                                                   
         DC    CL1'N',XL1'03'                                                   
         DC    CL1'X',XL1'04'                                                   
         DC    CL1'C',XL1'08'                                                   
         DC    X'FF'                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
* R1 EXPECTED TO ADDRESS BPCKEY                                                 
* ON EXIT - WORK WILL HAVE THE 3-CHAR CLIENT CODE, WORK+3 - CLT NAME            
***********************************************************************         
DISBCLT  NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,RSV                                                           
         MVC   SAVEKEY,0(R1)                                                    
*                                                                               
         LA    R3,SAVEKEY                                                       
         USING BPCKEY,R3                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(1),BPCKAM                                                  
         MVC   KEY+2(2),BPCKCLT                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   BYTE,CPROF+6-CLTHDR(R1)                                          
         MVC   WORK+3(L'CNAME),CNAME-CLTHDR(R1)                                 
         GOTO1 CLUNPK,DMCB,(BYTE,BPCKCLT),WORK                                  
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         BRAS  RE,SSV                                                           
         GOTO1 HIGH                                                             
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3                                                               
*                                                                               
*                                                                               
***********************************************************************         
* R1 EXPECTED TO ADDRESS BHLNKEY                                                
* ON EXIT - WORK WILL HAVE THE 3-CHAR PRD CODE, WORK+3 - PRD NAME               
***********************************************************************         
DISPRD   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,RSV                                                           
         MVC   SAVEKEY,0(R1)                                                    
*                                                                               
         LA    R3,SAVEKEY                                                       
         USING BPCKEY,R3                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BPCKAM     A/M,CLT                                      
         MVC   KEY+4(3),BPCKPRD    PRODUCT                                      
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
         MVC   WORK(3),PKEYPRD-PKEY(R1)                                         
         MVC   WORK+3(L'PNAME),PNAME-PRDHDR(R1)                                 
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         BRAS  RE,SSV                                                           
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3                                                               
*                                                                               
*                                                                               
***********************************************************************         
* R1 EXPECTED TO ADDRESS BHLNKEY                                                
* ON EXIT - WORK WILL HAVE THE 3-CHAR EST CODE, WORK+3 - EST NAME               
***********************************************************************         
DISEST   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,RSV                                                           
         MVC   SAVEKEY,0(R1)                                                    
         LA    R3,SAVEKEY                                                       
         USING BPCKEY,R3                                                        
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+1(3),BPCKAM                                                  
         MVC   KEY+4(3),BPCKPRD                                                 
         MVC   KEY+7(1),BPCKEST                                                 
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE                                                  
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETREC                                                           
         L     R1,AIO                                                           
*                                                                               
         MVC   SVESTART,ESTART-EKEY(R1)                                         
         MVC   SVEEND,EEND-EKEY(R1)                                             
         BRAS  RE,FINDDAT                                                       
*                                                                               
         ZIC   RF,EKEYEST-EKEY(R1)                                              
         EDIT  (RF),(3,WORK),ALIGN=LEFT                                         
         MVC   WORK+3(L'EDESC),EDESC-ESTHDR(R1)                                 
*                                                                               
         MVC   KEY,SAVEKEY                                                      
         BRAS  RE,SSV                                                           
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
         DROP  R3                                                               
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
* SVESTART, SVEEND EXPECTED TO BE FILLED IN                                     
* ON EXIT - SVEDATS WILL HAVE START, END DATES                                  
*           OF THE START/END BROADCAST MONTHS                                   
***********************************************************************         
FINDDAT  NTR1  BASE=*,LABEL=*                                                   
         OC    SVESTART,SVESTART                                                
         BNZ   *+6                                                              
         DC    H'0'                                                             
         OC    SVEEND,SVEEND                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
*                                                                               
         GOTO1 GETBROAD,DMCB,(1,SVESTART),WORK,GETDAY,ADDAY                     
         MVC   SVEDATS(6),WORK+6                                                
         MVC   SVEDATS+4(2),=C'00'                                              
*                                                                               
         GOTO1 GETBROAD,DMCB,(1,SVEEND),WORK,GETDAY,ADDAY                       
         MVC   SVEDATS+6(6),WORK+6                                              
         MVC   SVEDATS+10(2),=C'00'                                             
*                                                                               
         J     EQXIT                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
*                                                                               
ISALPHA  CLI   0(R1),C'A'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'Z'                                                       
         JH    ISNEQX                                                           
         J     ISEQX                                                            
*                                                                               
ISNUM    CLI   0(R1),C'0'                                                       
         JL    ISNEQX                                                           
         CLI   0(R1),C'9'                                                       
         JH    ISNEQX                                                           
*                                                                               
ISEQX    CR    RB,RB                                                            
         BR    RE                                                               
ISNEQX   LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
SVG      DS    0H                                                               
         ST    R6,SVGETEL                                                       
         MVC   SVGETEL+4(2),DATADISP                                            
         MVC   SVGETEL+6(1),ELCODE                                              
         BR    RE                                                               
*                                                                               
RSTG     DS    0H                                                               
         L     R6,SVGETEL                                                       
         MVC   DATADISP,SVGETEL+4                                               
         MVC   ELCODE,SVGETEL+6                                                 
         BR    RE                                                               
*                                                                               
*                                                                               
********************************************************************            
*                     SET SYSTEM VALUES                            *            
********************************************************************            
SSV      NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO1                                                         
         MVI   USEIONUM,1                                                       
         MVC   LKEY,=H'32'             DETAILS OF DIRECTORY AND KEY             
         MVC   LSTATUS,=H'4'                                                    
         MVC   DATADISP,=H'42'                                                  
         MVC   SYSFIL,=C'XSPFIL  '                                              
         MVC   SYSDIR,=C'XSPDIR  '                                              
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
********************************************************************            
*                   RESET SYSTEM VALUES                            *            
********************************************************************            
RSV      NTR1  BASE=*,LABEL=*                                                   
         MVC   AIO,AIO2                                                         
         MVI   USEIONUM,2                                                       
         MVC   LKEY,=H'13'         DETAILS OF DIRECTORY AND KEY                 
         MVC   LSTATUS,=H'1'                                                    
         MVC   DATADISP,=H'24'     USUALLY SPOTFILE                             
         MVC   SYSFIL,=C'SPTFIL  '                                              
         MVC   SYSDIR,=C'SPTDIR  '                                              
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
*                                                                               
***********************************************************************         
*              CLEAR SCREEN                                                     
***********************************************************************         
CLR     NTR1   BASE=*,LABEL=*                                                   
        LA     R2,BPCMOSH                                                       
*                                                                               
CLR10   CLI    0(R2),0             END OF SCREEN ?                              
        BE     CLRX                DONE                                         
        ZIC    RE,0(R2)                                                         
        SH     RE,=H'8'                                                         
        TM     1(R2),X'02'         EXTENDED HEADER ?                            
        BZ     *+8                 YES, SUBTRACT EXTENSION LEN                  
        SH     RE,=H'8'                                                         
        BCTR   RE,0                                                             
        EX     RE,*+8                                                           
        B      *+10                                                             
        XC     8(0,R2),8(R2)                                                    
        OI     6(R2),X'80'                                                      
*                                                                               
CLR50   ZIC    RE,0(R2)                                                         
        AR     R2,RE                                                            
        B      CLR10                                                            
*                                                                               
CLRX    J      EQXIT                                                            
        LTORG                                                                   
*                                                                               
*                                                                               
***********************************************************************         
*        HEDSPECS                                                     *         
***********************************************************************         
*                                                                               
HDHOOK   NTR1                                                                   
         MVC   H4+7(L'QMED),QMED                                                
         MVC   H4+11(L'MEDNM),MEDNM                                             
         MVC   H4+32(L'QCLT),QCLT                                               
         MVC   H4+38(L'CLTNM),CLTNM                                             
*                                                                               
         LA    R2,H6                                                            
         USING LINED,R2                                                         
*                                                                               
         MVC   LCLT,=CL3'Clt'                                                   
         MVC   LPRD,=CL3'Prd'                                                   
         MVC   LEST,=CL3'Est'                                                   
         MVC   LMOS,=CL3'MOS'                                                   
         MVC   LPCT,=CL6'Bill %'                                                
         MVC   LDATC,=CL12'Change Date'                                         
         MVC   LPIDC,=CL12'Change PID'                                          
*                                                                               
         LA    R2,H7                                                            
*                                                                               
         MVC   LCLT,=CL3'---'                                                   
         MVC   LPRD,=CL3'---'                                                   
         MVC   LEST,=CL3'---'                                                   
         MVC   LMOS,=CL3'---'                                                   
         MVC   LPCT,=CL6'------'                                                
         MVC   LDATC,=CL12'-----------'                                         
         MVC   LPIDC,=CL12'----------'                                          
*                                                                               
HEDX     J     EQXIT                                                            
         DROP  R2                                                               
*                                                                               
*                                                                               
HEDSPECS DS    0H                                                               
         SSPEC H1,1,RUN                                                         
         SSPEC H1,58,REQUESTOR                                                  
         SSPEC H2,58,REPORT                                                     
         SSPEC H2,71,PAGE                                                       
*                                                                               
         SSPEC H4,1,C'Media:'                                                   
         SSPEC H4,25,C'Client:'                                                 
*                                                                               
         DC    X'00'                                                            
*                                                                               
*                                                                               
INITDL   NTR1  BASE=*,LABEL=*                                                   
         XC    HEADHOOK,HEADHOOK                                                
         MVI   FORCEHED,C'Y'                                                    
         MVI   RCSUBPRG,0                                                       
*                                                                               
         MVI   DNFIRST,C'Y'                                                     
         LA    R2,DLCB                                                          
         USING DLCBD,R2                                                         
         OI    DLCBFLG1,DLCBFXTN                                                
         MVC   DLCXTND(7),MAXLINE                                               
         L     RF,=A(DNPRINT)                                                   
         A     RF,RELO                                                          
         ST    RF,DLCBAPR                                                       
         LA    R0,P                                                             
         ST    R0,DLCBAPL                                                       
         MVC   P,SPACES                  JUST IN CASE                           
         MVI   DLCBACT,C'I'              START AND INTIALIZE REPORT             
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
         J     EQXIT                                                            
         LTORG                                                                  
         DROP  R2                                                               
*                                                                               
*                                                                               
DNPRINT  NTR1  BASE=*,LABEL=*                                                   
         MVI   LINE,0                                                           
         MVI   FORCEHED,C'N'                                                    
         GOTO1 SPOOL,DMCB,(R8)                                                  
         MVI   FORCEHED,C'N'                                                    
         XIT1                                                                   
         LTORG                                                                  
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* WORK(2) EXPECTED TO HAVE 2-BYTE SECURITY ID                                   
* ON EXIT WORK WILL CONTAIN 8-CHARACTER PID (IF FOUND)                          
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
FINDPID  NTR1  BASE=*,LABEL=*                                                   
         MVC   SAVEKEY,KEY                                                      
*                                                                               
         GOTO1 GETFACT,DMCB,0                                                   
         L     R1,0(R1)                                                         
         USING FACTSD,R1                                                        
         MVC   WORK+2(2),FATAGYSC   SECURITY AGENCY                             
         DROP  R1                                                               
*                                                                               
         CLC   WORK(2),=X'0FFF'   DDS PID?                                      
         BH    FP30                                                             
         MVC   WORK,SPACES                                                      
         MVC   WORK(3),=C'DDS'                                                  
         B     FPQX                                                             
*                                                                               
* READ PASSWORD RECORD                                                          
*                                                                               
FP30     DS    0H                                                               
         XC    KEY,KEY                                                          
         MVI   KEY,C'0'                                                         
         MVC   KEY+1(2),WORK+2                                                  
         CLC   WORK+2(2),=C'  '    DO WE HAVE SECURITY AGY?                     
         BH    *+10                YES, USE IT INSTEAD OF AGENCY                
         MVC   KEY+1(2),AGENCY                                                  
         MVC   KEY+23(2),WORK      PID                                          
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'CTFILE',KEY,AIO2                  
         L     R6,AIO2                                                          
         CLC   KEY(25),0(R6)                                                    
         BNE   FPNQX                                                            
*                                                                               
         BRAS  RE,SVG                                                           
*                                                                               
         MVI   ELCODE,X'C3'                                                     
         MVC   DATADISP,=AL2(28)                                                
         BRAS  RE,GETEL                                                         
         BNE   FPNQX                    NO NAME ELEMENT                         
         MVC   WORK(8),2(R6)                                                    
*                                                                               
         BRAS  RE,RSTG                                                          
*                                                                               
FPQX     DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         J     EQXIT                                                            
*                                                                               
FPNQX    DS    0H                                                               
         MVC   KEY,SAVEKEY                                                      
         MVC   WORK,SPACES                                                      
         J     NEQXIT                                                           
         LTORG                                                                  
*                                                                               
*                                                                               
FORMP    NTR1  BASE=*,LABEL=*                                                   
         USING DLCBD,R4                                                         
         LA    R4,DLCB                                                          
*                                                                               
         MVC   DLCBFLD(L'PMED),PMED                                             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PCLT),PCLT                                             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PPRD),PPRD                                             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PEST),PEST                                             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PMOS),PMOS                                             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PPCT),PPCT                                             
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PDATC),PDATC                                           
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PPIDC),PPIDC                                           
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PPCT1),PPCT1                                           
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PDATC1),PDATC1                                         
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PPIDC1),PPIDC1                                         
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PPCT2),PPCT2                                           
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PDATC2),PDATC2                                         
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         MVC   DLCBFLD(L'PPIDC2),PPIDC2                                         
         MVI   DLCBTYP,C'T'        TEXT FIELD                                   
         MVI   DLCBACT,DLCBPUT                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
* SEND END OF LINE                                                              
*                                                                               
FORMP20  DS    0H                                                               
         MVI   DLCBACT,DLCBEOL                                                  
         GOTO1 =V(DLFLD),DLCB,RR=RELO                                           
*                                                                               
         J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
* HALF EXPECTED TO HAVE % VALUE                                                 
* NUMBER EDITED OUT TO WORK, LEGTH ASSUMED TO BE 5                              
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *           
DISPCT   NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK,SPACES                                                      
         OC    HALF,HALF                                                        
         BZ    DISPCTX                                                          
         NI    HALF,X'7F'          TURN OFF X'80'                               
         EDIT  HALF,(5,WORK),1,ALIGN=LEFT,ZERO=NOBLANK                          
*                                                                               
DISPCTX  J     EQXIT                                                            
         LTORG                                                                  
*                                                                               
*                                                                               
ACTLRPR  CLI   ACTNUM,ACTLIST                                                   
         BER   RE                                                               
         CLI   ACTNUM,ACTREP                                                    
         BER   RE                                                               
         LTR   RB,RB                                                            
         BR    RE                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        DSECTS                                                       *         
***********************************************************************         
SVMOSTBD DSECT                                                                  
SVMMOS   DS    XL2                                                              
SVMPCT   DS    XL2                                                              
SVMPROC  DS    C                                                                
SVMNTRLQ EQU   *-SVMMOS                                                         
*                                                                               
*                                                                               
* GOES TOGETHER WITH PRTFLDS                                                    
*                                                                               
LINED    DSECT                                                                  
*                                                                               
LCLT     DS    CL3                                                              
         DS    CL1                                                              
LPRD     DS    CL3                                                              
         DS    CL1                                                              
LEST     DS    CL3                                                              
*                                                                               
* FIELDS BELOW ARE USED BY REPORT                                               
*                                                                               
         DS    CL1                                                              
LMOS     DS    CL6                                                              
         DS    CL1                                                              
LPCT     DS    CL6                                                              
         DS    CL1                                                              
LDATC    DS    CL12                                                             
         DS    CL1                                                              
LPIDC    DS    CL12                                                             
         DS    CL1                                                              
LPCT1    DS    CL6                                                              
         DS    CL1                                                              
LDATC1   DS    CL12                                                             
         DS    CL1                                                              
LPIDC1   DS    CL12                                                             
*                                                                               
LINEDLQ  EQU   *-LINED                                                          
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
*        SAVED STORAGE DSECT                                          *         
***********************************************************************         
       ++INCLUDE SPSFMWORKD                                                     
*                                                                               
         ORG   SYSSPARE                                                         
RELO     DS    A                   RELOCATION FACTOR                            
GETBROAD DS    A                                                                
ERRNUM   DS    XL2                                                              
BPKEY    DS    XL32                                                             
SAVEKEY  DS    XL32                                                             
SVMONTH  DS    XL2                                                              
SVPCT    DS    XL2                                                              
AMOSTAB  DS    A                                                                
ADDING   DS    C                                                                
SVGETEL  DS    F                R6+                                             
         DS    XL3              + DATADISP+ELCODE (2ND PART OF SVGETEL)         
*                                                                               
SVFLDS   DS    0X                                                               
SVAM     DS    X                                                                
SVBCLT   DS    XL2                                                              
SVCPRF   DS    CL15                                                             
SVPRD    DS    CL3                                                              
SVEST    DS    X                                                                
SVESTART DS    CL6                                                              
SVEEND   DS    CL6                                                              
SVEDATS  DS    CL12                                                             
SVFLDSLQ EQU   *-SVFLDS                                                         
*                                                                               
MISCFLG1 DS    XL1                                                              
MF1KYCHG EQU   X'80'               A KEY FIELD WAS CHANGED                      
*                                                                               
PRTFLDS  DS    0X                  GOES TOGETHER WITH LINED                     
PMED     DS    CL3                                                              
PCLT     DS    CL3                                                              
PPRD     DS    CL3                                                              
PEST     DS    CL3                                                              
PMOS     DS    CL6                                                              
PPCT     DS    CL5                                                              
PDATC    DS    CL8                                                              
PPIDC    DS    CL8                                                              
PPCT1    DS    CL5                                                              
PDATC1   DS    CL8                                                              
PPIDC1   DS    CL8                                                              
PPCT2    DS    CL5                                                              
PDATC2   DS    CL8                                                              
PPIDC2   DS    CL8                                                              
PRTFLDLQ EQU   *-PRTFLDS                                                        
*                                                                               
MAXLINE  DS    H                                                                
DELIM    DS    C                                                                
EOTCHR   DS    C                                                                
EOTALT   DS    C                                                                
EOLCHR   DS    X                                                                
EORCHR   DS    C                                                                
*                                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
* TABLE OF MONTHS+PERCENTAGES DISPLAYED ON SCREEN                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
SVMOSTAB DS    0X                                                               
         DS    (12*SVMNTRLQ)X                                                   
SVMTBLQ  EQU   *-SVMOSTAB                                                       
*                                                                               
*                                                                               
* DLFLD PARAMETER BLOCK                                                         
*                                                                               
DLCB     DS    XL256                                                            
DNLINE   DS    CL132                                                            
DNLINE2  DS    CL132                                                            
DNLINE3  DS    CL132                                                            
DNFIRST  DS    X                                                                
*                                                                               
*                                                                               
*                                                                               
***********************************************************************         
       ++INCLUDE SPSFMFFD                                                       
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM3AD          MAINTENACE SCREEN                            
*                                                                               
         ORG   CONTAGH                                                          
       ++INCLUDE SCSFM39D          LIST SCREEN                                  
*                                                                               
       ++INCLUDE DDGENTWA                                                       
*                                                                               
       ++INCLUDE SPGENBPCT                                                      
*                                                                               
       ++INCLUDE SPGENCLT                                                       
*                                                                               
       ++INCLUDE SPGENPRD                                                       
*                                                                               
       ++INCLUDE SPGENEST                                                       
*                                                                               
       ++INCLUDE SPGENADV                                                       
*                                                                               
       ++INCLUDE DDFLDIND                                                       
*                                                                               
       ++INCLUDE DDSPOOLD                                                       
*                                                                               
       ++INCLUDE DDSPLWORKD                                                     
*                                                                               
*                                                                               
       ++INCLUDE FAFACTS           FOR FACTSD IN VALACC                         
*                                                                               
       ++INCLUDE SPGENAGY          AGENCY PROFILES                              
*                                                                               
       ++INCLUDE FAGETTXTD         ERROR MSGS                                   
*                                                                               
       ++INCLUDE DDCOMFACS                                                      
*                                                                               
       ++INCLUDE SPGENPGEST                                                     
*                                                                               
       ++INCLUDE DDDLCB                                                         
*                                                                               
       ++INCLUDE SPGENBILL                                                      
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'004SPSFM96   01/09/08'                                      
         END                                                                    
