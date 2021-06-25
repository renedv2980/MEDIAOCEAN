*          DATA SET ACCLB43S   AT LEVEL 009 AS OF 12/21/99                      
*PHASE T62143A                                                                  
CLB43    TITLE '- BILL PROGRAM - TRANSFER TMS'                                  
                                                                                
***********************************************************************         
* ROUTINE TO TRANSFER ON TMS                                          *         
*                                                                     *         
* NTRY: P1 = A(TRANSACTION RECORD)                                    *         
*       P2 = A(TRANSFER PTA ELEMENT)                                  *         
*       P3 = A(POSTVALS BLOCK)                                        *         
***********************************************************************         
                                                                                
CLB43    CSECT                                                                  
         PRINT NOGEN                                                            
XFRTMS   NMOD1 XTWORKL,**CB43**,CLEAR=YES,R7                                    
         USING WORKD,R9                                                         
         USING TWAD,RA                                                          
         USING XTWORKD,RC                                                       
         MVC   XTPARMS,0(R1)                                                    
         L     R8,XTPAPOST                                                      
         USING POSTVALS,R8         R8=A(POSTVALS BLOCK)                         
*                                                                               
         TM    BCCPYST7,CPYSTMSY   TEST USING TMS                               
         BZ    EXITL                                                            
         L     RE,XTPATRN                                                       
         MVC   XTTRNKEY,0(RE)      COPY SJ/1R TRANSACTION KEY & ELEMENT         
         MVC   XTTRNEL,TRNRFST-TRNRECD(RE)                                      
         USING TRNKEY,XTTRNKEY                                                  
         USING TRNELD,XTTRNEL                                                   
         CLC   UL1R,TRNKCUNT                                                    
         BNE   EXITL                                                            
*&&US                                                                           
         CLI   TRNKREF,C'T'                                                     
         BNE   EXITL                                                            
*&&                                                                             
*                                                                               
         MVC   XTOFFICE,BCSPACES                                                
         OC    XTOFFICE,CSOFFICE                                                
         XC    XTLINE#,XTLINE#                                                  
         ZAP   XTRATE,BCPZERO                                                   
         MVC   XTINCAC,BCSPACES                                                 
         MVC   XTNARR,BCSPACES                                                  
         XC    XTPIDEL,XTPIDEL                                                  
         XC    XTWOFMOA,XTWOFMOA                                                
         SR    RF,RF                                                            
         IC    RF,(TRNRFST-TRNRECD)+(TRNLN-TRNELD)(RE)                          
         SH    RF,=Y(TRNLN1Q)                                                   
         BZ    XTMS01                                                           
         BCTR  RF,0                                                             
         EX    RF,*+4                                                           
         MVC   XTNARR(0),(TRNRFST-TRNRECD)+(TRNNARR-TRNELD)(RE)                 
XTMS01   MVC   XT1CACT,BCCMPPRF+(PPRCOSTA-PPRELD)                               
         L     R3,XTPATRN                                                       
         LA    R3,TRNRFST-TRNRECD(R3)                                           
         XR    RF,RF                                                            
XTMS02   CLI   0(R3),0                                                          
         BE    XTMS20                                                           
         IC    RF,1(R3)                                                         
         USING SPAELD,R3           EXTRACT CLIENT COSITNG ACCOUNT               
         CLI   SPAEL,SPAELQ                                                     
         BNE   XTMS04                                                           
         CLI   SPATYPE,SPATCCST                                                 
         BNE   XTMS04                                                           
         MVC   XT1CACT,SPAAACT                                                  
         B     XTMS18                                                           
         DROP  R3                                                               
*                                                                               
         USING TRXELD,R3                                                        
XTMS04   CLI   TRXEL,TRXELQ                                                     
         BNE   XTMS05                                                           
         TM    TRXSTA2,TRXSNTMS    TEST TMS FOR THIS TRANSACTION                
         BO    EXITL               NO - MUST BE UPLOADED - GET OUT              
         B     XTMS08                                                           
         DROP  R3                                                               
*                                                                               
         USING PRTELD,R3                                                        
XTMS05   CLI   PRTEL,PRTELQ                                                     
         BNE   XTMS06                                                           
*&&US                              (PRTLINE# N/D IN THE UK)                     
         MVC   XTLINE#,PRTLINE#    SAVE TMS LINE NUMBER                         
*&&                                                                             
         MVC   XTRATE,PRTRATE      SAVE RATE                                    
         DROP  R3                                                               
*                                                                               
         USING SPDELD,R3                                                        
XTMS06   CLI   SPDEL,SPDELQ                                                     
         BNE   XTMS07                                                           
         LR    RE,RF               RE=L'THIS ELEMENT                            
         SH    RE,=Y(SPDACCS+1-SPDEL)                                           
         EX    RE,*+4                                                           
         MVC   XTINCAC(0),SPDACCS  SAVE INCOME ACCOUNT                          
         B     XTMS18                                                           
         DROP  R3                                                               
*                                                                               
         USING PTAELD,R3                                                        
XTMS07   CLI   PTAEL,PTAELQ        IF RECOVERING FIND WRITE-OFF MOA             
         BNE   XTMS08                                                           
         L     RE,XTPAPTA                                                       
         CLI   PTATYPE-PTAELD(RE),PTATWOFR                                      
         BNE   XTMS18                                                           
         CLI   PTATYPE,PTATWOF                                                  
         BNE   XTMS18                                                           
         CLC   PTASEQN,PTASEQN-PTAELD(RE)                                       
         BNE   XTMS18                                                           
         MVC   XTWOFMOA,PTAMOA     SAVE FOR RCVR GETTIM LOCK/UPDATE             
         DROP  R3                                                               
*                                                                               
XTMS08   DS    0H                                                               
*        USING APEELD,R3                                                        
*TMS08   CLI   APEEL,APEELQ        INCOME ACCOUNT MAY BE IN APEEL               
*        BNE   XTMS09                                                           
*        SR    RE,RE                                                            
*        IC    RE,APENLEN                                                       
*        SH    RE,=Y(APENACT+1-APENTRY)                                         
*        EX    RE,*+4                                                           
*        MVC   XTINCAC(0),APENACT                                               
*        B     XTMS18                                                           
*                                                                               
         USING ANOELD,R3                                                        
XTMS09   CLI   ANOEL,ANOELQ        TEST ANALYZED OFFICE ELEMENT                 
         BNE   XTMS10                                                           
         CLI   ANOTYPE,ANOTCLI     TEST CLIENT OFFICE                           
         BNE   XTMS18                                                           
         MVC   XTOFFICE,ANOOFFC                                                 
         DROP  R3                                                               
*                                                                               
XTMS10   DS    0H                                                               
*                                                                               
XTMS18   BXH   R3,RF,XTMS02                                                     
*                                                                               
         USING PTAELD,XTPTAEL                                                   
XTMS20   L     RE,XTPAPTA                                                       
         MVC   XTPTAEL,0(RE)                                                    
         CLI   XT1CACT,C' '        TEST HAVE CLIENT COSTING ACCOUNT             
         BNH   EXITL                                                            
         MVC   XTHALF,PTAHOURS     CONVERT NO. OF HOURS INTO PACKED             
         LH    RE,XTHALF                                                        
         CVD   RE,XTHRS                                                         
         ZAP   XTNET,PTANET                                                     
*                                                                               
         L     RE,AIO1             SAVE CALLERS IO1                             
         XR    RF,RF                                                            
         ICM   RF,3,TRNRLEN-TRNRECD(RE)                                         
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         SH    RE,=Y(L'IODA+L'IOWORK)                                           
         LA    R0,XTSAVIO1                                                      
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
FR       USING TMBLKD,XTFROM                                                    
         MVC   FR.TMK1RACT,TRNKCACT                                             
         MVC   FR.TMK1CACT,XT1CACT                                              
         MVC   FR.TMKJOB,BCJOBCOD                                               
         MVC   FR.TMKWC,TRNKWORK                                                
         MVC   FR.TMKCOFF,BCSPACES                                              
         OC    FR.TMKCOFF,XTOFFICE                                              
         MVC   FR.TMKPEDT,TRNKDATE                                              
         MVC   FR.TMKLINE#,XTLINE#                                              
         MVI   FR.TMKIND,0                                                      
         L     RF,XTPATRN                                                       
         LA    RF,TRNRFST-TRNRECD(RF)                                           
         GOTO1 VCONVMOS,BCPARM,(X'FD',(RF)),FR.TMKMOA                           
         CLI   PTATYPE,PTATWOFR                                                 
         BNE   *+10                                                             
         MVC   FR.TMKMOA,XTWOFMOA  FOR RECOVERY USE WRITE-OFF MOA               
*                                                                               
         CLI   XTPACTN,XTLOCKQ     TEST LOCK CALL                               
         BNE   XTMS22                                                           
         GOTO1 GETTIM,FR.TMBLKD    GET 'FROM TIMEL' BLOCK                       
         BE    XTMS21                                                           
         MVI   FR.TMKIND,TIMIADJ                                                
         XC    FR.TMKLINE#,FR.TMKLINE#                                          
         GOTO1 GETTIM,FR.TMBLKD                                                 
         BNE   XFRTMSH             SET ERROR FOR CALLER                         
XTMS21   ICM   RE,15,FR.TMDATIM                                                 
         SR    RF,RF               PICK UP COUNT CBIL LOCKS                     
         IC    RF,TIMLKCBI-TIMELD(RE)                                           
         CP    PTANET,BCPZERO      TEST IF THIS IS UNCLEARING                   
         BNE   *+10                                                             
         BCTR  RF,0                REDUCE COUNT OF LOCKS                        
         B     *+8                                                              
         LA    RF,1(RF)                                                         
         LTR   RF,RF               TEST RESULT LESS THAN ZERO                   
         BNM   *+6                                                              
         SR    RF,RF               THEN SET ZERO                                
         STC   RF,TIMLKCBI-TIMELD(RE)                                           
         NI    TIMSTAT-TIMELD(RE),FF-TIMLOCK                                    
         CLI   TIMLKCBI-TIMELD(RE),0                                            
         BE    *+8                                                              
         OI    TIMSTAT-TIMELD(RE),TIMLOCK                                       
         GOTO1 PUTTIM,FR.TMBLKD    WRITE IT BACK                                
         B     XFRTMSY                                                          
*                                                                               
XTMS22   GOTO1 HGHTIM,FR.TMBLKD                                                 
         GOTO1 GETTIM,FR.TMBLKD    GET 'FROM TIMEL' BLOCK                       
         BE    XTMS23              ORIGINAL FOUND                               
         MVI   FR.TMKIND,TIMIADJ   LOOK FOR ADJUSTMENT                          
         GOTO1 GETTIM,FR.TMBLKD    GET 'FROM TIMEL' BLOCK                       
         BNE   XFRTMSN                                                          
         MVI   FR.TMKIND,0                                                      
XTMS23   ICM   RE,15,FR.TMDATIM    REDUCE CLUSTER LOCK COUNT                    
         SR    RF,RF                                                            
         ICM   RF,1,TIMLKCBI-TIMELD(RE)                                         
         BZ    *+8                 IS ALREADY ZERO UNLOCK                       
         BCT   RF,*+8              REDUCE - UNLOCK IF ZERO                      
         NI    TIMSTAT-TIMELD(RE),FF-TIMLOCK                                    
         STC   RF,TIMLKCBI-TIMELD(RE)                                           
         GOTO1 PUTTIM,FR.TMBLKD                                                 
         MVC   FR.TMKMOA,PTAMOA    NOW USE POSTING MOA                          
         XC    FR.TMKLINE#,FR.TMKLINE#                                          
         LA    R0,XTTO             SAVE IN 'TO' BLOCK                           
         LA    R1,L'XTTO                                                        
         LA    RE,XTFROM                                                        
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         MVI   FR.TMKIND,TIMIADJ                                                
         GOTO1 GETTIM,FR.TMBLKD    GET 'FROM TIMEL' ADJUSTMENT BLOCK            
         BNE   XTMS24                                                           
         CLI   XTPTYPE,C'T'        IF XFER ALWAYS ADD NEW ADJUSTMENT            
         BE    XTMS24                                                           
         CLI   PTATYPE,PTATWOF     ALSO FOR WRITE-OFF                           
         BNE   XTMS25              FOR RECOVERY USE THE ONE FOUND               
XTMS24   GOTO1 CPYTIM,BCPARM,FR.TMBLKD,XTTO                                     
         MP    XTHRS,=P'-1'                                                     
         MP    XTNET,=P'-1'                                                     
         GOTO1 ADJTIM,BCPARM,(C'=',FR.TMBLKD)                                   
         MP    XTHRS,=P'-1'                                                     
         MP    XTNET,=P'-1'                                                     
         ICM   RE,15,FR.TMDATIM    SET CLUSTER AS ADJUSTED                      
         BZ    XTMS26                                                           
         OI    TIMIND-TIMELD(RE),TIMIADJ                                        
         B     XTMS26                                                           
*                                                                               
XTMS25   GOTO1 ADJTIM,BCPARM,(C'-',FR.TMBLKD)                                   
XTMS26   GOTO1 PUTTIM,FR.TMBLKD                                                 
*                                                                               
TO       USING TMBLKD,XTTO                                                      
         MVC   TO.TMK1RACT,TRNKCACT                                             
         MVC   TO.TMKPEDT,TRNKDATE                                              
         MVC   TO.TMKMOA,FR.TMKMOA                                              
         CLI   XTPTYPE,C'T'        TEST TRANSFER                                
         BE    XTMS28                                                           
         MVI   TO.TMKIND,TIMIWO+TIMIADJ                                         
         MVC   TO.TMKJOB,FR.TMKJOB                                              
         MVC   TO.TMKWC,FR.TMKWC                                                
         B     XTMS30                                                           
*                                                                               
XTMS28   MVC   TO.TMKJOB,PTATJOB                                                
         MVC   TO.TMKWC,PTATWRK                                                 
         MVI   TO.TMKIND,TIMIADJ                                                
*                                                                               
XTMS30   MVC   TO.TMK1CACT,XT1CACT                                              
         MVC   TO.TMKCOFF,BCSPACES                                              
         OC    TO.TMKCOFF,XTOFFICE                                              
         CLC   TO.TMKJOB,FR.TMKJOB                                              
         BE    XTMS36                                                           
         GOTO1 GETJOB,TO.TMBLKD                                                 
*                                                                               
XTMS36   GOTO1 HGHTIM,TO.TMBLKD                                                 
         GOTO1 GETTIM,TO.TMBLKD    GET 'TO TIMEL' BLOCK                         
         BNE   XTMS38                                                           
         CLI   PTATYPE,PTATWOFR    ADD NEW LINE IF NOT RECOVERY                 
         BNE   XTMS38                                                           
         GOTO1 ADJTIM,BCPARM,(C'+',TO.TMBLKD)                                   
         B     XTMS40                                                           
XTMS38   GOTO1 CPYTIM,BCPARM,TO.TMBLKD,FR.TMBLKD                                
         GOTO1 ADJTIM,BCPARM,(C'=',TO.TMBLKD)                                   
*                                                                               
XTMS40   ICM   RE,15,TO.TMDATIM    SET TO CLUSTER AS ADJUSTED/WOFF              
         BZ    XTMS42                                                           
         OI    TIMIND-TIMELD(RE),TIMIADJ                                        
         CLI   XTPTYPE,C'W'                                                     
         BNE   *+8                                                              
         OI    TIMIND-TIMELD(RE),TIMIWO                                         
XTMS42   GOTO1 PUTTIM,TO.TMBLKD    WRITE IT BACK                                
*                                                                               
         CLC   FR.TMK1CACT,TO.TMK1CACT                                          
         BNE   *+14                                                             
         CLC   FR.TMKCOFF,TO.TMKCOFF                                            
         BE    XFRTMSY                                                          
         MVC   POSTACT,TRNKULC     MAKE DUMMY 1R-1C POSTINGS                    
         MVC   POSTOFFC,FR.TMKCOFF                                              
         MVC   POSTDATE,TRNKDATE                                                
         MVC   POSTREF,TRNKREF                                                  
         ZAP   POSTAMNT,BCPZERO                                                 
         MVI   POSTSTAT,TRNSDR                                                  
         MVC   POSTCCPY,CUABIN                                                  
         MVC   POSTCUL,UL1C                                                     
         MVC   POSTCACT,FR.TMK1CACT                                             
         MVC   POSTCACN,FR.TMD1CNAM                                             
         MVI   POSTIND2,TRNIUBKO                                                
         ICM   RE,15,POSTXTRA                                                   
         USING SCIELD,RE                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITHOUR                                                 
         ZAP   SCIAMNT,XTHRS                                                    
         MP    SCIAMNT,=P'-1'                                                   
         MVI   SCIEL+SCILN1Q,0                                                  
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RE,15,POSTXTRA                                                   
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITHOUR                                                 
         ZAP   SCIAMNT,XTHRS                                                    
         DROP  RE                                                               
         MVC   POSTOFFC,TO.TMKCOFF                                              
         ZAP   POSTAMNT,BCPZERO                                                 
         MVC   POSTCACT,TO.TMK1CACT                                             
         MVC   POSTCACN,TO.TMD1CNAM                                             
         GOTO1 ABLDTRN,BODMCB,POSTVALS                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   POSTIND2,0                                                       
*                                                                               
         PUSH  USING                                                            
         USING ACTRECD,IOKEY       INCREMENT REVISION NUMBER                    
XFRTMSY  MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKULA,TRNKCUNT                                                 
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,ACTKDA                                                  
         POP   USING                                                            
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING RSTELD,R1           FIND STATUS ELEMENT                          
         XR    RF,RF                                                            
         CLI   RSTEL,RSTELQ                                                     
         BE    *+12                                                             
         IC    RF,RSTLN                                                         
         BXH   R1,RF,*-12                                                       
         ICM   RF,3,RSTSNUM        INCREMENT TIMESHEET#                         
         LA    RF,1(RF)                                                         
         STCM  RF,3,RSTSNUM                                                     
         DROP  R1                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO2                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    RE,XTSAVIO1         RESTORE CALLERS IO1                          
         XR    RF,RF                                                            
         ICM   RF,3,L'IODA+L'IOWORK+(TRNRLEN-TRNRECD)(RE)                       
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO1                                                          
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITY                                                            
*                                                                               
XFRTMSN  LA    RE,XTSAVIO1         RESTORE CALLERS IO1                          
         XR    RF,RF                                                            
         ICM   RF,3,L'IODA+L'IOWORK+(TRNRLEN-TRNRECD)(RE)                       
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO1                                                          
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITL                                                            
*                                                                               
XFRTMSH  LA    RE,XTSAVIO1         RESTORE CALLERS IO1                          
         XR    RF,RF                                                            
         ICM   RF,3,L'IODA+L'IOWORK+(TRNRLEN-TRNRECD)(RE)                       
         AH    RF,=Y(L'IODA+L'IOWORK)                                           
         L     R0,AIO1                                                          
         SH    R0,=Y(L'IODA+L'IOWORK)                                           
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
         B     EXITH                                                            
*                                                                               
         SPACE 1                                                                
         DROP  FR,TO                                                            
*                                                                               
EXITL    MVI   BCDUB,0             SET CC LOW                                   
         B     EXITCC                                                           
EXITH    MVI   BCDUB,2             SET CC HIGH                                  
         B     EXITCC                                                           
EXITY    MVI   BCDUB,1             SET CC EQUAL                                 
EXITCC   CLI   BCDUB,1                                                          
*                                                                               
EXIT     XIT1  ,                   EXIT WITH CC SET                             
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET HIGHEST SEQUENCE  NO. FOR TIMEL BLOCK                *         
*                                                                     *         
* NTRY: R1 = A(TMBLK)                                                 *         
* EXIT: CC = EQUAL IF CLUSTER FOUND                                   *         
*            IO1 = TIMREC CONTAINING CLUSTER                          *         
*       CC = HIGH IF CLUSTER NOT FOUND, BUT RECORD ON FILE            *         
*            IO1 = LAST TIMREC FOR 1R/1C                              *         
*       CC = LOW IF RECORD NOT ON FILE                                *         
*            IO1 = NEW TIMREC RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
HGHTIM   NTR1  ,                                                                
         LR    R4,R1                                                            
         USING TMBLKD,R4                                                        
         MVI   TMKHISEQ,0                                                       
*                                                                               
         USING TIMRECD,IOKEY       BUILD RECORD KEY                             
         MVC   TIMKCPY,CUABIN                                                   
         MVC   TIMKUNT(L'UL1R),UL1R                                             
         MVC   TIMKACT,TMK1RACT                                                 
         MVC   TIMKOFF,TMKCOFF                                                  
         MVC   TIMKCCPY,CUABIN                                                  
         MVC   TIMKCUNT(L'UL1C),UL1C                                            
         MVC   TIMKCACT,TMK1CACT                                                
         MVC   TIMKPEDT,TMKPEDT                                                 
         MVC   TIMKREF,=C'*TIME*'                                               
         MVI   TIMKSBR,0                                                        
         LA    R1,IOREAD+IOACCDIR                                               
         B     *+8                                                              
HTIM02   LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   HTIM10                                                           
         CLC   TIMKEY(TIMKSBR-TIMKEY),IOKEYSAV                                  
         BNE   HTIM10                                                           
         OI    TMDINDS,TMDIROF     SET RECORD ON FILE                           
         MVC   TMDSBR,TIMKSBR      SAVE SUB-REF#                                
         MVC   IODAOVER,TIMKDA     GET ACCMST RECORD                            
         GOTO1 AIO,IOGET+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R3,AIO1             FIND CORRECT TIMEL CLUSTER                   
         LA    R3,TIMRFST-TIMRECD(R3)                                           
         USING TIMELD,R3                                                        
         XR    RF,RF                                                            
HTIM04   CLI   TIMEL,0                                                          
         BE    HTIM02              EOR - GO LOOK AT NEXT RECORD                 
         CLI   TIMEL,TIMELQ        MATCH ON TIMEL                               
         BNE   HTIM08                                                           
         CLI   TIMETYP,TIMEINP                                                  
         BNE   HTIM08                                                           
         CLC   TMKHISEQ,TIMSEQ                                                  
         BNL   *+10                                                             
         MVC   TMKHISEQ,TIMSEQ                                                  
HTIM08   IC    RF,TIMLN                                                         
         BXH   R3,RF,HTIM04                                                     
*                                                                               
HTIM10   B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET TIMELS BLOCK                                         *         
*                                                                     *         
* NTRY: R1 = A(TMBLK)                                                 *         
* EXIT: CC = EQUAL IF CLUSTER FOUND                                   *         
*            IO1 = TIMREC CONTAINING CLUSTER                          *         
*       CC = HIGH IF CLUSTER NOT FOUND, BUT RECORD ON FILE            *         
*            IO1 = LAST TIMREC FOR 1R/1C                              *         
*       CC = LOW IF RECORD NOT ON FILE                                *         
*            IO1 = NEW TIMREC RECORD                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETTIM   NTR1  ,                                                                
         LR    R4,R1                                                            
         USING TMBLKD,R4                                                        
         MVI   TMDINDS,0                                                        
*                                                                               
         XC    TMDATA,TMDATA       ZEROISE DATA BLOCK                           
*                                                                               
         USING ACTRECD,IOKEY       SAVE NAME OF 1C ACCOUNT                      
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'UL1C),UL1C                                             
         MVC   ACTKACT,TMK1CACT                                                 
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TMD1CNAM,ACNAME                                                  
*                                                                               
GT       USING TIMINP,GTTIMINP     BUILD TIMEL 'KEY'                            
         MVC   GT.TIMACC(L'BCCPYPRD),BCCPYPRD                                   
         MVC   GT.TIMACC+L'BCCPYPRD(L'TMKJOB),TMKJOB                            
         MVC   GT.TIMTSK,TMKWC                                                  
         MVC   GT.TIMOFF,TMKCOFF                                                
         MVI   GT.TIMTTYP,TIMTCB                                                
         MVC   GT.TIMMOA,TMKMOA                                                 
         MVC   GT.TIMIND,TMKIND                                                 
         MVI   GT.TIMSTAT,0                                                     
*                                                                               
         USING TIMRECD,IOKEY       BUILD RECORD KEY                             
         MVC   TIMKCPY,CUABIN                                                   
         MVC   TIMKUNT(L'UL1R),UL1R                                             
         MVC   TIMKACT,TMK1RACT                                                 
         MVC   TIMKOFF,TMKCOFF                                                  
         MVC   TIMKCCPY,CUABIN                                                  
         MVC   TIMKCUNT(L'UL1C),UL1C                                            
         MVC   TIMKCACT,TMK1CACT                                                
         MVC   TIMKPEDT,TMKPEDT                                                 
         MVC   TIMKREF,=C'*TIME*'                                               
         MVI   TIMKSBR,0                                                        
         LA    R1,IOREAD+IOACCDIR                                               
         B     *+8                                                              
GTIM02   LA    R1,IOSEQ+IOACCDIR                                                
         GOTO1 AIO                                                              
         BNE   GTIM40                                                           
         CLC   TIMKEY(TIMKSBR-TIMKEY),IOKEYSAV                                  
         BNE   GTIM40                                                           
         OI    TMDINDS,TMDIROF     SET RECORD ON FILE                           
         MVC   TMDSBR,TIMKSBR      SAVE SUB-REF#                                
         MVC   IODAOVER,TIMKDA     GET ACCMST RECORD                            
         LA    R1,IOGET+IOACCMST+IO1                                            
         CLI   XTPACTN,XTLOCKQ     TEST LOCK CALL                               
         BE    *+12                ALWAYS UPDATE                                
         CLI   POSTMODE,POSTLVQ                                                 
         BNE   *+8                                                              
         LA    R1,IOLOCK(R1)       READ FOR UPDATE IF LIVE                      
         GOTO1 AIO,(R1)                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    RE,XTFROM           TEST THIS IS 'FROM' CALL                     
         CR    RE,R4                                                            
         BNE   GTIM03                                                           
         LA    R0,PIDELQ                                                        
         GOTO1 VHELLO,BCPARM,(C'G',XACCMST),((R0),AIO1),0                       
         CLI   BCPARM+12,0                                                      
         BNE   GTIM03                                                           
         L     RE,BCPARM+12                                                     
         MVC   XTPIDEL,0(RE)       SAVE PERSONAL ID ELEMENT                     
*                                                                               
GTIM03   L     R3,AIO1             FIND CORRECT TIMEL CLUSTER                   
         LA    R3,TIMRFST-TIMRECD(R3)                                           
         USING TIMELD,R3                                                        
         XR    RF,RF                                                            
GTIM04   CLI   TIMEL,0                                                          
         BE    GTIM02              EOR - GO LOOK AT NEXT RECORD                 
         CLI   TIMEL,TIMELQ        MATCH ON TIMEL                               
         BNE   GTIM18                                                           
         CLI   TIMETYP,TIMEINP                                                  
         BNE   GTIM18                                                           
         MVC   TMDSEQ,TIMSEQ       SAVE LAST SEQUENCE NUMBER                    
*        MVC   GT.TIMIND,TIMIND                                                 
         CLC   GTTIMINP,TIMINP                                                  
         BNE   GTIM18                                                           
         CLC   XTINCAC,TIMINC      MATCH ON INCOME ACCOUNT                      
         BNE   GTIM18                                                           
         CP    XTRATE,TIMRATE      MATCH ON RATE                                
         BNE   GTIM18                                                           
         LR    RE,R3               SET RE=A(THIS CLUSTER)                       
         CLI   PTATYPE,PTATWOFR    IF RECOVERY MATCH REVERSING AMOUNT           
         BNE   GTIM16                                                           
         ZAP   BODUB1,PTANET       SET RECOVERY AMOUNT                          
         TM    GT.TIMIND,TIMIWO    TEST LOOKING FOR BW LINE                     
         BNO   *+10                                                             
         MP    BODUB1,=P'-1'       THEN REVERSE FOR MATCHING                    
         CP    TIMAMNT,BODUB1                                                   
         BNE   GTIM18                                                           
GTIM16   IC    RF,TIMLN-TIMELD(RE) MATCH ON NARRATIVE                           
         AR    RE,RF                                                            
         CLC   TIMSEQ,(TIMSEQ-TIMELD)(RE)                                       
         BE    GTIM17                                                           
         CLC   XTNARR,BCSPACES     CHECK IF TRANSACTION HAD NARRATIVE           
         BNE   GTIM18              YES IT DID - NO MATCH                        
         B     GTIM20                                                           
GTIM17   CLI   TIMETYP-TIMELD(RE),TIMENAR                                       
         BNE   GTIM16                                                           
         IC    RF,TIMLN-TIMELD(RE)                                              
         SH    RF,=AL2(TIMNARR-TIMELD+1)                                        
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   XTNARR(0),TIMNARR-TIMELD(RE)                                     
         BE    GTIM20                                                           
GTIM18   IC    RF,TIMLN                                                         
         BXH   R3,RF,GTIM04                                                     
*                                                                               
GTIM20   STCM  R3,15,TMDATIM       SAVE A(CLUSTER)                              
         LA    R2,TMTIMELS         COPY CLUSTER OF TIMELS                       
         LR    R0,R2                                                            
GTIM22   CLI   TIMEL,TIMELQ                                                     
         BNE   GTIM30                                                           
         CLC   TIMSEQ,TMDSEQ                                                    
         BNE   GTIM30                                                           
         CLI   TIMETYP,TIMEINP                                                  
         BNE   *+10                                                             
         CR    R2,R0                                                            
         BNE   GTIM30                                                           
         IC    RF,TIMLN                                                         
         EX    RF,*+4                                                           
         MVC   0(0,R2),TIMELD                                                   
         AR    R2,RF                                                            
         BXH   R3,RF,GTIM22                                                     
GTIM30   MVI   0(R2),0                                                          
         SR    R2,R0                                                            
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  R2,3,TMDCLEN        SAVE LENGTH OF CLUSTER                       
*                                                                               
         B     EXITY               CC=EQUAL - RECORD & CLUSTER FOUND            
*                                                                               
GTIM40   TM    TMDINDS,TMDIROF                                                  
         BO    EXITH               CC=HIGH - RECORD FOUND ONLY                  
*                                                                               
         GOTO1 BLDTIM,TMBLKD                                                    
         B     EXITL               CC=LOW - RECORD NOT FOUND                    
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADJUST TIME ON RECORD                                    *         
*                                                                     *         
* NTRY: P1 BYTE 0 = C'=' TO SET HOURS TO XTHRS                        *         
*                   C'+' TO ADD XTHRS TO NUMBER OF HOURS              *         
*                   C'-' TO SUBTRACT XTHRS FROM NUMBER OF HOURS       *         
*             1-3 = A(TMBLK)                                          *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
ADJTIM   NTR1  ,                                                                
         XR    R4,R4                                                            
         ICM   R4,7,1(R1)                                                       
         USING TMBLKD,R4           R4=A(TMS BLOCK)                              
         ICM   R3,15,TMDATIM                                                    
         USING TIMELD,R3           R3=A(INPUT DETAIL ELEMENT)                   
*                                                                               
         ZAP   ATOLDHRS,TIMHRS                                                  
         ZAP   ATOLDNET,BCPZERO                                                 
         CLI   TIMLN,TIMILN2Q                                                   
         BL    *+10                                                             
         ZAP   ATOLDNET,TIMAMNT                                                 
*                                                                               
         CLI   0(R1),C'='                                                       
         BE    ATIM02                                                           
         CLI   0(R1),C'+'                                                       
         BE    ATIM04                                                           
         CLI   0(R1),C'-'                                                       
         BE    ATIM06                                                           
         DC    H'0'                                                             
*                                                                               
ATIM02   ZAP   ATNEWNET,XTNET      SET TO XTNET/XTHRS                           
         ZAP   ATNEWHRS,XTHRS                                                   
         B     ATIM10                                                           
*                                                                               
ATIM04   ZAP   ATNEWNET,ATOLDNET   ADD XTNET                                    
         AP    ATNEWNET,XTNET                                                   
         ZAP   ATNEWHRS,ATOLDHRS   ADD XTHRS                                    
         AP    ATNEWHRS,XTHRS                                                   
         BNZ   ATIM10                                                           
         GOTO1 DELTIM,TMBLKD       DELETE BLOCK IF NEW AMOUNT IS ZERO           
         B     ADJTIMX                                                          
*                                                                               
ATIM06   ZAP   ATNEWNET,ATOLDNET   SUBTRACT XTNET                               
         SP    ATNEWNET,XTNET                                                   
         ZAP   ATNEWHRS,ATOLDHRS   SUBTRACT XTHRS                               
         SP    ATNEWHRS,XTHRS                                                   
         BNZ   ATIM10                                                           
         GOTO1 DELTIM,TMBLKD       DELETE BLOCK IF NEW AMOUNT IS ZERO           
         B     ADJTIMX                                                          
*                                                                               
ATIM10   ZAP   TIMHRS,ATNEWHRS                                                  
         CLI   TIMLN,TIMILN2Q                                                   
         BL    *+10                                                             
         ZAP   TIMAMNT,XTNET                                                    
*                                                                               
         XR    RF,RF                                                            
         LR    R0,R3                                                            
ATIM14   CLI   TIMEL,TIMELQ        FIND TAX ELEMENT                             
         BNE   ADJTIMX                                                          
         CLC   TIMSEQ,TMDSEQ                                                    
         BNE   ADJTIMX                                                          
         CLI   TIMETYP,TIMEINP                                                  
         BNE   *+10                                                             
         CR    R3,R0                                                            
         BNE   ADJTIMX                                                          
         CLI   TIMETYP,TIMETAX                                                  
         BE    *+12                                                             
         IC    RF,TIMLN                                                         
         BXH   R3,RF,ATIM14                                                     
         ZAP   XTPL16,TIMTBAS      ADJUST TAX AMOUNT                            
         MP    XTPL16,ATNEWHRS                                                  
         SRP   XTPL16,2,0                                                       
         DP    XTPL16,ATOLDHRS                                                  
         SRP   XTPL16(L'XTPL16-L'ATOLDHRS),64-2,5                               
         ZAP   TIMTBAS,XTPL16(L'XTPL16-L'ATOLDHRS)                              
*                                                                               
ADJTIMX  B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DELETE TIMEL CLUSTER FROM RECORD                         *         
*                                                                     *         
* NTRY: R1 = A(TMBLKD)                                                *         
***********************************************************************         
         SPACE 1                                                                
DELTIM   NTR1  ,                                                                
         LR    R4,R1                                                            
         USING TMBLKD,R4                                                        
         L     R2,AIO1                                                          
         USING TIMRECD,R2                                                       
         OI    TMDINDS,TMDICLUD                                                 
*                                                                               
         ICM   R3,15,TMDATIM                                                    
         USING TIMELD,R3                                                        
         XR    RF,RF                                                            
         LR    R0,R3                                                            
DTIM02   CLI   TIMEL,TIMELQ        MARK ELEMENTS OF CLUSTER                     
         BNE   DTIM04                                                           
         CLC   TIMSEQ,TMDSEQ                                                    
         BNE   DTIM04                                                           
         CLI   TIMETYP,TIMEINP                                                  
         BNE   *+10                                                             
         CR    R3,R0                                                            
         BNE   DTIM04                                                           
         MVI   TIMEL,FF                                                         
         IC    RF,TIMLN                                                         
         BXH   R3,RF,DTIM02                                                     
*                                                                               
DTIM04   GOTO1 VHELLO,BCPARM,(C'D',XACCMST),('FF',TIMRECD),0                    
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R3,TIMRFST          TEST ANY TIMELS LEFT ON RECORD               
         XR    RF,RF                                                            
DTIM06   CLI   TIMEL,0                                                          
         BE    DTIM08                                                           
         CLI   TIMEL,TIMELQ                                                     
         BE    DTIM10                                                           
         IC    RF,TIMLN                                                         
         BXH   R3,RF,DTIM06                                                     
         DROP  R3                                                               
*                                                                               
DTIM08   OI    TIMRRI1,X'80'       NO MORE TIMELS - SHOULD BE DELETED           
         OI    TMDINDS,TMDIDEL                                                  
*                                                                               
DTIM10   XC    TMDATIM,TMDATIM                                                  
         B     EXIT                                                             
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO COPY A TIMEL CLUSTER                                     *         
*                                                                     *         
* NTRY: P1 = A(TO TMBLKD)                                             *         
*       P2 = A(FROM TMBLKD)                                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
CPYTIM   NTR1  ,                                                                
         LM    R3,R4,0(R1)                                                      
TO       USING TMBLKD,R3           R3=A(TO TMBLKD)                              
FR       USING TMBLKD,R4           R4=A(FROM TMBLKD)                            
         L     R2,AIO1                                                          
         USING TIMRECD,R2                                                       
         XR    RF,RF               TEST CLUSTER WILL FIT INTO RECORD            
         ICM   RF,3,TIMRLEN                                                     
         XR    RE,RE                                                            
         ICM   RE,3,FR.TMDCLEN                                                  
         BNZ   *+6                                                              
         DC    H'0'                                                             
         STCM  RE,3,TO.TMDCLEN                                                  
         AR    RF,RE                                                            
         CH    RF,=Y(2000)                                                      
         BL    CTIM02                                                           
         IC    RE,TO.TMDSBR        INCREMENT SUB-REF NUMBER FOR NEW REC         
         LA    RE,1(RE)                                                         
         STC   RE,TO.TMDSBR                                                     
         NI    TO.TMDINDS,FF-TMDIROF-TMDIDEL                                    
         GOTO1 BLDTIM,TO.TMBLKD                                                 
*                                                                               
CTIM02   IC    RE,TO.TMKHISEQ      INCREMENT SEQUENCE # FOR NEW CLUSTER         
         LA    RE,1(RE)                                                         
         STC   RE,TO.TMKHISEQ                                                   
*                                                                               
         LA    R1,TIMRFST          SET R1 TO END-OF-RECORD                      
         XR    RF,RF                                                            
         CLI   0(R1),0                                                          
         BE    *+12                                                             
         IC    RF,1(R1)                                                         
         BXH   R1,RF,*-12                                                       
         STCM  R1,15,TO.TMDATIM                                                 
*                                                                               
         LA    RE,FR.TMTIMELS                                                   
         USING TIMELD,RE                                                        
CTIM04   CLI   TIMEL,0                                                          
         BE    CTIM06                                                           
         IC    RF,TIMLN                                                         
         EX    RF,*+4                                                           
         MVC   0(0,R1),TIMELD                                                   
         MVC   TIMSEQ-TIMELD(L'TIMSEQ,R1),TO.TMKHISEQ                           
         AR    R1,RF                                                            
         BXH   RE,RF,CTIM04                                                     
CTIM06   MVI   0(R1),0                                                          
         L     RE,AIO1             SET NEW LENGTH OF RECORD                     
         SR    R1,RE                                                            
         LA    R1,1(R1)                                                         
         STCM  R1,3,TIMRLEN                                                     
         DROP  RE                                                               
*                                                                               
         ICM   R5,15,TO.TMDATIM    SET 'TO' DETAILS ON TIMEL                    
         USING TIMELD,R5                                                        
         MVC   TIMACC(L'BCCPYPRD),BCCPYPRD                                      
         MVC   TIMACC+L'BCCPYPRD(L'TMKJOB),TO.TMKJOB                            
         MVC   TIMTSK,TO.TMKWC                                                  
         MVC   TIMOFF,TO.TMKCOFF                                                
         MVI   TIMTTYP,TIMTCB                                                   
         MVI   TIMIND,0                                                         
         MVC   TIMMOA,TO.TMKMOA                                                 
         MVI   TIMSTAT,0                                                        
         MVC   TIMADAT,BCTODAYP                                                 
*                                                                               
         CLI   POSTMODE,POSTLVQ    TEST MAKING LIVE POSTINGS                    
         BNE   CPYTIMX                                                          
         TM    FR.TMDINDS,TMDICLUD TEST FULL AMOUNT WAS TRANSFERRD              
         BO    CPYTIMX             YES - USE SAME TIMESHEET#                    
*                                                                               
         USING ACTRECD,IOKEY       READ 1R ACCOUNT RECORD                       
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'UL1R),UL1R                                             
         MVC   ACTKACT,TO.TMK1RACT                                              
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   IODAOVER,ACTKDA                                                  
         GOTO1 AIO,IOGETRUP+IOACCMST+IO2                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R1,AIO2                                                          
         LA    R1,ACTRFST-ACTRECD(R1)                                           
         USING RSTELD,R1           FIND STATUS ELEMENT                          
         XR    RF,RF                                                            
         CLI   RSTEL,RSTELQ                                                     
         BE    *+12                                                             
         IC    RF,RSTLN                                                         
         BXH   R1,RF,*-12                                                       
         ICM   RF,3,RSTSNUM        INCREMENT TIMESHEET#                         
         LA    RF,1(RF)                                                         
         STCM  RF,3,RSTSNUM                                                     
         STCM  RF,3,TIMLINE#       SET TIMESHEET# FOR CLUSTER                   
         DROP  R1                                                               
         GOTO1 AIO,IOPUT+IOACCMST+IO2                                           
         BE    *+6                 1R RECORD UPDATED FOR TIMESHEET#             
         DC    H'0'                                                             
*                                                                               
CPYTIMX  B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO WRITE/ADD NEW RECORD                                     *         
*                                                                     *         
* NTRY: R1 = A(TMBLKD)                                                *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
PUTTIM   NTR1  ,                                                                
         CLI   XTPACTN,XTLOCKQ     TEST LOCK CALL                               
         BE    *+12                ALWAYS UPDATE                                
         CLI   POSTMODE,POSTLVQ    ONLY BOTHER IF MAKING LIVE POSTINGS          
         BNE   EXIT                                                             
         L     R2,AIO1                                                          
         USING TIMRECD,R2                                                       
DIR      USING TIMRECD,IOKEY                                                    
         LR    R4,R1                                                            
         USING TMBLKD,R4                                                        
*                                                                               
         TM    TMDINDS,TMDIDEL     TEST RECORD TO BE DELETED                    
         BO    PTIM30                                                           
*                                                                               
         MVC   TIMRLMOS,BCEFFS     SET HIGH/LOW MOA VALUES                      
         XC    TIMRHMOS,TIMRHMOS                                                
         LA    R3,TIMRFST                                                       
         USING TIMELD,R3                                                        
         XR    RF,RF                                                            
PTIM02   CLI   TIMEL,0                                                          
         BE    PTIM06                                                           
         CLI   TIMEL,TIMELQ                                                     
         BNE   PTIM04                                                           
         CLI   TIMETYP,TIMEINP                                                  
         BNE   PTIM04                                                           
         CLC   TIMRLMOS,TIMMOA                                                  
         BNH   *+10                                                             
         MVC   TIMRLMOS,TIMMOA                                                  
         CLC   TIMRHMOS,TIMMOA                                                  
         BNL   *+10                                                             
         MVC   TIMRHMOS,TIMMOA                                                  
PTIM04   IC    RF,TIMLN                                                         
         BXH   R3,RF,PTIM02                                                     
         DROP  R3                                                               
*                                                                               
PTIM06   TM    TMDINDS,TMDIRES     TEST RECORD TO BE RESTORED                   
         BO    PTIM10                                                           
         TM    TMDINDS,TMDIROF     TEST RECORD ON FILE                          
         BZ    PTIM20                                                           
*                                  * WRITE BACK RECORD *                        
PTIM10   NI    TIMRSTA,FF-X'80'                                                 
         LA    R3,TIMRFST                                                       
         USING RVNELD,R3           UPDATE REVISION NUMBER                       
         XC    XTELEM,XTELEM                                                    
         XR    RF,RF                                                            
PTIM12   CLI   RVNEL,0                                                          
         BE    PTIM14                                                           
         CLI   RVNEL,RVNELQ                                                     
         BE    *+12                                                             
         IC    RF,RVNLN                                                         
         BXH   R3,RF,PTIM12                                                     
         ICM   RE,3,RVNNO                                                       
         LA    RE,1(RE)                                                         
         STCM  RE,3,RVNNO                                                       
         DROP  R3                                                               
*                                                                               
PTIM14   XC    XTELEM,XTELEM                                                    
         LA    R0,PIDELQ           FIND PIDEL                                   
         GOTO1 VHELLO,BCPARM,(C'G',XACCMST),((R0),AIO1),0                       
         CLI   BCPARM+12,0                                                      
         BNE   PTIM18                                                           
         L     RF,BCPARM+12                                                     
         SR    RE,RE                                                            
         IC    RE,1(RF)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   XTELEM(0),0(RF)     SAVE ELEMENT                                 
*                                                                               
         MVI   0(RF),FF            REMOVE FROM RECORD                           
         GOTO1 VHELLO,BCPARM,(C'D',XACCMST),(X'FF',AIO1),0                      
         CLI   BCPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  ADD BACK IN CORRECT PLACE                    
         GOTO1 VHELLO,BCPARM,(C'P',XACCMST),AIO1,XTELEM                         
         CLI   BCPARM+12,0                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                  ADD BACK IN CORRECT PLACE                    
PTIM18   GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   DIR.TIMKEY,TIMKEY   WRITE BACK DIRECTORY RECORD                  
         GOTO1 AIO,IORDD+IOACCDIR                                               
         CLC   DIR.TIMKEY,IOKEYSAV                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    DIR.TIMKSTA,FF-X'80'                                             
         MVC   DIR.TIMKLMOS,TIMRLMOS                                            
         MVC   DIR.TIMKHMOS,TIMRHMOS                                            
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         TM    TMDINDS,TMDIRES     TEST RESTORING                               
         BZ    EXIT                                                             
         BAS   RE,BLDPAS           RESTORE PASSIVE                              
         GOTO1 AIO,IORDD+IOACCDIR                                               
         CLI   IOERR,IOEDEL                                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         NI    IOKEY+(TSWKSTA-TSWKEY),FF-X'80'                                  
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
*                                  * ADD RECORD *                               
PTIM20   OC    XTPIDEL,XTPIDEL     TEST ANY PIDEL                               
         BZ    PTIM22                                                           
         GOTO1 VHELLO,BCPARM,(C'P',XACCMST),AIO1,XTPIDEL                        
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
PTIM22   GOTO1 AIO,IOADD+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         BAS   RE,BLDPAS           ADD PASSIVE                                  
         GOTO1 AIO,IOADD+IOACCDIR+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         B     EXIT                                                             
*                                  * DELETE RECORD *                            
PTIM30   OI    TIMRSTA,X'80'                                                    
         GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   DIR.TIMKEY,TIMKEY                                                
         GOTO1 AIO,IORDUP+IOACCDIR DELETE DIRECTORY RECORD                      
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    DIR.TIMKSTA,X'80'                                                
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         BAS   RE,BLDPAS           DELETE PASSIVE                               
         GOTO1 AIO,IOREAD+IOACCDIR                                              
         BE    *+6                                                              
         DC    H'0'                                                             
         OI    IOKEY+(TSWKSTA-TSWKEY),X'80'                                     
         GOTO1 AIO,IOWRITE+IOACCDIR                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD NEW RECORD                                         *         
*                                                                     *         
* NTRY:  R1 = A(TMBLKD)                                               *         
* EXIT: IO1 = NEW RECORD                                              *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDTIM   NTR1  ,                                                                
         LR    R4,R1                                                            
         USING TMBLKD,R4                                                        
         LA    R2,IOKEY                                                         
         USING TIMRECD,R2                                                       
*                                                                               
BLDTIM02 MVC   TIMKCPY,CUABIN                                                   
         MVC   TIMKUNT(L'UL1R),UL1R                                             
         MVC   TIMKACT,TMK1RACT                                                 
         MVC   TIMKOFF,TMKCOFF                                                  
         MVC   TIMKCCPY,CUABIN                                                  
         MVC   TIMKCUNT(L'UL1C),UL1C                                            
         MVC   TIMKCACT,TMK1CACT                                                
         MVC   TIMKPEDT,TMKPEDT                                                 
         MVC   TIMKREF,=C'*TIME*'                                               
         MVC   TIMKSBR,TMDSBR                                                   
*                                                                               
         GOTO1 AIO,IORDD+IOACCDIR                                               
         CLI   IOERR,IOERNF        TEST RECORD NOT ON FILE                      
         BE    BLDTIM06                                                         
         CLI   IOERR,IOEDEL        TEST RECORD IS DELETED                       
         BE    BLDTIM04                                                         
         MVC   IODAOVER,TIMKDA                                                  
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     RF,AIO1                                                          
         SR    RE,RE                                                            
         ICM   RE,3,TIMRLEN-TIMRECD(RF)                                         
         SR    RF,RF                                                            
         ICM   RF,3,XTFROM+(TMDCLEN-TMBLKD)                                     
         AR    RF,RE                                                            
         CH    RF,=Y(2000)                                                      
         BH    *+12                                                             
         OI    TMDINDS,TMDIROF                                                  
         B     EXIT                                                             
         IC    RF,TMDSBR                                                        
         LA    RF,1(RF)                                                         
         STC   RF,TMDSBR                                                        
         B     BLDTIM02                                                         
*                                                                               
BLDTIM04 OI    TMDINDS,TMDIRES     SET RECORD TO BE RESTORED                    
         MVC   IODAOVER,TIMKDA                                                  
         GOTO1 AIO,IOGETRUP+IOACCMST+IO1                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
BLDTIM06 L     R2,AIO1                                                          
         XC    0(256,R2),0(R2)                                                  
         MVC   TIMKEY,IOKEYSAV                                                  
         MVI   TIMRRI1,TIMKRI1Q                                                 
         MVI   TIMRRI2,TIMKRI2Q                                                 
         MVI   TIMRRI3,TIMKRI3Q                                                 
         MVI   TIMRRI4,TIMKRI4Q                                                 
*                                                                               
         LA    R3,TIMRFST                                                       
*                                                                               
         SR    R3,R2               SET RECORD LENGTH                            
         STCM  R3,3,TIMRLEN                                                     
*                                                                               
         B     EXIT                                                             
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO BUILD PASSIVE POINTER KEY                                *         
*                                                                     *         
* NTRY:   IO1 = TIMREC                                                *         
* EXIT: IOKEY = PASSIVE KEY                                           *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
BLDPAS   NTR1  ,                                                                
         LA    R3,BCLDGTAB         LOCATE 1R LEDGER TABLE ENTRY                 
         USING LDGTABD,R3                                                       
         CLC   LDGTUL,UL1R                                                      
         BE    *+12                                                             
         LA    R3,LDGTABL(R3)                                                   
         B     *-14                                                             
*                                                                               
         L     R2,AIO1                                                          
         USING TIMRECD,R2                                                       
         USING TSWRECD,IOKEY       SET UP PASSIVE KEY                           
         XC    TSWKEY,TSWKEY                                                    
         MVI   TSWKTYP,TSWKTYPQ                                                 
         MVI   TSWKSUB,TSWKSUBQ                                                 
         MVC   TSWKCPY,CUABIN                                                   
         ICM   RF,7,TIMKPEDT                                                    
         LNR   RF,RF                                                            
         STCM  RF,7,TSWKEND                                                     
         MVC   TSWKULC,TIMKULC                                                  
         MVC   TSWKCOFC,TIMKOFF                                                 
         MVC   TSWKSBR,TIMKSBR                                                  
*                                                                               
         MVC   TSWKPER,BCSPACES                                                 
         XR    RF,RF                                                            
         IC    RF,LDGTLVD          RF=L(OFFICE/DEPT/SUB/PERSON)                 
         XR    RE,RE                                                            
         IC    RE,LDGTLVC          RE=L(OFFICE/DEPT/SUB)                        
         SR    RF,RE               RF=L(PERSON)                                 
         LA    R1,TIMKACT(RE)      R1=A(PERSON)                                 
         BCTR  RF,0                COPY PERSON CODE                             
         EX    RF,*+4                                                           
         MVC   TSWKPER(0),0(R1)                                                 
         MVC   TSWKODS,BCSPACES                                                 
         BCTR  RE,0                COPY OFFICE/DEPT/SUB CODES                   
         EX    RE,*+4                                                           
         MVC   TSWKODS(0),TIMKACT                                               
*                                                                               
         XC    TSWKSTA,TSWKSTA     SET UP STATUS                                
         MVC   TSWKDA,IODA         SET D/A                                      
*                                                                               
         B     EXIT                                                             
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO GET JOB DETAILS                                          *         
*                                                                     *         
* NTRY: R1 = TIMBLKD                                                  *         
***********************************************************************         
         SPACE 1                                                                
         PUSH  USING                                                            
GETJOB   NTR1  ,                                                                
         MVC   XTPRFS,BCPRFS       SAVE CURRENT PROFILES                        
         XC    BCPRFS(BCPRFSL),BCPRFS                                           
*                                                                               
         LR    R4,R1                                                            
         USING TMBLKD,R4                                                        
         USING ACTRECD,IOKEY                                                    
         MVC   ACTKEY,BCSPACES                                                  
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(L'BCCPYPRD),BCCPYPRD                                     
*                                                                               
         IC    RE,BCCLILEN         GET CLIENT RECORD                            
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),TMKJOB                                                
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   TMKCOFF,ACOFFC      SAVE CLIENT OFFICE                           
         ICM   RF,15,ACAPPR                                                     
         BZ    *+10                                                             
         MVC   BCCLIPRF,0(RF)                                                   
*                                                                               
         IC    RE,BCPROLEN         GET PRODUCT RECORD                           
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),TMKJOB                                                
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         CLC   ACOFFC,BCSPACES                                                  
         BNH   *+10                                                             
         MVC   TMKCOFF,ACOFFC      SAVE CLIENT OFFICE                           
         ICM   RF,15,ACAPPR                                                     
         BZ    *+10                                                             
         MVC   BCPROPRF,0(RF)                                                   
*                                                                               
         IC    RE,BCJOBLEN         GET JOB RECORD                               
         BCTR  RE,0                                                             
         EX    RE,*+4                                                           
         MVC   ACTKACT(0),TMKJOB                                                
         GOTO1 AGETACT,0                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
         ICM   RF,15,ACAPPR                                                     
         BZ    *+10                                                             
         MVC   BCJOBPRF,0(RF)                                                   
*                                                                               
         GOTO1 ACMPPRF             SAVE 1C ACCOUNT                              
         MVC   TMK1CACT,BCCMPPRF+(PPRCOSTA-PPRELD)                              
         CLI   TMK1CACT,C' '                                                    
         BH    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   BCPRFS(BCPRFSL),XTPRFS                                           
         B     EXITY                                                            
         SPACE 1                                                                
         POP   USING                                                            
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CONVERT CHARACTER FORM MOS TO PACKED MOA                 *         
*                                                                     *         
* NTRY: P1=A(INPUT CHARACTER MOS)                                     *         
*       P2=A(OUTPUT PACKED MOA)                                       *         
***********************************************************************         
         SPACE 1                                                                
CONMOS   NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
*                                                                               
         MVC   0(1,R3),0(R2)       CONVERT YEAR                                 
         NI    0(R3),X'0F'                                                      
         MVC   XTBYTE,BCTMONP                                                   
         NI    XTBYTE,X'F0'                                                     
         OC    0(1,R3),XTBYTE                                                   
*                                                                               
         MVC   1(1,R3),1(R2)       CONVERT MONTH                                
         NI    1(R3),X'0F'                                                      
         CLI   1(R2),C'0'                                                       
         BNL   CONMOSX                                                          
         OI    1(R3),X'10'                                                      
         IC    RE,1(R3)                                                         
         BCTR  RE,0                                                             
         STC   RE,1(R3)                                                         
*                                                                               
CONMOSX  B     EXIT                                                             
         EJECT                                                                  
         LTORG                                                                  
FF       EQU   X'FF'                                                            
XACCMST  DC    C'ACCMST '                                                       
UL1C     DC    C'1C'                                                            
UL1R     DC    C'1R'                                                            
         EJECT                                                                  
***********************************************************************         
* XFRTMS LOCAL W/S                                                    *         
***********************************************************************         
         SPACE 1                                                                
XTWORKD  DSECT                                                                  
*                                                                               
XTPARMS  DS    0XL12                                                            
XTPACTN  DS    0XL1                ACTION REQUIRED                              
XTLOCKQ  EQU   C'L'                LOCK CLUSTER ONLY                            
XTPATRN  DS    A                   A(TRANSACTION RECORD)                        
XTPTYPE  DS    0XL1                ACTIVITY TYPE - T=XFR W=WOFF                 
XTPAPTA  DS    A                   A(PTA ELEMENT)                               
XTPAPOST DS    A                   A(POSTVALS BLOCK)                            
*                                                                               
XTHRS    DS    D                   NO. OF HOURS TO TRANSFERRED/WRITEOFF         
XTNET    DS    PL8                 NET TRANSFERRED/WRITTEN OFF                  
XTTRNKEY DS    XL(L'TRNKEY)        TRANSACTION KEY                              
XTTRNEL  DS    XL(TRNLN1Q)         TRANSACTION ELEMENT                          
XT1CACT  DS    CL12                CLIENT COSTING ACCOUNT                       
XTLINE#  DS    XL2                 TRANSACTION LINE NUMBER                      
XTWOFMOA DS    XL2                 WRITE-OFF MOA FOR RECOVERY USE               
XTRATE   DS    PL4                 HOURLY RATE                                  
XTINCAC  DS    CL14                INCOME ACCOUNT                               
XTNARR   DS    CL100               NARRATIVE                                    
XTPRFS   DS    XL(BCPRFSL)         SAVED COMPOSITE JOB PROFILES                 
XTPTAEL  DS    XL(PTATLN2Q)        SAVED PTAELS                                 
XTPIDEL  DS    XL(PIDLNQ)          SAVED PIDEL                                  
XTOFFICE DS    CL2                 CLIENT OFFICE CODE                           
*                                                                               
XTPL16   DS    PL16                                                             
XTHALF   DS    H                                                                
XTBYTE   DS    XL1                                                              
*                                                                               
GTTIMINP DS    XL(TIMILNQ)         GETTIM ROUTINE ELEMENT 'KEY'                 
ATOLDHRS DS    PL3                 ADJTIM ROUTINE OLD NUMBER OF HOURS           
ATNEWHRS DS    PL3                 ADJTIM ROUTINE NEW NUMBER OF HOURS           
ATOLDNET DS    PL8                 ADJTIM ROUTINE OLD NET AMOUNT                
ATNEWNET DS    PL8                 ADJTIM ROUTINE NEW NET AMOUNT                
*                                                                               
XTFROM   DS    XL500               AREA FOR FROM TMBLK                          
XTTO     DS    XL500               AREA FOR TO TMBLK                            
XTELEM   DS    XL(SCILN2Q+1)       ELEMENT BUILD AREA FOR POSTING               
XTSAVIO1 DS    XL(IOAREA2-IOAREA1) AREA TO SAVE USER'S IO1                      
*                                                                               
XTWORKL  EQU   *-XTWORKD                                                        
         EJECT                                                                  
***********************************************************************         
* TIME MANAGEMENT SYSTEM BLOCK                                        *         
***********************************************************************         
         SPACE 1                                                                
TMBLKD   DSECT                                                                  
TMKEY    DS    0XL(TMKEYL)         * RECORD/CLUSTER KEY *                       
TMK1RACT DS    CL12                1R ACCOUNT                                   
TMKCOFF  DS    CL2                 CLIENT OFFICE                                
TMK1CACT DS    CL12                1C ACCOUNT                                   
TMKPEDT  DS    PL3                 PERIOD EFFECTIVE DATE                        
TMKEYRL  EQU   *-TMKEY             LENGTH OF RECORD KEY                         
TMKJOB   DS    CL12                SJ ACCOUNT                                   
TMKWC    DS    CL2                 WORK CODE                                    
TMKMOA   DS    PL2                 MONTH OF ACTIVITY                            
TMKIND   DS    XL1                 TIME INDICATOR                               
TMKLINE# DS    XL2                 LINE NUMBER                                  
TMKHISEQ DS    XL1                 HIGHEST ELEMENT SEQUENCE NUMBER              
TMKEYL   EQU   *-TMK1RACT                                                       
*                                                                               
TMDATA   DS    0XL(TMDATAL)                                                     
TMDINDS  DS    XL1                 INDICATOR BYTE                               
TMDIROF  EQU   X'80'               RECORD ON FILE                               
TMDIDEL  EQU   X'40'               RECORD IS DELETED                            
TMDIRES  EQU   X'20'               RECORD NEEDS TO BE RESTORED                  
TMDICLUD EQU   X'10'               CLUSTER DELETED FROM RECORD                  
TMD1CNAM DS    CL36                NAME OF 1C ACCOUNT                           
TMDSBR   DS    XL1                 SUB-REFERENCE                                
TMDSEQ   DS    XL1                 ELEMENT SEQUENCE NUMBER                      
TMDATIM  DS    AL4                 A(TIMEL CLUSTER IN RECORD)                   
TMDCLEN  DS    XL2                 CLUSTER LENGTH                               
TMDATAL  EQU   *-TMDINDS                                                        
*                                                                               
TMTIMELS DS    0X                  TIMELS BLOCK                                 
         EJECT                                                                  
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* DDCOREQUS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOREQUS                                                      
         PRINT ON                                                               
         SPACE 1                                                                
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
         SPACE 1                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
         SPACE 1                                                                
         EJECT                                                                  
         PRINT ON                                                               
* ACCLBWORK                                                                     
       ++INCLUDE ACCLBWORK                                                      
         SPACE 1                                                                
CLB43    CSECT                     ALLOW EASIER RE-LOAD OF PHASE                
         ORG   CLB43+(((*-CLB43)/512)+1)*512                                    
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009ACCLB43S  12/21/99'                                      
         END                                                                    
