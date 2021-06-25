*          DATA SET SPOMS0A    AT LEVEL 122 AS OF 03/14/16                      
*PHASE T2340AC                                                                  
***********************************************************************         
* *** NOTE ***                                                                  
* THIS VERSION OF SPOMS0A IS ONLY NEED TO RE-USE DELETED LAST METHOD            
* RECORDS THAT WE MANUALLY DELETED.                                             
***********************************************************************         
T2340A   TITLE 'SPOMS0A - OMS - MARKET LIST'                                    
T2340A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,*T2340A*,R7,RR=R3                                              
*                                                                               
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T234FFD,RA                                                       
         L     R9,ASYSD                                                         
         USING SYSD,R9                                                          
         LA    R8,SYSSPARE                R8=A(LOCAL STORAGE AREA)              
         USING LSSD,R8                                                          
*                                                                               
         L     R1,SYSPARMS                                                      
         L     R1,16(R1)           A(COMFACS)                                   
         L     RF,CCALLOV-COMFACSD(R1)    SETUP SQUASHER                        
         LA    R1,DMCB                                                          
         MVC   DMCB+4(4),=X'D9000A0D'                                           
         GOTO1 (RF),(R1),0                                                      
         CLI   DMCB+4,X'FF'                                                     
         BNE   *+6                                                              
         DC    H'0'                                                             
         MVC   VSQUASH,DMCB                                                     
*                                                                               
         OI    GENSTAT1,RDUPAPPL          SETTING RDUPDATE                      
         MVI   IOOPT,C'Y'                                                       
         MVI   RDUPDATE,C'N'              ASSUME NO                             
         OI    GLSTSTAT,APPLCDSP+RETEXTRA+CHNGLIST+EMPTYLIN                     
         OI    GENSTAT5,SEL1BYTE          1 BYTE SELECT FIELD                   
*                                                                               
         NI    DMINBTS,ALL-(X'08')        DON'T PASS BACK DELETES               
*                                                                               
         BAS   RE,SETUP                                                         
         BAS   RE,GETPF                   GET PFKEYS                            
         ST    RC,BASERC                                                        
         ST    R3,RELO                                                          
         BAS   RE,SETUP                                                         
*                                                                               
         L     RF,ACOMFACS                                                      
         MVC   VGLOBBER,CGLOBBER-COMFACSD(RF)                                   
*                                                                               
         LA    R3,RELOTAB                                                       
INIT3    CLI   0(R3),X'FF'                                                      
         BE    INIT5                                                            
         ICM   RF,15,0(R3)                                                      
         A     RF,RELO                                                          
         ICM   RE,15,4(R3)                                                      
         LA    RE,LSSD(RE)                                                      
         STCM  RF,15,0(RE)                                                      
         LA    R3,L'RELOTAB(R3)                                                 
         B     INIT3                                                            
*                                                                               
INIT5    CLI   MODE,VALKEY                VALIDATE KEY?                         
         BNE   INIT7                                                            
         BRAS  RE,VK                                                            
         TM    PCF,PCFKYCHG+PCFNEXTK      KEY CHANGE OR NEXT SCREEN             
         JNZ   XIT                                                              
*                                                                               
         LA    R3,LSTTAB                  TEST FIRST KEY DELETED?               
         USING LSTD,R3                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),LSTBTK                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'DBTKEY),KEYSAVE      NO, OK TO DISPLAY                     
         JE    XIT                                                              
         OI    MKTMEDH+4,FINPTHIS         YES, FORCE NEW START                  
         J     XIT                                                              
*                                                                               
INIT7    CLI   MODE,LISTKEY               EXAMINE LIST KEY?                     
         BE    LSTKEY                                                           
         CLI   MODE,LISTRECS              LIST RECORDS?                         
         BE    LST                                                              
         CLI   MODE,LVALREC               VALIDATE LISTED RECORD?               
         JNE   XIT                                                              
         B     VALR                                                             
         EJECT                                                                  
***********************************************************************         
* EXAMINE LIST KEY?                                                             
***********************************************************************         
LSTKEY   L     R1,AIO                                                           
         USING DBTKEY,R1                                                        
         TM    DBTRSTAT,X'80'             BATCH RECORD DELETED?                 
         JZ    LSTKX                                                            
         DROP  R1                                                               
         OI    DMINBTS,X'08'              READ FOR DELETED                      
LSTKX    J     XIT                                                              
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* BUILD A LIST                                                        *         
***********************************************************************         
LST      MVC   AGYMD,BAGYMD               AGENCY/MEDIA TO LOCAL STORAGE         
         MVI   NLST,0                     AND IN  LIST                          
         TWAXC MKTSELH,MKTSENTH,PROT=Y                                          
*                                                                               
         OI    MKTPFLNH+(FLDOIND-FLDHDRD),FOUTTRN                               
         MVC   MKTPFLN(L'PFDATA1),PFDATA1 SET PF KEY TEXT                       
         LA    RF,MKTPFLN+L'PFDATA1+1                                           
         MVC   0(L'PFDATA2,RF),PFDATA2                                          
         OI    MKTSENTH+(FLDOIND-FLDHDRD),FOUTTRN                               
         CLI   NSENT,0                     ANY SENT ?                           
         BE    LST00A                                                           
         MVC   MKTSENT(5),=C'Sent='                                             
         EDIT  (B1,NSENT),(2,MKTSENT+5),ALIGN=LEFT                              
*                                                                               
LST00A   TM    PCF,PCFMIS                 GOING TO MIS/BROWSE?                  
         BO    LST01A                     YES                                   
         TM    PCF2,PCFMIS2               RETURNING FROM MIS/BROWSE?            
         BNO   LST00B                     NO                                    
         NI    PCF,ALL-(PCFNEXTK)         REDISPLAY THE SAME LIST               
         NI    PCF2,ALL-(PCFMIS2)         TURNOFF COMING FROM MIS/BROWS         
         NI    TRNSTAT,X'FF'-(RACHANG)    GLOBBER/MULTI SELECT PROB             
         TM    PCF2,PCFBRO                COMING FROM BROWSE?                   
         BNO   *+8                        NO                                    
         OI    PCF2,PCFBRO2               YES, INDICATE IT                      
         B     LST01                                                            
*                                                                               
LST00B   TM    PCF,PCFKYCHG+PCFREFSH      KEY CHANGE/REFRESH ?                  
         BNZ   LST03                      YES, START WITH FIRST KEY             
***                                                                             
* REFRESH DUE TO XMIT ON LST PG?                                                
***                                                                             
         TM    PCF2,PCFPG1                REFRESH?                              
         BNZ   LST03                      YES                                   
*                                                                               
         TM    PCF,PCFEOLST               END OF LIST?                          
         BZ    LST00C                     NOPE                                  
         TM    PCF2,PCFSWAP               JUST COME FROM OTHER OVERLAY?         
         BZ    LST03                      NO, START WITH FIRST KEY              
*                                                                               
LST00C   TM    PCF,PCFNEXTK               START WITH NEXT KEY?                  
         BNO   LST01                                                            
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),BATCHKEY     SET LAST KEY                          
         NI    PCF,ALL-(PCFNEXTK)                                               
         B     LST05                                                            
*                                                                               
LST01A   NI    PCF,ALL-(PCFMIS)           TURNOFF GOING TO MIS                  
LST01    LA    R3,LSTTAB                  REDISPLAY THE CURRENT SCREEN          
         USING LSTD,R3                                                          
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),LSTBTK                                             
         MVC   NXTFLT,LSTFLT                                                    
         B     LST05                                                            
*                                                                               
LST03    MVI   PCF,0                                                            
         BAS   RE,FSTK                    SET FIRST KEY                         
*                                                                               
LST04    MVI   NXTFLT,0                   CLEAR NEXT FLIGHT NUMBER              
LST05    MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
*                                                                               
LST06    XR    R1,R1                      TEST MINIMUM KEY                      
         IC    R1,KYLN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         BNE   LST25                      ALL DONE                              
***NOP   CLI   KEY+DBTKSTA-DBTKEY,X'E8'   CHECK IF CABLE                        
***NOP   BNL   LST22                      YES, READ SEQ                         
*                                                                               
         BAS   RE,FLTRK            FILTER THE KEY                               
         BNE   LST04               IF NO  - KEY HAS BEEN BUMPED                 
         MVC   BATCHKEY,KEY                                                     
         MVC   BEST,KEY+(DBTKEST-DBTKEY)  SAVE ESTIMATE NUMBER                  
         MVC   DAD,KEY+(DBTKDA-DBTKEY)    SAVE DISK ADDRESS OF BATCH            
         MVI   RDUPDATE,C'N'                                                    
         MVC   AIO,AIO1                                                         
         GOTO1 GETREC              GET THE BATCH RECORD                         
*                                                                               
         MVC   BYTE,KEY+(DBTKPRD-DBTKEY)                                        
         LA    R1,QPRD                                                          
         BAS   RE,GETPRD           GET PRODUCT CODE                             
*                                                                               
         BRAS  RE,GETFLT           BUILD LIST OF FLIGHTS                        
         MVI   MISCFLG2,0                                                       
*                                                                               
LST07    L     R6,AIO1             R6=A(BATCH RECORD)                           
         MVI   ELCODE,DBFLTELQ     FIND X'20' FLIGHT ELEMENTS                   
         USING DBFLTELD,R6                                                      
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*&&DO                                                                           
         BE    LST08A                                                           
         LA    R5,FLITAB           FIND FLIGHT ENTRY IN TABLE                   
         USING FLITD,R5                                                         
LST07A   XC    FLITD(FLITDAK-FLITD),FLITD                                       
         LA    R5,FLITLNQ(R5)                                                   
         CLI   0(R5),EOT                                                        
         BNE   LST07A                                                           
         B     LST11               NO FLIGHT ELEMENTS, SHOW 0                   
         DROP  R5                                                               
*&&                                                                             
*                                                                               
LST08    BRAS  RE,NEXTEL                                                        
         BNE   LST11                                                            
LST08A   LA    R5,FLITAB           FIND FLIGHT ENTRY IN TABLE                   
         USING FLITD,R5                                                         
LST08B   TM    PCF,PCFFLGHT        FLIGHT FOUND?                                
         BZ    LST08E              NO                                           
         CLI   DBFLTFLT,0                                                       
         BNE   *+8                                                              
         MVI   DBFLTFLT,C'0'                                                    
*                                                                               
LST08E   CLC   DBFLTFLT,FLITNUM                                                 
         BNE   LST08H                                                           
*                                                                               
         MVC   BYTE,DBFLTFL1       XOR THE BOTH FLAG FIELDS AND                 
         XC    BYTE,FLITFLG        SEE WHAT FLAGS/BITS MATCHED                  
         TM    BYTE,DBFLTTRD       DO WE MATCH ON CASH/TRADE X'04'              
         BZ    LST09               YES, WE MATCHED                              
*                                                                               
LST08H   LA    R5,FLITLNQ(R5)                                                   
         CLI   0(R5),EOT                                                        
         BNE   LST08B                                                           
         B     LST08               BATCH NOT IN FLT TAB, OK                     
*                                                                               
LST09    MVC   FLITFLG,DBFLTFL1    SAVE BATCH FLAG                              
         TM    DBFLTFL1,DBFLTSNT   ALREADY SENT ?                               
         BO    LST08                                                            
         TM    DBFLTFL1,DBFLTDAR   LINKED TO DARE ?                             
         BO    LST10                                                            
*                                                                               
         LA    R1,DBFLTOVH         OM V1.0 OVERHEAD BEFORE COMMENT              
         TM    DBFLTFL1,DBFLTBSC   OM V2.0?                                     
         BNO   *+14                                                             
         MVC   FLITSTD,DBFLTSTD    SAVE WHAT'S HERE FOR STDCMT                  
         LA    R1,DBFLTLNQ         OM V2.0 OVERHEAD BEFORE COMMENT              
*                                                                               
         CLM   R1,1,DBFLTLEN       TEST ANY FREE FORM COMMENTS                  
         BNL   LST08                                                            
*                                                                               
         XR    RF,RF                      RF=GET LENGTH OF TEXT                 
         IC    RF,DBFLTLEN                                                      
         AHI   RF,-(DBFLTOVH)             LESS MINIMUM OVERHEAD                 
         LA    R1,DBFLTTXT                R1=A(START OF TEXT)                   
         TM    DBFLTFL1,DBFLTBSC          TEST OM V2.0                          
         BNO   *+12                       NOPE, OM V1.0                         
         AHI   RF,-(L'DBFLTSTD)                                                 
         LA    R1,DBFLTFFT                                                      
*                                                                               
         LTR   RF,RF                                                            
         BNP   LST08                                                            
         LA    R0,L'FLITFFT               LENGTH OF FIELD                       
         CR    RF,R0                                                            
         BL    *+6                                                              
         LR    RF,R0                      USE SHORTER                           
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLITFFT(0),0(R1)                                                 
         B     LST08                                                            
***                                                                             
* READING THE DARE ORDER REC HERE, MIGHT AS WELL UPDATE SALESPERSON             
* IF WE JUST CAME BACK FROM BROWSE WITH A SALESPERSON                           
***                                                                             
LST10    GOTO1 =A(GDARE),DMCB,BATCHKEY,DBFLTEL,FLITD,RR=RELO                    
         MVI   ELCODE,DBFLTELQ            RESET ELEMENT CODE                    
         B     LST08                                                            
*                                                                               
LST11    XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),BATCHKEY     RESTORE BATCH KEY                     
*                                                                               
LST12    BAS   RE,SPDL                    GET SPOTS & DOLLARS                   
         MVC   AIO,AIO1                   RESTORE AIO                           
         LA    R5,FLITAB                  ADD FLIGHT DATA TO LIST               
*                                                                               
LST13    CLI   NXTFLT,0                   STARTING FLIGHT NUMBER ?              
         BE    LST13A                                                           
         MVC   HALF(1),FLITNUM                                                  
         NI    HALF,X'0F'                                                       
         MVC   HALF+1(1),NXTFLT                                                 
         NI    HALF+1,X'0F'                                                     
         CLC   HALF(1),HALF+1             SKIP FLIGHTS ALREADY LISTED           
         BL    LST19                                                            
*                                                                               
LST13A   TM    OPT,OPTSHSNT               SHOW SENT                             
         BO    *+12                        YES, SKIP THIS CHECK                 
         TM    FLITFLG,DBFLTSNT           ALREADY SENT ?                        
         BO    LST19                                                            
*                                                                               
         TM    OPT,OPTSH0                 SHOW 0 DOLLARS                        
         BO    *+14                        YES, SKIP THIS CHECK                 
         OC    FLITSPT,FLITSPT            ANY SPOTS ON THIS FLIGHT ?            
         BZ    LST19                                                            
*                                                                               
         XR    R3,R3                      ADD ITEM TO THE LIST                  
         IC    R3,NLST                    INCREMENT NUMBER IN LIST              
         AHI   R3,1                                                             
         STC   R3,NLST                                                          
         AHI   R3,-(1)                                                          
         MHI   R3,LSTLNQ                  R3=A(NEXT ITEM IN LIST)               
         LA    R3,LSTTAB(R3)                                                    
         USING LSTD,R3                                                          
         XC    LSTD(LSTLNQ),LSTD                                                
         MVC   LSTBTK,BATCHKEY            BATCH RECORD KEY                      
         MVC   LSTBTDA,DAD                DISK ADDRESS OF BATCH                 
         MVC   LSTFLT,FLITNUM             FLIGHT NUMBER                         
         MVC   LSTFLAG,FLITFLG            FLAG                                  
*                                                                               
         TM    FLITFLG2,FLIT2LNG          COMMENT LONGER THAN CAN FIT?          
         BZ    *+8                                                              
         OI    LSTIND,LSTI2LNG            YES, NOT THE SAME BIT                 
*                                                                               
         MVC   LSTSPTS,FLITSPT            SPOTS                                 
         MVC   LSTDLRS,FLITDLR            DOLLARS                               
         MVC   LSTBUYR,FLITBYR            BUYER                                 
         MVC   LSTDAK,FLITDAK             DARE KEY                              
         MVC   LSTSTD,FLITSTD             STANDARD COMMENT                      
         MVC   LSTFFT,FLITFFT             FREE FORM TEXT                        
***                                                                             
* IF THERE IS NO STD COMMENT DONT DISPLAY IT (IT WILL BE X'25' ELEM)            
**                                                                              
         CLI   FLITSTD,X'25'              BEGINNING OF X'25' ELEMENT?           
         BE    LST18                      YES                                   
         MVC   LSTSTD,FLITSTD             STANDARD COMMENT                      
         MVC   LSTFFT,FLITFFT             FREE FORM TEXT                        
         B     LST18                                                            
*                                                                               
LST14    XC    LSTSTD,LSTSTD                                                    
         XC    LSTFFT,LSTFFT                                                    
*                                                                               
LST18    CLI   NLST,NLSTQ                 TEST LIST IS FULL                     
         BE    LST23                      YES, TIME TO DISPLAY                  
*                                                                               
LST19    LA    R5,FLITLNQ(R5)                                                   
         CLI   0(R5),EOT                                                        
         BNE   LST13                                                            
*                                                                               
LST21    XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),BATCHKEY     RESTORE BATCH KEY                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
LST22    GOTO1 SEQ                        AND GET NEXT                          
         MVI   NXTFLT,0                   CLEAR NEXT FLIGHT                     
         B     LST06                                                            
*                                                                               
LST23    OI    PCF,PCFNEXTK               SET TO START FROM LAST KEY            
         MVC   NXTFLT,LSTFLT                                                    
*                                                                               
LST25    CLI   NLST,0                     ANY DATA ?                            
         JE    NODATAR                    NO DATA TO DISPLAY                    
*                                                                               
         BAS   RE,DSPLY                   DISPLAY THE LIST                      
*                                                                               
         TM    PCF2,PCFBRO2               RETURNING FROM BROWSE?                
         BZ    LST30                      NO                                    
*                                                                               
         L     R1,ATIOB                   SET CURSOR FOR ERROR                  
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         MVC   TIOBCURD-TIOBD(L'TIOBCURD,R1),BRWCUR                             
*                                                                               
         TM    PCF2,PCFBRO3               READ ORDER REC?                       
         BZ    LST26                      NO                                    
*                                                                               
         TM    PCF2,PCFSUPD               DID WE UPDATE SALESPERSON?            
         BZ    *+12                       NO                                    
*                                                                               
LST26    TM    PCF2,PCFBATCH              DID WE UPDATE THE BATCH REC?          
         BO    LST27                      YES                                   
*                                                                               
         NI    PCF2,ALL-PCFBRO-PCFBRO2-PCFBRO3-PCFSUPD-PCFBATCH-PCFPG1          
         NI    PCF3,ALL-PCFLASTM                                                
         J     NOSALES                    NO                                    
*                                                                               
LST27    NI    PCF2,ALL-PCFBRO-PCFBRO2-PCFBRO3-PCFSUPD-PCFBATCH-PCFPG1          
         NI    PCF3,ALL-PCFLASTM                                                
*                                                                               
LST30    NI    PCF2,ALL-PCFSWAP-PCFPG1                                          
         NI    PCF,ALL-(PCFEOLST)                                               
         TM    PCF,PCFNEXTK                                                     
         JNZ   MAKESELS                   LIST DISPLAYED..                      
         OI    PCF,PCFEOLST               SET TO REFRESH                        
         J     EOFLSELS                   END OF LIST...                        
LSTXIT   NI    PCF,ALL-(PCFMIS)                                                 
         DROP  R3,R5,R6                                                         
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* ROUTINE TO GET THE SPOTS AND DOLLARS                                *         
***********************************************************************         
SPDL     NTR1  ,                                                                
         MVC   AIO,AIO3                                                         
         LA    R2,BATCHKEY                                                      
         USING DARBTCHD,R2                                                      
         XC    KEY,KEY                                                          
         LA    R4,KEY              BUILD BUY KEY                                
         USING BUYKEY,R4                                                        
         MVC   BUYKAM,DBTKAGMD     AGY/MEDIA                                    
         MVC   BUYKCLT,DBTKCLT     CLIENT                                       
         MVC   BUYKPRD,DBTKPRD     PRODUCT                                      
         MVC   BUYMSTA,DBTKMKT     MARKET                                       
         CLI   BUYKSTA,X'E8'       CABLE?                                       
         BNL   SPDL00                                                           
         MVC   BUYKEST,DBTKEST     ESTIMATE                                     
*                                                                               
SPDL00   MVI   RDUPDATE,C'N'                                                    
SPDL01   GOTO1 HIGH                                                             
*                                                                               
SPDL05   DS    0H                                                               
         CLI   KEYSAVE+BUYKSTA-BUYKEY,X'E8'   CHECK IF READING CABLE            
         BL    SPDL08                                                           
         CLC   KEY(BUYKSTA-BUYKEY),KEYSAVE   MED/CLT/PRD/MKT                    
         JNE   XIT                                                              
         MVC   FULL(L'BUYKSTA),BUYKSTA                                          
         NI    FULL+2,X'80'                                                     
         CLC   DBTKSTA,FULL            MATCH SYS?                               
         JNE   XIT                                                              
         CLC   BUYKEST,DBTKEST                                                  
         BE    SPDL09                                                           
         BH    SPDL06                                                           
         MVC   BUYKEST,DBTKEST                                                  
         B     SPDL01                                                           
SPDL06   MVI   BUYKEST,X'FF'                                                    
         B     SPDL01                                                           
*                                                                               
SPDL08   CLC   KEY(BUYKBUY-BUYKEY),KEYSAVE                                      
         JNE   XIT                 ALL DONE                                     
SPDL09   CLI   BUYKBUY,POLQ        POL BUYING?                                  
         BNE   SPDL10                                                           
         XC    BUYKBUY,BUYKBUY                                                  
         MVI   BUYKPRD,POLQ        GET ALL THE POL BUYLINES                     
         B     SPDL01                                                           
*                                                                               
SPDL10   MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
B        USING BUYRECD,R6                                                       
*                                                                               
         MVI   DAREFLG1,0                                                       
         OC    PDARTSRP(3),PDARTSRP ANY DARE SPECIAL REP IN PROFILE?            
         BZ    SPDL12               NO, NOT A TRADE BUY                         
         CLC   PDARTSRP(3),=C'000'                                              
         BE    SPDL12               NO, NOT A TRADE BUY                         
         OC    B.BDREP,B.BDREP      ANY SPECIAL REP ON BUYLINE?                 
         BZ    SPDL12                                                           
         GOTO1 RCPACK,DMCB,(C'U',B.BDREP),DUB                                   
         CLC   PDARTSRP(3),DUB     IF IT MATCHES THEN TRADE                     
         BNE   SPDL12                                                           
         OI    DAREFLG1,DF1TRADE   YES, PROCESS THE TRADE ORDER                 
         TM    OPT,OPTCASH         FILTERING ONLY FOR CASH?                     
         B     SPDL12A                                                          
         DROP  B                                                                
*                                                                               
SPDL12   TM    OPT,OPTTRADE        FILTERING ONLY FOR TRADE?                    
SPDL12A  BO    SPDL17              YES, SKIP THIS BUY                           
*                                                                               
SPDL14   LA    R6,BDELEM-BUYREC(R6)                                             
SPDL15   CLI   0(R6),0             TEST EOR                                     
         BNE   SPDL20                                                           
SPDL17   MVI   RDUPDATE,C'N'       YES, GET NEXT RECORD                         
         GOTO1 SEQ                                                              
         B     SPDL05                                                           
*                                                                               
         USING REGELEM,R6                                                       
SPDL20   CLI   RCODE,RCORGQ        X'06' ORIGINAL                               
         BE    SPDL30                                                           
         CLI   RCODE,RCOTOQ        X'07' OTO                                    
         BE    SPDL30                                                           
         CLI   RCODE,RCPOLOQ       X'0B' POOL ORIGINAL                          
         BE    SPDL30                                                           
         CLI   RCODE,RCPOTOQ       X'0C' POOL OTO                               
         BE    SPDL30                                                           
SPDL25   XR    R0,R0                                                            
         IC    R0,RLEN                                                          
         AR    R6,R0                                                            
         B     SPDL15                                                           
*                                                                               
SPDL30   TM    RSTATUS,RSMINUSQ+RSMINSDQ  SKIP MINUS OR MISSED                  
         BNZ   SPDL25                                                           
         CLI   RCODE,RCPOLOQ                                                    
         BL    SPDL50              BRANCH IF 'REGULAR' BUYS                     
*                                                                               
*                                  ** POOL BUYS **                              
         CLI   RLEN,RPALLOC-REGELEM   ANY PRODUCT ALLOCATION?                   
         BH    SPDL35              YES                                          
         CLI   DBTKPRD,POLQ        POL BATCH RECORD ?                           
         BNE   SPDL25                                                           
         B     SPDL50                                                           
*                                                                               
SPDL35   CLI   DBTKPRD,POLQ        POL BATCH RECORD ?                           
         BE    SPDL50              YES, ADD IT IN                               
         CLC   DBTKPRD,RPPRD       MATCH PRODUCT                                
         BE    SPDL40                                                           
         CLI   RLEN,RLPOL1LQ       ANY PB ALLOCATION?                           
         BNH   SPDL25              NO                                           
         CLI   DBTKPRD2,0          YES, THIS ENTRY ALSO?                        
         BE    SPDL25              NO                                           
         CLC   DBTKPRD,RPPRD+L'RPALLOC    MATCH AGAINST PB?                     
         BNE   SPDL25                                                           
         CLC   DBTKPRD2,RPPRD      YES, NTRY PB MATCH PRD?                      
         BNE   SPDL25                                                           
         B     SPDL50                                                           
*                                                                               
SPDL40   CLI   RLEN,RLPOL1LQ       ANY PB ALLOCATION?                           
         BH    SPDL45              YES                                          
         CLI   DBTKPRD2,0          NO, NONE FOR NTRY ALSO?                      
         BNE   SPDL25                                                           
         B     SPDL50                                                           
*                                                                               
SPDL45   CLI   DBTKPRD2,0                                                       
         BE    SPDL25                                                           
         CLC   DBTKPRD2,RPPRD+L'RPALLOC   MATCH AGAINST PB?                     
         BNE   SPDL25                                                           
*                                                                               
SPDL50   LA    R5,FLITAB           R5=A(FLIGHT TABLE)                           
         USING FLITD,R5                                                         
SPDL55   OC    FLITBYR,FLITBYR                                                  
         BZ    SPDL60                                                           
         OC    BUYR,BUYR           HAVE KEY BUYER?                              
         BZ    SPDL60                                                           
         CLC   FLITBYR,BUYR        SAME BUYER AS KEY FIELD?                     
         BE    SPDL60                                                           
         XC    FLITSPT,FLITSPT     NO, FORCE NO SPOTS, SO DON'T LIST            
         B     SPDL70                                                           
*                                                                               
SPDL60   MVC   BYTE,DAREFLG1       XOR THE BOTH FLAG FIELDS AND                 
         XC    BYTE,FLITFLG        SEE WHAT FLAGS/BITS MATCHED                  
         TM    BYTE,DBFLTTRD       DO WE MATCH ON CASH/TRADE X'04'              
         BNZ   SPDL70              NO                                           
*                                                                               
SPDL62   CLC   FLITEND,FFS         TEST NON-FLIGHT                              
         BE    SPDL65              NO, OK TO ADD IN                             
         GOTO1 DATCON,DMCB,(2,RDATE),(3,FULL)                                   
         CLC   FULL(L'FLITSTR),FLITSTR     TEST DATES IN RANGE                  
         BL    SPDL70                                                           
         CLC   FULL(L'FLITEND),FLITEND                                          
         BH    SPDL70              NO                                           
*                                                                               
SPDL65   XR    R1,R1               TOTAL NUMBER OF SPOTS                        
         ICM   R1,3,FLITSPT                                                     
         XR    R0,R0                                                            
         IC    R0,RNUM                                                          
         CLI   RCODE,RCPOLOQ                                                    
         BL    *+8                 BRANCH IF 'REGULAR' BUYS                     
         LA    R0,1                ADD 1 SPOT FOR 'POL'                         
         AR    R1,R0                                                            
         STCM  R1,3,FLITSPT                                                     
         MVC   BYTE,DBTKPRD                                                     
         BAS   RE,GETDLRS          GET COST AND ADD TO TOTAL                    
*                                                                               
         CLI   RCODE,RCPOLOQ                                                    
         BL    SPDL70              BRANCH IF 'REGULAR' BUYS                     
         CLI   DBTKPRD2,0                                                       
         BE    SPDL70                                                           
         MVC   BYTE,DBTKPRD2       GET COST OF PB                               
         BAS   RE,GETDLRS                                                       
*                                                                               
SPDL70   LA    R5,FLITLNQ(R5)      NEXT FLIGHT ENTRY                            
         CLI   0(R5),EOT                                                        
         BE    SPDL25              NEXT ELEMENT                                 
         B     SPDL55              NEXT FLIGHT                                  
*                                                                               
GETDLRS  LR    R0,RE               GET COST FOR SPOTS                           
         XR    RF,RF                                                            
         IC    RF,SVAPROF+7                                                     
         GOTO1 GETRATE,DMCB,(BYTE,WORK),(0,AIO),(0,RCODE),((RF),0)              
         ICM   R1,15,WORK+4        UPDATE TOTALS DOLLARS                        
         ICM   RF,15,FLITDLR                                                    
         AR    RF,R1                                                            
         STCM  RF,15,FLITDLR                                                    
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO DISPLAY THE LIST                                         *         
***********************************************************************         
DSPLY    NTR1  ,                                                                
         LA    R2,MKTSELH                 R2=SCREEN                             
         USING SCRND,R2                                                         
         LA    R3,LSTTAB                  R3=LIST                               
         USING LSTD,R3                                                          
         SR    R5,R5                      R5=NUMBER TO DISPLAY                  
         ICM   R5,1,NLST                  NUMBER IN LIST                        
         CHI   R5,NSCRQ                   TEST MAX ON SCREEN                    
         BNH   *+8                                                              
         BCT   R5,*-8                     REDUCE, UNTIL IT FITS                 
         STC   R5,NLISTS                  SET NUMBER TO LIST                    
         STC   R5,NSCRN                   NUMBER ON SCREEN                      
*                                                                               
DSPLY3   LA    R4,LSTBTK                  R4=BATCH KEY                          
         USING DARBTCHD,R4                                                      
         GOTO1 MSUNPK,DMCB,(X'80',DBTKMKT),FULL,SCRNSTA                         
         MVC   BYTE,DBTKPRD                                                     
         LA    R1,SCRNPRD                                                       
         BAS   RE,GETPRD                  PRODUCT CODE                          
         CLI   DBTKPRD2,0                                                       
         BE    DSPLY5                                                           
         MVC   BYTE,DBTKPRD2                                                    
         MVI   SCRNPRD+3,C'/'                                                   
         LA    R1,SCRNPRD+4               SECOND PRODUCT                        
         BAS   RE,GETPRD                                                        
*                                                                               
DSPLY5   EDIT  (B1,DBTKEST),(3,SCRNEST),FILL=0  ESTIMATE                        
*&&DO                                                                           
* REMOVING FORCING FAX TO MEDIAOCEAN BECAUSE OF OOW  WHOA  2007-05-21           
         BRAS  RE,CHKMOSTA                                                      
*&&                                                                             
*                                                                               
         CLI   LSTFLT,0                                                         
         BE    DSPLY12                                                          
         MVC   SCRNFLT,=C'00'                                                   
         CLI   LSTFLT,C'0'                                                      
         BE    DSPLY12                                                          
         EDIT  (B1,LSTFLT),(2,SCRNFLT),FILL=0  FLIGHT                           
*                                                                               
DSPLY12  EDIT  (B2,LSTSPTS),(4,SCRNSPT)         SPOTS                           
         ICM   R1,15,LSTDLRS                                                    
         CVD   R1,DUB                                                           
         SRP   DUB,64-2,5                                                       
         EDIT  (P8,DUB),(10,SCRNDLR)            DOLLARS                         
*                                                                               
         OI    SCRNDSTH+6,X'80'                                                 
         NI    SCRNDSTH+1,X'FF'-X'20'     UNPROTECTED FIELD                     
*                                                                               
         TM    MISCFLG1,MF1FXRAD    FAXING ALL RADIO ORDERS?                    
         BZ    DSPLY15              NO, RADIO EDI  (REDI)                       
         OI    SCRNDSTH+1,X'20'           PROTECTED FIELD                       
*                                                                               
DSPLY15  MVC   SCRNFFT,LSTFFT                   FREE FORM TEXT                  
         MVC   SCRNSTC,=C'Stdcmt:'                                              
         MVC   SCRNORC,=C'Ordcmt:'                                              
         MVI   SCRNCMT,C'N'                                                     
*                                                                               
         LA    R6,OCOMBLK                 GET OCOM RECORDS                      
         XC    OCOMBLK,OCOMBLK                                                  
         USING OMCOMD,R6                                                        
         MVC   OMCABUFF,AIO3                                                    
         MVC   OMCLBUFF,=Y(LIOS)                                                
         MVC   OMCACOMF,ACOMFACS                                                
         MVC   OMCAGMD,BAGYMD                                                   
         MVC   OMCCLT,DBTKCLT                                                   
         MVC   OMCPRD,DBTKPRD                                                   
         MVC   OMCEST,DBTKEST                                                   
         MVC   OMCFLT,LSTFLT                                                    
         MVI   OMCACT,OMCACT_GET                                                
         GOTO1 SPOMCOM,OMCOMD                                                   
         OC    OMCCOUNT,OMCCOUNT          TEST ANY RECORDS                      
         BZ    *+8                                                              
         MVI   SCRNCMT,C'Y'                                                     
*                                                                               
         TM    LSTFLAG,DBFLTDAR           IS IT LINKED TO A DARE ORDER?         
         BZ    DSPLY20                    NO                                    
         MVI   SCRNLNK,C'*'                                                     
         CLI   TWAOFFC,C'*'               DDS TERMINAL?                         
         BNE   DSPLY20                                                          
         MVI   SCRNLNKH+1,X'20'           SHOW IT ON THE SCREEN                 
*                                                                               
DSPLY20  MVI   SCRNTYPE,C'C'                                                    
         TM    LSTFLAG,DBFLTTRD           IS IT TRADE?                          
         BZ    *+8                                                              
         MVI   SCRNTYPE,C'T'                                                    
*                                                                               
         CLI   PDARPORN,C'Y'              USES PREV/NEW?                        
         BNE   *+8                        NO...DO NOT MARK IN LIST              
         BAS   RE,MARKPORN                                                      
*                                                                               
         MVC   SCRNSTD,LSTSTD             STANDARD COMMENT(FROM BATCH)          
         OC    SCRNSTD,BLANKS                                                   
         MVC   SCRNFFT,LSTFFT             FREE FORM TEXT                        
         OC    SCRNFFT,BLANKS                                                   
*                                                                               
         TM    LSTIND,LSTI2LNG            COMMENT LONGER THAN CAN FIT?          
         BZ    *+8                                                              
         MVI   SCRNORC+L'SCRNORC-1,C'*'   GEEZ!                                 
*                                                                               
         MVC   DMDSKADD,LSTBTDA                                                 
         GOTO1 LISTMON                                                          
*                                                                               
         LA    R2,SCRNLNQ(R2)             NEXT SCREEN LINE                      
         LA    R3,LSTLNQ(R3)              NEXT LIST ITEM                        
         BCT   R5,DSPLY3                                                        
*                                                                               
         LA    R2,MKTSELH                 R2=SCREEN                             
         LA    R3,LSTTAB                  R3=LIST                               
         SR    R5,R5                      R5=NUMBER TO DISPLAY                  
         IC    R5,NSCRN                   NUMBER ON SCREEN                      
*                                                                               
DSPLY25  LA    R4,LSTBTK                  R4=BATCH KEY                          
DSPLY27  MVC   BSTA,DBTKSTA               GET THE ROUTE                         
         MVC   BCLT,DBTKCLT                                                     
         MVC   QSTA,SCRNSTA                                                     
         CLI   QSTA+4,C' '                                                      
         BH    *+10                                                             
         MVC   QSTA+4(1),QMED                                                   
*                                                                               
         CLI   QMED,C'R'           USING UNIQUE ID'S?                           
         BNE   DSPLY30                                                          
         TM    MISCFLG1,MF1FXRAD   FAX ALL RADIO ORDERS?                        
         BO    DSPLY30                                                          
         MVC   FLDH,SCRNROUH                                                    
         MVI   FLDH+5,L'QSTA                                                    
         XC    FLD,FLD                                                          
         MVC   FLD(L'QSTA),QSTA                                                 
         LR    R0,R2                                                            
         LA    R2,FLDH                                                          
         GOTO1 VALISTA                                                          
         LR    R2,R0                                                            
*                                                                               
DSPLY30  MVI   SCRNDSTH+5,0                                                     
         OI    SCRNDSTH+4,FINPVAL         SET VALIDATED                         
*                                                                               
*  GOING TO PASS A FAKE FIELD AND USE SQUASHER SO THE ROUTE WOULD...            
*   FIT IN THE SCRNROU FIELD                      MHC 06/03/02                  
         ST    R4,REGSAVE          SAVE OFF REGISTER 4                          
         MVC   FLDH,SCRNROUH                                                    
         MVI   FLDH,68             LENGTH OF HEADER + FIELD                     
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,DMCB+4           <=== WE'RE OVERWRITING 2ND PARAM             
*                                     ARE WE RADIO & IS PROFILE SET TO          
         CLI   SCRNPORN,C'P'                                                    
         BE    DSPLY32                                                          
*&&DO                                                                           
* REMOVING FORCING FAX TO MEDIAOCEAN BECAUSE OF OOW  WHOA  2007-05-21           
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
         TM    LSTFLAG2,LSTMOFAX   FAXING OOW TO MO?                            
         BO    DSPLY32             YES                                          
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
*&&                                                                             
         TM    MISCFLG1,MF1FXRAD   FAX ALL RADIO ORDERS?                        
         BNO   DSPLY35                                                          
DSPLY32  MVI   DMCB+4,X'80'        YES...CALL RD4ROUTE WITH HOB=X'80'           
*                                                                               
DSPLY35  GOTO1 RD4ROUTE,DMCB,SCRNDSTH,,(MISCFLG1,0),0                           
         BNE   DSPLY99             CHECK ERROR TYPE                             
*                                                                               
DSPLY40  CLI   QMED,C'R'           RADIO?                                       
         BNE   DSPLY50             NO, ONLY RADIO DISPLAYS SALESPERSON          
         BRAS  RE,HAVESLP          DO WE HAVE A SALPESPERSON ELEMENT?           
         JE    DSPLY60             YES, HAVESLP DISPLAYED IT                    
         BRAS  RE,LMSALES          DOES LAST METHOD HAVE SALESPERSON?           
         JE    DSPLY60             YES, LMSALES DISPLAYED IT                    
*                                                                               
DSPLY50  LA    R4,L'FLD            LENGTH OF FIELD (FOR SQUASHER)               
         CLC   =CL5'EML: ',FLD                                                  
         BNE   *+8                                                              
         BAS   RE,ADDCOMMA         ADD COMMA WHEN IT'S EMAIL ROUTE              
         GOTO1 VSQUASH,DMCB,FLD,(R4)   CALL TO SQUASHER                         
         MVC   SCRNROU,FLD         JUST MOVE ALL OF IT                          
DSPLY60  L     R4,REGSAVE          RESTORE REGISTER 4                           
*  END OF NEW CODE                              MHC 06/03/02                    
*                                                                               
         TM    PCF2,PCFBRO2               COMING BACK FROM BROWSE?              
         BZ    DSPLY70                    NO                                    
*                                                                               
         TM    LSTFLAG,DBFLTDAR           IS IT LINKED TO A DARE ORDER?         
         BO    DSPLY62                                                          
         TM    LSTFLAG2,LSTSALP           OR HAVE SALESPERSON ELEM?             
         BZ    DSPLY65                                                          
DSPLY62  CLC   LSTBTK,BRWKEY2             THEN KEY MUST MATCH                   
         BNE   DSPLY70                                                          
*                                                                               
DSPLY65  CLC   LSTBTK+5(5),BRWKEY2+5      DARE REC = BROWSE SELECTED?           
         BNE   DSPLY70                    NO                                    
*                                                                               
         BRAS  RE,GETSALES                DID WE GET SALEP VIA GLOBBER?         
         BNE   DSPLY70                    NO, ERROR                             
*                                                                               
         CLC   LSTBTK,BRWKEY2             DID WE BROWSE FROM HERE?              
         BNE   *+12                                                             
         BRAS  RE,UPDBTSLP                UPDATE SALESPER IN DR BATCH           
         OI    PCF2,PCFBATCH                                                    
*                                                                               
         TM    PCF3,PCFLASTM              LAST METHOD UPDATED ALREADY?          
         BO    DSPLY70                    YES                                   
         BRAS  RE,UPDLMREC                UPDATE LAST METHOD RECORD             
         OI    PCF3,PCFLASTM                                                    
* UPDATE THE SALESPERSON ON CURRENT LINE                                        
         L     R6,AIO                                                           
         MVI   ELCODE,DMSPELQ      X'20'                                        
         BRAS  RE,GETEL                                                         
         USING DMSPELD,R6                                                       
         MVC   SCRNROU,DMSPNAME                                                 
*                                                                               
DSPLY70  TM    OPT,OPTDAD                 DISPLAY DISK ADDRESS                  
         BNO   DSPLY80                                                          
DSPLY75  MVC   SCRNROU,BLANKS                                                   
         GOTO1 HEXOUT,DMCB,LSTBTDA,SCRNROU,4,0,0                                
*                                                                               
DSPLY80  OI    SCRNDSTH+4,FINPVAL         DESTINATION - VALIDATED               
         OI    SCRNSTDH+4,FINPVAL         STANDARD COMMENT - VALIDATED          
         OI    SCRNFFTH+4,FINPVAL         FREE FORM TEXT -- VALIDATED           
         LA    R2,SCRNLNQ(R2)             NEXT SCREEN LINE                      
         LA    R3,LSTLNQ(R3)              NEXT LIST ITEM                        
         BCT   R5,DSPLY25                                                       
*                                                                               
         L     RF,ATIOB                   SET CURSOR TO FIRST SELECT            
         USING TIOBD,RF                                                         
         MVC   TIOBCURD,=Y(MKTSELH-T234FFD)                                     
         OI    TIOBINDS,TIOBSETC                                                
         MVI   TIOBCURI,0                                                       
         J     XIT                                                              
*                                                                               
****** ERROR ROUTINES                                                           
DSPLY99  TM    OPT,OPTDAD                 DISPLAY DISK ADDRESS                  
         BO    DSPLY75                                                          
*                                                                               
         CLC   GERROR,=AL2(NODESTN)  NO SFM DESTINE                             
         BNE   DSPLY99A                                                         
         XC    SCRNROU,SCRNROU                                                  
         MVC   SCRNROU(26),=CL26'SFM DESTINE record missing'                    
         J     DSPLY99Z                                                         
*                                                                               
DSPLY99A CLC   GERROR,=AL2(NODSTDOV) DESTINE FOR REP, USE DESTOV                
         BE    DSPLY99X                                                         
*                                                                               
DSPLY99D CLC   GERROR,=AL2(NODRADM)  NO DARADM FAX RECORD FOR REP               
         JNE   INVLFLD               SOME OTHER ERROR                           
DSPLY99X XC    SCRNROU,SCRNROU                                                  
         MVC   SCRNROU(26),=CL26'need special fax#- use PF5'                    
DSPLY99Z XC    GERROR,GERROR                                                    
         B     DSPLY80                                                          
****** ERROR ROUTINES                                                           
         DROP  R2,R3,R4,R6,RF                                                   
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD CHANGES                                         *         
***********************************************************************         
VALR     DS    0H                                                               
         SR    R0,R0                                                            
         L     R1,ATHISLST                                                      
         SR    R1,RA                                                            
         AHI   R1,-(MKTSELH-T234FFD)                                            
         LA    R3,SCRNLNQ                                                       
         DR    R0,R3                                                            
         STC   R1,PFLINE                                                        
         B     VALIN01                    VALIDATE THE LINE                     
         EJECT                                                                  
***********************************************************************         
* VALIDATE A LINE ON THE SCREEN                                       *         
***********************************************************************         
VALIN    NTR1  ,                                                                
VALIN01  BRAS  RE,VCHNG            VALIDATE CHANGED DATA                        
*                                                                               
         SR    R4,R4                                                            
         IC    R4,PFLINE           R4=LINE NUMBER                               
         MHI   R4,SCRNLNQ                                                       
         LA    R4,MKTSELH(R4)      R4=A(SCREEN LINE)                            
         USING SCRND,R4                                                         
         LR    R0,R4                                                            
         SR    R0,RA                                                            
         STCM  R0,3,CURLINE        DISPLACEMENT TO CURRENT LINE                 
*                                                                               
         CLI   SCRNSELH+5,0                                                     
         BE    VALIN03                                                          
         CLI   SCRNSEL,C'O'        OVERRIDE                                     
         BE    VALIN03                                                          
         CLI   SCRNSEL,C'X'        SEND                                         
         BE    VALIN03                                                          
         CLI   SCRNSEL,C'T'        TRANSMIT?                                    
         BE    VALIN02                                                          
         CLI   SCRNSEL,C'P'        PREVIOUSLY SENT? (AVN)                       
         BE    *+12                                                             
         CLI   SCRNSEL,C'N'        NOT PREVIOUSLY SENT (NO AVN)                 
         BNE   *+12                                                             
         CLI   PDARPORN,C'Y'       USES PREV/NEW?                               
         BE    VALIN03             IF NOT,CANNOT HAVE THESE OPTS                
         L     R1,ATIOB            SET CURSOR FOR ERROR                         
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         MVC   TIOBCURD-TIOBD(L'TIOBCURD,R1),CURLINE                            
         J     INVLFLD                                                          
*                                                                               
VALIN02  MVI   LISTSW,C'T'         REDISPLAY SAME PAGE                          
         SR    R0,R0                                                            
         IC    R0,NSENT            ADD 1 TO SENT                                
         AHI   R0,1                                                             
         STC   R0,NSENT            SAVE NUMBER SENT                             
*                                                                               
VALIN03  SR    R3,R3                                                            
         IC    R3,PFLINE                                                        
         MHI   R3,LSTLNQ                                                        
         LA    R3,LSTTAB(R3)                                                    
         USING LSTD,R3                                                          
VALIN03A CLI   SCRNSEL,C'P'        PREVIOUSLY SENT?                             
         BE    VALIN03B                                                         
         CLI   SCRNSEL,C'N'        NOT PREVIOUSLY SENT (NO AVN)                 
         BE    VALIN03B                                                         
         TM    LSTIND,LSTICHG      ANY CHANGES ?                                
         BNO   VALIN25                                                          
*                                                                               
VALIN03B MVI   ELC,0               INITIALIZE ELEMENT FLAGS                     
         CLI   SCRNSTDH+5,0        ANY STANDARD COMMENT ?                       
         BE    *+8                                                              
         OI    ELC,ELCSTD          SET STANDARD COMMENT INPUT                   
         CLI   SCRNFFTH+5,0        ANY FREE FORM TEXT                           
         BE    *+8                                                              
         OI    ELC,ELCFFT          ORDER COMMENT INPUT                          
         CLI   SCRNSEL,C'P'                                                     
         BE    *+12                                                             
         CLI   SCRNSEL,C'N'                                                     
         BNE   *+8                                                              
         OI    ELC,ELCPRV          PREV/NEW                                     
*                                                                               
VALIN04  XC    ELEM,ELEM           ** MAKE BATCH ELEMENT                        
         LA    R6,ELEM                                                          
         USING DBFLTELD,R6                                                      
         MVI   DBFLTEL,DBFLTELQ    ELEMENT CODE                                 
         MVI   DBFLTLEN,DBFLTOVH   LENGTH W/O STD.C OR FF TEXT                  
         CLI   SCRNSEL,C'P'        PREVIOUSLY SENT?                             
         BNE   *+8                 NO                                           
         OI    DBFLTFL1,DBFLTPRV   TURN ON PREVIOUSLY SENT                      
         CLI   SCRNSEL,C'N'        MARK AS NEW?                                 
         BNE   *+8                 NO                                           
         OI    DBFLTFL1,DBFLTNEW   TURN ON MARKED AS NEW                        
         CLI   SCRNTYPE,C'T'       IS THIS A TRADE BATCH?                       
         BNE   *+8                                                              
         OI    DBFLTFL1,DBFLTTRD   TURN ON TRADE                                
         TM    ELC,ELCSTD+ELCFFT   TEST STD. COMMENT OR TEXT                    
         BZ    VALIN05                                                          
         MVI   DBFLTLEN,DBFLTLNQ   SET LENGTH WITH STANDARD                     
         MVC   DBFLTSTD,SCRNSTD    MOVE IN STANDARD                             
         OC    DBFLTSTD,BLANKS                                                  
         OI    DBFLTFL1,DBFLTBSC   OM V2.0 FOR STDCMT AND FFT                   
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,SCRNFFTH+5     ANY TEXT                                     
         BZ    VALIN05                                                          
         CLC   =C'ATTN: ',SCRNFFT  SPECIAL KEYWORD?                             
         BE    VALIN04A            YES                                          
*                                                                               
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   DBFLTFFT(0),SCRNFFT MOVE FREE FORM TEXT                          
         AHI   RF,DBFLTLNQ+1       ADJUST ELEMENT LENGTH                        
         STC   RF,DBFLTLEN                                                      
         B     VALIN05                                                          
***                                                                             
* SAVE SALESPERSON (FIRST SAVE OLD FFT)                                         
***                                                                             
VALIN04A MVC   AIO,AIO1            GET THE BATCH RECORD                         
         MVI   RDUPDATE,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),LSTBTK       READ BATCH KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'DBTKEY),KEYSAVE                                            
         BNE   VALIN33             DATA NO LONGER CURRENT                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
***                                                                             
* FIRST SAVE FREE FORM TEXT IN ELEM SINCE WE HAVE SPECIAL KEYWORD               
* IN FREE-FORM TEXT FIELD                                                       
***                                                                             
         L     R6,AIO                                                           
         USING DBFLTELD,R6                                                      
         MVI   ELCODE,DBFLTELQ                                                  
         BRAS  RE,GETEL                                                         
         BNE   VALIN04F                                                         
         B     VALIN04C                                                         
*                                                                               
VALIN04B BRAS  RE,NEXTEL                                                        
         BNE   VALIN04F            MAKE SURE SAME FLIGHT #                      
*                                                                               
VALIN04C CLC   DBFLTFLT,LSTFLT     MATCH FLIGHT                                 
         BNE   VALIN04B                                                         
*                                                                               
         MVC   BYTE,DBFLTFL1       XOR THE BOTH FLAG FIELDS AND                 
         XC    BYTE,LSTFLAG        SEE WHAT FLAGS/BITS MATCHED                  
         TM    BYTE,DBFLTTRD       DO WE MATCH ON CASH/TRADE X'04'              
         BNZ   VALIN04B            NO                                           
*                                                                               
         TM    DBFLTFL1,DBFLTDAR   LINKED TO DARE ORDER NOW?                    
         BO    VALIN04D            YES                                          
*                                                                               
         CLI   MKTMED,C'T'         MEDIA = T                                    
         BE    VALIN04E            YES                                          
*                                                                               
         CLC   =C'STA',SCRNDST     DEST = STA                                   
         BE    VALIN04E            YES                                          
*                                                                               
         CLC   =C'EML:',SCRNROU    ROUTE = EMAIL?                               
         BE    VALIN04E            YES                                          
*                                                                               
         CLC   =C'FAX:',SCRNROU    ROUTE = FAX?                                 
         BE    VALIN04E            YES                                          
*                                                                               
* REP/INBOX WITH CAN NOT USE THIS SPECIAL ATTN: KEYWORD                         
*                                                                               
VALIN04D XC    SCRNFFT,SCRNFFT     YES...CLEAR FFT FIELD                        
         OI    SCRNFFTH+6,X'80'    XMIT                                         
         B     VALIN28             AND EXIT                                     
*                                                                               
VALIN04E CLI   DBFLTLEN,DBFLTLNQ   DO WE HAVE FFT?                              
         BNH   VALIN04F            NO                                           
         ZIC   RF,DBFLTLEN                                                      
         SHI   RF,DBFLTLNQ                                                      
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ELEM+DBFLTLNQ(0),DBFLTFFT  SAVE FREE FORM TEXT                   
         AHI   RF,DBFLTLNQ+1              ADJUST ELEMENT LENGTH                 
         STC   RF,ELEM+DBFLTLEN-DBFLTELD                                        
         DROP  R6                                                               
*                                                                               
VALIN04F L     R6,AIO                                                           
         USING DBSLPELD,R6                                                      
         MVI   ELCODE,DBSLPELQ     GET SALESPERSON ELEMENT                      
         BRAS  RE,GETEL                                                         
         BNE   VALIN04I                                                         
         B     VALIN04H                                                         
*                                                                               
VALIN04G BRAS  RE,NEXTEL                                                        
         BNE   VALIN04I            MAKE SURE SAME FLIGHT #                      
*                                                                               
VALIN04H CLC   DBSLPFLT,LSTFLT     MATCH FLIGHT                                 
         BNE   VALIN04G                                                         
*                                                                               
         MVC   BYTE,DBSALESP       XOR THE BOTH FLAG FIELDS AND                 
         XC    BYTE,LSTFLAG        SEE WHAT FLAGS/BITS MATCHED                  
         TM    BYTE,DBSTRADE       DO WE MATCH ON CASH/TRADE X'04'              
         BNZ   VALIN04G            NO                                           
*                                                                               
         MVC   DBSALESP,SCRNFFT+6  CLOBBER WITH NEW SALESPERSON                 
         DROP  R6                                                               
         B     VALIN04J                                                         
*                                                                               
VALIN04I XC    ELEM2,ELEM2                                                      
         LA    R5,ELEM2                                                         
         USING DBSLPELD,R5                                                      
         MVI   DBSLPEL,DBSLPELQ                                                 
         MVI   DBSLPLEN,DBSLPLNQ                                                
         MVC   DBSLPFLT,LSTFLT     FLIGHT                                       
         MVC   DBSALESP,SCRNFFT+6  SALESPERSON                                  
         CLI   SCRNTYPE,C'T'       IS THIS A TRADE BATCH?                       
         BNE   *+8                                                              
         OI    DBSALFL1,DBSTRADE   YES, MARK IT AS TRADE                        
         DROP  R5                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM2,(R6)  ADD NEW ELEMENT                
VALIN04J GOTO1 PUTREC                                                           
*                                                                               
VALIN05  MVC   AIO,AIO1            GET THE BATCH RECORD                         
         MVI   RDUPDATE,C'Y'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),LSTBTK       READ BATCH KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'DBTKEY),KEYSAVE                                            
         BNE   VALIN33             DATA NO LONGER CURRENT                       
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DBFLTELQ     GET BATCH ELEMENT                            
         USING DBFLTELD,R6                                                      
         BRAS  RE,GETEL                                                         
         BE    VALIN07                                                          
*****    CLI   LSTFLT,0            IF NONE, FLIGHT MUST BE ZERO                 
*****    BNE   VALIN33                                                          
         MVC   ELEM+DBFLTFLT-DBFLTELD(L'LSTFLT),LSTFLT    FLIGHT                
         L     R6,AIO                                                           
         MVI   ELCODE,DBINFELQ     INFO ELEMENT                                 
         BRAS  RE,GETEL            REQUIRED                                     
         BE    *+6                                                              
         DC    H'0'                                                             
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     VALIN11             ADD NEW ELEMENT                              
*                                                                               
VALIN06  LR    R5,R6                                                            
         BRAS  RE,NEXTEL                                                        
         BE    VALIN07             MAKE SURE SAME FLIGHT #                      
***                                                                             
* NO FLT ELEM IN BATCH MAKE SURE WE ADD IT AFTER LAST X'20' ELEM                
***                                                                             
         MVC   ELEM+DBFLTFLT-DBFLTELD(L'LSTFLT),LSTFLT                          
         LR    R6,R5               INSERT AFTER LAST X'20'                      
         ZIC   RF,1(R6)                                                         
         AR    R6,RF                                                            
         B     VALIN11             GO ADD ONE                                   
*                                                                               
VALIN07  CLC   DBFLTFLT,LSTFLT     MATCH FLIGHT                                 
         BNE   VALIN06                                                          
*                                                                               
         MVC   BYTE,DBFLTFL1       XOR THE BOTH FLAG FIELDS AND                 
         XC    BYTE,LSTFLAG        SEE WHAT FLAGS/BITS MATCHED                  
         TM    BYTE,DBFLTTRD       DO WE MATCH ON CASH/TRADE X'04'              
         BNZ   VALIN06             NO                                           
*                                                                               
         LA    R2,SCRNSELH                                                      
         CLC   DBFLTFL1,LSTFLAG    TEST SAME STATUS                             
         BNE   VALIN33                                                          
         TM    DBFLTFL1,DBFLTSNT   HAS IT ALREADY BEEN SENT ?                   
         BO    VALIN33             YES, SKIP IT                                 
         OI    ELC,ELCOFL          SET FLIGHT ELEMENT FOUND                     
         TM    DBFLTFL1,DBFLTDAR   LINKED TO A DARE ORDER ?                     
         BNO   VALIN08             NO                                           
         CLI   SCRNSEL,C'P'        PREVIOUSLY AVN?                              
         BE    ERRPRV              YES...ERROR                                  
         CLI   SCRNSEL,C'N'        MARKED AS NEW AVN?                           
         BE    ERRPRV              YES...ERROR                                  
         B     VALIN13             YES, UPDATE DARE RECORD                      
*                                                                               
VALIN08  LA    RF,ELEM                                                          
N        USING DBFLTELD,RF                                                      
         MVC   N.DBFLTFLT,DBFLTFLT SAVE FLIGHT #                                
         MVC   BYTE,DBFLTFL1                                                    
         TM    ELC,ELCSTD+ELCFFT   TEST STD. COMMENT OR TEXT                    
         BNZ   *+8                                                              
         NI    BYTE,X'FF'-DBFLTBSC                                              
         OC    N.DBFLTFL1,BYTE     AND "ALL" FLAGS                              
*                                                                               
VALIN08A CLI   SCRNSEL,C'P'        PREVIOUSLY AVN?                              
         BNE   *+8                 NO                                           
         NI    N.DBFLTFL1,X'FF'-DBFLTNEW  TURN OFF MARKED AS NEW                
         CLI   SCRNSEL,C'N'               MARK AS NEW?                          
         BNE   *+8                        NO                                    
         NI    N.DBFLTFL1,X'FF'-DBFLTPRV  TURN OFF MARKED AS PREVIOUS           
         SR    R1,R1                                                            
         IC    R1,N.DBFLTLEN                                                    
*                                                                               
         CLM   R1,1,DBFLTLEN       SAME LENGTH                                  
         BNE   VALIN09             NO, MUST FIX RECORD                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DBFLTEL(0),N.DBFLTEL       TEST CHANGE OF ELEMENT DATA           
         BE    VALIN25                    NO, LEAVE IT ALONE                    
         DROP  N                                                                
*                                                                               
VALIN09  GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)  DELETE ELEMENT                  
VALIN11  TM    ELC,ELCOFL+ELCSTD+ELCFFT+ELCPRV    ANY ACTIVITY ?                
         BZ    VALIN12                                                          
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)  ADD NEW ELEMENT                 
VALIN12  GOTO1 PUTREC                                                           
         B     VALIN25                                                          
*                                                                               
VALIN13  MVC   AIO,AIO2            ** UPDATE DARE ORDER **                      
         MVI   RDUPDATE,C'Y'                                                    
         OC    LSTDAK,LSTDAK                                                    
         BNZ   *+6                                                              
         DC    H'0'                MISSING DARE KEY                             
         XC    KEY,KEY                                                          
         MVC   KEY(L'DOKEY),LSTDAK DARE KEY                                     
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BNE   VALIN33             START AGAIN                                  
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ      GET DARE ID ELEMENT                          
         BRAS  RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                NO DARE ELEMENT                              
         USING DOIDELD,R6                                                       
         MVC   NEWCMT,SCRNSTD      GET COMMENT FROM SCREEN                      
         OC    NEWCMT,BLANKS                                                    
         MVC   OLDCMT,DOISCOM1     GET COMMENT FROM RECORD                      
         OC    OLDCMT,BLANKS                                                    
         CLC   NEWCMT,OLDCMT       ARE THEY THE SAME?                           
         BE    VALIN15             YES, RECORD IS OK                            
         MVC   DOISCOM1,NEWCMT     UPDATE THE COMMENT                           
         GOTO1 PUTREC                                                           
*                                                                               
VALIN15  XC    ELEM,ELEM           ** UPDATE DARE COMMENT **                    
         LA    R6,ELEM                                                          
         USING DOCOMELD,R6         MAKE A COMMENT ELEMENT                       
         XR    R1,R1                                                            
         ICM   R1,1,SCRNFFTH+5                                                  
         BZ    VALIN17                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DOCOMTXT(0),SCRNFFT ORDER COMMENT TO ELEMENT                     
         AHI   R1,DOCOMOVH+1                                                    
         STC   R1,DOCOMLEN         SET ELEMENT LENGTH                           
         MVI   DOCOMEL,DOCOMELQ    ELEMENT CODE                                 
         MVI   DOCOMLIN,1          FOR LINE ONE                                 
*                                                                               
VALIN17  MVC   AIO,AIO2                                                         
         L     RF,AIO2                                                          
         MVC   KEY,0(RF)           BUILD DARE COMMENT KEY                       
         LA    RF,KEY                                                           
         USING DOKEY,RF                                                         
         MVI   DOKCMT,1                                                         
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 HIGH                READ FOR COMMENT                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    VALIN18                                                          
*                                                                               
         L     RF,AIO2             ** ADD DARE COMMENT RECORD **                
         MVI   DOKCMT,1                                                         
         MVC   DORAGY,AGENCY                                                    
         XR    R1,R1                                                            
         ICM   R1,1,DOCOMLEN                                                    
         BZ    VALIN25             NO COMMENT DATA                              
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DORFRST(0),DOCOMEL                                               
         AHI   R1,DORFRST-DOKEY+1                                               
         STCM  R1,3,DORLEN                                                      
         GOTO1 ADDREC                                                           
         B     VALIN25                                                          
         DROP  RF                                                               
*                                                                               
VALIN18  MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DOCOMELQ     GET DARE COMMENT ELEMENT                     
         BRAS  RE,GETEL                                                         
         BNE   VALIN21                                                          
         CLI   DOCOMLIN,1          MUST BE FIRST COMMENT                        
         BNE   VALIN21                                                          
         XR    R1,R1               WAS A COMMENT INPUT ?                        
         ICM   R1,1,ELEM+(DOCOMLEN-DOCOMEL)                                     
         BZ    VALIN19             NO, DELETE OLD                               
         CLM   R1,1,DOCOMLEN       SAME LENGTH ?                                
         BNE   VALIN19             NO, DELETE OLD                               
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   DOCOMEL(0),ELEM     SAME DATA ?                                  
         BE    VALIN25             YES,                                         
*                                                                               
VALIN19  GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)  DELETE OLD                      
VALIN21  CLI   ELEM+(DOCOMLEN-DOCOMEL),0        NEW COMMENT ?                   
         BE    VALIN23                          NO,                             
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)  ADD NEW                         
VALIN23  GOTO1 PUTREC                                                           
*                                                                               
VALIN25  NI    LSTIND,ALL-(LSTICHG)       DON'T CHANGE IT AGAIN                 
         OI    SCRNDSTH+4,FINPVAL         SET FIELD VALIDATED                   
         OI    SCRNDSTH+6,FOUTTRN         SET TRANSMIT                          
         OI    SCRNSTDH+4,FINPVAL                                               
         OI    SCRNSTDH+6,FOUTTRN                                               
         OI    SCRNFFTH+4,FINPVAL                                               
         OI    SCRNFFTH+6,FOUTTRN                                               
         LA    R2,SCRNSELH                                                      
         CLI   SCRNSEL,C'P'        PREVIOUSLY AVN?                              
         BNE   VALIN26                                                          
         MVI   SCRNSEL,0                                                        
         OI    SCRNSELH+6,X'80'                                                 
         MVI   SCRNPORN,C'P'       YES...MARK LIST WITH A C'P'                  
         OI    SCRNPRNH+1,X'08'    HIGH INTENSITY                               
         OI    SCRNPRNH+6,X'80'    XMIT                                         
*        J     PRVMSG                                                           
         B     VALIN28                                                          
*                                                                               
VALIN26  CLI   SCRNSEL,C'N'        MARKED AS NEW?                               
         BNE   VALIN28                                                          
         MVI   SCRNSEL,0                                                        
         OI    SCRNSELH+6,X'80'                                                 
         MVI   SCRNPORN,C'N'       YES...MARK LIST WITH A C'P'                  
         OI    SCRNPRNH+1,X'08'    HIGH INTENSITY                               
         OI    SCRNPRNH+6,X'80'    XMIT                                         
*        J     NEWMSG                                                           
*                                                                               
VALIN28  J     XIT                                                              
*                                                                               
VALIN33  OI    PCF,PCFNOCUR        SET 'DATA NOT CURRENT'                       
         L     R1,ATIOB            SET CURSOR FOR ERROR                         
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         MVC   TIOBCURD-TIOBD(L'TIOBCURD,R1),CURLINE                            
         J     NOTCURNT                                                         
*                                                                               
         DROP  R3,R4,R6                                                         
         EJECT                                                                  
***********************************************************************         
* READ DARE BATCH                                                     *         
***********************************************************************         
RDBATCH  NTR1                                                                   
         BRAS  RE,VCHNG                   VALIDATE CHANGED DATA                 
*                                                                               
         ZIC   R4,PFLINE                  R4=LINE NUMBER                        
         MHI   R4,SCRNLNQ                                                       
         LA    R4,MKTSELH(R4)             R4=A(SCREEN LINE)                     
         USING SCRND,R4                                                         
         LR    R0,R4                                                            
         SR    R0,RA                                                            
         STCM  R0,3,CURLINE               DISPLACEMENT TO CURRENT LINE          
*                                                                               
         ZIC   R3,PFLINE                                                        
         MHI   R3,LSTLNQ                                                        
         LA    R3,LSTTAB(R3)                                                    
         USING LSTD,R3                                                          
***                                                                             
* NOT P/N, IF PFKEY = SENDALL AND FLIGHT ELEMENT IS NOT MARKED IN BATCH         
* RECORD THEN GIVE AN ERROR                                                     
***                                                                             
         MVC   AIO,AIO1                   GET THE BATCH RECORD                  
         MVI   RDUPDATE,C'N'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),LSTBTK       READ BATCH KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'DBTKEY),KEYSAVE                                            
         BNE   RDBERR2                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING DBFLTELD,R6                                                      
         MVI   ELCODE,DBFLTELQ            FLIGHT ELEMENT                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   RDBERR1                    MAKE SURE SAME FLIGHT #               
*                                                                               
         CLC   DBFLTFLT,LSTFLT            MATCH FLIGHT                          
         BNE   *-14                                                             
*                                                                               
*        TM    DBFLTFL1,DBFLTDAR          LINKED TO A DARE ORDER ?              
*        BO    ERRPRV                     YES...ERROR                           
*                                                                               
         TM    DBFLTFL1,DBFLTSNT          HAS IT ALREADY BEEN SENT ?            
         BO    RDBERR1                    YES...ERROR                           
*                                                                               
         TM    DBFLTFL1,DBFLTPRV          MARKED AS PREVIOUS?                   
         BO    RDBX                       YES...EXIT                            
*                                                                               
         TM    DBFLTFL1,DBFLTNEW          MARKED AS NEW?                        
         BO    RDBX                       YES...EXIT                            
*                                                                               
RDBERR1  LA    R2,SCRNSELH                BATCH REC NOT MARKED                  
         B     ERRPRV3                                                          
                                                                                
RDBERR2  OI    PCF,PCFNOCUR               SET 'DATA NOT CURRENT'                
         L     R1,ATIOB                   SET CURSOR FOR ERROR                  
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         MVC   TIOBCURD-TIOBD(L'TIOBCURD,R1),CURLINE                            
         J     NOTCURNT                                                         
*                                                                               
RDBX     J     XIT                                                              
         EJECT                                                                  
         DROP  R3,R4,R6                                                         
***********************************************************************         
* MARK P OR N IF IT IS SET IN THE BATCH RECORD                        *         
***********************************************************************         
MARKPORN NTR1                                                                   
*                                                                               
         USING SCRND,R2                                                         
         USING LSTD,R3                                                          
*                                                                               
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STCM  R0,3,CURLINE               DISPLACEMENT TO CURRENT LINE          
* READ BATCH                                                                    
         MVC   AIO,AIO1                   GET THE BATCH RECORD                  
         MVI   RDUPDATE,C'N'                                                    
         XC    KEY,KEY                                                          
         MVC   KEY(L'DBTKEY),LSTBTK       READ BATCH KEY                        
         GOTO1 HIGH                                                             
         CLC   KEY(L'DBTKEY),KEYSAVE                                            
         BNE   MARKERR                                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         USING DBFLTELD,R6                                                      
         MVI   ELCODE,DBFLTELQ            FLIGHT ELEMENT                        
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
         BRAS  RE,NEXTEL                                                        
         BNE   MARKX                      MAKE SURE SAME FLIGHT #               
*                                                                               
         CLC   DBFLTFLT,LSTFLT            MATCH FLIGHT                          
         BNE   *-14                                                             
*                                                                               
         TM    DBFLTFL1,DBFLTPRV          MARKED AS PREVIOUS?                   
         BNO   MARK10                     NO...CHECK NEW                        
         MVI   SCRNPORN,C'P'              YES...MARK LIST WITH A C'P'           
         OI    SCRNPRNH+1,X'08'           HIGH INTENSITY                        
         OI    SCRNPRNH+6,X'80'           XMIT                                  
         B     MARKX                                                            
*                                                                               
MARK10   TM    DBFLTFL1,DBFLTNEW          MARKED AS NEW?                        
         BNO   MARKX                      YES...EXIT                            
         MVI   SCRNPORN,C'N'              YES...MARK LIST WITH A C'N'           
         OI    SCRNPRNH+1,X'08'           HIGH INTENSITY                        
         OI    SCRNPRNH+6,X'80'           XMIT                                  
*                                                                               
MARKX    J     XIT                                                              
*                                                                               
MARKERR  OI    PCF,PCFNOCUR               SET 'DATA NOT CURRENT'                
         L     R1,ATIOB                   SET CURSOR FOR ERROR                  
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         MVC   TIOBCURD-TIOBD(L'TIOBCURD,R1),CURLINE                            
         J     NOTCURNT                                                         
         EJECT                                                                  
         DROP  R2,R3,R6                                                         
***********************************************************************         
* 2-BYTE ERROR MESSAGES                                               *         
***********************************************************************         
ERRPRV   LHI   RF,965               LINKED TO DARE ORDER CANNOT CHANGE          
         B     SPERREX                                                          
ERRPRV2  LHI   RF,976               NOT PREVIOUSLY AVN                          
         B     SPERREX                                                          
ERRPRV3  LHI   RF,977               MUST BE MARKED P/N                          
         B     SPERREX                                                          
NOSALES  LHI   RF,1160              DID NOT SELECT SALESPERSON                  
         B     SPERREX                                                          
NODAREBK LHI   RF,1161              BATCH REC DOES NOT EXIST                    
         B     SPERREX                                                          
BATCHSAL LHI   RF,1162              SALESPERSON NOT UPDATED!                    
         B     SPERREX                                                          
NOTREP   LHI   RF,1163              NOT REP CAN'T SELECT SALEPERSON             
         B     SPERREX                                                          
ERRMULT  LHI   RF,1164              CANNOT MULTI-SELECT FOR BROWS               
         B     SPERREX                                                          
ERRROUTE LHI   RF,1165              CANT SEL SALEP IF ROUTE = EML/FAX           
         B     SPERREX                                                          
NOIDREC  LHI   RF,1166              NO ID REC                                   
         B     SPERREX                                                          
NOIDREC2 LHI   RF,1167              NO AGY ID ELEM                              
         B     SPERREX                                                          
ERREFRSH LHI   RF,1168              DATA CORRUPT PLEASE REFRESH SCREEN          
         B     SPERREX                                                          
ERRBROW  LHI   RF,1171              CAN'T GO TO BROWSE WITH MEDIA T             
*                                                                               
SPERREX  STCM  RF,3,ERRNUM                                                      
         OI    GENSTAT2,USGETTXT                                                
         LA    RF,GETTXTCB                                                      
         USING GETTXTD,RF                                                       
         MVC   GTMSGNO,ERRNUM                                                   
         MVI   GTMTYP,GTMERR                                                    
         MVI   GTMSYS,2                                                         
         DROP  RF                                                               
         MVC   AIO,AIO1                                                         
         GOTO1 ERREX                                                            
***********************************************************************         
* ROUTINE TO GET THE PRODUCT CODE                                     *         
***********************************************************************         
GETPRD   LA    RF,SVCLIST                                                       
         MVC   0(3,R1),=C'???'            IF NOT FOUND                          
GETPRD3  CLI   0(RF),0                    TEST END OF LIST                      
         BER   RE                                                               
         CLC   BYTE,3(RF)                 MATCH THE CODE                        
         BE    *+12                                                             
         LA    RF,4(RF)                                                         
         B     GETPRD3                                                          
         MVC   0(3,R1),0(RF)              CODE TO OUTPUT AREA                   
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A COMMA AFTER A LAST NAME FOR EMAIL                  *         
***********************************************************************         
ADDCOMMA NTR1  ,                                                                
         LA    R3,L'FLD                                                         
         LA    R2,FLD                                                           
ADDC01   CLC   0(2,R2),BLANKS      CHECK FOR 2 CONSECUTIVE SPACES               
         BE    ADDCX               (END OF LAST NAME)                           
         LA    R2,1(R2)                                                         
         BCT   R3,ADDC01                                                        
*                                                                               
         B     *+8                                                              
ADDCX    MVI   0(R2),C','                                                       
         XIT1                                                                   
***********************************************************************         
*  ROUTINE TO BUILD FIRST KEY FOR READ HIGH                           *         
***********************************************************************         
FSTK     NTR1  ,                                                                
         MVI   KYLN,0                     INIT.  MINIMUM KEY COMPARE            
         XC    KEY,KEY                    SET KEY                               
         LA    R2,KEY                                                           
         USING DARBTCHD,R2                                                      
         MVI   DBTKTYP,DBTKTYPQ           TYPE                                  
         MVI   DBTKSTYP,DBTKSTYQ          SUB-TYPE                              
         MVC   KYMSK,KEY                  KEY MASK FOR FILTER DATA              
*                                                                               
         L     R3,AKEYTAB                                                       
         USING DKYD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,DKYNUM                  R0=NUMBER OF KEY FIELDS               
         LA    R3,DKYDATA                                                       
         USING DKYDATA,R3                                                       
         LA    R4,L'DBTKTYP+L'DBTKSTYP    R4=DEFAULT LENGTH                     
*                                                                               
FSTK03   MVC   KYFLDLN,DKYLEN             SET LENGTH OF KEY FIELD               
         GOTOR SETKFLD,DMCB,KYMSK,DKYDATA SET FILTER FIELDS IN MASK             
         BNE   FSTK05                     NO KEY DATA                           
         SR    RE,RE                                                            
         ICM   RE,1,KYFLDLN               ANY LENGTH OVERRIDE?                  
         AR    R4,RE                                                            
         CLC   DKYLEN,KYFLDLN             LENGTH OF KEY FIELD CHANGE?           
         BE    FSTK07                     NO, OK TO CONTINUE                    
*                                                                               
FSTK05   CLI   KYLN,0                     KEY LENGTH ALREADY SET ?              
         BNE   FSTK07                     YES                                   
         STC   R4,KYLN                    SAVE LENGTH OF MINIMUM KEY            
*                                                                               
FSTK07   LA    R3,DKYLNQ(R3)                                                    
         BCT   R0,FSTK03                                                        
*                                                                               
         CLI   KYLN,0                     KEY LENGTH ALREADY SET?               
         BNE   FSTK09                     YES                                   
         STC   R4,KYLN                    IF ALL KEY FIELDS ARE OKAY            
*                                                                               
FSTK09   SR    R1,R1                                                            
         IC    R1,KYLN                    MINIMUM KEY LENGTH                    
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   KEY(0),KYMSK               SET START KEY FROM MASK               
*                                                                               
         J     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO SET KEY FIELDS WITH FILTER DATA                         *         
*  PARM 1  =    A(KEY)                                                *         
*       2  =    A(DKYDATA ENTRY)                                      *         
***********************************************************************         
SETKFLD  NTR1  ,                                                                
         LM    R2,R3,0(R1)                                                      
         USING DKYDATA,R3                                                       
         SR    RF,RF                                                            
         IC    RF,DKYDSP                                                        
         AR    R2,RF                      R2=A(DEST. FIELD IN KEY)              
         SR    R1,R1                                                            
         IC    R1,DKYLEN                                                        
         BCTR  R1,0                       R1=LENGTH FOR EX OF DEST.             
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,DKYSRC                SOURCE NUMBER                         
         JZ    NO                         NO FILTER ON THIS FIELD               
         MHI   R4,FDLNQ                                                         
         A     R4,AFDTAB                  R4=A(FILTER DEFINITION)               
*                                                                               
         USING FDD,R4                                                           
         SR    RF,RF                                                            
         ICM   RF,3,FDKSET                SPECIAL SET KEY ROUTINE               
         BZ    SETKFLD3                                                         
         AR    RF,RB                                                            
         BASR  RE,RF                      LET SPECIAL ROUTINE HANDLE            
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,R2),0(R2)              TEST ANY DATA SET                     
         JZ    NO                                                               
         J     YES                                                              
*                                                                               
SETKFLD3 SR    R6,R6                                                            
         ICM   R6,1,FDFLBIT               IS THERE A FLAG BIT TO TEST?          
         BZ    SETKFLD5                                                         
         SR    RF,RF                                                            
         ICM   RF,3,FDFLFLD               GET DISP. TO FLAG FIELD               
         LA    RF,LSSD(RF)                                                      
         EX    R6,*+8                     TEST FILTER DATA INPUT                
         B     *+8                                                              
         TM    0(RF),0                                                          
         JNO   NO                         NO FILTER DATA                        
*                                                                               
SETKFLD5 SR    RF,RF                                                            
         ICM   RF,3,FDSRC                                                       
         JZ    NO                                                               
         LA    RF,LSSD(RF)                LOCAL AREA                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R2),0(RF)              MOVE FILTER DATA TO KEY               
         J     YES                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
***********************************************************************         
*  ROUTINE TO FILTER KEY                                              *         
*   IF KEY DOES NOT PASS FILTER - SET KEY FOR NEXT READ HIGH          *         
***********************************************************************         
FLTRK    NTR1  ,                                                                
FLTRK1   L     R3,AKEYTAB                                                       
         USING DKYD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,DKYNUM                  R0=NUMBER OF KEY FIELDS               
         LA    R3,DKYDATA                                                       
         USING DKYDATA,R3                                                       
*                                                                               
FLTRK3   SR    R1,R1                                                            
         IC    R1,DKYLEN                  R1=LENGTH OF KEY FIELD                
         BCTR  R1,0                                                             
         SR    R2,R2                                                            
         IC    R2,DKYDSP                  DISPLACEMENT TO DATA                  
*                                                                               
         SR    R4,R4                                                            
         ICM   R4,1,DKYSRC                TEST SOURCE NUMBER                    
         BZ    FLTRK9                     NONE, FIELD IS OK                     
         MHI   R4,FDLNQ                                                         
         A     R4,AFDTAB                  R4=A(FILTER DATA)                     
         USING FDD,R4                                                           
         SR    RF,RF                                                            
         ICM   RF,3,FDKFLT                SPECIAL COMPARE ROUTINE               
         BZ    FLTRK5                                                           
         LA    R2,KEY(R2)                                                       
         AR    RF,RB                                                            
         BASR  RE,RF                      LET SPECIAL ROUTINE HANDLE            
         BE    FLTRK9                     OK, TO KEEP THIS KEY                  
         B     FLTRK17                    SKIP TO NEXT                          
         DROP  R4                                                               
*                                                                               
FLTRK5   LA    RE,KYMSK(R2)                                                     
         EX    R1,*+8                                                           
         B     *+10                                                             
         OC    0(0,RE),0(RE)              TEST ANY DATA IN MASK                 
         BZ    FLTRK9                     NO, THIS FIELD IS OK                  
         LA    R6,KEY(R2)                                                       
         EX    R1,FLTRKCMP                CLC KEY FIELD,KYMSK FIELD             
         BE    FLTRK9                                                           
*                                                                               
FLTRK7   EX    R1,FLTRKCMP                CLC KEY FIELD,KYMSK FIELD             
         BH    FLTRK11                    KEY IS HIGHER                         
         BL    FLTRK13                    KEY IS LOWER                          
FLTRK9   LA    R3,DKYLNQ(R3)                                                    
         BCT   R0,FLTRK3                                                        
         J     YES                        SET KEY OK                            
*                                                                               
FLTRK11  LA    RE,FFS                     SET FIELD TO FF'S                     
         CLI   0(R6),X'FF'                UNLESS IT IS FF'S                     
         BNE   FLTRK13                                                          
         GOTO1 SEQ                        READ PASSED IT                        
         XR    R1,R1                      TEST MINIMUM KEY                      
         IC    R1,KYLN                                                          
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   KEY(0),KEYSAVE                                                   
         JNE   NO                         ALL DONE                              
         B     FLTRK1                     START AGAIN                           
*                                                                               
FLTRK13  EX    R1,*+8                     MOVE FILTER DATA TO KEY               
         B     *+10                                                             
         MVC   0(0,R6),0(RE)                                                    
         B     FLTRK17                                                          
*                                                                               
FLTRK15  SR    R1,R1                                                            
         IC    R1,DKYLEN                  CLEAR REMAINDER OF FIELDS             
         BCTR  R1,0                                                             
         SR    R2,R2                                                            
         IC    R2,DKYDSP                  DISPLACEMENT TO DATA                  
         LA    R6,KEY(R2)                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         XC    0(0,R6),0(R6)              CLEAR KEY FIELD                       
*                                                                               
FLTRK17  LA    R3,DKYLNQ(R3)                                                    
         BCT   R0,FLTRK15                                                       
         J     NO                                                               
*                                                                               
FLTRKCMP CLC   0(0,R6),0(RE)              TEST KEY FIELD EQUAL MASK             
         DROP  R3                                                               
         EJECT                                                                  
***********************************************************************         
* GET THE PFKEY INFORMATION                                           *         
***********************************************************************         
GETPF    NTR1  ,                                                                
         MVI   PNFLAG,0                                                         
         NI    PCF,ALL-(PCFDSTCH+PCFREFSH)                                      
         TM    PCF,PCFNOCUR               TEST DATA NOT CURRENT                 
         BNO   *+16                                                             
         NI    MKTMEDH+4,ALL-(FINPVAL)    FORCE KEY CHANGE                      
         NI    PCF,ALL-(PCFNOCUR)                                               
         J     XIT                                                              
*                                                                               
         CLI   PFKEY,PFRSHQ               REFRESH ?                             
         JNE   *+16                                                             
         NI    MKTMEDH+4,ALL-(FINPVAL)    FORCE KEY CHANGE                      
         OI    PCF,PCFREFSH                                                     
         J     XIT                                                              
         CLI   PFKEY,PFXMTQ        SEND ALL?                                    
         BE    GETPFSND            YES                                          
*                                                                               
         LA    R2,MKTSELH                                                       
         USING SCRND,R2                                                         
         SR    R0,R0               NUMBER ON SCREEN                             
         ICM   R0,1,NSCRN          DON'T EXIT...COULD BE FIRST                  
         JZ    GETPF01             TIME THROUGH!!!                              
         TM    TRNSTAT,RACHANG                                                  
         BNZ   GETPF01                                                          
***                                                                             
* MAKE SURE NO FIELDS ARE MARKED P/N IF PDARPORN = N                            
* MARK BATCH RECORD IF PDARPORN = Y AND SEL FILED = P/N                         
***                                                                             
         MVI   PFLINE,0                                                         
GETPFPN  CLI   PDARPORN,C'Y'       PREVIOUSLY AVN?                              
         BNE   GETPFPN1            NO                                           
*                                                                               
         CLI   SCRNSEL,C'P'        NOT SEND ALL + 'P'?                          
         BNE   *+12                NO                                           
         MVI   PNFLAG,1                                                         
         BRAS  RE,VALIN            YES...MARK BATCH REC                         
         CLI   SCRNSEL,C'N'        NOT SEND ALL + 'N'?                          
         BNE   *+12                NO                                           
         MVI   PNFLAG,1                                                         
         BRAS  RE,VALIN            YES...MARK BATCH REC                         
         BAS   RE,BUMPPF                                                        
         B     GETPFPN5                                                         
*                                                                               
GETPFPN1 CLI   SCRNSEL,C'P'        REMIND USER THAT PROF IS NOT PREV            
         JE    ERRPRV2             AVN...SHOULD NOT THINK THAT THIS             
         CLI   SCRNSEL,C'N'        OPTION WILL DO ANYTHING                      
         JE    ERRPRV2                                                          
*                                                                               
GETPFPN5 LA    R2,SCRNLNQ(R2)                                                   
         BCT   R0,GETPFPN                                                       
         CLI   PNFLAG,1                                                         
         BNE   GETPF01                                                          
         NI    PCF,ALL-(PCFNEXTK)         REDISPLAY THE SAME LIST               
         B     GETPF01                                                          
*        J     XIT                                                              
***                                                                             
* PFKEY = SENDALL                                                               
***                                                                             
GETPFSND LA    R2,MKTSELH                                                       
         SR    R0,R0                                                            
         ICM   R0,1,NSCRN          NUMBER ON SCREEN                             
         JZ    XIT                                                              
*                                                                               
         CLI   PDARPORN,C'Y'       PREVIOUSLY AVN?                              
         BNE   GETPF00             NO...SKIP THIS CHECK                         
*                                                                               
         MVI   PFLINE,0                                                         
GETPFSN0 CLI   SCRNSEL,C'P'        PREVIOUSLY SENT?                             
         BE    *+16                YES...SKIP READ BATCH                        
         CLI   SCRNSEL,C'N'        NEW?                                         
         BE    *+8                 YES...SKIP READ BATCH                        
*                                                                               
         BRAS  RE,RDBATCH          READ BATCH TO SEE IF PREV/NEW IS SET         
         BAS   RE,BUMPPF                                                        
*                                                                               
         LA    R2,SCRNLNQ(R2)                                                   
         BCT   R0,GETPFSN0                                                      
*                                                                               
         LA    R2,MKTSELH                                                       
         IC    R0,NSCRN            NUMBER ON SCREEN                             
         MVI   PFLINE,0                                                         
***                                                                             
* SET FLIGHT ELEM IF NECESSARY AND TRANSMIT ALL OF THEM                         
***                                                                             
GETPF00  CLI   PDARPORN,C'Y'       PREVIOUSLY AVN?                              
         BNE   GETPF00A            NO...JUST TRANSMIT                           
         BRAS  RE,VALIN            SET BIT IN FLIGHT ELEM                       
         BAS   RE,BUMPPF                                                        
         B     GETPF00B            AND TRANSMIT                                 
*                                                                               
GETPF00A CLI   SCRNSEL,C'P'        REMIND USER THAT PROF IS NOT PREV            
         JE    ERRPRV2             AVN...SHOULD NOT THINK THAT THIS             
         CLI   SCRNSEL,C'N'        OPTION WILL DO ANYTHING                      
         JE    ERRPRV2                                                          
*                                                                               
GETPF00B MVI   SCRNSEL,C'T'        SET TRANSMIT FOR ALL                         
         MVI   SCRNSELH+5,1                                                     
         NI    SCRNSELH+4,ALL-(FINPVAL)                                         
         LA    R2,SCRNLNQ(R2)                                                   
         BCT   R0,GETPF00                                                       
         DROP  R2                                                               
*                                                                               
GETPF01  GOTO1 INITIAL,DMCB,PFTABLE       INITIALIZE PFKEY                      
         CLI   PFKEY,0                                                          
         JE    XIT                                                              
*                                                                               
         LH    R2,CURDISP                                                       
         AR    R2,RA                                                            
         LA    R0,MKTSELH                 CURSOR SHOULD BE WITHIN LIST          
         CR    R2,R0                                                            
         BL    *+14                       SET INVALID PF KEY                    
         LA    R0,MKTPFLNH                                                      
         CR    R2,R0                                                            
         BL    GETPF02                                                          
         L     R1,ATIOB                   SET CURSOR FOR ERROR                  
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         MVC   TIOBCURD-TIOBD(L'TIOBCURD,R1),CURDISP                            
         J     INVLDPF                                                          
*                                                                               
GETPF02  CLI   PFKEY,PFMISQ               MIS ?                                 
         JE    XIT                        YES                                   
*                                                                               
         CLI   PFKEY,PFBROQ               BROWSE?                               
         JE    XIT                        YES                                   
*                                                                               
         XR    R0,R0                                                            
         IC    R0,PFKEY                                                         
         AHI   R0,PFACTLQ                                                       
         STC   R0,PFKEY                                                         
*                                                                               
         XR    R0,R0                      GET FIRST BYTE                        
         LH    R1,CURDISP                 OF SELECT FIELD FOR CURSOR            
         AHI   R1,-(MKTSELH-T234FFD)                                            
         LA    R3,SCRNLNQ                                                       
         DR    R0,R3                                                            
         STC   R1,PFLINE                  SAVE LINE NUMBER                      
         MHI   R1,SCRNLNQ                                                       
         AHI   R1,(MKTSELH-T234FFD)                                             
         LR    R0,R1                                                            
         STCM  R0,3,CURDISP               DISPLACEMENT TO CURRENT LINE          
*                                                                               
         LA    R2,0(R1,RA)                R2=SCREEN LINE                        
         USING SCRND,R2                                                         
         SR    R3,R3                                                            
         IC    R3,PFLINE                                                        
         MHI   R3,LSTLNQ                                                        
         LA    R3,LSTTAB(R3)              R3=LIST ENTRY                         
         USING LSTD,R3                                                          
*                                                                               
         CLI   PFKEY,PFOVRAQ              DESTINATION OVERRIDE ?                
         BE    GETPF10                                                          
         CLI   PFKEY,PFCMTAQ              COMMENT PREVIEW ?                     
         BE    GETPF20                                                          
         CLI   PFKEY,PFSNDAQ              SEND ?                                
         BE    GETPF20                                                          
         CLI   PFKEY,PFPOLAQ              BRANDS?                               
         BE    GETPF20                                                          
         CLI   PFKEY,PFTRNAQ              TRANSMIT ?                            
         BE    GETPF15                                                          
*                                                                               
         L     R1,ATIOB                   SET CURSOR FOR ERROR                  
         OI    TIOBINDS-TIOBD(R1),TIOBSETC                                      
         MVC   TIOBCURD-TIOBD(L'TIOBCURD,R1),CURLINE                            
         J     INVLDPF                                                          
*                                                                               
GETPF10  MVI   MOFAXFLG,C' '                                                    
*&&DO                                                                           
* REMOVING FORCING FAX TO MEDIAOCEAN BECAUSE OF OOW  WHOA                       
         TM    LSTFLAG2,LSTMOFAX                                                
         BZ    GETPF45                                                          
         MVI   MOFAXFLG,C'O'                                                    
*&&                                                                             
         B     GETPF45                                                          
*                                                                               
GETPF15  SR    R0,R0                       INCREMENT SENT                       
         IC    R0,NSENT                                                         
         AHI   R0,1                                                             
         STC   R0,NSENT                                                         
         BRAS  RE,XMITALL                  SEE IF XMIT ALL ON EOLIST            
*                                                                               
GETPF20  OC    LSTBUYR,BLANKS                                                   
         CLC   LSTBUYR,BLANKS                                                   
         BNH   GETPF25                                                          
         CLC   LSTBUYR,BUYR               IS THIS THE ASSIGNED BUYER?           
         BE    GETPF25                                                          
         MVI   BLOCK,L'BUYR+1             L'LENGTH + L'MESSAGE                  
         MVC   BLOCK+1(L'BUYR),LSTBUYR                                          
         MVI   BLOCK+L'BUYR+1,0           TERMINATING 0                         
         J     BYRALRDY                   BUYER ALREADY ASSIGNED                
*                                                                               
GETPF25  XC    PASSPRD,PASSPRD                                                  
         MVC   PASSPRD,SCRNPRD                                                  
*                                                                               
         CLC   SCRNPRD(3),=C'POL'                                               
         BNE   GETPF27                                                          
         MVC   PASSPRD,=CL7'***'                                                
         B     GETPF30                                                          
*                                                                               
GETPF27  CLI   PFKEY,PFPOLAQ       PRODUCT IS NOT POL                           
         JE    INVLDPF             AND THEN WANT TO DO BRANDS?  NO GOOD         
*                                                                               
GETPF30  CLI   PFKEY,PFCMTAQ       ONLY CREATE CASH FOR COMMENT PREVIEW         
         BE    GETPF35                                                          
*                                                                               
GETPF35  XC    ESTWFLT,ESTWFLT                                                  
         MVC   ESTWFLT(L'SCRNEST),SCRNEST                                       
*                                                                               
         TM    SCRNDSTH+4,FINPVAL         DESTINATION CHANGE?                   
         BNO   GETPF37                    YES,                                  
         TM    SCRNSTDH+4,FINPVAL         STANDARD COMMENT CHANGE?              
         BNO   GETPF37                    YES,                                  
         TM    SCRNFFTH+4,FINPVAL         TEXT CHANGE ?                         
         BO    GETPF38                                                          
*                                                                               
GETPF37  BRAS  RE,VALIN                   VALIDATE / CHANGE LINE                
*                                                                               
GETPF38  CLC   SCRNFLT,BLANKS             TEST FLIGHT FLD                       
         BNH   *+14                                                             
         MVI   ESTWFLT+L'SCRNEST,C'/'                                           
         MVC   ESTWFLT+L'SCRNEST+1(L'SCRNFLT),SCRNFLT                           
*                                                                               
         CLI   SCRNLNK,C'*'                                                     
         BE    GETPF45                                                          
         CLC   SCRNSTD,BLANKS                                                   
         BNH   GETPF40                                                          
         L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',SCRNSTDH,,GLVBUY1                             
*                                                                               
GETPF40  L     RF,ACOMFACS                                                      
         L     RF,CGLOBBER-COMFACSD(RF)                                         
         GOTO1 (RF),DMCB,=C'PUTF',SCRNFFTH,,GLVSMSG                             
*                                                                               
GETPF45  NI    PCF,ALL-(PCFNEXTK)         REDISPLAY THE SAME LIST               
         OI    CTLRFLG1,CF1TSELQ          DON'T TEST THE SEL CODES              
*                                                                               
         CLI   PFKEY,PFSNDAQ              SEND ?                                
         BE    *+12                       YES                                   
*                                                                               
         CLI   PFKEY,PFTRNAQ              TRANSMIT ?                            
         BNE   GETPF50                                                          
*                                                                               
         CLI   QMED,C'R'           RADIO?                                       
         BNE   GETPF50             NO, ONLY RADIO REQUIRES SALESPERSON          
*                                                                               
         BRAS  RE,RDBTCSLP         DOES BATCH RECORD HAVE SALESPERSON?          
         BE    GETPF50             YES                                          
*                                                                               
         BRAS  RE,RDLMSLP          DOES LAST METH REC HAVE SALESPERSON?         
         BNE   GETPF50             NO                                           
         USING DMSPELD,R6                                                       
         XC    BLOCK,BLOCK                                                      
         LA    R4,BLOCK                                                         
         USING GLBRWKW,R4                                                       
         MVC   GLBRWKW(20),DMSPNAME       SLP FROM LM REC                       
         TM    DMSPFLAG,DMSPPPER          IS THIS A POINTPERSON?                
         BZ    *+8                        NO                                    
         OI    GLBRWFLG,X'80'                                                   
         MVC   BRWKEY2,LSTBTK             BATCH KEY                             
         BRAS  RE,UPDBTSLP                YES, UPDATE THE BATCH                 
*                                                                               
GETPF50  OI    PCF2,PCFSWAP               SO WE REDISPLAY LAST PAGE             
         GOTO1 INITIAL,DMCB,PFTABLE                                             
         J     XIT                                                              
         DROP  R2,R3,R4,R6                                                      
         EJECT                                                                  
*                                                                               
SETUP    MVI   BLANKS,C' '                                                      
         MVC   BLANKS+1(L'BLANKS-1),BLANKS                                      
         MVI   FFS,X'FF'                                                        
         MVC   FFS+1(L'FFS-1),FFS                                               
         BR    RE                                                               
         EJECT                                                                  
RELOTAB  DS    0D                                                               
         DC    AL4(OPTTAB),AL4(AOPTTAB-LSSD)                                    
         DC    AL4(KEYTAB),AL4(AKEYTAB-LSSD)                                    
         DC    AL4(FDTAB),AL4(AFDTAB-LSSD)                                      
*****    DC    V(SPOMCOM),AL4(SPOMCOM-LSSD)                                     
         DC    X'FF'                                                            
*                                                                               
PFDATA1  DC    C'PF2=Rfrsh 4=Sndall 5=Ovrd'                                     
PFDATA2  DC    C'6=Xmit 7=Brands 8=Cmnts 9=Ord 10=MIS 11=BRW'                   
         EJECT                                                                  
***********************************************************************         
* BUMP THE PFLINE...WE ARE LOOPING IN GETPF WITH P/N STUFF            *         
***********************************************************************         
BUMPPF   DS    0H                                                               
         ZIC   R5,PFLINE                                                        
         AHI   R5,1                                                             
         STC   R5,PFLINE                                                        
         BR    RE                                                               
***********************************************************************         
* LIST PFKEY TABLE DEFINITIONS                                        *         
***********************************************************************         
PFENTQ   EQU   0                          'ENTER'                               
PFRSHQ   EQU   2                          REFRESH                               
PFXMTQ   EQU   4                          SEND ALL(TRANSMIT ALL)                
PFOVRQ   EQU   5                          DESTOV  - SCREEN                      
PFTRNQ   EQU   6                          TRANSMIT                              
PFPOLQ   EQU   7                          BRANDS - POL                          
PFCMTQ   EQU   8                          COMMENT PREVIEW                       
PFSNDQ   EQU   9                          SEND - SCREEN                         
PFMISQ   EQU   10                         MIS                                   
PFBROQ   EQU   11                         BROWSE                                
PFRTNQ   EQU   12                         RETURN                                
PFACTLQ  EQU   12                                                               
PFOVRAQ  EQU   PFOVRQ+PFACTLQ             OVERRIDE                              
PFTRNAQ  EQU   PFTRNQ+PFACTLQ             TRANSMIT                              
PFPOLAQ  EQU   PFPOLQ+PFACTLQ             BRANDS                                
PFCMTAQ  EQU   PFCMTQ+PFACTLQ             COMMENT PREVIEW                       
PFSNDAQ  EQU   PFSNDQ+PFACTLQ             SEND                                  
*                                                                               
*                                                                               
PFTABLE  DS    0C                                                               
*                                                                               
* REFRESH                                                                       
         DC    AL1(PF02X-*,PFRSHQ,0,0,0,0)                                      
         DC    CL3' ',CL8' ',CL8' '                                             
PF02X    EQU   *                                                                
*                                                                               
* SEND ALL                                                                      
         DC    AL1(PF04X-*,PFXMTQ,0,0,0,0)                                      
         DC    CL3' ',CL8' ',CL8' '                                             
PF04X    EQU   *                                                                
*                                                                               
* OVERRIDE                                                                      
         DC    AL1(PF05X-*,PFOVRQ,0,0,0,PFTRETRN)                               
         DC    CL3'O',CL8' ',CL8' '                                             
PF05X    EQU   *                                                                
*                                                                               
* TRANSMIT                                                                      
         DC    AL1(PF06X-*,PFTRNQ,0,0,0,PFTRETRN)                               
         DC    CL3'T',CL8' ',CL8' '                                             
PF06X    EQU   *                                                                
*                                                                               
* BRANDS                                                                        
         DC    AL1(PF07X-*,PFPOLQ,0,0,0,PFTRETRN)                               
         DC    CL3'B',CL8' ',CL8' '                                             
PF07X    EQU   *                                                                
*                                                                               
* COMMENT PREVIEW                                                               
         DC    AL1(PF08X-*,PFCMTQ,0,0,0,PFTRETRN)                               
         DC    CL3'C',CL8' ',CL8' '                                             
PF08X    EQU   *                                                                
*                                                                               
* SEND                                                                          
         DC    AL1(PF09X-*,PFSNDQ,0,0,0,PFTRETRN)                               
         DC    CL3'X',CL8' ',CL8' '                                             
PF09X    EQU   *                                                                
*                                                                               
* MIS                                                                           
         DC    AL1(PF10X-*,PFMISQ,0,0,0,PFTRETRN)                               
         DC    CL3'M',CL8' ',CL8' '                                             
PF10X    EQU   *                                                                
*                                                                               
* BROWSE                                                                        
         DC    AL1(PF11X-*,PFBROQ,0,0,0,PFTRETRN)                               
         DC    CL3'=',CL8' ',CL8' '                                             
PF11X    EQU   *                                                                
*                                                                               
* RETURN TO CALLER                                                              
         DC    AL1(PF12X-*,PFRTNQ,PFTRPROG,0,0,0)                               
         DC    CL3' ',CL8' ',CL8' '                                             
PF12X    EQU   *                                                                
*                                                                               
                                                                                
* ACTUAL DESTINATION OVERRIDE                                                   
         DC    AL1(PF17X-*,PFOVRAQ,PFTCPROG,0,(PF17X-PF17)/KEYLNQ,0)            
         DC    CL3' ',CL8'DESTOV',CL8'MAINT'                                    
PF17     DC    AL1(KEYTYTWA,L'MKTMED-1),AL2(MKTMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'MKTBUYR-1),AL2(MKTBUYR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'SCRNSTA-1),AL2(SCRNSTA-SCRNBUY)                   
         DC    AL1(KEYTYTWA,L'MKTCLT-1),AL2(MKTCLT-T234FFD)                     
         DC    AL1(KEYTYWS,L'MOFAXFLG),AL2(MOFAXFLG-LSSD)                       
         DC    AL1(KEYTYCUR,L'SCRNDST-1),AL2(SCRNDST-SCRNBUY)                   
PF17X    EQU   *                                                                
                                                                                
* ACTUAL TRANSMIT                                                               
         DC    AL1(PF18X-*,PFTRNAQ,PFTCPROG,0,(PF18X-PF18)/KEYLNQ,0)            
         DC    CL3' ',CL8'ORDER',CL8'TRANSMIT'                                  
PF18     DC    AL1(KEYTYTWA,L'MKTMED-1),AL2(MKTMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'MKTBUYR-1),AL2(MKTBUYR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'SCRNTYPE-1),AL2(SCRNTYPE-SCRNBUY)                 
         DC    AL1(KEYTYTWA,L'MKTCLT-1),AL2(MKTCLT-T234FFD)                     
         DC    AL1(KEYTYWS,L'PASSPRD-1),AL2(PASSPRD-LSSD)                       
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-LSSD)                       
         DC    AL1(KEYTYCUR,L'SCRNSTA-1),AL2(SCRNSTA-SCRNBUY)                   
PF18X    EQU   *                                                                
*                                                                               
* ACTUAL ORDER/BRANDS                                                           
         DC    AL1(PF19X-*,PFPOLAQ,PFTCPROG,0,(PF19X-PF19)/KEYLNQ,0)            
         DC    CL3' ',CL8'ORDER',CL8'BRANDS'                                    
PF19     DC    AL1(KEYTYTWA,L'MKTMED-1),AL2(MKTMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'MKTBUYR-1),AL2(MKTBUYR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'SCRNTYPE-1),AL2(SCRNTYPE-SCRNBUY)                 
         DC    AL1(KEYTYTWA,L'MKTCLT-1),AL2(MKTCLT-T234FFD)                     
         DC    AL1(KEYTYWS,L'PASSPRD-1),AL2(PASSPRD-LSSD)                       
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-LSSD)                       
         DC    AL1(KEYTYCUR,L'SCRNSTA-1),AL2(SCRNSTA-SCRNBUY)                   
PF19X    EQU   *                                                                
*                                                                               
* COMMENT PREVIEW                                                               
         DC    AL1(PF20X-*,PFCMTAQ,PFTCPROG,0,(PF20X-PF20)/KEYLNQ,0)            
         DC    CL3' ',CL8'ORDER',CL8'PREVIEW'                                   
PF20     DC    AL1(KEYTYTWA,L'MKTMED-1),AL2(MKTMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'MKTBUYR-1),AL2(MKTBUYR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'SCRNTYPE-1),AL2(SCRNTYPE-SCRNBUY)                 
         DC    AL1(KEYTYTWA,L'MKTCLT-1),AL2(MKTCLT-T234FFD)                     
         DC    AL1(KEYTYWS,L'PASSPRD-1),AL2(PASSPRD-LSSD)                       
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-LSSD)                       
         DC    AL1(KEYTYCUR,L'SCRNSTA-1),AL2(SCRNSTA-SCRNBUY)                   
PF20X    EQU   *                                                                
*                                                                               
* ACTUAL SEND                                                                   
         DC    AL1(PF21X-*,PFSNDAQ,PFTCPROG,0,(PF21X-PF21)/KEYLNQ,0)            
         DC    CL3' ',CL8'ORDER',CL8'SEND'                                      
PF21     DC    AL1(KEYTYTWA,L'MKTMED-1),AL2(MKTMED-T234FFD)                     
         DC    AL1(KEYTYTWA,L'MKTBUYR-1),AL2(MKTBUYR-T234FFD)                   
         DC    AL1(KEYTYCUR,L'SCRNTYPE-1),AL2(SCRNTYPE-SCRNBUY)                 
         DC    AL1(KEYTYTWA,L'MKTCLT-1),AL2(MKTCLT-T234FFD)                     
         DC    AL1(KEYTYWS,L'PASSPRD-1),AL2(PASSPRD-LSSD)                       
         DC    AL1(KEYTYWS,L'ESTWFLT-1),AL2(ESTWFLT-LSSD)                       
         DC    AL1(KEYTYCUR,L'SCRNSTA-1),AL2(SCRNSTA-SCRNBUY)                   
PF21X    EQU   *                                                                
*                                                                               
         DC    X'FF'               END OF TABLE                                 
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* VALIDATE THE DESTINATION FIELD                                                
***********************************************************************         
         USING SCRND,R4                                                         
VALIDEST NTR1  BASE=*,LABEL=*                                                   
         LA    R8,SYSSPARE         R8=A(LOCAL STORAGE AREA)                     
         USING LSSD,R8                                                          
*  NEED THE ABOVE FOR THE STUPID ADDCOMMA SUBROUTINE FAKE FIELD                 
*                                                                               
         SR    R3,R3                                                            
         IC    R3,PFLINE                                                        
         MHI   R3,LSTLNQ                                                        
         LA    R3,LSTTAB(R3)                                                    
         USING LSTD,R3                                                          
*                                                                               
         LA    R2,SCRNDSTH                                                      
         TM    4(R2),X'20'         PREVIOUSLY VALIDATED                         
         BNZ   VRDSTYES                                                         
         CLC   =C'REP',8(R2)                                                    
         BE    *+14                                                             
         CLC   =C'STA',8(R2)                                                    
         BNE   VRDSTNO                                                          
*                                                                               
         MVC   FLDH,SCRNROUH                                                    
         MVI   FLDH,68             LENGTH OF HEADER + FIELD                     
*                                                                               
         LA    RE,FLDH                                                          
         ST    RE,DMCB+4           <=== WE'RE OVERWRITING 2ND PARAM             
*                                     ARE WE RADIO & IS PROFILE SET TO          
         CLI   SCRNPORN,C'P'                                                    
         BE    VRDST04                                                          
*&&DO                                                                           
* REMOVING FORCING FAX TO MEDIAOCEAN BECAUSE OF OOW  WHOA  2007/05/21           
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
         TM    LSTFLAG2,LSTMOFAX   FAXING OOW TO MO?                            
         BO    VRDST04             YES                                          
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04& IS PROFILE SET TO             
*&&                                                                             
         TM    MISCFLG1,MF1FXRAD   FAX ALL RADIO ORDERS?                        
         BNO   VRDST05                                                          
VRDST04  MVI   DMCB+4,X'80'        FORCE FAX/EML (PREV BIT)                     
*                                                                               
*  ADDING IN FAKE FIELD TO PASS THROUGH SQUASHER SO THE NAME FITS               
VRDST05  GOTO1 RD4ROUTE,DMCB,SCRNDSTH,,(MISCFLG1,0),0                           
         BE    VRDST06                                                          
*                                                                               
         CLC   GERROR,=AL2(NODRADM)                                             
         BNE   VRDST05D                                                         
         XC    SCRNROU,SCRNROU                                                  
         MVC   SCRNROU(26),=CL26'need special fax#- use PF5'                    
         XC    GERROR,GERROR                                                    
         B     VRDST10                                                          
*                                                                               
VRDST05D CLC   GERROR,=AL2(NODSTDOV) DESTINE FOR REP, USE DESTOV                
         JNE   ERREXIT                                                          
         XC    SCRNROU,SCRNROU                                                  
         MVC   SCRNROU(26),=CL26'DESTINE IS FOR REP. PF5'                       
         XC    GERROR,GERROR                                                    
         B     VRDST10                                                          
*                                                                               
VRDST06  LA    R5,L'SCRNROU        LENGTH OF FIELD (FOR SQUASHER)               
         CLC   =CL5'EML: ',FLD                                                  
         BNE   *+8                                                              
         BRAS  RE,ADDCOMMA         ADD COMMA WHEN IT'S EMAIL ROUTE              
         GOTO1 VSQUASH,DMCB,FLD,(R5)   CALL TO SQUASHER                         
         MVC   SCRNROU,FLD                                                      
         OI    SCRNROUH+6,X'80'                                                 
*                                                                               
VRDST10  LA    R6,KEY              LET'S SEE IF WE HAVE LSTM                    
         XC    KEY,KEY                                                          
         USING DMTHKEY,R6                                                       
         MVI   DMTHTYPE,DMTHTYPQ        X'0D3E'                                 
         MVI   DMTHSBTY,DMTHSBTQ                                                
         MVC   DMTHAGMD,BAGYMD                                                  
         MVC   DMTHBYR,QBYR                                                     
         MVC   DMTHSTA,BSTA                                                     
         MVC   DMTHCLT,BCLT                                                     
*                                                                               
         OI    DMINBTS,X'08'        PASS BACK DELETED                           
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO3                                                         
         CLC   KEY(13),KEYSAVE     ANY LAST METHOD BY THIS CLIENT?              
         BE    VRDST50             YES                                          
*                                                                               
         L     RE,AIO                                                           
         LHI   RF,LIOS                                                          
         XCEFL                                                                  
*                                                                               
         MVI   ELEM,0              DO NOT ADD SALESPERSON ELEMENT               
         BRAS  RE,ADDLMREC                                                      
         B     VRDSTYES                                                         
*                                                                               
VRDST50  MVI   RDUPDATE,C'Y'        NEED IF DELETED TO WRITE BACK               
         GOTO1 HIGH                                                             
         TM    KEY+13,X'80'         DELETED KEY?                                
         BZ    VRDST55              NO, NO NEED TO WRITE BACK                   
         NI    KEY+13,X'FF'-X'80'                                               
         GOTO1 WRITE                                                            
*                                                                               
VRDST55  OI    DMINBTS,X'08'        PASS BACK DELETED                           
         MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC               UPDATE THE LAST METHOD RECORD               
         L     R6,AIO                                                           
         NI    15(R6),X'FF'-X'80'   DELETED BIT IS AFTER LENGTH                 
         LA    R6,24(R6)                                                        
         USING DMTHELD,R6                                                       
         MVI   DMTHMTHD,C'I'                                                    
         CLC   =C'FAX: ',SCRNROU   WAS IT FAXING?                               
         BNE   *+12                                                             
         MVI   DMTHMTHD,C'F'                                                    
         B     VRDST60                                                          
*                                                                               
         CLC   =C'EML: ',SCRNROU   WAS IT EMAILING?                             
         BNE   VRDST60                                                          
         MVI   DMTHMTHD,C'E'                                                    
         L     R1,AIO2             SHOULD HAVE BDE RECORD                       
         LA    R1,GBDRFST(R1)      X'01' - FULL NAME ELEM                       
         USING GBNELD,R1                                                        
         MVC   DMTHBDEC,GBNFNM                                                  
         DROP  R1                                                               
*                                                                               
VRDST60  CLC   DMTHDEST,SCRNDST                                                 
         BE    VRDST70                                                          
         MVC   DMTHDEST,SCRNDST    COPY 1ST LETTER OF DEST                      
         DROP  R6                                                               
         MVI   ELCODE,DMFXELQ        X'10' - TEMP FAX OVERRIDE ELEM             
         BRAS  RE,NEXTEL                                                        
         BNE   VRDST70                                                          
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)    DELETE THIS                   
*                                                                               
VRDST70  MVI   ACTELOPT,C'Y'                                                    
         XC    KEY,KEY                                                          
         L     R6,AIO                                                           
         MVC   KEY(13),0(R6)                                                    
         GOTO1 PUTREC                                                           
         MVI   ACTELOPT,C'N'                                                    
*                                                                               
VRDSTYES OI    SCRNDSTH+4,X'20'                                                 
         CR    R1,R1                                                            
         B     VRDSTX                                                           
VRDSTNO  LTR   RB,RB                                                            
VRDSTX   MVC   AIO,AIO1                                                         
         J     XIT                                                              
         DROP  R4,R3                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* VALIDATE CHANGED DATA FIELDS ON THE ENTIRE SCREEN                   *         
***********************************************************************         
VCHNG    NTR1  BASE=*,LABEL=*                                                   
         LA    R4,MKTSELH                                                       
         USING SCRND,R4                                                         
         LA    R3,LSTTAB                                                        
         USING LSTD,R3                                                          
         SR    R0,R0                                                            
         IC    R0,NSCRN                                                         
*                                                                               
VCHNG03  CLI   SCRNSEL,C' '               ANYTHING IN SELECT FIELD?             
         BNH   VCHNG03A                                                         
         CLI   PFKEY,0                    AND PFKEY IS NOT SET                  
         BE    VCHNG15                    SKIP VALIDATION FOR THIS LINE         
*                                                                               
VCHNG03A TM    SCRNDSTH+4,FINPVAL         DESTINATION STILL VALID?              
         BO    VCHNG11                                                          
         OI    LSTIND,LSTICHG             SET LINE ITEM CHANGE                  
         OI    PCF,PCFDSTCH               SET DESTINATION CHANGE                
         LA    R2,SCRNDSTH                TEST DESTINATION                      
         CLI   5(R2),3                    LENGTH MUST BE 3                      
         JNE   INVLFLD                                                          
         CLC   SCRNDST(3),=C'REP'         MUST BE REP                           
         BE    *+14                                                             
         CLC   SCRNDST(3),=C'STA'         OR STA(TION)                          
         JNE   INVLFLD                                                          
*                                                                               
         LA    RF,LSTBTK                  VALIDATE DESTINATION                  
         USING DBTKEY,RF                                                        
         MVC   BSTA,DBTKSTA                                                     
         MVC   BCLT,DBTKCLT                                                     
*                                                                               
         MVC   QSTA,SCRNSTA                                                     
         CLI   QSTA+4,C' '                                                      
         BH    *+10                                                             
         MVC   QSTA+4(1),QMED                                                   
         DROP  RF                                                               
*                                                                               
         CLI   QMED,C'R'                                                        
         BNE   VCHNG08                                                          
         MVC   FLDH(8),SCRNDSTH                                                 
         MVC   FLD(5),SCRNSTA                                                   
         MVI   FLDH+5,5                                                         
         LA    R2,FLDH                                                          
         GOTO1 VALISTA             THIS WAY WE GET QUNIQID                      
*                                                                               
VCHNG08  BRAS  RE,VALIDEST                                                      
         BNE   *+12                                                             
         OI    PCF2,PCFSWAP               SO WE REDISPLAY LAST PAGE             
         B     VCHNG11                                                          
*                                                                               
         LA    R2,SCRNROUH                                                      
         CLC   GERROR,=AL2(NODESTN)  NO SFM DESTINE                             
         BNE   VCHNG09                                                          
         XC    SCRNROU,SCRNROU                                                  
         MVC   SCRNROU(26),=CL26'SFM DESTINE record missing'                    
         J     ERREXIT                                                          
*                                                                               
VCHNG09  CLC   GERROR,=AL2(NODRADM)  NO DARADM FAX RECORD FOR REP               
         JNE   INVLFLD             SOME OTHER ERROR                             
         XC    SCRNROU,SCRNROU                                                  
         MVC   SCRNROU(26),=CL26'need special fax#- use PF5'                    
         J     ERREXIT                                                          
*                                                                               
VCHNG11  OI    SCRNDSTH+4,FINPVAL         SET DESTINATION VALID                 
         TM    SCRNSTDH+4,FINPVAL         CHANGED STANDARD COMMENTS ?           
         BO    VCHNG13                                                          
         OI    LSTIND,LSTICHG             SET LINE ITEM CHANGE                  
         CLI   SCRNSTDH+5,0               ANY STANDARD COMMENT ?                
         BE    VCHNG13                                                          
         LA    R2,SCRNSTDH                VALIDATE STANDARD COMMENT             
         GOTO1 VALISCOM,DMCB,8(R2)                                              
         JNE   RECNTFND                                                         
*                                                                               
VCHNG13  OI    SCRNSTDH+4,FINPVAL         SET STANDARD COMMENT VALID            
         TM    SCRNFFTH+4,FINPVAL         CHANGED TEXT ?                        
         BO    *+8                                                              
         OI    LSTIND,LSTICHG             SET LINE ITEM CHANGE                  
         OI    SCRNFFTH+4,FINPVAL         SET FREE FORM TEXT VALID              
*                                                                               
VCHNG15  TM    LSTIND,LSTICHG             TEST LINE CHANGED                     
         BZ    VCHNG20                                                          
         NI    PCF,ALL-(PCFNEXTK)         YES, REDISPLAY THE PAGE               
         NI    SCRNSTDH+4,ALL-FINPVAL    FORCE GENCON/VREC ON THIS LINE         
*                                                                               
VCHNG20  LA    R4,SCRNLNQ(R4)                                                   
         LA    R3,LSTLNQ(R3)                                                    
         BCT   R0,VCHNG03                                                       
*                                                                               
****     TM    PCF,PCFDSTCH               SET DESTINATION CHANGE                
****     JO    DSTNUP                                                           
         J     YES                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
         LTORG                                                                  
***********************************************************************         
* VALIDATE THE KEY                                                    *         
***********************************************************************         
VK       NMOD1 0,*VALK                                                          
         L     RC,BASERC                                                        
         USING GEND,RC                                                          
         NI    PCF,ALL-(PCFKYCHG)         TURNOFF KEY CHANGED                   
****     MVC   LLIST,=Y(SCRNLNQ)          LENGTH OF SCREEN ENTRY                
         MVC   LLIST,=Y(SCRNLNQ2)         LENGTH OF THE FACTS                   
         MVI   NLISTS,NSCRQ               MAX ON SCREEN                         
*                                                                               
         TM    MKTMEDH+4,FINPVAL          TEST CHANGES TO MEDIA                 
         BNO   VK05                                                             
         TM    MKTBUYRH+4,FINPVAL         BUYER                                 
         BNO   VK05                                                             
         TM    MKTMRKTH+4,FINPVAL         MARKET                                
         BNO   VK05                                                             
         TM    MKTSTAH+4,FINPVAL          STATION                               
         BNO   VK05                                                             
         TM    MKTCLTH+4,FINPVAL          CLIENT                                
         BNO   VK05                                                             
         TM    MKTPRDH+4,FINPVAL          PRODUCT                               
         BNO   VK05                                                             
         TM    MKTESTH+4,FINPVAL          ESTIMATE                              
         BNO   VK05                                                             
         TM    MKTFLTH+4,FINPVAL          FLIGHT                                
         BNO   VK05                                                             
         TM    MKTOPTNH+4,FINPVAL         OPTIONS                               
         BNO   VK05                                                             
         B     VKMED                                                            
*                                                                               
VK05     NI    MKTMEDH+4,ALL-(FINPVAL)    TURNOFF VALIDATED                     
         NI    MKTBUYRH+4,ALL-(FINPVAL)                                         
         NI    MKTMRKTH+4,ALL-(FINPVAL)                                         
         NI    MKTSTAH+4,ALL-(FINPVAL)                                          
         NI    MKTCLTH+4,ALL-(FINPVAL)                                          
         NI    MKTPRDH+4,ALL-(FINPVAL)                                          
         NI    MKTESTH+4,ALL-(FINPVAL)                                          
         NI    MKTFLTH+4,ALL-(FINPVAL)                                          
         NI    MKTOPTNH+4,ALL-(FINPVAL)                                         
         OI    PCF,PCFKYCHG               SET KEY CHANGED                       
         MVI   NSENT,0                    INITIALIZE NUMBER 'SENT'              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE MEDIA                                                  *         
***********************************************************************         
         MVI   MISCFLG1,0                                                       
VKMED    LA    R2,MKTMEDH                                                       
         CLI   5(R2),0                    NEED THE MEDIA                        
         BNE   VKMED3                                                           
         J     NEEDFLDS                                                         
*                                                                               
VKMED3   GOTO1 VALIMED                                                          
         OI    4(R2),FINPVAL                                                    
         MVC   AGYMD,BAGYMD                                                     
*                                                                               
VKMED10  DS    0H                                                               
         MVC   BYTE,BAGYMD                                                      
         NI    BYTE,X'0F'                                                       
         CLI   BYTE,X'02'                                                       
         BNE   *+8                                                              
         OI    MISCFLG1,MF1RADIO                                                
         OI    6(R2),X'80'                                                      
*                                                                               
VKMEDX   DS    0H                                                               
         EJECT                                                                  
***********************************************************************         
* VALIDATE BUYER                                                      *         
***********************************************************************         
VKBYR    XC    BUYR,BUYR                                                        
         LA    R2,MKTBUYRH                                                      
         CLI   5(R2),0                                                          
         JNE   *+20                                                             
         CLI   TWAOFFC,C'*'               DDS TERMINAL?                         
         BNE   MISSFLD                                                          
         OI    6(R2),X'80'                                                      
         OI    4(R2),FINPVAL                                                    
         B     VKBYRX                                                           
         GOTO1 VALIBUYR,DMCB,8(R2)                                              
         JNE   INVLFLD                                                          
         OI    4(R2),FINPVAL                                                    
         MVC   BUYR,8(R2)                                                       
         OC    BUYR,BLANKS                                                      
         OI    6(R2),X'80'                                                      
*                                                                               
         MVI   BRWPREF,0                                                        
         L     R6,AIO                                                           
         MVI   ELCODE,BYRDCD2Q                                                  
         BAS   RE,GETEL                                                         
         BNE   VKBYRX                                                           
         USING BYRDSCD2,R6                                                      
         CLI   BYRBRWPF,0                                                       
         BE    VKBYRX                                                           
         MVC   BRWPREF,BYRBRWPF                                                 
*                                                                               
VKBYRX   DS    0H                                                               
                                                                                
***********************************************************************         
* VALIDATE MARKET                                                     *         
***********************************************************************         
VKMKT    NI    FFLAG,ALL-(FFMKT)          TURNOFF MARKET FILTER                 
         LA    R2,MKTMRKTH                DID THIS FIELD CHANGE?                
         CLI   5(R2),0                    TEST MARKET INPUT                     
         BNE   VKMKT3                                                           
         CLI   MKTSTAH+5,0                NO, THEN MUST HAVE STATION            
         JE    MISSFLD                                                          
         B     VKMKTX                                                           
*                                                                               
VKMKT3   TM    4(R2),FINPNUM              VALID NUMERIC?                        
         JZ    INVLFLD                    NO                                    
         XR    R1,R1                      SAVE THE MARKET NUMBER                
         IC    R1,5(R2)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         STCM  R1,3,FLTMKT                                                      
         OI    FFLAG,FFMKT                                                      
*                                                                               
VKMKTX   OI    4(R2),FINPVAL              SET FIELD VALID                       
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE STATION                                                    *         
***********************************************************************         
VKSTA    NI    FFLAG,ALL-(FFSTA)          TURNOFF STATION FILTER                
         LA    R2,MKTSTAH                 DID THIS FIELD CHANGE?                
         CLI   5(R2),0                    ANY INPUT?                            
         BE    VKSTAX                                                           
         CLI   8(R2),C'0'                 CABLE STATION?                        
         JL    VKSTA10                                                          
         CLI   5(R2),4                    YES, LENGTH OF 4?                     
         JNE   INVLFLD                    YES, NOT YET                          
*                                                                               
VKSTA10  GOTO1 VALISTA                                                          
         OI    FFLAG,FFSTA                                                      
         MVC   FLTSTA,BSTA                                                      
*                                                                               
VKSTAX   OI    4(R2),FINPVAL              SET FIELD IS VALID                    
         OI    6(R2),X'80'                                                      
                                                                                
***********************************************************************         
* VALIDATE CLIENT                                                     *         
***********************************************************************         
VKCLT    NI    FFLAG,ALL-(FFCLT)          TURNOFF CLIENT FILTER                 
         LA    R2,MKTCLTH                 DID THIS FIELD CHANGE?                
         CLI   5(R2),0                    ANY INPUT?                            
         JE    MISSFLD                                                          
         GOTO1 VALICLT                                                          
         OI    FFLAG,FFCLT                                                      
         MVC   FLTCLT,BCLT                                                      
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(4),=C'sDAR'                                                 
         MVC   WORK+4(2),AGENCY                                                 
         MVC   WORK+6(1),QMED                                                   
         MVC   WORK+7(3),QCLT                                                   
*                                                                               
         CLI   CLTOFFCE,C' '                                                    
         BNH   *+14                                                             
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),CLTOFFCE                                              
*                                                                               
         GOTO1 GETPROF,DMCB,WORK,PROFDAR,DATAMGR                                
*                                                                               
         MVC   WORK(4),=C'S0OM'                                                 
         GOTO1 (RF),(R1),WORK,PROFOM,DATAMGR                                    
*                                                                               
         TM    MISCFLG1,MF1RADIO   AM I RADIO?                                  
         BZ    VKCLTX                                                           
         CLI   POMFXRAD,C'N'       ONLY FAX RADIO ORDERS?                       
         BE    *+8                 NO                                           
         OI    MISCFLG1,MF1FXRAD   YES!!                                        
*                                                                               
VKCLTX   OI    4(R2),FINPVAL              SET FIELD IS VALID                    
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
***********************************************************************         
* VALIDATE PRODUCT FILTER                                             *         
***********************************************************************         
VKPRD    NI    FFLAG,ALL-(FFPRD+FFPR2)    TURNOFF PRODUCT FILTERS               
         MVC   FLTPRDC,BLANKS                                                   
         MVC   FLTPR2C,BLANKS                                                   
         LA    R2,MKTPRDH                 DID THIS FIELD CHANGE?                
         CLI   5(R2),0                    ANY INPUT?                            
         BE    VKPRDX                                                           
         TM    FFLAG,FFCLT                                                      
         BNZ   VKPRD3                                                           
         LA    R2,MKTCLTH                                                       
         J     MISSFLD                                                          
*                                                                               
VKPRD3   CLI   5(R2),7                    XX(X)  OR  XX(X)-XX(X)                
         JH    INVLFLD                                                          
*                                                                               
         L     RE,AIO3                                                          
         LHI   RF,LIOS                                                          
         XCEFL                                                                  
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'82',AIO3),C',=--'                           
         CLI   4(R1),0                                                          
         JE    INVLFLD                                                          
         CLI   4(R1),2                                                          
         JH    INVLFLD                                                          
*                                                                               
         L     R5,ATIOB                                                         
         USING TIOBD,R5                                                         
         LR    R0,R2                      SET CURSOR TO FIRST PRODUCT           
         SR    R0,RA                                                            
         STCM  R0,3,TIOBCURD                                                    
         OI    TIOBINDS,TIOBSETC                                                
         MVI   TIOBCURI,0                                                       
*                                                                               
         L     R3,AIO3                                                          
         USING SCANBLKD,R3                                                      
         BAS   RE,VKPRDB                  VALIDATE THE PRODUCT                  
         MVC   FLTPRD,BPRD                SAVE BINARY NUMBER                    
         MVC   FLTPRDC,SC1STFLD           SAVE PRODUCT CODE                     
         OI    FFLAG,FFPRD                                                      
*                                                                               
         SR    R0,R0                      SET CURSOR TO SECOND PRODUCT          
         IC    R0,SC1STLEN                                                      
         AHI   R0,1                                                             
         STC   R0,TIOBCURI                                                      
*                                                                               
         LA    R3,SCBLKLQ(R3)             IS THERE A SECOND PRODUCT             
         CLI   SC1STLEN,0                                                       
         BE    VKPRD9                                                           
*                                                                               
         CLC   FLTPRDC,=C'POL'            IF FIRST 'POL' ?                      
         BE    INVLFLD                    CAN'T HAVE A SECOND                   
         CLC   SC1STFLD(3),=C'POL'        SECOND CAN NEVER BE POL               
         BE    INVLFLD                                                          
         BAS   RE,VKPRDB                  VALIDATE SECOND PRODUCT               
         MVC   FLTPR2,BPRD                SAVE SECOND BINARY NUMBER             
         MVC   FLTPR2C,SC1STFLD           SAVE SECOND PRODUCT CODE              
         OI    FFLAG,FFPR2                                                      
*                                                                               
VKPRD9   XC    TIOBCURD,TIOBCURD          REOMVE CURSOR POSITING                
         NI    TIOBINDS,ALL-(TIOBSETC)                                          
         MVI   TIOBCURI,0                                                       
*                                                                               
VKPRDX   OI    MKTPRDH+4,FINPVAL           SET FIELD IS VALID                   
         OI    6(R2),X'80'                                                      
         B     VKEST                                                            
*                                                                               
VKPRDB   LR    R0,RE                                                            
         CLI   SC1STLEN,3                 CAN'T BE MORE THAN 3                  
         JH    INVLFLD                                                          
         MVC   FLDH+(FLDILEN-FLDHDRD),SC1STLEN                                  
         MVC   FLD(4),SC1STFLD                                                  
         LA    R2,FLDH                                                          
         GOTO1 VALIPRD                    THIS WILL GET PRODUCT                 
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
         DROP  R3,R5                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE THE ESTIMATE                                              *          
**********************************************************************          
VKEST    NI    FFLAG,ALL-(FFEST)          TURNOFF ESTIMATE FILTER               
         LA    R2,MKTESTH                 DID THIS FIELD CHANGE?                
         CLI   5(R2),0                    ANY INPUT?                            
         BE    VKESTX                                                           
*                                                                               
         TM    4(R2),FINPNUM              VALID NUMERIC FIELD?                  
         JZ    INVLFLD                                                          
*                                                                               
         SR    RF,RF                                                            
         IC    RF,MKTESTH+5                                                     
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         PACK  DUB,MKTEST(0)                                                    
         CVB   RF,DUB                                                           
         STC   RF,FLTEST                                                        
         STC   RF,BEST                                                          
         OI    FFLAG,FFEST                SET ESTIMATE FILTER                   
*                                                                               
VKESTX   OI    4(R2),FINPVAL              VALIDATE THIS FIELD                   
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
**********************************************************************          
* VALIDATE THE FLIGHT                                                *          
**********************************************************************          
VKFLT    NI    FFLAG,ALL-(FFFLT)          TURNOFF FLIGHT FILTER                 
         LA    R2,MKTFLTH                 DID THIS FIELD CHANGE?                
         CLI   5(R2),0                    ANY INPUT?                            
         BE    VKFLTX                                                           
         CLI   MKTPRDH+5,0                NEED PRODUCT                          
         BNE   *+12                                                             
         LA    R2,MKTPRDH                                                       
         JNE   MISSFLD                                                          
         CLI   MKTESTH+5,0                AND ESTIMATE                          
         BNE   VKFLT3                                                           
         LA    R2,MKTESTH                                                       
         J     MISSFLD                                                          
*                                                                               
VKFLT3   TM    4(R2),FINPNUM              VALID NUMERIC FIELD?                  
         JZ    INVLFLD                                                          
         XR    RE,RE                                                            
         IC    RE,5(R2)                                                         
         BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         PACK  DUB,8(0,R2)                                                      
         CVB   R1,DUB                                                           
         CHI   R1,MXFLIT                  FLIGHT NUMBER MUST BE <=16            
         JH    INVLFLD                                                          
         STC   R1,FLTFLT                                                        
         OI    FFLAG,FFFLT                                                      
*                                                                               
         GOTOR GETFLT                     ANY FLIGHT RECORDS?                   
         JNE   INVLFLD                                                          
*                                                                               
VKFLTX   OI    4(R2),FINPVAL              SET FIELD IS VALID                    
         OI    6(R2),X'80'                                                      
         EJECT                                                                  
**********************************************************************          
* VALIDATE OPTIONS                                                   *          
**********************************************************************          
VOPT     LA    R2,MKTOPTNH                                                      
         MVI   OPT,0                                                            
         CLI   5(R2),0                    TEST SPECIAL OPTIONS                  
         BE    VOPTX                      NONE                                  
*                                                                               
         L     R0,AIO3                    USE IO3 FOR SCAN BLOCK                
         LHI   R1,LIOS                                                          
         LR    RE,R1                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         GOTO1 SCANNER,DMCB,(R2),(X'8F',AIO3)                                   
         CLI   4(R1),0                                                          
         JE    INVLFLD                                                          
*                                                                               
         L     R5,ATIOB                   SET UP ERROR MESSAGE CURSOR           
         USING TIOBD,R5                                                         
         OI    TIOBINDS,TIOBSETC                                                
         LR    R0,R2                                                            
         SR    R0,RA                                                            
         STH   R0,TIOBCURD                                                      
*                                                                               
         L     R3,AIO3                                                          
         USING SCANBLKD,R3                                                      
VOPT3    CLI   SC1STLEN,0                 ANY MORE FILTER FIELDS?               
         BNE   VOPT5                      YES                                   
         CLI   SC2NDLEN,0                                                       
         BE    VOPT11                     NO MORE                               
         J     INVLFLD                                                          
*                                                                               
VOPT5    MVC   TIOBCURI,SC1STNUM          DISP. TO FIRST FIELD                  
         L     R4,AOPTTAB                 R4=A(FILTER TABLE)                    
*                                                                               
         USING OPTND,R4                                                         
VOPT7    CLC   OPTNTXT,SC1STFLD           MATCH TO OPTION TABLE                 
         BE    VOPT9                                                            
         LA    R4,OPTNLNQ(R4)                                                   
         CLI   0(R4),EOT                  END OF TABLE?                         
         BNE   VOPT7                                                            
         J     INVLFLD                    YES, NO SUCH FILTER                   
*                                                                               
VOPT9    SR    RF,RF                                                            
         ICM   RF,3,OPTNROU                                                     
         AR    RF,RB                                                            
         BASR  RE,RF                                                            
         LA    R3,SCBLKLQ(R3)             NEXT OPTION IN FIELD                  
         B     VOPT3                                                            
*                                                                               
VOPT11   NI    TIOBINDS,ALL-(TIOBSETC)                                          
         XC    TIOBCURD,TIOBCURD                                                
         MVI   TIOBCURI,0                                                       
*                                                                               
VOPTX    OI    4(R2),FINPVAL              SET FIELD IS VALID                    
         LA    R2,MKTPRNXH                NO...                                 
         OI    1(R2),X'0C'                CHANGE TEXT TO LOW INTENSITY          
*                                                                               
         CLI   PDARPORN,C'Y'              USES PREV/NEW?                        
         BNE   *+8                        YES...LEAVE TEXT ALONE                
         NI    1(R2),X'FF'-X'04'                                                
         OI    6(R2),X'80'                TRANSMIT                              
*                                                                               
VOPTXX   CLI   PFKEY,PFMISQ               PF TO MIS PROGRAM?                    
         JE    *+12                       YES                                   
         CLI   PFKEY,PFBROQ               PF TO BROWSE PROGRAM?                 
         JNE   XIT                        NO                                    
         J     VR                         YES                                   
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
*                                                                               
VOPTTRD  TM    OPT,OPTCASH                TEST ALREADY SET                      
         JO    INVLFLD                                                          
         OI    OPT,OPTTRADE               TRADE ONLY                            
         BR    RE                                                               
*                                                                               
VOPTCSH  TM    OPT,OPTTRADE               TEST ALREADY SET                      
         JO    INVLFLD                                                          
         OI    OPT,OPTCASH                BOTH                                  
         BR    RE                                                               
*                                                                               
VOPTDAD  CLI   TWAOFFC,C'*'                                                     
         JNE   INVLFLD                                                          
         OI    OPT,OPTDAD                 SET DISPLAY DISK ADDRESS              
         BR    RE                                                               
*                                                                               
VOPTSH0  OI    OPT,OPTSH0                 SHOW 0 SPOTS                          
         BR    RE                                                               
*                                                                               
VOPTSHST OI    OPT,OPTSHSNT               SHOW SENT IF ORDER SENT               
         BR    RE                                                               
         LTORG                                                                  
***********************************************************************         
* MAKE SURE USER DID NOT MULTI-SELECT BROWSE                          *         
***********************************************************************         
MULTIBRW NTR1                                                                   
*                                                                               
         LA    R2,SCRNLNQ(R2)      NEXT LIST ENTRY                              
         LA    R0,MKTPFLNH         LINE AFTER LAST SELECT FIELD                 
*                                                                               
MBRW00   CLI   8(R2),C'='          ANOTHER BROWSE SELECTION?                    
         JE    ERRMULT             YES, ERROR                                   
         LA    R2,SCRNLNQ(R2)      NEXT LIST ENTRY                              
         CR    R2,R0               END OF LIST?                                 
         BL    MBRW00              NO                                           
*                                                                               
MBRWX    J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE THE RECORD ON SCREEN (PFKEY TO MIS PROGRAM)                *         
***********************************************************************         
VR       DS    0H                                                               
         MVI   PCF2,0                                                           
         LA    R2,MKTSELH          CHECK ALL SELECT FIELDS                      
         XC    BRWKEY,BRWKEY       KEY OF ORDER REC FOR BROWSE SLPERSON         
         XC    BRWKEY2,BRWKEY2     KEY OF BTC REC FOR BROWSE SLPERSON           
         XC    BRWCUR,BRWCUR       KEY OF ORDER REC FOR BROWSE SLPERSON         
         MVI   BRWSEL,0            SELECTED ENTRY (0-6)                         
         USING SCRND,R2                                                         
         LA    R3,LSTTAB                                                        
         USING LSTD,R3                                                          
*                                                                               
VRSEL05  LA    R0,MKTPFLNH         LINE AFTER LAST SELECT FIELD                 
         CR    R2,R0               DID WE HIT PF10 AFTER VALID DATA?            
         BNL   VRSELX              YES                                          
*                                                                               
         CLI   8(R2),C'M'          GO TO MIS PROGRAM?                           
         BE    VRSEL25             YES                                          
*                                                                               
         CLI   8(R2),C'='          GO TO BROWSE PROGRAM?                        
         BNE   *+12                                                             
         BAS   RE,MULTIBRW         USER CANNOT MULTI-SELECT BROWSE              
         B     VRSEL25                                                          
*                                  CHECK PFKEY                                  
         LH    R1,CURDISP          R1=CURSOR POSITION                           
         AR    R1,RA                                                            
         CR    R1,R2                                                            
         BL    VRSEL100            CURSOR ON OR BEFORE CURRENT                  
         LA    R4,SCRNLNQ(R2)      R4=A(START OF NEXT LINE)                     
         CR    R1,R4                                                            
         BNL   VRSEL100            MUST BE ON HIGHER LINE                       
*                                                                               
VRSEL25  LR    R1,R2                                                            
         SR    R1,RA                                                            
         STCM  R1,3,CURLINE                                                     
*                                                                               
         CLI   PFKEY,PFBROQ        GOING TO BROWSE?                             
         BNE   VRSEL26             FILL IN MIS DATA ONLY                        
         CLI   MKTMED,C'R'         MEDIA = R                                    
         JNE   ERRBROW             NO, ERROR                                    
         CLC   =C'REP',SCRNDST     DESTINATION REP?                             
         JNE   NOTREP              NO, CANNOT SELECT SALESPERSON                
         CLC   =C'FAX:',SCRNROU    ROUTE = FAX?                                 
         JE    ERRROUTE            YES, ERROR                                   
         CLC   =C'EML:',SCRNROU    ROUTE = EMAIL?                               
         JE    ERRROUTE            YES, ERROR                                   
         MVC   BRWKEY2,LSTBTK      SAVE THIS BATCH KEY                          
         MVC   BRWKEY,LSTDAK       SAVE THIS BATCH KEY                          
         MVC   BRWCUR,CURLINE      SAVE CURSOR POSITION                         
***                                                                             
* CALL RD4ROUTE TO GET THE ROUTE IN CASE THERE IS A SALEPERSON'S                
* NAME IN THE ROUTE FIELD                                                       
***                                                                             
         MVC   FLDH,SCRNROUH         MUST HAVE CORRECT BSTA                     
         MVI   FLDH+5,L'QSTA                                                    
         XC    FLD,FLD                                                          
         MVC   FLD(L'QSTA),SCRNSTA                                              
         LR    R0,R2                                                            
         LA    R2,FLDH                                                          
         GOTO1 VALISTA                                                          
         LR    R2,R0                                                            
*                                                                               
         MVC   FLDH,SCRNROUH                                                    
         MVI   FLDH,68             LENGTH OF HEADER + FIELD                     
         GOTO1 RD4ROUTE,DMCB,SCRNDSTH,FLDH,(MISCFLG1,0),0                       
***                                                                             
* READ CTFILE FOR ID RECORDS                                                    
***                                                                             
         XC    KEY,KEY                                                          
         LA    R6,KEY                                                           
         USING CTIREC,R6                                                        
         MVI   CTIKTYP,CTIKTYPQ    C'I'                                         
         MVC   CTIKID,FLD          REP NAME                                     
         OC    CTIKID,BLANKS       SPACE PADDED                                 
         MVC   KEYSAVE,KEY                                                      
*                                                                               
         GOTO1 DATAMGR,DMCB,=CL8'DMRDHI',=C'CTFILE',KEY,AIO3,(0,DMWORK)         
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         L     R6,AIO3             CTFILE ID REC                                
         CLC   KEY(CTIKNUM-CTIKEY),0(R6)                                        
         JNE   NOIDREC             NO ID RECORD                                 
*                                                                               
         MVC   DATADISP,=H'28'     CHANGE DATADISP FOR GETEL                    
         MVI   ELCODE,CTAGYELQ     X'06'                                        
         BRAS  RE,GETEL                                                         
         MVC   DATADISP,=H'24'     RESTORE DATADISP                             
         JNE   NOIDREC2            NO ID RECORD                                 
         USING CTAGYD,R6                                                        
***                                                                             
* CREATE BROWSE ELEMENT FOR GLOBBER                                             
***                                                                             
         XC    BLOCK(GLBRWLNQ),BLOCK                                            
         LA    R3,BLOCK                                                         
         USING GLBRWKW,R3                                                       
*                                                                               
         MVC   GLBRWREC,=C'BSP'                                                 
         CLI   BRWPREF,C'P'                                                     
         BNE   *+10                                                             
         MVC   GLBRWREC,=C'BPP'                                                 
*                                                                               
         MVC   GLBRWADV(2),CTAGYID    2-BYTE AGENCY ID                          
         MVC   GLBRWADV+2(2),DARROUTE+3      OFFICE CODE                        
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,GLBRWLNQ,GLRBRWSE                   
         CLI   8(R1),0                                                          
         JNE   CANTSWTC                                                         
         OI    PCF2,PCFBRO                                                      
         B     VRSEL30                                                          
         DROP  R3,R6                                                            
***                                                                             
* MEDIA                                                                         
***                                                                             
VRSEL26  GOTO1 VGLOBBER,DMCB,=C'PUTF',MKTMEDH,,GLVSPMD                          
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC            CAN'T SWITCH TO BUY PROGRAM                  
***                                                                             
* CLIENT                                                                        
***                                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTF',MKTCLTH,,GLVSPCLT                         
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
***                                                                             
* PRODUCT                                                                       
***                                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',SCRNPRD,L'SCRNPRD,GLVSPPRD                
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
***                                                                             
* ESTIMATE                                                                      
***                                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',SCRNEST,L'SCRNEST,GLVSPEST                
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
***                                                                             
* MARKET                                                                        
***                                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTF',MKTMRKTH,,GLVSPMKT                        
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
***                                                                             
* STATION                                                                       
***                                                                             
         GOTO1 VGLOBBER,DMCB,=C'PUTD',SCRNSTA,L'SCRNSTA,GLVSPSTA                
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
VRSEL30  XC    BLOCK(12),BLOCK                                                  
         LA    R1,BLOCK                                                         
         MVC   0(6,R1),=C'SPODAR'  FROM THE SPOT SYSTEM                         
                                                                                
         OI    6(R1),X'80'         HEADLINE CHANGE IN DARE                      
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,12,GLVNOTE                          
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         XC    BLOCK(14),BLOCK                                                  
         LA    R1,BLOCK                                                         
         USING GLVXFRSY,R1                                                      
         MVC   GLVXFRSY,=C'SPO'    FROM THE SPOT SYSTEM                         
         MVC   GLVXFRPR,=C'DAR'    DARE PROGRAM                                 
         MVC   GLVXTOSY,=C'SPO'    TO THE SPOT SYSTEM                           
         MVC   GLVXTOPR,=C'MIS'    MIS PROGRAM                                  
         OI    GLVXFLG1,GLV1SEPS   CALL BASE ON TRANSFER                        
*                                                                               
         CLI   PFKEY,PFMISQ        GOING TO MIS?                                
         BE    *+10                YES                                          
         MVC   GLVXTOPR,=C'BRO'    BROWSE PROGRAM                               
         DROP  R1                                                               
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'PUTD',BLOCK,14,GLVXCTL                          
         CLI   DMCB+8,0                                                         
         JNE   CANTSWTC                                                         
*                                                                               
         XC    8(L'MKTSEL,R2),8(R2)    CLEAR SELECT FIELD                       
         OI    4(R2),X'20'             VALIDATE IT FOR NEXT TIME                
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
*                                                                               
         MVC   CONHEAD(23),=CL23'** BACK TO SPOT/DARE **'                       
         OI    CONHEADH+6,X'80'                                                 
         B     VRSELX                                                           
*                                                                               
VRSEL100 DS    0H                                                               
         XC    8(L'MKTSEL,R2),8(R2)    CLEAR SELECT FIELD                       
         OI    4(R2),X'20'             VALIDATE IT FOR NEXT TIME                
         MVI   5(R2),0                                                          
         OI    6(R2),X'80'             TRANSMIT                                 
*                                                                               
         DS    0H                                                               
         LA    R2,SCRNLNQ(R2)          NEXT LIST ENTRY                          
         LA    R3,LSTLNQ(R3)           NEXT ITEM IN LIST                        
         ZIC   R1,BRWSEL               BUMP SELECTED ENTRY                      
         AHI   R1,1                                                             
         STC   R1,BRWSEL                                                        
         B     VRSEL05                                                          
         DROP  R2                                                               
*                                                                               
VRSELX   SR    R0,R0                                                            
         LA    R1,MKTPFLNH             FIRST PF LINE                            
*                                                                               
VRSELX10 ICM   R0,1,0(R1)              END OF SCREEN?                           
         BZ    VRSELX20                YES                                      
         AR    R1,R0                   NO - BUMP                                
         B     VRSELX10                                                         
*                                                                               
VRSELX20 MVC   1(2,R1),=X'0101'        RETRANSMIT SCREEN                        
         OI    GENSTAT2,DISTHSPG                                                
*                                                                               
VRX      DS    0H                                                               
         OI    PCF2,PCFMIS2                                                     
         OI    PCF,PCFMIS                                                       
         NI    PCF,ALL-(PCFNEXTK)         REDISPLAY THE SAME LIST               
         J     XIT                                                              
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GET THE FLIGHT RECORD - BUILD LIST OF FLIGHT DATES                  *         
*                                                                               
* ON EXIT : FLITAB HAS LIST OF FLIGHTS                                          
*           CC EQ : PCFFLGHT IS ON AND FLIGHTS WERE FOUND                       
*           CC NEQ: PCFFLGHT IS OFF AND NO FLIGHTS WERE FOUND                   
***********************************************************************         
GETFLT   NTR1  BASE=*,LABEL=*                                                   
         OI    PCF,PCFFLGHT               SET FLIGHT INDICATOR                  
*                                                                               
         XC    KEY,KEY                                                          
         LA    R6,KEY                     POL ON THE ESTIMATE                   
         USING DFLRECD,R6                                                       
         MVI   DFLKTYP,DFLKTYPQ                                                 
         MVI   DFLKSUB,DFLKSUBQ                                                 
         MVC   DFLKAGMD,AGYMD                                                   
         MVC   DFLKCLT,BCLT                                                     
         MVC   DFLKPRD,=C'POL'                                                  
         MVC   DFLKEST,BEST                                                     
         SR    R0,R0                                                            
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DFLKEY),KEYSAVE      TEST FLIGHT RECORD FOR POL            
         BE    GETFLT3                    YES                                   
*                                                                               
         MVC   KEY,KEYSAVE                                                      
         MVC   DFLKPRD,QPRD                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DFLKEY),KEYSAVE      TEST FOR PRODUCT                      
         BE    GETFLT3                    YES                                   
*                                                                               
GETFLT1  MVC   KEY,KEYSAVE                                                      
         MVC   DFLKPRD,=C'ALL'                                                  
         MVC   DFLKEST,BEST                                                     
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DFLKEY),KEYSAVE      TEST FOR ALL                          
         JNE   GETFLTNO                   NO, FLIGHT RECORD                     
*                                                                               
GETFLT3  MVC   AIO,AIO3                                                         
         GOTO1 GETREC                     GET FLIGHT RECORD                     
         L     R6,AIO                                                           
         LA    R6,DFLEL                                                         
         LA    R5,FLITAB                  R5=FLIGHT TABLE                       
         USING FLITD,R5                                                         
         XR    R3,R3                      NUMBER OF ENTRIES                     
*                                                                               
GETFLT5  CLI   0(R6),DFINFELQ             TEST NON-FLIGHT ELEMENT               
         BNE   GETFLT7                                                          
         USING DFINFEL,R6                                                       
         TM    FFLAG,FFFLT                FILTER ON FLIGHT ?                    
         BNO   *+12                                                             
         CLI   FLTFLT,0                   FILTER ON NON-FLIGHT                  
         BNE   GETFLT11                                                         
         XC    FLITD(FLITLNQ),FLITD       CLEAR AN ENTRY                        
         MVI   FLITNUM,C'0'               NON-FLIGHT IS ZERO                    
         MVC   FLITEND,DFINFSDT           SET NON-FLIGHT END DATE               
         B     GETFLT9                                                          
*                                                                               
GETFLT7  CLI   0(R6),DFFLTELQ             GET FLIGHT ELEMENT                    
         BNE   GETFLT11                                                         
         USING DFFLTEL,R6                                                       
         TM    FFLAG,FFFLT                FILTER ON FLIGHT ?                    
         BNO   *+14                                                             
         CLC   FLTFLT,DFFLTNUM            FILTER ON NON-FLIGHT                  
         BNE   GETFLT11                                                         
*                                                                               
         XC    FLITD(FLITLNQ),FLITD       CLEAR AN ENTRY                        
         MVC   FLITNUM,DFFLTNUM           SET FLIGHT NUMBER                     
         MVC   FLITSTR,DFFLTSTR           START DATE                            
         MVC   FLITEND,DFFLTEND           END DATE                              
*                                                                               
GETFLT9  MVC   FLITLNQ(FLITLNQ,R5),0(R5) COPY FOR TRADE                         
         LA    R5,FLITLNQ(R5)                                                   
         OI    FLITFLG,DBFLTTRD    SET TRADE FOR THIS FLIGHT ENTRY              
*                                                                               
         LA    R5,FLITLNQ(R5)             BUMP TO NEXT ENTRY                    
         MVI   0(R5),EOT                  MARK NEW END OF TABLE                 
         AHI   R3,1                       COUNT NUMBER IN TABLE C/T             
         CHI   R3,MXFLIT                  TEST MAX NUMBER OF FLIGHTS            
         BNH   *+6                                                              
         DC    H'0'                       TOO MANY FLIGHTS                      
*                                                                               
GETFLT11 XR    R0,R0                      GET NEXT ELEMENT                      
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),0                                                          
         BNE   GETFLT5                                                          
*                                                                               
         LTR   R3,R3                      ANYTHING IN TABLE ?                   
         JNZ   YES                                                              
GETFLTNO NI    PCF,ALL-(PCFFLGHT)         TURNOFF FLIGHT INDICATOR              
         LA    R5,FLITAB           SET FLIGHT TABLE FOR CASH                    
         USING FLITD,R5                                                         
         XC    FLITD(FLITLNQ),FLITD                                             
         MVC   FLITEND,FFS         USE HIGH END DATE                            
*                                                                               
         MVC   FLITLNQ(FLITLNQ,R5),0(R5) COPY FOR TRADE                         
         LA    R5,FLITLNQ(R5)                                                   
         OI    FLITFLG,DBFLTTRD    SET TRADE FOR THIS FLIGHT ENTRY              
         MVI   FLITD+FLITLNQ,EOT                                                
         J     NO                                                               
         DROP  R5,R6                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* GENERAL ERROR MESSAGES                                              *         
***********************************************************************         
*                                                                               
NODATARQ EQU   812                 NO DATA TO REPORT                            
NOTCURQ  EQU   140                 DATA NO LONGER CURRENT...                    
DSTNUPQ  EQU   65                  DESTINATIONS UPDATED                         
NOMISPRG EQU   3                   CAN'T SWITCH TO THE MIS PROGRAM              
*                                                                               
MISSFLD  MVI   GERROR1,MISSING                                                  
         J     ER5EXIT                                                          
*                                                                               
INVLFLD  MVI   GERROR1,INVALID                                                  
         J     ER5EXIT                                                          
*                                                                               
RECNTFND MVI   GERROR1,NOTFOUND           RECORD NOT FOUND                      
         J     ER5EXIT                                                          
*                                                                               
CANTSWTC MVI   GERROR1,NOMISPRG    CAN'T SWITCH TO THE MIS PROGRAM              
*                                                                               
ER5EXIT  MVI   GERROR,0            SOMETIMES THERE IS GARBAGE HERE              
         J     ERREXIT                                                          
*                                                                               
***********************************************************************         
* ERROR MESSAGES (SYSTEM 2)                                           *         
***********************************************************************         
NOPOLPRD MVI   GERROR1,CNTBEPOL           PRODUCT CAN'T BE POL                  
         J     ER2EXIT                                                          
*                                                                               
NODATAR  CLI   NSENT,0             ANYTHING SENT?                               
         JNE   ALLSENT                                                          
         LHI   RF,NODATARQ         NO DATA TO REPORT                            
         STCM  RF,3,GERROR                                                      
         L     R1,ATIOB                   CURSOR TO BUYER FIELD                 
         USING TIOBD,R1                                                         
         NI    TIOBINDS,X'FF'-TIOBSETC                                          
         LA    R2,MKTBUYRH                                                      
         J     ER2EXIT                                                          
*                                                                               
*                                                                               
ER2EXIT  MVI   GETMSYS,2                  SPOT MESSAGES                         
         J     ERREXIT                                                          
***********************************************************************         
* ERROR MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
INVLDPF  MVI   GERROR1,ERINVPFK           INVALID PF KEY                        
         J     ERREXIT                                                          
NOTCURNT MVI   GERROR1,NOTCURQ            DATA NO LONGER CURRENT..              
         J     ERREXIT                                                          
***********************************************************************         
* ERRORS WITH REPLACEMENT TEXT (&1)                                   *         
***********************************************************************         
BYRALRDY MVI   GERROR1,GOTBYRAL    BUYER &1 ALREADY ASSIGNED TO THIS ..         
         J     ERRRTEXT                                                         
***********************************************************************         
* GENERAL INFO MESSAGES                                               *         
***********************************************************************         
NEEDFLDS MVI   GERROR1,REQFIELD           PLEASE ENTER FIELDS AS REQ...         
         J     INFEXIT                                                          
*                                                                               
EOFLSELS MVI   GERROR1,33                 END OF LIST - INPUT ...               
         J     INFEXIT                                                          
*                                                                               
MAKESELS MVI   GERROR1,32                 LIST DISPLAYED - INPUT....            
         J     INFEXIT                                                          
*                                                                               
PRVMSG   MVI   GERROR1,50                 ORDER NOW MARKED AS PREVIOUS          
         J     INFEXIT                                                          
*                                                                               
NEWMSG   MVI   GERROR1,52                 ORDER NOW MARKED AS NEW               
         J     INFEXIT                                                          
*                                                                               
ALLSENT  MVI   GERROR1,60          ALL ORDERS HAVE BEEN SENT                    
         J     INFEXIT                                                          
         EJECT                                                                  
***********************************************************************         
* INFO  MESSAGES (SYSTEM 23)                                                    
***********************************************************************         
DSTNUP   MVI   GERROR1,DSTNUPQ     DESTINATIONS UPDATED SELECT..                
         L     R1,ATIOB                   SET CURSOR TO FIRST SELECT            
         USING TIOBD,R1                                                         
         LA    RF,MKTSELH-T234FFD                                               
         STCM  RF,3,TIOBCURD                                                    
         OI    TIOBINDS,TIOBSETC                                                
         MVI   TIOBCURI,0                                                       
         J     MYINFXIT                                                         
***********************************************************************         
* MESSAGE ROUTINES                                                    *         
***********************************************************************         
INFRTEXT SR    R1,R1                                                            
         J     *+8                                                              
ERRRTEXT LA    R1,1                                                             
         OI    GENSTAT2,USGETTXT                                                
         XC    GETTXTCB,GETTXTCB                                                
         LA    R3,GETTXTCB                                                      
         USING GETTXTD,R3                                                       
         MVI   GTMSYS,23                                                        
         LA    RE,BLOCK                                                         
         STCM  RE,7,GTASUBST                                                    
         LTR   R1,R1                                                            
         JZ    MYINFXIT                                                         
         J     ERREXIT                                                          
         DROP  R3                                                               
*                                                                               
INFEXIT  MVI   GETMSYS,255                GENERAL MESSAGES                      
MYINFXIT MVI   GMSGTYPE,C'I'              INFO MESSAGES                         
ERREXIT  GOTO1 MYERR                                                            
*                                                                               
         GETEL R6,DATADISP,ELCODE                                               
*                                                                               
YES      CR    RB,RB                                                            
         J     XIT                                                              
NO       LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
         DROP  R1                                                               
         EJECT                                                                  
***********************************************************************         
* KEY TABLE - SEE DKYD                                                *         
***********************************************************************         
KEYTAB   DS   0F                                                                
DBTAB    DC   AL1(DBTKSTYQ,DBTX-*+1)                                            
         DC   AL1((DBTX-*+1)/DKYLNQ)                                            
         DC   AL1(L'DBTKAGMD,DBTKAGMD-DBTKEY,FDAGMQ)    AGENCY/MEDIA            
         DC   AL1(L'DBTKMKT,DBTKMKT-DBTKEY,FDMKTQ)      MARKET                  
         DC   AL1(L'DBTKSTA,DBTKSTA-DBTKEY,FDSTAQ)      STATION                 
         DC   AL1(L'DBTKCLT,DBTKCLT-DBTKEY,FDCLTQ)      CLIENT                  
         DC   AL1(L'DBTKPRD,DBTKPRD-DBTKEY,FDPRDQ)      PRODUCT                 
         DC   AL1(L'DBTKEST,DBTKEST-DBTKEY,FDESTQ)      ESTIMATE                
         DC   AL1(L'DBTKPRD2,DBTKPRD2-DBTKEY,FDPR2Q)    PRODUCT2                
DBTX     EQU  *                                                                 
*                                                                               
         EJECT                                                                  
***********************************************************************         
* FILTER DATA - SEE FDD                                               *         
***********************************************************************         
FDTAB    DS   0XL(FDLNQ)                                                        
         ORG  FDTAB+FDLNQ                                                       
*                                                                               
*                                  AGENCY MEDIA                                 
FDAGMQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(0),AL2(0)                                                     
         DC   AL1(0),AL2(AGYMD-LSSD)                                            
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  MARKET                                       
FDMKTQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFMKT),AL2(FFLAG-LSSD)                                        
         DC   AL1(0),AL2(FLTMKT-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  STATION                                      
FDSTAQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFSTA),AL2(FFLAG-LSSD)                                        
         DC   AL1(0),AL2(FLTSTA-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  CLIENT                                       
FDCLTQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFCLT),AL2(FFLAG-LSSD)                                        
         DC   AL1(0),AL2(FLTCLT-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  PRODUCT                                      
FDPRDQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFPRD),AL2(FFLAG-LSSD)                                        
         DC   AL1(0),AL2(FLTPRD-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  ESTIMATE                                     
FDESTQ   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFEST),AL2(FFLAG-LSSD)                                        
         DC   AL1(0),AL2(FLTEST-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
*                                                                               
*                                  PRODUCT2                                     
FDPR2Q   EQU  ((*-FDTAB)/FDLNQ)                                                 
         DC   AL1(FFPR2),AL2(FFLAG-LSSD)                                        
         DC   AL1(0),AL2(FLTPR2-LSSD)                                           
         DC   AL2(0)                                                            
         DC   AL2(0)                                                            
         EJECT                                                                  
***********************************************************************         
* OPTION TABLE                                                                  
***********************************************************************         
OPTTAB   DS    0H                                                               
         DC    CL8'CASH    ',AL2(VOPTCSH-VK)                                    
         DC    CL8'TRADE   ',AL2(VOPTTRD-VK)                                    
         DC    CL8'DAD     ',AL2(VOPTDAD-VK)                                    
         DC    CL8'SHOW0   ',AL2(VOPTSH0-VK)                                    
         DC    CL8'SHOWSENT',AL2(VOPTSHST-VK)                                   
         DC    AL1(EOT)                                                         
         EJECT                                                                  
***********************************************************************         
* GET THE SALESPERSON FROM THE GLOBBER BROWSE ELEMENT                 *         
***********************************************************************         
GETSALES NTR1  BASE=*,LABEL=*                                                   
         XC    BLOCK(GLBRWLNQ),BLOCK                                            
         LA    R3,BLOCK                                                         
         USING GLBRWKW,R3          LOOK FOR GLOBBER BROWSE ELEM                 
*                                                                               
         GOTO1 VGLOBBER,DMCB,=C'GETD',(R3),GLBRWLNQ,GLRBRWSE                    
         CLI   8(R1),0             GLOBBER ERRORS?                              
         JNE   NO                  YES, DON'T HAVE SALESPERSON                  
         OC    GLBRWKW,GLBRWKW     SALESPERSON FIELD BLANK?                     
         JZ    NO                  YES, ERROR RETURN CC NOT EQUAL               
         J     YES                                                              
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE SALESPERON IN DARE BATCH RECORD (FROM =BRW GLOBBER)          *         
***********************************************************************         
UPDBTSLP NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LSTD,R3                                                          
         LA    R4,BLOCK                                                         
         USING GLBRWKW,R4                                                       
*                                                                               
         XC    KEY,KEY             BRWKEY2 SAVED B4 GLOBBER TO BROWSE           
         MVC   KEY(L'DOKEY),BRWKEY2                                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVI   RDUPDATE,C'Y'              UPDATE SALESPER IN DR BATCH           
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING DBSLPELD,R6                                                      
         MVI   ELCODE,DBSLPELQ            GET SALESPERSON ELEMENT               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
UPD00    BRAS  RE,NEXTEL                                                        
         BNE   UPD10                      MAKE SURE SAME FLIGHT #               
*                                                                               
         CLC   DBSLPFLT,LSTFLT            MATCH FLIGHT                          
         BNE   UPD00                                                            
*                                                                               
         MVC   DBSALESP(20),GLBRWKW       CLOBBER WITH NEW SALESPERSON          
         OC    DBSALESP,BLANKS            SPACE PAD IT                          
         NI    DBSALFL1,X'FF'-DBSALPT     TURN OFF POINTPERSON                  
         TM    GLBRWFLG,X'80'             POINTPERSON VIA GLOBBER?              
         BZ    *+8                        NO                                    
         OI    DBSALFL1,DBSALPT           YES, SET POINTPERSON FLAG             
         B     UPD20                                                            
         DROP  R6                                                               
UPD10    XC    ELEM2,ELEM2                                                      
         LA    R1,ELEM2                                                         
         USING DBSLPELD,R1                                                      
         MVI   DBSLPEL,DBSLPELQ                                                 
         MVI   DBSLPLEN,DBSLPLNQ                                                
         MVC   DBSLPFLT,LSTFLT            FLIGHT                                
         MVC   DBSALESP(20),GLBRWKW       SALESPERSON                           
         OC    DBSALESP,BLANKS            SPACE PAD IT                          
         NI    DBSALFL1,X'FF'-DBSALPT     TURN OFF POINTPERSON                  
         TM    GLBRWFLG,X'80'             POINTPERSON VIA GLOBBER?              
         BZ    *+8                        NO                                    
         OI    DBSALFL1,DBSALPT           YES, SET POINTPERSON FLAG             
         DROP  R1                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM2,(R6)  ADD NEW ELEMENT                
UPD20    GOTO1 PUTREC                                                           
         J     XIT                                                              
         DROP  R3,R4                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY SALSPERSON NAME IN THE ROUTE FIELD IF IT EXISTS IN BTC REC  *         
***********************************************************************         
HAVESLP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SCRND,R2                                                         
         USING LSTD,R3                                                          
*                                                                               
         CLC   =CL5'EML: ',FLD     ROUTE = EMAIL?                               
         JE    NO                  YES, DON'T LOOK FOR SALESPERSON              
*                                                                               
         CLC   =CL5'FAX: ',FLD     ROUTE = FAX?                                 
         JE    NO                  YES, DON'T LOOK FOR SALESPERSON              
*                                                                               
         CLC   =C'REP',SCRNDST     DESTINATION REP?                             
         JNE   NO                  NO, DON'T LOOK FOR SALESPERSON               
*                                                                               
         MVC   KEY(L'DOKEY),LSTBTK                                              
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE                                             
         BE    *+6                                                              
         DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING DBSLPELD,R6                                                      
         MVI   ELCODE,DBSLPELQ            GET SALESPERSON ELEMENT               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
         BRAS  RE,NEXTEL                                                        
         JNE   NO                         MAKE SURE SAME FLIGHT #               
*                                                                               
         CLC   DBSLPFLT,LSTFLT            MATCH FLIGHT                          
         BNE   *-14                                                             
*                                                                               
         CLC   DBSALESP,BLANKS            HAVE AN ACTUAL NAME?                  
         JNH   NO                         NOPE                                  
         MVC   SCRNROU(L'DBSALESP),DBSALESP                                     
         OI    LSTFLAG2,LSTSALP                                                 
         J     YES                                                              
         DROP  R2,R3,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* DISPLAY SALSPERSON NAME IN THE ROUTE FIELD IF IT EXISTS IN BTC REC  *         
***********************************************************************         
LMSALES  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SCRND,R2                                                         
         USING LSTD,R3                                                          
*                                                                               
         CLC   =CL5'EML: ',FLD     ROUTE = EMAIL?                               
         JE    NO                  YES, DON'T LOOK FOR SALESPERSON              
*                                                                               
         CLC   =CL5'FAX: ',FLD     ROUTE = FAX?                                 
         JE    NO                  YES, DON'T LOOK FOR SALESPERSON              
*                                                                               
         CLC   =C'REP',SCRNDST     DESTINATION REP?                             
         JNE   NO                  NO, DON'T LOOK FOR SALESPERSON               
*                                                                               
         BRAS  RE,RDLMSLP                                                       
         JNE   NO                                                               
*                                                                               
         USING DMSPELD,R6                                                       
         CLC   DMSPNAME,BLANKS            HAVE AN ACTUAL NAME?                  
         JNH   NO                         NOPE                                  
         MVC   SCRNROU(L'DMSPNAME),DMSPNAME                                     
         J     YES                                                              
         DROP  R2,R3,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEE IF THE LM REC HAS A SALESPERSON                                 *         
***********************************************************************         
RDLMSLP  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING SCRND,R2                                                         
         USING LSTD,R3                                                          
*                                                                               
         LA    R6,KEY              LET'S SEE IF WE HAVE LSTM                    
         XC    KEY,KEY                                                          
         USING DMTHKEY,R6                                                       
         MVI   DMTHTYPE,DMTHTYPQ   X'0D3E'                                      
         MVI   DMTHSBTY,DMTHSBTQ                                                
         MVC   DMTHAGMD,BAGYMD     A/M                                          
         MVC   DMTHBYR,QBYR        BUYER                                        
*                                                                               
         LA    R5,LSTBTK           GET STATION FROM THE BATCH KEY               
         USING DBTKEY,R5                                                        
         MVC   DMTHSTA,DBTKSTA     STATION                                      
         DROP  R5                                                               
*                                                                               
         MVC   DMTHCLT,BCLT        CLIENT                                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(13),KEYSAVE     ANY LAST METHOD BY THIS CLIENT?              
         JNE   RLMNO               NO                                           
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING DMSPELD,R6                                                       
         MVI   ELCODE,DMSPELQ      GET SALESPERSON ELEMENT                      
         BRAS  RE,GETEL                                                         
         JNE   RLMNO                                                            
*                                                                               
RLMYES   CR    RB,RB               SET CC EQU                                   
         B     RLMXIT                                                           
RLMNO    LTR   RB,RB                                                            
*                                                                               
RLMXIT   XIT1  REGS=(R6)                                                        
         DROP  R2,R3,R6                                                         
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* SEE IF THE BTC REC HAS A SALESPERSON BEFORE WE GO TO ORD/SEND       *         
***********************************************************************         
RDBTCSLP NTR1  BASE=*,LABEL=*                                                   
         USING LSTD,R3                                                          
         MVC   KEY(L'DOKEY),LSTBTK        BATCH KEY                             
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE       MATCH?                                
         JNE   ERREFRSH                   NO, DON'T DIE JUST REFRESH            
*        BE    *+6                                                              
*        DC    H'0'                                                             
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         USING DBSLPELD,R6                                                      
         MVI   ELCODE,DBSLPELQ            GET SALESPERSON ELEMENT               
         BRAS  RE,GETEL                                                         
         B     *+8                                                              
*                                                                               
SLP10    BRAS  RE,NEXTEL                                                        
         JNE   NO                         MAKE SURE SAME FLIGHT #               
*                                                                               
         CLC   DBSLPFLT,LSTFLT            MATCH FLIGHT                          
         BNE   SLP10                                                            
*                                                                               
         OC    DBSALESP,DBSALESP          HAVE A SALESPERSON NAME?              
         JZ    NO                         NOPE                                  
         J     YES                        YUP                                   
         DROP  R3                                                               
               LTORG                                                            
               EJECT                                                            
***********************************************************************         
* SET BIT IF END OF LIST AND TRANSMIT ALL TO REDISPLAY FROM FIRST KEY *         
***********************************************************************         
XMITALL  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         USING LSTD,R3                                                          
         USING SCRND,R2                                                         
*                                                                               
         CLI   PCF,PCFEOLST               END OF LIST?                          
         BNE   XMITX                      NO                                    
         LA    R2,SCRNLNQ(R2)             BUMP 1..WE KNOW FIRST IS XMIT         
         ZIC   R5,NLST                    NUMBER ON SCREEN                      
         BCTR  R5,0                       MINUS ONE                             
         CHI   R5,0                       ONLY ONE ON SCREEN?                   
         BE    XMIT20                     YUP                                   
*                                                                               
XMIT10   CLI   SCRNSEL,C'T'               TRANSMIT?                             
         BNE   XMITX                      NOPE                                  
         LA    R2,SCRNLNQ(R2)             BUMP                                  
         BCT   R5,XMIT10                  CHECK NEXT                            
*                                                                               
XMIT20   OI    PCF2,PCFPG1                TELL LST TO DISPLAY FIRST KEY         
XMITX    J     XIT                                                              
         DROP  R2,R3                                                            
***********************************************************************         
* ROUTINE TO GET THE DARE RECORDS                                     *         
*  PARM 1 = A(BATCH KEY)                                              *         
*       2 = A(DBFLTEL)                                                *         
*       3 = A(FLIGHT TABLE ENTRY)                                     *         
*  NOTE: THIS UPDATES THE SALESPERSON IN THE BATCH RECORD (X'01')     *         
*        IF WE ARE COMING BACK FROM BROWSE AND THE KEY MATCHES        *         
*        IN ADDTION WHILE WE ARE HERE, WE UPDATE THE SALESPERON       *         
*        IN THE LAST METHOD RECORD                                    *         
***********************************************************************         
GDARE    NTR1  BASE=*,LABEL=*                                                   
         LM    R2,R4,0(R1)                                                      
         USING DBTKEY,R2                                                        
         USING DBFLTELD,R3                                                      
         USING FLITD,R4                                                         
*                                                                               
GDARE01  XC    KEY,KEY                                                          
         LA    R5,KEY                                                           
         USING DOKEY,R5                                                         
*                                                                               
         MVI   DCKTYPE,DCKTYPQ     BUILD DARE CLIENT KEY                        
         MVI   DCKSUBTY,DCKSTYPQ                                                
         MVC   DCKAGMD,DBTKAGMD                                                 
         MVC   DCKCLT,DBTKCLT                                                   
         MVC   DCKPRD,DBTKPRD                                                   
         MVC   DCKEST,DBTKEST                                                   
         MVC   DCKSTA,DBTKSTA                                                   
         MVC   DCKPRD2,DBTKPRD2                                                 
         CLI   DBFLTFLT,C'0'       FLIGHT0?                                     
         BE    *+10                                                             
         MVC   DCKFLTNM,DBFLTFLT                                                
         TM    FLITFLG,DBFLTTRD    PROCESS THE TRADE ORDER?                     
         BZ    *+8                                                              
         OI    DCKFLAG,DCKFTRDE    YES                                          
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE  TEST DARE RECORD                           
         BNE   GDAREX                                                           
*                                                                               
         CLI   KEY+13,X'01'        IS IT CONFIRMED?                             
         BNE   GDARE10                                                          
         OI    FLITFLG,DBFLTSNT                                                 
         B     GDAREX                                                           
*                                                                               
GDARE10  MVC   FLITDAK,KEY         KEY OF DARE RECORD                           
         MVC   AIO,AIO2            GET DARE ORDER RECORD                        
***                                                                             
* IF THE KEY MATCHES BRWKEY (RECORD SAVED BEFORE WE WENT TO BROWSE)             
* (BRWKEY = TRADE KEY IF OPTIONS BOTH/TRADE) MINUS THE TRADE FLAG               
* AND WE ARE COMING BACK FROM BROWSE NOW, THEN UPDATE THE SALESPERSON           
* IN THE DARE CASH/TRADE ORDER RECORD                                           
***                                                                             
         CLC   KEY(L'DOKEY-1),BRWKEY  DARE REC = BROWSE SELECTED?               
         BNE   GDARE12                NO                                        
         TM    PCF2,PCFBRO2        COMING BACK FROM BROWSE?                     
         BZ    GDARE12             NO                                           
         OI    PCF2,PCFBRO3        INDICATE WERE CHANGING SALESP                
         MVI   RDUPDATE,C'Y'                                                    
GDARE12  GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSTELQ             WAS IT SENT ALREADY?                  
         BRAS  RE,GETEL                                                         
         BNE   GDARE15                                                          
         OI    FLITFLG,DBFLTSNT                                                 
         B     GDAREX                                                           
*                                                                               
GDARE15  L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ             GET DARE ID ELEMENT                   
         BRAS  RE,GETEL                                                         
         BNE   GDARE40                                                          
         USING DOIDELD,R6                                                       
         MVC   FLITBYR,DOIDBYR            ASSIGNED BUYER                        
         MVC   FLITSTD,DOISCOM1                                                 
* DON'T THINK THIS TEST IS NECESSARY                                            
*        CLC   KEY(L'DOKEY-1),BRWKEY      DARE REC = BROWSE SELECTED?           
*        BNE   GDARE40                    NO                                    
         TM    PCF2,PCFBRO3               UPDATE SALEPERSON?                    
         BZ    GDARE40                    NO                                    
***                                                                             
* MOVE IN SALESPERSON                                                           
***                                                                             
         BRAS  RE,GETSALES                DID WE GET SALEP VIA GLOBBER?         
         BNE   GDARE40                    NO                                    
         LA    R1,BLOCK                                                         
         USING GLBRWKW,R1                                                       
         MVC   DOIDSPER(20),GLBRWKW  20 BYTE SALESPERSON FROM BROWSE            
         OC    DOIDSPER,BLANKS       SPACE PAD FOR 25 (NOT 20!)                 
         DROP  R6                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOSPELQ      SUPPLEMENTARY ID ELEMENT                     
         BRAS  RE,GETEL                                                         
         BE    GDARE20                                                          
***                                                                             
* ADD A SUPPLEMENTARY ID ELEMENT                                                
***                                                                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DOSPELD,R6                                                       
         MVI   DOSPEL,DOSPELQ                                                   
         MVI   DOSPLEN,DOSPLNQ2    <===  SPECIAL LENGTH                         
         ZAP   DOSPTOTL,=P'0'                                                   
         TM    GLBRWFLG,X'80'      GOT A POINTPERSON VIA GLOBBER?               
         BZ    *+8                 NO                                           
         OI    DOSPFLG1,DOSPPPER   POINTPERSON                                  
         DROP  R1                                                               
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOI2ELQ      SECONDARY ID ELEMENT (X'02')                 
         BRAS  RE,GETEL            FOUND IT?                                    
         BE    *+16                YES                                          
         L     R6,AIO                                                           
         MVI   ELCODE,DOIDELQ      PRIMARY ID ELEMEN (X'01')                    
         BRAS  RE,GETEL                                                         
         ZIC   R1,1(R6)                                                         
         AR    R6,R1                                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)  ADD NEW ELEMENT                 
*                                                                               
         LA    R1,BLOCK                                                         
         USING GLBRWKW,R1                                                       
         B     GDARE30                                                          
*                                                                               
         USING DOSPELD,R6                                                       
GDARE20  NI    DOSPFLG1,X'FF'-DOSPPPER                                          
         TM    GLBRWFLG,X'80'      GOT A POINTPERSON VIA GLOBBER?               
         BZ    *+8                 NO                                           
         OI    DOSPFLG1,DOSPPPER   POINTPERSON                                  
         DROP  R6                                                               
*                                                                               
GDARE30  OI    PCF2,PCFSUPD        INDICATE THAT WE'VE UPDATED THIS             
         GOTO1 PUTREC                                                           
         MVC   AIO,AIO2            POINT BACK TO BATCH RECORD                   
*                                                                               
GDARE40  L     R6,AIO                                                           
         MVC   DOKEY,0(R6)                SET DARE ORDER KEY                    
         MVI   DOKCMT,1                   SET COMMENT RECORD                    
*                                                                               
         GOTO1 HIGH                                                             
         CLC   KEY(L'DOKEY),KEYSAVE       COMMENT RECORD EXISTS?                
         BNE   GDAREX                     NO                                    
*                                                                               
         MVC   AIO,AIO2                                                         
         GOTO1 GETREC                                                           
*                                                                               
         L     R6,AIO                                                           
         MVI   ELCODE,DOCOMELQ            COMMENT ELEMENT                       
         BRAS  RE,GETEL                                                         
         BNE   GDAREX                                                           
         USING DOCOMELD,R6                                                      
         CLI   DOCOMLIN,1                 TEST COMMENT ON FIRST LINE            
         JNE   GDAREX                                                           
*                                                                               
         XR    RE,RE                                                            
         IC    RE,DOCOMLEN                RE=LENGTH OF DATA                     
         AHI   RE,-(DOCOMOVH)                                                   
         LA    R0,L'FLITFFT               R0=LENGTH OF FIELD                    
         CR    R0,RE                      TEST FIELD IS BIGGER                  
         BH    GDARE50                    IT IS - THAT'S GOOD                   
         LR    RE,R0                      USE LENGTH OF FIELD                   
         OI    FLITFLG2,FLIT2LNG          COMMENT LONGER THAN CAN FIT           
*                                                                               
GDARE50  BCTR  RE,0                                                             
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   FLITFFT(0),DOCOMTXT                                              
*                                                                               
         BRAS  RE,NEXTEL                  MULTIPLE LINE COMMENTS?               
         BNE   GDAREX                     NO                                    
         OI    FLITFLG2,FLIT2LNG          COMMENT LONGER THAN CAN FIT           
*                                                                               
GDAREX   J     XIT                                                              
         DROP  R2,R3,R4,R5,R6                                                   
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO ADD A LAST METHOD RECORD                                 *         
***********************************************************************         
         USING SCRND,R4                                                         
ADDLMREC NTR1  BASE=*,LABEL=*                                                   
         LA    R8,SYSSPARE                R8=A(LOCAL STORAGE AREA)              
         USING LSSD,R8                                                          
*                                                                               
         L     R6,AIO                                                           
         XC    0(255,R6),0(R6)          CLEAR AIO (LMREC CURRENLY 134)          
         USING DMTHKEY,R6                                                       
         MVI   DMTHTYPE,DMTHTYPQ        X'0D3E'                                 
         MVI   DMTHSBTY,DMTHSBTQ                                                
         MVC   DMTHAGMD,BAGYMD                                                  
         MVC   DMTHBYR,QBYR                                                     
         MVC   DMTHSTA,BSTA                                                     
         MVC   DMTHCLT,BCLT                                                     
         MVC   DMTHAGY,AGENCY                                                   
         LA    R1,DMTHLNQ+DMTHFRST-DMTHKEY+1                                    
         STCM  R1,3,DMTHLEN            L(REC) W/O THE ACTVD                     
*                                                                               
         L     R6,AIO                                                           
         LA    R6,24(R6)                                                        
         USING DMTHELD,R6                                                       
         MVI   DMTHEL,DMTHELQ                                                   
         MVI   DMTHELLN,DMTHLNQ                                                 
         MVC   DMTHDEST,SCRNDST    COPY 1ST LETTER OF DEST                      
         MVI   DMTHMTHD,C'I'                                                    
*                                                                               
         CLC   =C'FAX: ',SCRNROU   WAS IT FAXING?                               
         BE    *+14                                                             
         CLC   =C'need special fax#- use PF5',SCRNROU  was IT FAXING?           
         BNE   *+12                                                             
         MVI   DMTHMTHD,C'F'                                                    
         B     LASTM10                                                          
*                                                                               
         CLC   =C'EML: ',SCRNROU   WAS IT EMAILING?                             
         BNE   LASTM10                                                          
         MVI   DMTHMTHD,C'E'                                                    
         L     R1,AIO2             SHOULD HAVE BDE RECORD                       
         LA    R1,GBDRFST(R1)      X'01' - FULL NAME ELEM                       
         USING GBNELD,R1                                                        
         MVC   DMTHBDEC,GBNFNM                                                  
         DROP  R1,R6                                                            
*                                                                               
LASTM10  CLI   ELEM,DMSPELQ        HAVE A SALESPERSON ELEMENT TO ADD?           
         BNE   LASTM20                                                          
*                                                                               
         AHI   R6,DMTHLNQ                                                       
*                                                                               
         GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)    ADD NEW SALESPERSON           
*                                                                               
LASTM20  BRAS  RE,UPDATTIM                                                      
*                                                                               
         MVI   ACTELOPT,C'Y'                                                    
         GOTO1 ADDREC                                                           
         MVI   ACTELOPT,C'N'                                                    
         J     XIT                                                              
         DROP  R4,R8                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO UPDATE A LAST METHOD RECORD                              *         
***********************************************************************         
UPDLMREC NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R8,SYSSPARE                R8=A(LOCAL STORAGE AREA)              
         USING LSSD,R8                                                          
         LA    R3,BLOCK                                                         
         USING GLBRWKW,R3                                                       
***                                                                             
* BUILD A SALESPERSON ELEMENT IN ELEM                                           
***                                                                             
         XC    ELEM,ELEM                                                        
         LA    R6,ELEM                                                          
         USING DMSPELD,R6                                                       
         MVI   DMSPEL,DMSPELQ      X'20'                                        
         MVI   DMSPLEN,DMSPLENQ    LENGTH                                       
         MVC   DMSPNAME(20),GLBRWKW  SALESPERSON NAME                           
         OC    DMSPNAME,BLANKS       SPACE PADDED (FOR 25)                      
         NI    DMSPFLAG,X'FF'-DMSPPPER                                          
         TM    GLBRWFLG,X'80'      GOT A POINTPERSON VIA GLOBBER?               
         BZ    *+8                 NO                                           
         OI    DMSPFLAG,DMSPPPER   POINTPERSON                                  
*                                                                               
         LA    R6,KEY              LET'S SEE IF WE HAVE LSTM                    
         XC    KEY,KEY                                                          
         USING DMTHKEY,R6                                                       
         MVI   DMTHTYPE,DMTHTYPQ   X'0D3E'                                      
         MVI   DMTHSBTY,DMTHSBTQ                                                
         MVC   DMTHAGMD,BAGYMD     A/M                                          
         MVC   DMTHBYR,QBYR        BUYER                                        
*                                                                               
         LA    R5,BRWKEY2          GET STATION FROM THE BATCH KEY THAT          
         USING DBTKEY,R5           WE SAVED OFF BE4 GOING TO BROWSE             
         MVC   DMTHSTA,DBTKSTA     STATION                                      
         DROP  R5                                                               
*                                                                               
         MVC   DMTHCLT,BCLT        CLIENT                                       
*                                                                               
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         MVC   AIO,AIO3            USE AIO3 FOR LAST METHOD                     
         CLC   KEY(13),KEYSAVE     ANY LAST METHOD BY THIS CLIENT?              
         BE    UPDLM01             YES                                          
***                                                                             
* SET CURSOR POSITION BEFORE ADDING THE LAST METHOD RECORD                      
***                                                                             
         LA    R4,MKTSELH          1ST SELECT FIELDS                            
         ZIC   R1,BRWSEL                                                        
         MHI   R1,SCRNLNQ                                                       
         AR    R4,R1                                                            
*                                                                               
         LA    R5,BRWKEY2          GET STATION FROM THE BATCH KEY THAT          
         USING DBTKEY,R5           WE SAVED OFF BE4 GOING TO BROWSE             
         MVC   BSTA,DBTKSTA        CHANGE BSTA SO WE ADD CORRECT LMREC          
         DROP  R5                                                               
*                                                                               
         BRAS  RE,ADDLMREC         ADD A LAST METHOD RECORD                     
         B     UPDLMX              AND EXIT                                     
*                                                                               
UPDLM01  MVI   RDUPDATE,C'Y'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVI   ELCODE,DMSPELQ      SALESPERSON ELEMENT                          
         BRAS  RE,GETEL            HAVE ONE?                                    
         BNE   UPDLM02             NO                                           
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6) DELETE OLD SALESPERSON           
         B     UPDLM04             AND ADD A NEW ONE                            
*                                                                               
UPDLM02  L     R6,AIO                                                           
         MVI   ELCODE,DMFXELQ      X'10'                                        
         BRAS  RE,GETEL            HAVE ONE?                                    
         BE    UPDLM03             YES                                          
*                                                                               
         L     R6,AIO              NO, PUT SALESPERSON AFTER X'01'              
         MVI   ELCODE,DMTHELQ                                                   
         BRAS  RE,GETEL                                                         
*                                                                               
UPDLM03  ZIC   R1,1(R6)            ELEM LENGTH                                  
         AR    R6,R1               INSERT SALESPERSON RECORD HERE               
UPDLM04  GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)                                  
*                                                                               
         BRAS  RE,UPDATTIM                                                      
*                                                                               
         GOTO1 PUTREC                                                           
UPDLMX   J     XIT                                                              
         DROP  R3,R6                                                            
         LTORG                                                                  
         EJECT                                                                  
***********************************************************************         
* UPDATE THE NEW TIME STAMP ELEMENT                                             
***********************************************************************         
UPDATTIM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    ELEM,ELEM                                                        
         LA    R2,ELEM                                                          
         USING DATTIMD,R2                                                       
         MVI   DATTIM,DATTIMLQ     X'D1'                                        
         MVI   DATTIMLN,DATTMLNQ                                                
*                                                                               
         GOTO1 DATCON,DMCB,(5,0),(19,DATTMGDT)                                  
         THMS  DDSTIME=YES                                                      
         STCM  R0,15,PACKOF4B                                                   
         ST    R1,FULL                                                          
         AP    PACKOF4B,FULL                                                    
*                                                                               
         CP    PACKOF4B,=P'240000'    PAST MIDNIGHT?                            
         BL    UPDT10                                                           
         SP    PACKOF4B,=P'240000'    YES, BUMP TO NEXT DAY AND ADJUST          
         GOTO1 DATCON,DMCB,(5,0),(0,DUB)   THE TIME                             
         GOTO1 ADDAY,DMCB,DUB,DUB,F'1'                                          
         GOTO1 DATCON,DMCB,(0,DUB),(19,DATTMGDT)                                
*                                                                               
UPDT10   ICM   R1,15,PACKOF4B                                                   
         SRL   R1,12               GET RID OF SECONDS AND SIGN                  
         STCM  R1,3,DATTMGTM                                                    
         MVC   DATTMCDT,DATTMGDT                                                
         MVC   DATTMCTM,DATTMGTM                                                
*                                                                               
         L     R6,AIO              WE DON'T KNOW CREATION DATE SO WE'LL         
         MVI   ELCODE,X'F1'           USE THE X'F1' ELEM TO GET IT              
         BRAS  RE,GETEL                                                         
         BNE   UPDT15                                                           
         USING ACTVD,R6                                                         
         GOTO1 DATCON,DMCB,(3,ACTVADDT),(19,DATTMCDT)                           
         DROP  R6                                                               
*                                                                               
UPDT15   L     R6,AIO                                                           
         MVI   ELCODE,DATTIMLQ     X'D1'                                        
         BRAS  RE,GETEL            DO WE HAVE ONE?                              
         BNE   UPDT20              NO, ADD IT                                   
OLD      USING DATTIMD,R6                                                       
         MVC   DATTMCDT,OLD.DATTMCDT                                            
         MVC   DATTMCTM,OLD.DATTMCTM                                            
         GOTO1 RECUP,DMCB,(C'S',AIO),(R6),(R6)    DELETE OLD DATTIMELEM         
UPDT20   GOTO1 RECUP,DMCB,(C'S',AIO),ELEM,(R6)    ADD NEW DATTIMELEM            
         DROP  OLD,R2,R8                                                        
         XIT1                                                                   
         LTORG                                                                  
*&&DO                                                                           
* REMOVING FORCING FAX TO MEDIAOCEAN BECAUSE OF OOW  WHOA  2007-05-21           
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
***********************************************************************         
* UPDATE THE NEW TIME STAMP ELEMENT                                             
***********************************************************************         
CHKMOSTA NTR1  BASE=*,LABEL=*                                                   
         USING LSTD,R3                                                          
         USING DARBTCHD,R4                                                      
         USING SCRND,R2                                                         
         LA    R8,SYSSPARE                R8=A(LOCAL STORAGE AREA)              
         USING LSSD,R8                                                          
         MVC   WORK(L'SCRNSTA),SCRNSTA                                          
         OC    WORK(L'SCRNSTA),BLANKS                                           
*                                                                               
         LA    RE,MOSTATAB                                                      
CKMOS05  CLI   0(RE),X'FF'         END OF TABLE?                                
         BE    CKMOSNO             YES                                          
*                                                                               
CKMOS10  ZIC   R1,0(RE)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   1(0,RE),WORK        MATCH ON STATION?                            
         BE    CKMOS20             YES                                          
         LA    RE,L'MOSTATAB(RE)   NO, BUMP TO NEXT ENTRY                       
         B     CKMOS05                                                          
*                                                                               
CKMOS20  LA    R5,MOESTTAB                                                      
         ZIC   RE,DBTKEST                                                       
         LA    R5,0(RE,R5)                                                      
         CLI   0(R5),1             DOES IT START ON MONDAY?                     
         BH    CKMOSYES            NO, SET OOW                                  
         BE    CKMOSNO             YES, EXIT                                    
* READ THE ESTIMATE RECORD                                                      
         LA    R6,KEY                                                           
         USING ESTHDRD,R6                                                       
         XC    KEY,KEY                                                          
         MVI   EKEYTYPE,0                                                       
         MVC   EKEYAM,BAGYMD                                                    
         MVC   EKEYCLT,DBTKCLT                                                  
         MVC   EKEYPRD,=C'POL'                                                  
         MVC   EKEYEST,DBTKEST                                                  
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 HIGH                                                             
         CLC   KEY(L'EKEY),KEYSAVE                                              
         BE    *+6                                                              
         DC    H'0'                                                             
                                                                                
         MVC   AIO,AIO3                                                         
         MVI   RDUPDATE,C'N'                                                    
         GOTO1 GETREC                                                           
         L     R6,AIO                                                           
         MVC   0(1,R5),EOWSDAY     SAVE OUT OF WEEK START DAY                   
         CLI   0(R5),0                                                          
         BNE   CKMOSYES                                                         
         MVI   0(R5),1                                                          
         B     CKMOSNO                                                          
*                                                                               
CKMOSYES OI    LSTFLAG2,LSTMOFAX                                                
CKMOSNO  J     XIT                                                              
         DROP  R2,R3,R4,R6                                                      
*&&                                                                             
       ++INCLUDE DDMOREPTAB                                                     
         LTORG                                                                  
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
***********************************************************************         
*                                                                               
***********************************************************************         
***********************************************************************         
* LOCAL SAVED STORAGE                                                           
***********************************************************************         
LSSD     DSECT                                                                  
RELO     DS    A                                                                
BASERC   DS    A                                                                
*                                                                               
AOPTTAB  DS    A                   A(OPTION TABLE)                              
AKEYTAB  DS    A                   A(KEY TABLE)                                 
AFDTAB   DS    A                   A(FILTER DATA TABLE)                         
*                                                                               
REGSAVE  DS    A                   SPACE TO SAVE OF A REGISTER                  
VSQUASH  DS    V                                                                
VGLOBBER DS    A                                                                
*                                                                               
AGYMD    DS    XL1                 AGENCY/MEDIA                                 
KYLN     DS    XL1                 LENGTH OF MINIMUM KEY                        
KYMSK    DS    XL(L'DBTKEY)        FILTER KEY MASK                              
KYFLDLN  DS    XL1                 KEY FIELD LENGTH                             
*                                                                               
MISCFLG1 DS    XL1                 MISCELLANEOUS FLAGS 2                        
MF1RADIO EQU   X'80'               - MEDIA IS RADIO                             
MF1FXRAD EQU   X'40'               - FAXING ALL RADIO ORDERS                    
*                                                                               
MISCFLG2 DS    XL1                 MISCELLANEOUS FLAGS 2                        
MF1NOFLT EQU   X'80'               - NO FLIGHT ELEMENT IN BATCH                 
*                                                                               
DAREFLG1 DS    XL1                 DARE ORDER FLAG                              
DF1TRADE EQU   X'04'               - UPDATE THE TRADE ORDER                     
*                                      WARNING: DON'T CHANGE THIS BIT           
*                                                                               
PCF      DS    XL1                 PROCESS CONTROL FLAGS                        
PCFNEXTK EQU   X'80'                 CONTINUE WITH NEXT KEY                     
PCFFLGHT EQU   X'40'                 USING FLIGHTS                              
PCFNOCUR EQU   X'20'                 DATA NOT CURRENT                           
PCFKYCHG EQU   X'10'                 KEY CHANGED                                
PCFDSTCH EQU   X'08'                 CHANGED A DESTINATION                      
PCFREFSH EQU   X'04'                 REFRESH                                    
PCFEOLST EQU   X'02'                 END OF LIST, REDISPLAY                     
PCFMIS   EQU   X'01'                 RE-DISPLAY SAME KEY FROM MIS               
*                                                                               
PCF2     DS    XL1                 PROCESS CONTROL FLAGS                        
PCFMIS2  EQU   X'80'                 RE-DISPLAY ON RETURN FROM MIS              
PCFBRO   EQU   X'40'                 RE-DISPLAY GOING TO BROWSE                 
PCFBRO2  EQU   X'20'                 RE-DISPLAY ON RETURN FROM BROWSE           
PCFBRO3  EQU   X'10'                 UPDATING SALESPERSON FROM BROWSE           
PCFSUPD  EQU   X'08'                 SALESPERSON UPDATED (ORDER REC)            
PCFBATCH EQU   X'04'                 SALESPERSON UPDATED (BATCH REC)            
PCFSWAP  EQU   X'02'                 SWITCHED OVERLAYS,REDISP LAST PAGE         
PCFPG1   EQU   X'01'                 RE-DISPLAY FROM 1ST PAGE                   
*                                                                               
PCF3     DS    XL1                 PROCESS CONTROL FLAGS                        
PCFLASTM EQU   X'80'                 UPDATED THE LAST METHOD RECORD             
*                                                                               
FFLAG    DS    XL1                 FILTER FLAG                                  
FFMKT    EQU   X'80'                 FILTER ON THE MARKET                       
FFSTA    EQU   X'40'                               STATION                      
FFCLT    EQU   X'20'                               CLIENT                       
FFPRD    EQU   X'10'                               PRODUCT                      
FFPR2    EQU   X'08'                               PRODUCT2                     
FFEST    EQU   X'04'                               ESTIMATE                     
FFFLT    EQU   X'02'                               FLIGHT                       
*                                                                               
FLTMKT   DS    XL2                 MARKET FILTER                                
FLTSTA   DS    XL3                 STATION                                      
FLTCLT   DS    XL2                 CLIENT                                       
FLTPRD   DS    XL1                 PRODUCT                                      
FLTPR2   DS    XL1                 PRODUCT2                                     
FLTPRDC  DS    CL3                 PRODUCT - CHARACTER                          
FLTPR2C  DS    CL3                 PRODUCT2 - CHARACTER                         
FLTEST   DS    XL1                 ESTIMATE                                     
FLTFLT   DS    XL1                 FLIGHT                                       
*                                                                               
BRWPREF  DS    C                   BROWSE PREFERENCE                            
BUYR     DS    CL3                 BUYER                                        
CURLINE  DS    XL2                 DISPLACEMENT TO CURRENT LINE                 
*                                                                               
OPT      DS    XL1                 VARIOUS OPTION BITS FOR LIST MODE            
OPTTRADE EQU   X'80'                 TRADE ONLY                                 
OPTCASH  EQU   X'40'                 CASH ONLY                                  
OPTDAD   EQU   X'20'                 DISPLAY DISK ADDRESS                       
OPTSH0   EQU   X'10'                 SHOW BATCH FOR 0 DOLLARS                   
OPTSHSNT EQU   X'08'                 SHOW BATCH FOR ORDER SENT                  
*                                                                               
ELC      DS    XL1                ELEMENT CONTROL FLAGS                         
ELCOFL   EQU   X'80'               OLD FLIGHT ELEMENT FOUND                     
ELCSTD   EQU   X'40'               STARDARD COMMENT INPUT                       
ELCFFT   EQU   X'20'               FREE FORM TEXT                               
ELCPRV   EQU   X'10'               PREVIOUS OR NEW                              
*                                                                               
NSENT    DS    XL1                 NUMBER SENT                                  
*                                                                               
PACKOF4B DS    PL4                 PACKED OF 4 BYTES                            
*                                                                               
PASSPRD  DS    CL7                 PASSED PRODUCT CODE TO ORDER/SEND            
ESTWFLT  DS    CL7                 PASSED EST W/ FLT   TO ORDER/SEND            
XMITCTB  DS    CL1                 TRANSMIT CASH/TRADE/BOTH                     
*                                                                               
BATCHKEY DS    XL(L'DBTKEY)        LAST BATCH KEY LISTED ON CURR SCRN           
DAD      DS    XL4                 DISK ADDRESS OF BATCH RECORD                 
NXTFLT   DS    XL1                 NEXT FLIGHT FOR SAVE BATCH KEY               
*                                                                               
PFLINE   DS    XL1                                                              
*                                                                               
BLANKS   DS    CL80                SPACES                                       
FFS      DS    XL14                X'FFFFFF'                                    
*                                                                               
NSCRQ    EQU   (MKTPFLNH-MKTSELH)/SCRNLNQ  MAX ON SCREEN                        
NLSTQ    EQU   NSCRQ+1                     MAX IN LIST                          
NLST     DS    XL1                         NUMBER IN THE LIST TABLE             
NSCRN    DS    XL1                         NUMBER ON THE SCREEN                 
*                                                                               
FLDH     DS    XL8                 FIELD HEADER                                 
FLD      DS    CL60                FIELD                                        
*                                                                               
NEWCMT   DS    CL(L'DOISCOM1)                                                   
OLDCMT   DS    CL(L'DOISCOM1)                                                   
*                                                                               
OCOMBLK  DS    XL(OMCLENQ)                                                      
*                                                                               
LSTTAB   DS    (NLSTQ)XL(LSTLNQ)   BATCH LIST                                   
*                                                                               
BTCHFLT  DS    C                   BATCH FLIGHT ELEMENT(Y/N)                    
LASTSTA  DS    XL(L'BSTA)                 LAST STATION                          
LASTDST  DS    CL(L'SCRNDST)                   DESTINATION                      
LASTROU  DS    CL(L'SCRNROU)                   ROUTE                            
*                                                                               
PROFDAR  DS    CL16                DARE PROFILE                                 
PDARDEMS EQU   PROFDAR+1             INCLUDE DEMOS IN ORDER? (N,Y,1-4)          
PDARBCOM EQU   PROFDAR+2             INCLUDE BUY COMMENTS IN ORDER?             
PDARTSRP EQU   PROFDAR+6   3 BYTES   TRADE SPECIAL REP DIGITS                   
PDARPORN EQU   PROFDAR+9             USES PREV/NEW?                             
*                                                                               
PROFOM   DS    CL16                OM PROFILE                                   
POMUSEOM EQU   PROFOM               - USES ORDER MANAGER?                       
POMAUCFM EQU   PROFOM+2             - PARITAL CONFIRM WORKFLOW                  
POMFXRAD EQU   PROFOM+3             - FAX RADIO ALL RADIO ORDERS?               
POMMGREP EQU   PROFOM+15            - CREATE MKGD TRANSACTION REPORT?           
*                                                                               
ALL      EQU   X'FF'                                                            
EOT      EQU   X'FF'                                                            
POLQ     EQU   X'FF'                                                            
*                                                                               
ERRNUM   DS    XL2                                                              
ELEM2    DS    XL(L'ELEM)                                                       
PNFLAG   DS    CL1                                                              
BRWKEY   DS    XL(L'DBTKEY)        KEY OF DARE ORDER RECORD                     
BRWKEY2  DS    XL(L'DBTKEY)        KEY OF DARE ORDER RECORD                     
BRWCUR   DS    CL2                 DISP CUR POSITION OF =BROWSE                 
BRWSEL   DS    CL1                 ENTRY NUMBER OF =BROWSE (0-6)                
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
MOESTTAB DS    XL256                                                            
MOFAXFLG DS    C                                                                
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
*                                                                               
MXFLIT   EQU   17                  MAX # OF FLIGHTS - FLT0 TO FLT16             
MXTRADE  EQU   2                   MAX # OF TRADE                               
FLITAB   DS    (MXTRADE*MXFLIT)XL(FLITLNQ)                                      
*                                                                               
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER A SCREEN ENTRY                                       *         
***********************************************************************         
SCRND    DSECT                                                                  
SCRNSELH DS    XL8                    SELECT HEADER                             
SCRNSEL  DS    CL1                    SELECT                                    
*                                                                               
SCRNBUYH DS    XL8                    BUY HEADER                                
SCRNBUY  DS    0XL(SCRNBUYX-SCRNSTA)  BUY DETAILS                               
SCRNSTA  DS    CL5                    STATION                                   
         DS    CL3                                                              
SCRNTYPE DS    CL1                                                              
         DS    CL2                                                              
SCRNPRD  DS    CL7                    PRODUCT                                   
         DS    CL1                                                              
SCRNEST  DS    CL3                    ESTIMATE                                  
         DS    CL1                                                              
SCRNFLT  DS    CL2                    FLIGHT                                    
         DS    CL1                                                              
SCRNSPT  DS    CL4                    SPOTS                                     
         DS    CL1                                                              
SCRNDLR  DS    CL10                   DOLLARS                                   
SCRNBUYX EQU   *                                                                
*                                                                               
SCRNDSTH DS    CL8                    DESTINATION HEADER                        
SCRNDST  DS    CL4                    DESTINATION                               
*                                                                               
SCRNROUH DS    CL8                    ROUTE HEADER                              
SCRNROU  DS    CL26                   ROUTE                                     
*                                                                               
SCRNCMTH DS    CL8                    COMMENT HEADER                            
SCRNCMT  DS    CL1                    COMMENT                                   
*                                                                               
SCRNPRNH DS    CL8                    P OR N HEADER (NOT PORN!)                 
SCRNPORN DS    CL1                    P OR N (SEE ABOVE!)                       
*                                                                               
SCRNLNKH DS    CL8                    LINKED TO ORDER HEADER                    
SCRNLNK  DS    CL1                    LINKED                                    
*                                                                               
SCRNSTCH DS    CL8                    'STDCMT:' HEADER                          
SCRNSTC  DS    CL7                    'STDCMT:'                                 
*                                                                               
SCRNSTDH DS    CL8                    STANDARD COMMENT HEADER                   
SCRNSTD  DS    CL8                    STANDARD COMMENT                          
*                                                                               
SCRNORCH DS    CL8                    'ORDCMT:' HEADER                          
SCRNORC  DS    CL7                    'ORDCMT:'                                 
*                                                                               
SCRNFFTH DS    CL8                    FREE FORM TEXT HEADER                     
SCRNFFT  DS    CL48                   ORDER COMMENT                             
SCRNLNQ  EQU   *-SCRND                                                          
SCRNLNQ2 EQU   *-SCRNBUYH          JUST THE FACTS MAAM                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER AN ITEM IN THE LIST                                  *         
***********************************************************************         
LSTD     DSECT                                                                  
LSTBTK   DS    XL(L'DBTKEY)        KEY OF BATCH ORDER RECORD                    
LSTBTDA  DS    XL4                 DISK ADDRESS OF BATCH RECORD                 
LSTDAK   DS    XL(L'DBTKEY)        KEY OF DARE ORDER RECORD                     
LSTFLT   DS    XL1                 FLIGHT NUMBER                                
LSTSPTS  DS    XL2                 NUMBER OF SPOTS                              
LSTDLRS  DS    XL4                 DOLLARS                                      
LSTFLAG  DS    XL1                 FLAG (SEE DBFLTFL1)                          
LSTFLAG2 DS    XL1                 FLAG2                                        
LSTSALP  EQU   X'80'               WE HAVE SALESPERSON                          
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
LSTMOFAX EQU   X'40'               FAXING OOW TO MO                             
* ADDED CODE FOR FAXING OOW TO MO  HWON 01/20/04                                
LSTIND   DS    XL1                 STATUS INDICATORS                            
LSTICHG  EQU   X'80'               - LINE HAS BEEN CHANGED                      
LSTI2LNG EQU   X'40'               - COMMENT LONGER THAN CAN FIT                
LSTBUYR  DS    CL(L'DOIDBYR)       BUYER(IF ALREADY ASSIGNED)                   
LSTSTD   DS    CL8                 STANDARD COMMENT CODE                        
LSTFFT   DS    XL(L'SCRNFFT)       FREE FORM TEXT                               
LSTLNQ   EQU   *-LSTD                                                           
***********************************************************************         
* DSECT TO COVER AN ITEM IN THE FLIGHT TABLE                          *         
***********************************************************************         
FLITD    DSECT                                                                  
FLITNUM  DS    XL1                 FLIGHT NUMBER                                
FLITSTR  DS    XL3                 START DATE                                   
FLITEND  DS    XL3                 END DATE                                     
FLITSPT  DS    XL2                 SPOTS                                        
FLITDLR  DS    XL4                 DOLLARS                                      
FLITBYR  DS    CL3                 BUYER                                        
FLITDAK  DS    XL(L'DOKEY)         KEY OF DARE ORDER RECORD                     
FLITFLG  DS    XL1                 FLAG (SEE DBFLTFL1)                          
FLITFLG2 DS    XL1                 FLAG2                                        
FLIT2LNG EQU   X'80'               - COMMENT LONGER THAN CAN FIT                
FLITSTD  DS    CL8                 STANDARD COMMENT                             
FLITFFT  DS    XL(L'SCRNFFT)       FREE FORM TEXT                               
FLITLNQ  EQU   *-FLITD                                                          
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER KEY DATA TABLE                                                 
***********************************************************************         
DKYD     DSECT                                                                  
DKYTYP   DS   XL1                  SUB-TYPE                                     
DKYTLN   DS   XL1                  LENGTH OF TABLE ENTRY                        
DKYNUM   DS   XL1                  NUMBER OF KEY FIELDS                         
DKYDATA  DS   0X                   KEY FIELD DATA                               
DKYLEN   DS   XL1                  LENGTH OF KEY FIELD                          
DKYDSP   DS   XL1                  DISPLACEMENT TO KEY FIELD                    
DKYSRC   DS   XL1                  SOURCE DATA EQUATE                           
DKYLNQ   EQU  *-DKYDATA                                                         
                                                                                
                                                                                
***********************************************************************         
* DSECT TO COVER FILTER DATA  TABLE                                   *         
***********************************************************************         
FDD      DSECT                                                                  
FDFLBIT  DS    AL1                 FLAG BITS TO TEST FOR FILTER DATA            
FDFLFLD  DS    AL2                 DISPLACEMENT TO THE FLAG FIELD               
         DS    XL1                 N/D                                          
FDSRC    DS    AL2                 DISPLACEMENT TO DATA                         
FDKSET   DS    AL2                 DISP. TO SPECAIL FIRST TIME HOOK             
FDKFLT   DS    AL2                 DISP. TO SPECIAL FILTER ROUTINE              
FDLNQ    EQU   *-FDD                                                            
                                                                                
         EJECT                                                                  
***********************************************************************         
* DSECT TO COVER THE OPTION TABLE                                               
***********************************************************************         
OPTND    DSECT                                                                  
OPTNTXT  DS    CL8                 KEYWORD                                      
OPTNROU  DS    XL2                 ROUTINE                                      
OPTNLNQ  EQU   *-OPTND                                                          
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPOMSFFD                                                       
         EJECT                                                                  
         ORG   CONTAGH                                                          
       ++INCLUDE SPOMSEAD                                                       
         EJECT                                                                  
       ++INCLUDE DDGENTWA                                                       
         EJECT                                                                  
* SPOMSWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPOMSWORKD                                                     
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDSCANBLKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDSPOOLD                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
* DDSPLWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
* FAGETTXTD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FAGETTXTD                                                      
         PRINT ON                                                               
* FAFACTS                                                                       
         PRINT OFF                                                              
       ++INCLUDE FAFACTS                                                        
         PRINT ON                                                               
* FATIOB                                                                        
         PRINT OFF                                                              
       ++INCLUDE FATIOB                                                         
         PRINT ON                                                               
BUYRECD  DSECT                                                                  
* SPGENBUY                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENBUY                                                       
         PRINT ON                                                               
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
* SPGENCOM                                                                      
         PRINT OFF                                                              
       ++INCLUDE SPGENCOM                                                       
         PRINT ON                                                               
* SPGENDRBTC                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRBTC                                                     
         PRINT ON                                                               
* SPGENDRMTH                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRMTH                                                     
         PRINT ON                                                               
* SPGENDRFLT                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRFLT                                                     
         PRINT ON                                                               
* SPGENDRORD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPGENDRORD                                                     
         PRINT ON                                                               
* SPADBUYER                                                                     
         PRINT OFF                                                              
       ++INCLUDE SPADBUYER                                                      
         PRINT ON                                                               
ESTHDRD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPGENEST                                                       
         PRINT ON                                                               
* SPOMCOMD                                                                      
* GEGENBDE                                                                      
* REGLBRW                                                                       
         PRINT OFF                                                              
       ++INCLUDE SPOMCOMD                                                       
       ++INCLUDE GEGENBDE                                                       
       ++INCLUDE REGLBRW                                                        
       ++INCLUDE CTGENFILE                                                      
       ++INCLUDE DDACTIVD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'122SPOMS0A   03/14/16'                                      
         END                                                                    
