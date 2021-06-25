*          DATA SET ACREPXY02A AT LEVEL 114 AS OF 05/01/02                      
*PHASE ACXY02A                                                                  
*INCLUDE HELEN                                                                  
*INCLUDE HELLO                                                                  
*INCLUDE DATCON                                                                 
*INCLUDE DATVAL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE PRNTBL                                                                 
         TITLE 'USE BATCH ITEM RECS TO FIND TRANS TO FIX'                       
ACXY02   CSECT                                                                  
         PRINT NOGEN                                                            
         USING ACWORKD,RA                                                       
         USING ACXYD,RC                                                         
         NMOD1 0,**ACXY**,R9                                                    
         L     RA,0(,R1)                                                        
         LA    RC,SPACEND                                                       
         EJECT                                                                  
         USING BIGPRNTD,R7                                                      
         USING BOXD,R6                                                          
         L     R7,VBIGPRNT                                                      
         L     R6,ADBXAREA                                                      
         MVC   BOXWIDTH,=F'165'                                                 
         DROP  R6,R7                                                            
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   REQ                                                              
         ZAP   RECOCNT,=P'0'                                                    
         ZAP   DROTOT,=P'0'        OVERALL TOTAL                                
         ZAP   CROTOT,=P'0'        OVERALL TOTAL                                
         ZAP   TOTOINC,=P'0'                                                    
         B     XIT                                                              
*                                                                               
REQ      CLI   MODE,REQFRST                                                     
         BNE   RUNL                                                             
         TM    POSTFLAG,POSTOPEN                                                
         BZ    *+8                                                              
         BAS   RE,CLOSEWKR                                                      
*                                                                               
         MVI   POSTFLAG,0                                                       
         XC    ID,ID               POTENTIAL POSTING FILE NAME                  
         MVC   ID(2),ORIGINUM                                                   
         MVC   ID+2(3),=C'AA1'                                                  
         PACK  DUB(2),RCDATE+3(3)                                               
         MVC   ID+6(1),DUB                                                      
         MVI   ID+7,C'P'                                                        
*                                                                               
         USING CPYELD,R2                                                        
         MVI   COSTING,NO                                                       
         MVI   NEWOFF,NO                                                        
         L     R2,ADCMPEL                                                       
         TM    CPYSTAT1,X'10'      MAKE COSTING POSTINGS                        
         BZ    *+8                                                              
         MVI   COSTING,YES                                                      
         TM    CPYSTAT4,CPYSOFF2   TWO BYTE OFFICE ?                            
         BZ    *+8                 NO                                           
         MVI   NEWOFF,YES                                                       
         DROP  R2                                                               
*                                                                               
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         ZAP   RECCCNT,=P'0'                                                    
         ZAP   DRCTOT,=P'0'        COMPANY TOTAL                                
         ZAP   CRCTOT,=P'0'        COMPANY TOTAL                                
         ZAP   TOTCINC,=P'0'                                                    
*                                                                               
         USING TBARECD,R5                                                       
         LA    R5,DKEY                                                          
         XC    DKEY,DKEY                                                        
         MVI   TBAKTYP,TBAKTYPQ    SET KEY FOR TRANS BATCH RECORDS              
         MVC   TBAKCPY,RCCOMPFL                                                 
         MVI   HIT,0                                                            
         BAS   RE,HIGH                                                          
         B     RS01                                                             
*                                                                               
RS00     BAS   RE,SEQ                                                           
*                                                                               
RS01     LA    R5,DIR                                                           
         CLC   TBAKCPY,RCCOMPFL                                                 
         BNE   RECAP                                                            
         CLI   TBARECD,TBAKTYPQ    HAS TO BE TRANS BATCH RECORD                 
         BNE   RECAP               I WON'T BE BACK                              
*                                                                               
         OC    TBAKTSEQ,TBAKTSEQ   HEADER ?                                     
         BNZ   RS01AC              NO                                           
         MVI   HIT,0                                                            
         TM    TBAKHSTA,TBAHSUPD   IS IT MARKED UPDATED ?                       
         BZ    RS00                NOT UPDATED                                  
         OI    HIT,HITBATCH        YES                                          
         B     RS00                UPDATED                                      
*                                                                               
RS01AC   TM    HIT,HITBATCH                                                     
         BZ    RS00                NOT ONE WE WANT                              
*        MVI   HIT,0                                                            
         CLI   TBAKBTYP,7          TYPE 7                                       
         BNE   RS00                                                             
         CLC   TBAKADDT,=X'38AA'   OCT21/99                                     
         BH    RS00                                                             
         CLC   TBAKADDT,=X'38A0'   OCT31/99                                     
         BL    RS00                                                             
         MVC   BATCHMOS,TBAKBMOS                                                
         MVC   BATCHCMO,BATCHMOS                                                
         OI    BATCHCMO,X'F0'                                                   
         OI    BATCHCMO+1,X'F0'                                                 
         CLI   BATCHMOS+1,X'10'                                                 
         BL    RS02                                                             
         MVI   BATCHCMO+1,C'A'                                                  
         BE    RS02                                                             
         MVI   BATCHCMO+1,C'B'                                                  
         CLI   BATCHMOS+1,X'11'                                                 
         BE    RS02                                                             
         MVI   BATCHCMO+1,C'C'                                                  
*                                                                               
RS02     MVC   BATCHCDE,TBAKBREF                                                
         MVC   DKEYSV,DIR          SAVE KEY FOR DIRECTORY RESET                 
         L     R3,AIO2                                                          
         BAS   RE,GET              GET BATCH RECORD                             
         BAS   RE,DMPGET                                                        
         L     R5,AIO2                                                          
*                                                                               
         USING BIAELD,R2                                                        
         LA    R2,TBARFST                                                       
RS03     CLI   0(R2),0                                                          
         BE    RS12                                                             
         CLI   0(R2),BIAELQ        X'E8'                                        
         BNE   RS04                                                             
         ZAP   ITEMAMNT,BIAAMT                                                  
         B     RS06                                                             
*                                                                               
RS04     CLI   0(R2),ASKELQ        X'E2'                                        
         BE    RS08                                                             
*                                                                               
RS06     SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     RS03                                                             
*                                                                               
         USING ASKELD,R2                                                        
         USING TRNRECD,R5                                                       
RS08     DS    0H                                                               
         LA    R5,ASKKEY           FIND SJ IN BATCH                             
         CLC   TRNKUNT(2),=C'SJ'                                                
         BNE   RS10                                                             
         MVC   SJJOB,TRNKEY                                                     
         MVC   BILLNUM,TRNKREF                                                  
         MVC   BILLDATE,TRNKDATE                                                
         GOTO1 DATCON,DMCB,(1,BILLDATE),(2,BILLCMPS)                            
         OI    HIT,HITSJ                                                        
         B     RS06                                                             
*                                                                               
RS10     LA    R5,ASKKEY                                                        
         CLC   TRNKUNT(2),=C'1C'                                                
         BNE   RS11                                                             
         MVC   COSTKEY,TRNKEY                                                   
         OI    HIT,HIT1C                                                        
         B     RS06                                                             
*                                                                               
RS11     CLC   TRNKUNT(2),=C'SK'   SHOULD NOT HAVE ANY OF THESE                 
         BNE   RS06                                                             
         DC    H'00'                                                            
*                                                                               
RS12     TM    HIT,HIT1C+HITSJ     MISSING ONE OF THESE                         
         BO    *+6                                                              
         DC    H'00'                                                            
         AP    RECCCNT,=P'1'                                                    
         AP    RECOCNT,=P'1'                                                    
         BAS   RE,DMPGET                                                        
*                                                                               
         LA    R5,DKEY                                                          
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKEY(L'SJJOB),SJJOB                                            
         MVI   TRNKOFF,X'41'                                                    
         BAS   RE,HIGH                                                          
         B     RS15                                                             
*                                                                               
RS14     BAS   RE,SEQ                                                           
         BAS   RE,DMPPDIR2                                                      
*                                                                               
RS15     LA    R5,DIR                                                           
         MVC   SAVEDIR,DIR                                                      
         TM    TRNKSTAT,TRNSDRFT   DRAFT ?                                      
         BO    RS14                YES, SO SKIP                                 
         CLC   TRNKOFF,=C'99'                                                   
         BE    RS50                DON'T READ BILLED AMOUNTS                    
         CLC   TRNKCPY(15),SJJOB                                                
         BNE   RS50                DIDN'T FIND ONE                              
         CLC   TRNKDATE,SPACES                                                  
         BNH   RS14                NOT A TRANACTION, GET NEXT                   
         MVC   SKKEY,TRNKCULC                                                   
         CLC   TRNKSMOS,BATCHMOS   MATCH ON MOS                                 
*        BNE   RS14                                                             
         BE    RS14                                                             
         CLC   TRNKCUNT(2),=C'SK'                                               
         BE    RS20                                                             
         MVC   SKULA,SPACES                                                     
         CLC   TRNKCUNT(2),=C'1R'                                               
         BNE   RS14                                                             
*                                                                               
RS20     L     R3,AIO2                                                          
         BAS   RE,GET                                                           
         LR    R5,R3                                                            
         CLC   TRNKCUNT(2),=C'1R'                                               
         BNE   RS25                                                             
         LA    R2,TRNRFST                                                       
         SR    R1,R1                                                            
*                                                                               
RS21     CLI   0(R2),0             EOR                                          
         BE    RS14                GET NEXT RECORD                              
         CLI   0(R2),SPDELQ        X'4C' -   SUBSIDIARY                         
         BE    RS22                                                             
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     RS21                                                             
*                                                                               
         USING SPDELD,R2                                                        
RS22     IC    R1,SPDLN                                                         
         SHI   R1,SPDLN1Q                                                       
         LA    R3,SPDACCS          POINT TO FIRST ACCOUNT                       
*                                                                               
RS23     CLC   =C'SK',0(R3)                                                     
         BE    RS24                FOUND IT                                     
         LA    R3,14(,R3)          NEXT POSSIBLE ACCOUNT                        
         SHI   R1,14                                                            
         BNP   RS14                GET NEXT RECORD                              
         B     RS23                TRY AGAIN                                    
         DROP  R2                                                               
*                                                                               
RS24     BCTR  R1,0                                                             
         EX    R1,*+4                                                           
         MVC   SKULA(0),0(R3)                                                   
*                                                                               
RS25     LA    R2,TRNRFST                                                       
         CLI   0(R2),TRNELQ        X'44'                                        
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
RS28     CLI   0(R2),0             EOR                                          
         BE    RS14                READ SEQUENTIAL                              
         CLI   0(R2),PTAELQ        X'77'                                        
         BE    RS30                                                             
*                                                                               
RS29     SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     RS28                                                             
*                                                                               
         USING PTAELD,R2                                                        
RS30     CLI   PTATYPE,PTATRAL                                                  
         BNE   RS29                                                             
         TM    PTASTAT1,PTASPEND                                                
         BO    RS29                                                             
         CLC   PTARBLNO,BILLNUM                                                 
         BNE   RS29                NOT THE ONE                                  
         CLC   PTARBLDT,BILLCMPS                                                
         BNE   RS29                NOT THE ONE                                  
         ZAP   INCAMT,PTANET                                                    
         AP    TOTCINC,INCAMT                                                   
         AP    TOTOINC,INCAMT                                                   
         DROP  R2                                                               
*                                                                               
         USING TRNELD,R2                                                        
         L     R5,AIO2             FOUND ORIGINAL SJ (TY 7) BATCH               
         LA    R2,TRNRFST                                                       
         LA    RE,DRCTOT                                                        
         LA    RF,DROTOT                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BO    RS48                                                             
         LA    RE,CRCTOT                                                        
         LA    RF,CROTOT                                                        
*                                                                               
RS48     MVC   TREF,TRNREF                                                      
         MVC   TDATE,TRNDATE                                                    
         MVC   TBATREF,TRNBTCH                                                  
         AP    0(L'DRCTOT,RE),TRNAMNT                                           
         AP    0(L'DROTOT,RF),TRNAMNT                                           
         DROP  R2                                                               
*                                                                               
         MVI   REPORTSW,C'B'       BEFORE                                       
         BAS   RE,REPORTIT                                                      
         BAS   RE,OPENWRK                                                       
*                                                                               
         USING ACTRECD,R5                                                       
         LA    R5,DKEY                                                          
         L     R3,AIO1                                                          
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,SJJOB      GET JOB                                      
         BAS   RE,READ                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         BAS   RE,GET                                                           
         BAS   RE,GETNAME                                                       
         MVC   JOBNAME,NAMEWORK                                                 
         DROP  R5                                                               
*                                                                               
         BAS   RE,SKPOST                                                        
         BAS   RE,SIPOST                                                        
         CLI   COSTING,NO                                                       
         BE    RS49                DON'T  POST 1C AND 12                        
         BAS   RE,COSTPOST                                                      
*                                                                               
RS49     MVC   DKEY,SAVEDIR        NEXT TRANSACTION                             
         BAS   RE,READ                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,DMPPDIR1                                                      
         B     RS14                                                             
*                                                                               
RS50     MVC   DKEY,DKEYSV         RESET DIRECTORY FOR BATCH RECORDS            
         BAS   RE,READ             NEXT BATCH                                   
         BE    *+6                                                              
         DC    H'00'                                                            
         B     RS00                                                             
         EJECT                                                                  
         USING BIGPRNTD,R7                                                      
         USING PLINE,R6                                                         
RUNL     CLI   MODE,RUNLAST                                                     
         BNE   XIT                                                              
         TM    POSTFLAG,POSTOPEN                                                
         BZ    *+8                                                              
         BAS   RE,CLOSEWKR                                                      
*                                                                               
         L     R7,VBIGPRNT                                                      
         MVI   RCSUBPRG,1                                                       
         MVI   FORCEHED,C'Y'                                                    
         GOTO1 ACREPORT                                                         
         LA    R6,XP                                                            
         MVC   XP(19),=C'** OVERALL TOTAL **'                                   
         EDIT  RECOCNT,(14,PCOUNT),0,MINUS=YES                                  
         EDIT  DROTOT,(14,PDEBITS),2,MINUS=YES                                  
         EDIT  CROTOT,(14,PCREDITS),2,MINUS=YES                                 
         EDIT  TOTOINC,(14,PINCAMT),2,MINUS=YES                                 
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
*                                                                               
RECAP    DS    0H                                                               
         L     R7,VBIGPRNT                                                      
         GOTO1 ACREPORT                                                         
         LA    R6,XP                                                            
         MVC   XP(11),=C'** TOTAL **'                                           
         EDIT  RECCCNT,(14,PCOUNT),0,MINUS=YES                                  
         EDIT  DRCTOT,(14,PDEBITS),2,MINUS=YES                                  
         EDIT  CRCTOT,(14,PCREDITS),2,MINUS=YES                                 
         EDIT  TOTCINC,(14,PINCAMT),2,MINUS=YES                                 
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         EJECT                                                                  
         USING PLINE2,R6                                                        
         USING PSHEADD,R5                                                       
POSTRPT  NTR1                                                                   
         L     R7,VBIGPRNT                                                      
         LA    R5,T                                                             
         LA    R6,XP                                                            
         MVC   PACCT,PSHDACC+1                                                  
         MVC   PCNTR,PSHDSBAC+1                                                 
         MVC   PCNAME,PSHDSBNM                                                  
         MVC   PSALEAC,SALEKEY+1                                                
*                                                                               
         USING TRNELD,R2                                                        
         LR    R2,R5                                                            
         ZIC   RF,1(,R2)                                                        
         AR    R2,RF                                                            
         GOTO1 DATCON,DMCB,(1,TRNDATE),(5,PBDTE)                                
         MVC   PREF2,TRNREF                                                     
         MVC   PBREF2,TRNBTCH                                                   
         MVC   POFF,TRNOFFC                                                     
         LA    R3,PDRAMT                                                        
         TM    TRNSTAT,TRNSDR                                                   
         BO    POSTR10                                                          
         LA    R3,PCRAMT                                                        
         AP    POSTAMNT,TRNAMNT                                                 
*                                                                               
POSTR10  EDIT  (P6,TRNAMNT),(14,(R3)),2,MINUS=YES                               
         GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R2,R5,R6                                                         
         EJECT                                                                  
         USING TRNRECD,R5                                                       
         USING PLINE,R6                                                         
REPORTIT NTR1                                                                   
         L     R7,VBIGPRNT                                                      
         L     R5,AIO2                                                          
         LA    R6,XP                                                            
         GOTO1 HEXOUT,DMCB,TRNKCPY,PCMPNY,1                                     
         MVC   PIND,REPORTSW                                                    
         MVC   PACCOUNT,TRNKULA                                                 
         MVC   PCONTRA,TRNKULC                                                  
         GOTO1 DATCON,DMCB,(1,TRNKDATE),(5,PDATE)                               
         MVC   PREF,TRNKREF                                                     
         MVC   WORK(2),TRNRSMOS                                                 
         MVC   WORK+2(1),=X'01'                                                 
         GOTO1 DATCON,DMCB,(1,WORK),(9,PMOA)                                    
         EDIT  TRNKSBR,(3,PSEQ)                                                 
         LA    R2,TRNRFST                                                       
         CLI   0(R2),X'44'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         USING TRNELD,R2                                                        
         MVC   PBREF,TRNBTCH                                                    
         LA    R3,PDEBITS                                                       
         TM    TRNSTAT,TRNSDR                                                   
         BO    *+8                                                              
         LA    R3,PCREDITS                                                      
         EDIT  (P6,TRNAMNT),(14,(R3)),2,MINUS=YES                               
         EDIT  (P6,INCAMT),(14,PINCAMT),2,MINUS=YES                             
         MVC   PSKACCT,SKULA                                                    
         MVC   PINPBAT,BATCHREF                                                 
         CP    TRNAMNT,INCAMT                                                   
         BE    REPORTX                                                          
         MVI   PDIFF,C'*'                                                       
REPORTX  GOTO1 ACREPORT                                                         
         B     XIT                                                              
         DROP  R7,R6                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SK POSTINGS                                                            
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING TRNRECD,R5                                                       
SKPOST   NTR1                                                                   
         LA    R5,DKEY                                                          
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,COSTKEY                                                 
         BAS   RE,READ             VERIFY COSTING ACCOUNT                       
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R3,AIO3                                                          
         BAS   RE,GET                                                           
         BAS   RE,GETNAME                                                       
         MVC   COSTNAME,NAMEWORK                                                
*                                                                               
         LA    R5,DKEY                                                          
         MVC   TRNKEY,SPACES                                                    
         MVC   TRNKCULA,SKKEY      READ SK                                      
         BAS   RE,READ                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         MVI   TRNKLDG,C'I'        SWITCH TO SI                                 
         BAS   RE,READ                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         CLI   COSTING,NO                                                       
         BE    SKPOST40                                                         
         MVC   REV12CPY,SKCPY                                                   
         MVC   REV12UL,=C'12'                                                   
         MVC   REV12ACT,SPACES                                                  
*                                                                               
         USING ACTRECD,R5                                                       
         L     R3,AIO3                                                          
         BAS   RE,GET                                                           
         LR    R5,R3                                                            
         LA    R2,ACTRFST                                                       
*                                                                               
SKPOST20 CLI   0(R2),0             TEST EOR                                     
         BNE   *+6                                                              
         DC    H'00'                                                            
*                                                                               
         USING SPAELD,R2                                                        
         CLI   0(R2),SPAELQ                                                     
         BNE   SKPOST25                                                         
         CLI   SPATYPE,SPATANAL                                                 
         BNE   SKPOST28                                                         
         MVC   REV12ACT,SPAAULA                                                 
         B     SKPOST30                                                         
*                                                                               
         USING RSTELD,R2                                                        
SKPOST25 CLI   0(R2),RSTELQ                                                     
         BNE   SKPOST28                                                         
         MVC   REV12ACT(L'RSTCOSTG),RSTCOSTG                                    
         B     SKPOST30                                                         
*                                                                               
SKPOST28 ZIC   R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     SKPOST20                                                         
         DROP  R2                                                               
*                                                                               
SKPOST30 CLI   REV12ACT,C' '                                                    
         BH    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         LA    R5,DKEY                                                          
         MVC   ACTKCULA,REV12                                                   
         BAS   RE,READ             GET 12 ACCOUNT                               
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R3,AIO3             12 ACCOUNT                                   
         BAS   RE,GET                                                           
         LR    R5,R3                                                            
         LA    R2,ACTRFST                                                       
         BAS   RE,GETNAME                                                       
         MVC   REVNAME,NAMEWORK                                                 
*                                                                               
SKPOST40 LA    R5,DKEY             GET PRODUCT                                  
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(9),SJJOB                                                 
         BAS   RE,READ                                                          
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         L     R3,AIO3             SJCCCPPP                                     
         BAS   RE,GET                                                           
         BAS   RE,GETNAME                                                       
         BAS   RE,GETSALES                                                      
         MVC   PRDNAME,NAMEWORK                                                 
         BAS   RE,GETOFFC                                                       
         BE    SKPOST42                                                         
*                                                                               
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY(6),SJJOB                                                 
         BAS   RE,READ             GET CLIENT RECORD                            
         BE    *+6                                                              
         DC    H'00'                                                            
         BAS   RE,GET                                                           
*                                                                               
         BAS   RE,GETOFFC                                                       
         BE    SKPOST42                                                         
         DC    H'00'                                                            
*                                                                               
         USING TRNRECD,R5                                                       
SKPOST42 LA    R5,DKEY                                                          
         MVC   TRNKEY,SPACES                                                    
         MVI   TRNKSBR,0                                                        
         MVC   TRNKCULA,SKKEY                                                   
         MVC   TRNKCULC,SJJOB                                                   
         CLI   NEWOFF,YES                                                       
         BNE   *+10                                                             
         MVC   TRNKOFF,OFF2POST                                                 
         MVC   TRNKREF,TREF                                                     
         MVC   TRNKDATE,TDATE                                                   
         L     R3,AIO1                                                          
         BAS   RE,HIGH                                                          
*                                                                               
SKPOST45 CLC   TRNKEY(L'TRNKEY-1),DIR                                           
         BE    *+6                                                              
         DC    H'00'               SHOULD HAVE FOUND AT LEAST ONE               
         BAS   RE,GET                                                           
*                                                                               
         USING PSHEADD,R2                                                       
         LA    R2,T                                                             
         XC    TLEN,TLEN                                                        
         LR    RE,R2                                                            
         LA    RF,L'T                                                           
         XCEFL                                                                  
         MVC   PSHDEL(2),=X'5046'                                               
         MVC   PSHDACC,SKKEY       ACCOUNT                                      
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,SJJOB      CONTRA                                       
         MVC   PSHDSBNM,JOBNAME                                                 
         ZIC   R1,1(,R2)                                                        
         AR    R2,R1               POINT TO NEW LOCATION                        
*                                                                               
OLD      USING TRNELD,RE                                                        
         USING TRNELD,R2                                                        
         LR    R5,R3                                                            
         LA    RE,TRNRFST          POINT TO X'44' IN AIO1                       
         CLI   0(RE),TRNELQ                                                     
         BE    *+6                                                              
         DC    H'00'                                                            
*                                                                               
         TM    OLD.TRNSTAT,TRNSDR                                               
         BO    SKPOST46                                                         
         CLC   OLD.TRNOFFC,OFF2POST                                             
         BNE   SKPOST46                                                         
         CLC   OLD.TRNBTCH,TBATREF                                              
         BE    SKPOST50                                                         
*                                                                               
SKPOST46 BAS   RE,SEQ                                                           
         B     SKPOST45                                                         
         DROP  OLD                                                              
*                                                                               
SKPOST50 ZIC   RF,1(,RE)                                                        
         BCTR  RF,0                                                             
         EXMVC RF,0(R2),0(RE)                                                   
         MVI   TRNTYPE,TRNTCLBL    SET TO TYPE 7                                
         ZAP   TRNAMNT,INCAMT                                                   
         MVI   TRNSTAT,TRNSDR      MAKE IT A DR                                 
         MVC   TRNBTCH,BATCHREF                                                 
         MVC   TRNOFFC,OFF2POST                                                 
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        SI POSTINGS                                                            
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
SIPOST   NTR1                                                                   
         LA    R2,T                                                             
         XC    TLEN,TLEN                                                        
         LR    RE,R2                                                            
         LA    RF,L'T                                                           
         XCEFL                                                                  
         MVC   PSHDEL(2),=X'5046'                                               
         MVC   PSHDACC,SKKEY       ACCOUNT                                      
         MVI   PSHDACC+2,C'I'      MAKE SI                                      
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,SPACES                                                  
         MVC   PSHDSBAC(9),SJJOB   CONTRA (SJCCCPPP)                            
         MVC   PSHDSBNM,PRDNAME                                                 
         CLC   SALEKEY,SPACES      SALES ACCOUNT OVERRIDE                       
         BNH   SIPOST10                                                         
         MVC   PSHDSBAC,SALEKEY                                                 
         MVC   PSHDSBNM,SALENAME                                                
*                                                                               
         USING TRNELD,R2                                                        
SIPOST10 ZIC   R1,1(,R2)                                                        
         AR    R2,R1               POINT TO NEW LOCATION                        
         L     R5,AIO1                                                          
         XC    TRNEL(TRNLN1Q),TRNEL                                             
         XC    TRNNARR,TRNNARR                                                  
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q                                                    
         MVI   TRNSTAT,0                                                        
         MVC   TRNDATE,BILLDATE                                                 
         MVC   TRNREF,BILLNUM                                                   
         MVI   TRNTYPE,TRNTCLBL    SET TO TYPE 7                                
         ZAP   TRNAMNT,INCAMT                                                   
         MVC   TRNBTCH,BATCHREF                                                 
         MVC   TRNOFFC,OFF2POST                                                 
         SR    R1,R1                                                            
         IC    R1,1(,R2)                                                        
         AR    R2,R1                                                            
*                                                                               
         USING MDTELD,R2                                                        
         XC    MDTEL(MDTLNQ),MDTEL                                              
         MVI   MDTEL,MDTELQ        X'1A' - MEDIA TRANSFER                       
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,C'J'         SYSTEM =  PROD                               
         MVC   MDTMED,SJJOB+9      MEDIA     CODE                               
         MVC   MDTCLI(12),SJJOB+3  CLI/PRD/JOB                                  
         MVC   MDTMOS,BATCHMOS     MOS  (PACKED)                                
         MVC   MDTDSCP,JOBNAME     JOB  NAME                                    
         XC    MDTGRS,MDTGRS                                                    
         ZAP   DUB,INCAMT                                                       
         CVB   R0,DUB              INTERNAL  GOES INTO INCOME                   
         STCM  R0,15,MDTCOM                                                     
         XC    MDTNET,MDTNET                                                    
         XC    MDTCD,MDTCD                                                      
         XC    MDTINTL,MDTINTL                                                  
         XC    MDTRECV,MDTRECV                                                  
         XC    MDTVAT,MDTVAT                                                    
*                                                                               
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        1C POSTINGS, 12 CONTRA                                                 
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
COSTPOST NTR1                                                                   
         LA    R2,T                                                             
         XC    TLEN,TLEN                                                        
         LR    RE,R2                                                            
         LA    RF,L'T                                                           
         XCEFL                                                                  
         MVC   PSHDEL(2),=X'5046'                                               
         MVC   PSHDACC,COSTKEY              1C ACCOUNT                          
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,REV12               12 ACCOUNT                          
         MVC   PSHDSBNM,REVNAME                                                 
         ZIC   R1,1(,R2)                                                        
         AR    R2,R1               POINT TO NEW LOCATION                        
*                                                                               
         USING TRNELD,R2                                                        
         L     R5,AIO1                                                          
         XC    TRNEL(TRNLN1Q),TRNEL                                             
         XC    TRNNARR,TRNNARR                                                  
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q                                                    
         MVI   TRNSTAT,TRNSDR      DEBIT                                        
         MVC   TRNDATE,BILLDATE                                                 
         MVC   TRNREF,BILLNUM                                                   
         MVI   TRNTYPE,TRNTCLBL    SET TO TYPE 7                                
         ZAP   TRNAMNT,INCAMT                                                   
         MVC   TRNBTCH,BATCHREF                                                 
         MVC   TRNOFFC,OFF2POST                                                 
         BAS   RE,PUTIT                                                         
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        12 POSTINGS, 1C CONTRA                                                 
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING PSHEADD,R2                                                       
         LA    R2,T                                                             
         XC    TLEN,TLEN                                                        
         LR    RE,R2                                                            
         LA    RF,L'T                                                           
         XCEFL                                                                  
         MVC   PSHDEL(2),=X'5046'                                               
         MVC   PSHDACC,REV12          12 COMPANY CODE                           
         MVC   PSHDANAL,SPACES                                                  
         MVC   PSHDSBAC,COSTKEY       1C ACCOUNT                                
         MVC   PSHDSBNM,COSTNAME                                                
         ZIC   R1,1(,R2)                                                        
         AR    R2,R1               POINT TO NEW LOCATION                        
*                                                                               
         USING TRNELD,R2                                                        
         XC    TRNEL(TRNLN1Q),TRNEL                                             
         XC    TRNNARR,TRNNARR                                                  
         MVI   TRNEL,TRNELQ                                                     
         MVI   TRNLN,TRNLN1Q                                                    
         MVI   TRNSTAT,0           CREDIT                                       
         MVC   TRNDATE,BILLDATE                                                 
         MVC   TRNREF,BILLNUM                                                   
         MVI   TRNTYPE,TRNTCLBL    SET TO TYPE 7                                
         ZAP   TRNAMNT,INCAMT                                                   
         MVC   TRNBTCH,BATCHREF                                                 
         MVC   TRNOFFC,OFF2POST                                                 
         BAS   RE,PUTIT                                                         
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        R3 IS ASSUMED TO BE POINTING AT IO AREA                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
         USING PPRELD,R2                                                        
GETOFFC  NTR1                                                                   
         MVC   OFF2POST,SPACES     GET ACCOUNT NAME                             
         LA    R2,ACTRFST                                                       
GETOF10  CLI   0(R2),0                                                          
         BE    GETOF90                                                          
         CLI   0(R2),PPRELQ        X'24'                                        
         BE    GETOF20                                                          
         ZIC   R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     GETOF10                                                          
*                                                                               
GETOF20  MVC   OFF2POST,PPRGAOFF                                                
*                                                                               
GETOF90  CLC   OFF2POST,SPACES                                                  
         BNH   GETOFNO                                                          
*                                                                               
         SR    RE,RE                                                            
GETOFNO  LTR   RE,RE                                                            
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        R3 IS ASSUMED TO BE POINTING AT IO AREA                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
         USING SANELD,R2                                                        
GETSALES NTR1                                                                   
         MVC   SALENAME,SPACES      GET ACCOUNT NAME                            
         MVC   SALEKEY,SPACES                                                   
         LA    R2,ACTRFST                                                       
GETSL10  CLI   0(R2),0                                                          
         BE    XIT                                                              
         CLI   0(R2),SANELQ        X'??'                                        
         BE    GETSL20                                                          
         ZIC   R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     GETSL10                                                          
*                                                                               
GETSL20  MVC   SALEKEY,SANCODE                                                  
         MVC   SALENAME,SANNAME                                                 
         B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
*---------------------------------------------------------------------*         
*        R3 IS ASSUMED TO BE POINTING AT IO AREA                                
*---------------------------------------------------------------------*         
         SPACE 1                                                                
         USING ACTRECD,R3                                                       
         USING NAMELD,R2                                                        
GETNAME  NTR1                                                                   
         MVC   NAMEWORK,SPACES     GET ACCOUNT NAME                             
         LA    R2,ACTRFST                                                       
GETNM10  CLI   0(R2),0                                                          
         BE    GETNM90                                                          
         CLI   0(R2),NAMELQ        X'20'                                        
         BE    GETNM20                                                          
         ZIC   R1,1(,R2)                                                        
         AR    R2,R1                                                            
         B     GETNM10                                                          
*                                                                               
GETNM20  ZIC   R1,NAMLN                                                         
         SHI   R1,(NAMLN1Q+1)                                                   
         EX    R1,*+4                                                           
         MVC   NAMEWORK(0),NAMEREC                                              
*                                                                               
GETNM90  B     XIT                                                              
         DROP  R2,R3                                                            
         EJECT                                                                  
***********************************************************************         
* WORKER POSTING FILE  ROUTINES                                       *         
***********************************************************************         
         SPACE 1                                                                
OPENWRK  TM    POSTFLAG,POSTOPEN                                                
         BOR   RE                                                               
         ZAP   POSTREC,=P'0'                                                    
         OI    POSTFLAG,POSTOPEN                                                
         MVC   COMMAND,=CL6'OPEN'                                               
         B     FILE                                                             
*                                                                               
ADDPOST  MVC   COMMAND,=CL6'ADD'                                                
         OI    POSTFLAG,POSTADD                                                 
         B     FILE                                                             
*                                                                               
CLOSEWKR CP    POSTREC,=P'0'                                                    
         BE    CLOSEW20                                                         
*                                                                               
         ST    RE,SVRE                                                          
         XC    T(80),T                                                          
         MVC   TLEN,=X'0021'                                                    
         MVC   T(2),=X'521D'                                                    
         MVC   T+2(15),=CL15'POSTINGS'                                          
         ZAP   T+17(6),POSTREC                                                  
         ZAP   T+23(6),POSTAMNT                                                 
         BAS   RE,ADDPOST                                                       
         L     RE,SVRE                                                          
*                                                                               
CLOSEW20 MVC   COMMAND,=CL6'CLOSE'                                              
         NI    POSTFLAG,X'FF'-POSTOPEN                                          
*                                                                               
FILE     NTR1                                                                   
         CLI   RCPOSTNG,C'N'                                                    
         BE    XIT                                                              
         LA    R3,TLEN                                                          
         L     R4,=A(POSTBUFF)                                                  
         GOTO1 WORKER,DMCB,COMMAND,(R4),ID,(R3)                                 
         TM    DMCB+8,X'C0'                                                     
         BZ    *+6                                                              
         DC    H'0'                                                             
         B     XIT                                                              
         EJECT                                                                  
*---------------------------------------------------------------------*         
*              PUT WORKER RECORD TO ACPOST                                      
*---------------------------------------------------------------------*         
PUTIT    NTR1                                                                   
         AP    POSTREC,=P'1'                                                    
         BAS   RE,POSTRPT                                                       
         LA    R2,T                                                             
         ZIC   R3,1(,R2)                                                        
PUT2     AR    R2,R3                                                            
         CLI   0(R2),0                                                          
         BE    PUT4                                                             
         ZIC   R3,1(,R2)                                                        
         LTR   R3,R3                                                            
         BNZ   PUT2                                                             
         MVI   0(R2),0                                                          
PUT4     LA    R2,1(,R2)                                                        
         LA    R3,TLEN                                                          
         SR    R2,R3                                                            
         STH   R2,TLEN                                                          
         BAS   RE,ADDPOST                                                       
         BAS   RE,DMPPOST                                                       
         B     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* DATA MANAGER ROUTINES                                               *         
***********************************************************************         
         SPACE 1                                                                
HIGH     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRDHI,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
READ     LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMREAD,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
SEQ      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMRSEQ,ACCDIR,DKEY,DIR                              
         MVC   DA,DIR+(ACCKDA-ACCRECD)                                          
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
WRT      LR    R0,RE               SAVE RETURN POINT                            
         GOTO1 DATAMGR,DMCB,DMWRT,ACCDIR,DIR,DIR                                
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
GET      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,GETREC,ACCMST,DA,(R3),DMWORK                        
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
ADD      LR    R0,RE                                                            
         XC    DA,DA                                                            
         GOTO1 DATAMGR,DMCB,ADDREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
*                                                                               
PUT      LR    R0,RE                                                            
         GOTO1 DATAMGR,DMCB,PUTREC,ACCMST,DA,(R3),DMWORK                        
         ORG   *-2                                                              
         CLI   RCWRITE,C'N'                                                     
         BE    *+6                                                              
         BASR  RE,RF                                                            
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
***********************************************************************         
* DUMP SOME RECORDS                                                   *         
***********************************************************************         
         SPACE 1                                                                
DMPPOST  NTR1                                                                   
         LA    R6,=C'PUTP'                                                      
         LA    R3,TLEN                                                          
         SR    R4,R4                                                            
         LH    R4,TLEN                                                          
         B     DUMPIT                                                           
*                                                                               
DMPGDIR  NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GETD'                                                      
         LA    R4,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DUMPIT                                                           
*                                                                               
DMPPDIR1 NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'DRRD'                                                      
         LA    R4,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DUMPIT                                                           
*                                                                               
DMPPDIR2 NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'DRSQ'                                                      
         LA    R4,L'DIR                                                         
         LA    R3,DIR                                                           
         B     DUMPIT                                                           
*                                                                               
DMPGET   NTR1                                                                   
         AP    DUMPCNT,=P'1'                                                    
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         AP    PDUMP,=P'1'                                                      
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         LA    R6,=C'GET '                                                      
         B     DUMP                                                             
*                                                                               
DMPPUT   NTR1                                                                   
         CP    PDUMP,MAXDUMP                                                    
         BH    XIT                                                              
         ZAP   DUB,DUMPCNT                                                      
         DP    DUB,EVERY                                                        
         CP    DUB+4(4),=P'0'                                                   
         BNE   XIT                                                              
         LA    R6,=C'PUT '                                                      
*                                                                               
DUMP     L     R3,AIO2                                                          
         SR    R4,R4                                                            
         ICM   R4,3,TIMRLEN-TIMRECD(R3)                                         
*                                                                               
DUMPIT   CLI   QOPT1,C'D'                                                       
         BNE   XIT                                                              
         GOTO1 PRNTBL,DMCB,(4,(R6)),(R3),C'DUMP',(R4),=C'2D'                    
         B     XIT                                                              
         EJECT                                                                  
*              ROUTINE TO GET AN ELEMENT                                        
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
GETEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(,R1)                                                        
         ZIC   R5,4(,R1)                                                        
         GOTO1 HELLO,DMCB,(C'G',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
         SPACE 2                                                                
*              ROUTINE TO DELETE AN ELEMENT                                     
         SPACE 1                                                                
*              P1   BYTE 0    ELEMENT CODE                                      
*                   BYTE 1-3  A(RECORD)                                         
*              P2   BYTE 0    LENGTH OF SEARCH ARGUMENT                         
*                   BYTE 1-3  A(SEARCH ARGUMENT)                                
         SPACE 1                                                                
DELEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         ZIC   R4,0(R1)                                                         
         ZIC   R5,4(R1)                                                         
         GOTO1 HELLO,DMCB,(C'D',ACCMST),((R4),(R2)),((R5),(R3))                 
         B     XIT                                                              
         SPACE 1                                                                
*              ROUTINE TO ADD AN ELEMENT                                        
         SPACE 1                                                                
*              P1   A(RECORD)                                                   
*              P2   A(ELEMENT)                                                  
         SPACE 1                                                                
ADDEL    NTR1                                                                   
         LM    R2,R3,0(R1)                                                      
         GOTO1 HELLO,DMCB,(C'P',ACCMST),(R2),(R3)                               
         CLI   DMCB+12,0                                                        
         BE    XIT                                                              
         DC    H'0'                CAN'T ADD THE ELEMENT                        
         SPACE 1                                                                
XIT      XIT1                                                                   
         EJECT                                                                  
*****************************************                                       
*        LITERALS                                                               
*****************************************                                       
ACCDIR   DC    CL8'ACCDIR'                                                      
ACCMST   DC    CL8'ACCMST'                                                      
GETREC   DC    CL8'GETREC'                                                      
ADDREC   DC    CL8'ADDREC'                                                      
PUTREC   DC    CL8'PUTREC'                                                      
         EJECT                                                                  
*****************************************                                       
*        EQUATES                                                                
*****************************************                                       
YES      EQU   C'Y'                                                             
NO       EQU   C'N'                                                             
EOT      EQU   0                   END OF TABLE                                 
*                                                                               
TRECCNT  DC    PL8'0'                                                           
RECCCNT  DC    PL8'0'                                                           
DRCTOT   DC    PL8'0'                                                           
CRCTOT   DC    PL8'0'                                                           
RECOCNT  DC    PL8'0'                                                           
DROTOT   DC    PL8'0'                                                           
CROTOT   DC    PL8'0'                                                           
TOTCINC  DC    PL8'0'                                                           
TOTOINC  DC    PL8'0'                                                           
ITEMAMNT DC    PL6'0'                                                           
DUMPCNT  DC    PL4'0'                                                           
EVERY    DC    PL4'1'                                                           
PDUMP    DC    PL4'0'                                                           
POSTREC  DC    PL6'0'                                                           
POSTAMNT DC    PL6'0'                                                           
MAXDUMP  DC    PL4'2000'                                                        
*                                                                               
         EJECT                                                                  
PRNTBL   DC    V(PRNTBL)                                                        
HELLO    DC    V(HELLO)                                                         
DATVAL   DC    V(DATVAL)                                                        
HEXIN    DC    V(HEXIN)                                                         
*                                                                               
AIO1     DC    A(IO1)                                                           
AIO2     DC    A(IO2)                                                           
AIO3     DC    A(IO3)                                                           
*                                                                               
DKEY     DS    CL(L'ACCKEY)                                                     
DKEYSV   DS    CL(L'ACCKEY)                                                     
SAVEDIR  DS    CL(L'ACCKEY)                                                     
DIR      DS    CL64                                                             
DA       DS    F                                                                
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
IO1      DS    CL2000                                                           
IO2      DS    CL2000                                                           
IO3      DS    CL2000                                                           
*                                                                               
POSTBUFF DS   0D                                                                
         DC   4500X'00'                                                         
*                                                                               
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1       (1)                                                    
PCMPNY   DS    CL2       (2-3)     COMPANY CODE                                 
         DS    CL3       (4-6)                                                  
PIND     DS    CL1       (7)       BEFORE AFTER INDICATOR                       
         DS    CL2       (8-9)                                                  
PACCOUNT DS    CL14      (10-23)   ACCOUNT                                      
         DS    CL2       (24-25)                                                
PCONTRA  DS    CL14      (26-39)   CONTRA                                       
         DS    CL2       (40-41)                                                
PDATE    DS    CL8       (42-49)   DATE                                         
         DS    CL2       (50-51)                                                
PREF     DS    CL6       (52-57)   REFERENCE                                    
         DS    CL2       (58-59)                                                
PBREF    DS    CL6       (60-65)   BAT REFERENCE                                
         DS    CL2       (66-67)                                                
PSEQ     DS    CL3       (68-70)   SEQ NUMBER                                   
         DS    CL2       (71-72)                                                
PMOA     DS    CL6       (73-78)   MOA                                          
         DS    CL2       (79-80)                                                
PDEBITS  DS    CL14      (81-94)   DR                                           
         ORG   PDEBITS                                                          
PCOUNT   DS    CL14                RECORD COUNT                                 
         DS    CL2       (95-96)                                                
PCREDITS DS    CL14      (97-110)  CR                                           
         DS    CL2       (111-112)                                              
PINCAMT  DS    CL14      (113-126)                                              
         DS    CL2       (127-128)                                              
PSKACCT  DS    CL14      (129-142)                                              
         DS    CL2       (143-144)                                              
PINPBAT  DS    CL6       (145-150)                                              
PDIFF    DS    CL1       (151)                                                  
PLNQ     EQU   *-PLINE                                                          
*                                                                               
PLINE2   DSECT                                                                  
         DS    CL9       (1-9)                                                  
PACCT    DS    CL14      (10-23)                                                
         DS    CL2       (24-25)                                                
PCNTR    DS    CL14      (26-39)                                                
         DS    CL2       (40-41)                                                
PBDTE    DS    CL8       (42-49)                                                
         DS    CL2       (50-51)                                                
PREF2    DS    CL6       (52-57)                                                
         DS    CL2       (58-59)                                                
PBREF2   DS    CL6       (60-65)                                                
         DS    CL2       (66-67)                                                
POFF     DS    CL2       (68-69)                                                
         DS    CL11      (70-80)                                                
PDRAMT   DS    CL14      (81-94)                                                
         DS    CL2       (95-96)                                                
PCRAMT   DS    CL14      (97-100)                                               
         DS    CL2       (101-102)                                              
PCNAME   DS    CL36      (103-138)                                              
         DS    CL2       (139-140)                                              
PSALEAC  DS    CL14      (141-154)                                              
*                                                                               
ACXYD    DSECT                                                                  
SVRE     DS    F                                                                
COSTING  DS    CL1                                                              
COMMAND  DS    CL6                                                              
ELCODE   DS    XL1                                                              
ELM      DS    CL255                                                            
SJJOB    DS    XL15                                                             
COSTKEY  DS    XL15                                                             
COSTNAME DS    CL36                                                             
SKKEY    DS    XL15                                                             
         ORG   SKKEY                                                            
SKCPY    DS    XL1                                                              
SKULA    DS    CL14                                                             
REV12    DS    0CL15                                                            
REV12CPY DS    XL1                                                              
REV12UL  DS    CL2                                                              
REV12ACT DS    CL12                                                             
SALEKEY  DS    CL15                                                             
REV12NME DS    CL36                                                             
PRDNAME  DS    CL36                                                             
JOBNAME  DS    CL36                                                             
SALENAME DS    CL36                                                             
REVNAME  DS    CL36                                                             
OFF2POST DS    CL2                                                              
*                                                                               
TREF     DS    CL6                 ORIGINAL REFERENCE                           
TDATE    DS    XL3                 ORIGINAL DATE                                
TBATREF  DS    CL6                 ORIGINAL BATCH REF                           
*                                                                               
BATCHMOS DS    XL2                 PACKED FORMAT                                
BATCHREF DS    0CL6                                                             
BATCHCMO DS    CL2                 CHARACTER FORMAT                             
BATCHCDE DS    CL4                                                              
BILLNUM  DS    CL6                                                              
BILLDATE DS    XL3                                                              
BILLCMPS DS    XL2                 COMPRESSED BILL DATE                         
INCAMT   DS    PL6                 SK AMOUNT WANTED                             
NAMEWORK DS    CL36                                                             
*                                                                               
POSTFLAG DS    XL1                                                              
POSTOPEN EQU   X'80'                                                            
POSTADD  EQU   X'40'                                                            
REPORTSW DS    CL1                                                              
NEWOFF   DS    CL1                                                              
HIT      DS    XL1                                                              
HITSJ    EQU   X'80'                                                            
HIT1C    EQU   X'40'                                                            
HITBATCH EQU   X'01'                                                            
TLEN     DS    F                   RECORD LENGTH                                
T        DS    CL400                                                            
         DS    CL500                                                            
AREA     DS    CL200                                                            
ID       DS    CL16                                                             
         EJECT                                                                  
*  ACREPWORKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
         PRINT ON                                                               
*  ACGENFILE                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
*  ACGENPOST                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACGENPOST                                                      
         PRINT ON                                                               
*  ACGENMODES                                                                   
         PRINT OFF                                                              
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
*  ACGENPOST                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBIGPRNTD                                                     
         PRINT ON                                                               
*  ACGENPOST                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDBIGBOX                                                       
         PRINT ON                                                               
*  DMDTFIS                                                                      
         PRINT OFF                                                              
       ++INCLUDE DMDTFIS                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'114ACREPXY02A05/01/02'                                      
         END                                                                    
