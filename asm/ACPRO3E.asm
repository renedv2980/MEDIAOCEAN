*          DATA SET ACPRO3E    AT LEVEL 064 AS OF 09/12/02                      
*PHASE T60B3EA,*                                                                
         TITLE 'T60B3E - PRICE MAINTENANCE'                                     
T60B3E   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 (MYDEND-MYD),T60B3E**,CLEAR=YES                                  
         LR    R4,RC                                                            
         USING MYD,R4                                                           
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASUBSYSD                                                      
         USING SUBSYSD,R9                                                       
         L     RA,ATWA                                                          
         USING CONHEADH-64,RA                                                   
*                                                                               
         CLI   MODE,VALKEY                                                      
         BNE   MODE2                                                            
         BAS   RE,VKEY                                                          
         B     XIT                                                              
*                                                                               
MODE2    CLI   MODE,VALREC                                                      
         BNE   MODE4                                                            
         BAS   RE,VREC                                                          
         BAS   RE,DKEY                                                          
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE4    CLI   MODE,DISPKEY                                                     
         BNE   MODE6                                                            
         BAS   RE,DKEY                                                          
         B     XIT                                                              
*                                                                               
MODE6    CLI   MODE,DISPREC                                                     
         BNE   MODE8                                                            
         BAS   RE,DREC                                                          
         B     XIT                                                              
*                                                                               
MODE8    CLI   MODE,RECREST                                                     
         BNE   XIT                                                              
         BAS   RE,DREC                                                          
         B     XIT                                                              
         EJECT                                                                  
*              VALIDATE KEY FIELDS                                              
*                                                                               
         USING PRCRECD,R6                                                       
VKEY     NTR1                                                                   
         LA    R6,USERKEY                                                       
         XC    PRCKEY,PRCKEY                                                    
         MVI   PRCKTYP,PRCKTYPQ                                                 
         MVI   PRCKSUB,PRCKSUBQ                                                 
         MVC   PRCKCPY(3),CUL                                                   
*                                                                               
         LA    R2,PRCOFGH          OFFICE GROUP                                 
         CLI   5(R2),0                                                          
         BE    VKEY2                                                            
         GOTO1 VALOG                                                            
         MVC   PRCKOFG,EFFOFG                                                   
*                                                                               
VKEY2    LA    R2,PRCOFFH          OFFICE                                       
         CLI   5(R2),0                                                          
         BE    VKEY4                                                            
         MVI   ERROR,NOTOFNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PRCOFGH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALOFF                                                           
         MVC   PRCKOFC,EFFOFFC                                                  
*                                                                               
VKEY4    LA    R2,PRCCLIH          CLIENT                                       
         CLI   5(R2),0                                                          
         BE    VKEY6                                                            
         MVI   ERROR,NOTCLNOG      NOT COMPATIBLE WITH OFFICE GROUP             
         CLI   PRCOFGH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTCLNOF      NOT COMPATIBLE WITH OFFICE                   
         CLI   PRCOFFH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALCLI                                                           
         MVC   PRCKCLI,CLICODE                                                  
*                                                                               
VKEY6    LA    R2,PRCPROH          PRODUCT                                      
         CLI   5(R2),0                                                          
         BE    VKEY8                                                            
         MVI   ERROR,NEEDCLI       IF INPUT, NEED CLIENT AS WELL                
         CLI   PRCCLIH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALPROD                                                          
         MVC   PRCKPRO,PRODCODE                                                 
*                                                                               
VKEY8    LA    R2,PRCJOBH          JOB                                          
         CLI   5(R2),0                                                          
         BE    VKEY10                                                           
         MVI   ERROR,NEEDPRO       IF INPUT, NEED PRODUCT AS WELL               
         CLI   PRCPROH+5,0                                                      
         BE    ERREND                                                           
         GOTO1 VALJOB                                                           
         MVC   PRCKJOB,JOBNUM                                                   
*                                                                               
VKEY10   LA    R2,PRCMGRH          MEDIA GROUP                                  
         CLI   5(R2),0                                                          
         BE    VKEY12                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PRCJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMG                                                            
         MVC   PRCKMGR,MGROUP                                                   
*                                                                               
VKEY12   LA    R2,PRCMEDH          MEDIA                                        
         CLI   5(R2),0                                                          
         BE    VKEY14                                                           
         MVI   ERROR,NOTMENMG      NOT COMPATIBLE WITH MEDIA GROUP              
         CLI   PRCMGRH+5,0                                                      
         BNE   ERREND                                                           
         MVI   ERROR,NOTJBNME      NOT COMPATIBLE WITH JOB                      
         CLI   PRCJOBH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALMED                                                           
         MVC   PRCKMED,MEDIA                                                    
*                                                                               
VKEY14   LA    R2,PRCWGRH          WORK GROUP                                   
         CLI   5(R2),0                                                          
         BE    VKEY16                                                           
         GOTO1 VALWG                                                            
         MVC   PRCKWGR,WGROUP                                                   
*                                                                               
VKEY16   LA    R2,PRCWRKH          WORK CODE                                    
         CLI   5(R2),0                                                          
         BE    VKEY18                                                           
         MVI   ERROR,NOTWKNWG      NOT COMPATIBLE WITH WORK GROUP               
         CLI   PRCWGRH+5,0                                                      
         BNE   ERREND                                                           
         GOTO1 VALWORK                                                          
         MVC   PRCKWRK,WORKCODE                                                 
         SPACE 1                                                                
VKEY18   LA    R2,PRCEFFH                                                       
         GOTO1 ANY                                                              
         CLI   ACTNUM,ACTDEL       DELETE MUST SPECIFY DATE                     
         BE    VKEY24                                                           
         GOTO1 DATCON,DMCB,(5,0),(1,TODAY)                                      
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=CL5'TODAY'                                              
         BNE   VKEY20                                                           
         MVC   EFFDATE,TODAY                                                    
         B     VKEY26                                                           
*                                                                               
VKEY20   CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BE    VKEY24              YES                                          
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=CL4'LAST'                                               
         BNE   VKEY22                                                           
         MVC   EFFDATE,EFFS                                                     
         B     VKEY26                                                           
*                                                                               
VKEY22   EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   WORK(0),=CL8'PREVIOUS'                                           
         BNE   VKEY24                                                           
         ICM   RE,7,EFFDATE                                                     
         BCTR  RE,0                                                             
         STCM  RE,7,EFFDATE                                                     
         B     VKEY26                                                           
*                                                                               
VKEY24   MVI   ERROR,INVDATE                                                    
         GOTO1 DATVAL,DMCB,WORK,DUB                                             
         OC    DMCB,DMCB                                                        
         BZ    ERREND                                                           
         GOTO1 DATCON,DMCB,DUB,(1,EFFDATE)                                      
*                                                                               
VKEY26   MVC   PRCKEFF,EFFDATE                                                  
         XC    PRCKEFF,EFFS                                                     
         MVC   KEY,USERKEY         MOVE THE KEY IN                              
*                                                                               
         CLI   ACTNUM,ACTADD       ARE WE ADDING ?                              
         BNE   VKEY30              YES                                          
         ZIC   R3,PRCBASH+5                                                     
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   PRCBAS(0),=C'HOURS'                                              
         BNE   VKEY28                                                           
         MVC   PRCBAS(5),=C'HOURS'                                              
         OI    PRCBASH+6,X'80'                                                  
         B     VKEYX                                                            
*                                                                               
VKEY28   MVC   PRCBAS,=C'UNITS'                                                 
         OI    PRCBASH+6,X'80'                                                  
         B     VKEYX                                                            
*                                                                               
VKEY30   CLI   ACTNUM,ACTREST                                                   
         BE    VKEYX                                                            
         CLI   ACTNUM,ACTDEL                                                    
         BE    VKEYX                                                            
         GOTO1 HIGH                                                             
         L     R6,AIO                                                           
         MVI   ERROR,NOTFOUND                                                   
         CLC   PRCKEY(PRCKEFF-PRCKEY),KEYSAVE                                   
         BNE   ERREND                                                           
         MVC   EFFDATE,PRCKEFF                                                  
         XC    EFFDATE,EFFS                                                     
         XC    PRCEFF,PRCEFF                                                    
         GOTO1 DATCON,DMCB,(1,EFFDATE),(8,PRCEFF)                               
         OI    PRCEFFH+6,X'80'                                                  
*                                                                               
VKEYX    B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY KEY                                                      
*                                                                               
         USING PRCRECD,R6                                                       
DKEY     NTR1                                                                   
         L     R6,AIO                                                           
         MVC   PRCOFG,PRCKOFG                                                   
         OI    PRCOFGH+6,X'80'                                                  
         MVC   PRCOFF,PRCKOFC                                                   
         OI    PRCOFFH+6,X'80'                                                  
         MVC   PRCCLI,PRCKCLI                                                   
         OI    PRCCLIH+6,X'80'                                                  
         MVC   PRCPRO,PRCKPRO                                                   
         OI    PRCPROH+6,X'80'                                                  
         MVC   PRCJOB,PRCKJOB                                                   
         OI    PRCJOBH+6,X'80'                                                  
         MVC   PRCMGR,PRCKMGR                                                   
         OI    PRCMGRH+6,X'80'                                                  
         MVC   PRCMED,PRCKMED                                                   
         OI    PRCMEDH+6,X'80'                                                  
         MVC   PRCWGR,PRCKWGR                                                   
         OI    PRCWGRH+6,X'80'                                                  
         MVC   PRCWRK,PRCKWRK                                                   
         OI    PRCWRKH+6,X'80'                                                  
         MVC   EFFDATE,PRCKEFF                                                  
         XC    EFFDATE,EFFS                                                     
         GOTO1 DATCON,DMCB,(1,EFFDATE),(8,PRCEFF)                               
         OI    PRCEFFH+6,X'80'                                                  
         B     XIT                                                              
         DROP  R6                                                               
         EJECT                                                                  
*              DISPLAY RECORD                                                   
*                                                                               
DREC     NTR1                                                                   
         USING PRCRECD,R6                                                       
         L     R6,AIO                                                           
         GOTO1 PERSOUT                                                          
         MVC   PRCLACT,SPACES                                                   
         MVC   PRCLACT(14),=C'LAST ACTIVITY:'                                   
         MVC   PRCLACT+15(20),WORK+20                                           
         OI    PRCLACTH+6,X'80'                                                 
         GOTO1 VCLEARF,DMCB,PRCLACTH,PRCLAST                                    
         MVI   PCSTAT,0                                                         
         MVC   PRCBAS,=C'UNITS'                                                 
         TM    PRCRSTA,PRCSQTRH                                                 
         BZ    DREC01                                                           
         MVC   PRCBAS,=C'HOURS'                                                 
         OI    PRCBASH+6,X'80'                                                  
         OI    PCSTAT,PCHOURS                                                   
*                                                                               
         USING PRCELD,R6                                                        
DREC01   MVI   ELCODE,PRCELQ                                                    
         BAS   RE,GETELIO                                                       
         BNE   XIT                                                              
*                                                                               
         USING LND,R2                                                           
         LA    R2,PRCSUNH          START QUANTITY                               
*                                                                               
DREC02   DS    0H                                                               
         TM    PCSTAT,PCHOURS                                                   
         BO    DREC05                                                           
         EDIT  PRCSUNT,(5,LNDSUNT),0,ALIGN=LEFT,ZERO=NOBLANK                    
         EDIT  PRCEUNT,(5,LNDEUNT),0,ALIGN=LEFT                                 
         B     DREC07                                                           
DREC05   EDIT  PRCSUNT,(6,LNDSUNT),2,ALIGN=LEFT,ZERO=NOBLANK                    
         EDIT  PRCEUNT,(6,LNDEUNT),2,ALIGN=LEFT                                 
*                                                                               
DREC07   EDIT  PRCPRC1,(8,LNDPRC1),2,ALIGN=RIGHT                                
         EDIT  PRCPRC2,(8,LNDPRC2),2,ALIGN=RIGHT                                
         EDIT  PRCPRC3,(8,LNDPRC3),2,ALIGN=RIGHT                                
         EDIT  PRCPRC4,(8,LNDPRC4),2,ALIGN=RIGHT                                
         LA    R2,LNDLNQ(R2)                                                    
         BAS   RE,NEXTEL                                                        
         BE    DREC02                                                           
         B     XIT                                                              
         DROP  R6,R2                                                            
         EJECT                                                                  
*              VALIDATE RECORD                                                  
*                                                                               
VREC     NTR1                                                                   
*                                                                               
         USING PRCRECD,R6                                                       
         L     R6,AIO                                                           
         MVI   PCSTAT,0                                                         
         NI    PRCRSTA,X'FF'-PRCSQTRH                                           
         MVI   ERROR,INVALID                                                    
         LA    R2,PRCBASH                                                       
         CLI   5(R2),0                                                          
         BE    VREC01A                                                          
         ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   PRCBAS(0),=C'UNITS'                                              
         BNE   VREC00                                                           
         MVC   PRCBAS(5),=C'UNITS'                                              
         B     VREC01                                                           
*                                                                               
VREC00   ZIC   R3,5(R2)                                                         
         BCTR  R3,0                                                             
         EX    R3,*+8                                                           
         B     *+10                                                             
         CLC   PRCBAS(0),=C'HOURS'                                              
         BNE   ERREND                                                           
         MVC   PRCBAS(5),=C'HOURS'                                              
         OI    PRCRSTA,PRCSQTRH                                                 
         OI    PCSTAT,PCHOURS                                                   
*                                                                               
VREC01   OI    6(R2),X'80'                                                      
         MVI   ERROR,X'FF'                                                      
         DROP  R6                                                               
*                                                                               
         USING LND,R5                                                           
         USING PRCELD,R6                                                        
VREC01A  MVI   ELCODE,PRCELQ       REMOVE EXISTING ELEMENTS                     
         GOTO1 REMELEM                                                          
         LA    RE,UNTTAB           CLEAR ELEMENT TABLE                          
         LA    RF,UNTTABL                                                       
         SR    R1,R1                                                            
         MVCL  RE,R0                                                            
         MVI   UNTCNT,0            CLEAR COUNTER                                
*                                                                               
         LA    R0,MAXNUM           NUMBER OF SCREEN LINES                       
         LA    R5,PRCSUNH          FIRST UNIT FIELD                             
*                                                                               
VREC02   LA    R6,ELEMENT                                                       
         XC    ELEMENT,ELEMENT     FORMAT ELEMENT                               
         MVI   PRCEL,PRCELQ                                                     
         MVI   PRCLN,PRCLNQ                                                     
         MVC   PRCPRC1(ZEROL),ZEROS                                             
*                                                                               
         LA    R2,LNDSUNTH                                                      
         CLI   5(R2),0             ANYTHING ON THIS LINE?                       
         BNE   VREC04              YES                                          
         BAS   RE,CHKZERO          NO, MAKE SURE ALL FIELDS BLANK               
         B     VREC12                                                           
*                                                                               
VREC04   GOTO1 ANY                 YES, VALIDATE QUANTITIES                     
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         TM    PCSTAT,PCHOURS                                                   
         BO    VREC05                                                           
         GOTO1 CASHVAL,DMCB,(C'0',8(R2)),(RF)                                   
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BNE   ERREND                                                           
         ZAP   PRCSUNT,9(3,R1)     SAVE IN ELEMENT                              
         BNP   ERREND              ZERO IS INVALID                              
         B     VREC05B                                                          
*                                                                               
VREC05   GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(RF)                                  
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BNE   ERREND                                                           
         ZAP   PRCSUNT,9(3,R1)     SAVE IN ELEMENT                              
         BNP   ERREND              ZERO IS INVALID                              
         ZAP   DUB,PRCSUNT                                                      
         DP    DUB,=PL2'25'                                                     
         CP    DUB+6(2),=P'0'                                                   
         BH    ERREND                                                           
*                                                                               
VREC05B  LA    R2,LNDEUNTH                                                      
         GOTO1 ANY                                                              
         SR    RF,RF                                                            
         IC    RF,5(R2)                                                         
         TM    PCSTAT,PCHOURS                                                   
         BO    VREC05G                                                          
         GOTO1 CASHVAL,DMCB,(C'0',8(R2)),(RF)                                   
         CLI   DMCB,0                                                           
         BNE   ERREND                                                           
         ZAP   PRCEUNT,9(3,R1)     SAVE IN ELEMENT                              
         BNP   ERREND              ZERO IS INVALID                              
         B     VREC05J                                                          
*                                                                               
VREC05G  GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(RF)                                  
         MVI   ERROR,INVALID                                                    
         CLI   DMCB,0                                                           
         BNE   ERREND                                                           
         ZAP   PRCEUNT,9(3,R1)     SAVE IN ELEMENT                              
         BNP   ERREND              ZERO IS INVALID                              
         ZAP   DUB,PRCEUNT                                                      
         DP    DUB,=PL2'25'                                                     
         CP    DUB+6(2),=P'0'                                                   
         BH    ERREND                                                           
*                                                                               
VREC05J  CP    PRCEUNT,PRCSUNT     END MUST BE GREATER THAN START               
         BL    HIGHERR                                                          
*                                                                               
         LA    R2,LNDPRC1H                                                      
         LA    R3,4                NUMBER OF PRICES                             
         LA    R6,PRCPRC1                                                       
*                                                                               
VREC06   SR    RF,RF               VALIDATE AMOUNTS                             
         ICM   RF,1,5(R2)                                                       
         BZ    VREC08              NO PRICE HERE                                
         GOTO1 CASHVAL,DMCB,(X'82',8(R2)),(RF)                                  
         CLI   DMCB,0                                                           
         BNE   ERREND                                                           
         ZAP   0(4,R6),8(4,R1)     SAVE IN ELEMENT                              
         BM    ERREND              NEGATIVE AMOUNT IS INVALID                   
*                                                                               
VREC08   BAS   RE,BUMP             GET NEXT PRICE ON SCREEN                     
         LA    R6,L'PRCPRC1(R6)    GET NEXT PRICE IN RECORD                     
         BCT   R3,VREC06                                                        
*                                                                               
         LA    R6,ELEMENT          GET BACK TO START OF ELEMENT                 
         CLC   PRCPRC1(ZEROL),ZEROS                                             
         BNE   VREC10              MUST HAVE A LEAST ONE PRICE                  
         LA    R2,LNDPRC1H                                                      
         MVI   ERROR,MISSING                                                    
         B     ERREND                                                           
*                                                                               
VREC10   BAS   RE,INSERT           INSERT DATA IN ELEMENT TABLE                 
         BNE   OVERERR             OVERLAP ERROR                                
*                                                                               
VREC12   LA    R5,LNDLNQ(R5)       GET NEXT SCREEN LINE                         
         BCT   R0,VREC02                                                        
*                                                                               
         USING PRCELD,R6                                                        
         LA    R0,MAXNUM           MAXIMUM NUMBER OF ENTRIES                    
         LA    R5,UNTTAB                                                        
         LA    R6,ELEMENT                                                       
         LA    R7,0                SEQUENCE NUMBER                              
*                                                                               
VREC14   OC    0(PRCLNQ,R5),0(R5)  ANYTHING TO ADD?                             
         BZ    VREC30              NO, ALL DONE                                 
         XC    ELEMENT,ELEMENT     YES, CLEAR OUT ELEMENT                       
         MVC   ELEMENT(PRCLNQ),0(R5)                                            
         LA    R7,1(R7)                                                         
         STC   R7,PRCSEQ           ADD A SEQUENCE NUMBER                        
         GOTO1 ADDELEM                                                          
         LA    R5,PRCLNQ(R5)       GET NEXT TABLE ENTRY                         
         BCT   R0,VREC14           KEEP ADDING                                  
*                                                                               
VREC30   DS    0H                                                               
         CLI   UNTCNT,0                                                         
         BH    VREC35                                                           
         MVI   ERROR,MISSING                                                    
         LA    R2,PRCSUNH                                                       
         B     ERREND                                                           
*                                                                               
VREC35   GOTO1 PERSIN                                                           
         OI    GENSTAT2,RETEQSEL                                                
         DROP  R5,R6                                                            
         TM    PCSTAT,PCHOURS                                                   
         BZ    XIT                                                              
         BAS   RE,GAPCK                                                         
         B     XIT                                                              
         EJECT                                                                  
         USING PRCELD,R5                                                        
INSERT   NTR1                                                                   
         LA    R0,MAXNUM                                                        
         LA    R5,UNTTAB           TABLE OF ELEMENT DATA                        
         LA    R6,ELEMENT                                                       
*                                                                               
INST02   OC    PRCSUNT,PRCSUNT     EMPTY SPACE?                                 
         BZ    INSTMVC             YES, MOVE ELEMENT IN                         
*                                                                               
         CP    3(3,R6),PRCSUNT     RECORD START > TABLE START?                  
         BL    INST04              NO, CHECK RECORD END                         
         BE    INSTNG              NO, ERROR IF EQUAL                           
         CP    3(3,R6),PRCEUNT     YES, RECORD START > TABLE END ?              
         BNH   INSTNG              NO, ERROR                                    
         LA    R5,PRCLNQ(R5)       YES, LOOK AT NEXT TABLE ENTRY                
         BCT   R0,INST02                                                        
         B     INSTNG                                                           
*                                                                               
INST04   CP    6(3,R6),PRCSUNT     RECORD END < TABLE START?                    
         BNL   INSTNG              NO, OVERLAP                                  
         BAS   RE,SHIFT            YES, SHIFT EVERYTHING DOWN                   
*                                                                               
INSTMVC  MVC   PRCEL(PRCLNQ),ELEMENT                                            
         ZIC   RF,UNTCNT                                                        
         LA    RF,1(RF)                                                         
         STC   RF,UNTCNT                                                        
*                                                                               
INSTOK   CR    R8,R8                                                            
         B     INSTXIT                                                          
*                                                                               
INSTNG   LTR   R8,R8                                                            
*                                                                               
INSTXIT  B     XIT                                                              
         DROP  R5                                                               
         EJECT                                                                  
         USING LND,R2                                                           
CHKZERO  CLI   LNDEUNTH+5,0        MAKE SURE ALL FIELDS ARE ZERO                
         BNE   CHKZNG                                                           
         CLI   LNDPRC1H+5,0                                                     
         BNE   CHKZNG                                                           
         CLI   LNDPRC2H+5,0                                                     
         BNE   CHKZNG                                                           
         CLI   LNDPRC3H+5,0                                                     
         BNE   CHKZNG                                                           
         CLI   LNDPRC4H+5,0                                                     
         BER   RE                                                               
*                                                                               
CHKZNG   MVI   ERROR,MISSING       IF NOT, WE NEED A QUANTITY                   
         B     ERREND                                                           
         EJECT                                                                  
SHIFT    LA    R6,UNTTABE          END OF TABLE                                 
         CR    R5,R6                                                            
         BNE   *+6                                                              
         DC    H'0'                SHOULD NEVER HAVE MORE THAN 13               
         LR    R3,R6                                                            
*                                                                               
SHIFT2   SH    R3,=YL2(PRCLNQ)                                                  
         CR    R3,R5                                                            
         BLR   RE                                                               
*                                                                               
         MVC   0(PRCLNQ,R6),0(R3)  MOVE DATA DOWN A LINE                        
         SH    R6,=YL2(PRCLNQ)                                                  
         B     SHIFT2                                                           
         EJECT                                                                  
*                                                                               
         USING PRCELD,R6                                                        
G        USING PRCELD,R7                                                        
GAPCK    NTR1                                                                   
         LA    R0,MAXNUM                                                        
         BCTR  R0,0                                                             
         LA    R6,UNTTAB                                                        
GAP10    OC    0(PRCLNQ,R6),0(R6)                                               
         BZ    GAPXIT                                                           
         LA    R7,PRCLNQ(R6)                                                    
         OC    0(PRCLNQ,R7),0(R7)                                               
         BZ    GAPXIT                                                           
         ZAP   FULL,PRCEUNT                                                     
         AP    FULL,=P'25'                                                      
         CP    FULL,G.PRCSUNT                                                   
         BL    GAPERR                                                           
         LA    R6,PRCLNQ(R6)                                                    
         BCT   R0,GAP10                                                         
GAPXIT   B     XIT                                                              
*                                                                               
GAPERR   MVC   CONHEAD,SPACES                                                   
         MVC   CONHEAD(44),=C'** WARNING - THERE ARE GAPS BETWEEN HOURSX        
                **'                                                             
         OI    GENSTAT2,USMYOK                                                  
         OI    CONHEADH+1,X'08'                                                 
         MVI   ERROR,X'FF'                                                      
*                                                                               
         LA    R7,MAXNUM                                                        
         SR    R7,R0                                                            
         LA    R2,PRCSUNH                                                       
GAP50    LA    R2,LNDLNQ(R2)                                                    
         BCT   R7,GAP50                                                         
         ST    R2,ACURFORC                                                      
         B     XIT                                                              
         DROP  R6,G                                                             
         EJECT                                                                  
BUMP     SR    RF,RF                                                            
         IC    RF,0(R2)            BUMP TO NEXT FIELD                           
         AR    R2,RF                                                            
         BR    RE                                                               
*                                                                               
GETELIO  L     R6,AIO                                                           
         GETEL (R6),DATADISP,ELCODE                                             
*                                                                               
EFFS     DC    8X'FF'                                                           
*                                                                               
ZEROS    DC    4PL4'0'                                                          
ZEROL    EQU   *-ZEROS                                                          
*                                                                               
HIGHMSG  DC    CL60'END UNITS MUST BE EQUAL TO OR GREATER THAN START UNX        
               ITS'                                                             
*                                                                               
OVERMSG  DC    CL60'THIS ENTRY OVERLAPS A PREVIOUS ENTRY'                       
*                                                                               
OVERERR  MVC   CONHEAD(L'OVERMSG),OVERMSG                                       
         LR    R2,R5               SET CURSOR                                   
         B     MYEND                                                            
*                                                                               
HIGHERR  MVC   CONHEAD(L'HIGHMSG),HIGHMSG                                       
*                                                                               
MYEND    OI    GENSTAT2,USMYOK     USING MY OWN ERROR MSG                       
         OI    CONHEADH+1,X'08'                                                 
         MVI   ERROR,X'FE'                                                      
*                                                                               
ERREND   GOTO1 VERRCUR                                                          
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
         EJECT                                                                  
MYD      DSECT                                                                  
UNTTAB   DS    CL(PRCLNQ*MAXNUM)   TABLE OF ELEMENTS                            
UNTTABL  EQU   *-UNTTAB            LENGTH OF TABLE                              
UNTTABE  EQU   *-PRCLNQ            ADDRESS OF LAST TABLE ENTRY                  
UNTCNT   DS    X                   NUMBER OF PRESENT ENTRIES                    
MAXNUM   EQU   13                  MAXIMUM NUMBER OF LINES ON SCREEN            
MYDEND   EQU   *                                                                
         EJECT                                                                  
LND      DSECT                                                                  
LNDSUNTH DS    CL8                                                              
LNDSUNT  DS    CL6                                                              
LNDEUNTH DS    CL8                                                              
LNDEUNT  DS    CL6                                                              
LNDPRC1H DS    CL8                                                              
LNDPRC1  DS    CL8                                                              
LNDPRC2H DS    CL8                                                              
LNDPRC2  DS    CL8                                                              
LNDPRC3H DS    CL8                                                              
LNDPRC3  DS    CL8                                                              
LNDPRC4H DS    CL8                                                              
LNDPRC4  DS    CL8                                                              
LNDLNQ   EQU   *-LND                                                            
         EJECT                                                                  
*ACPROWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE ACPROWORKD                                                     
         PRINT ON                                                               
*DDSPOOLD                                                                       
         PRINT OFF                                                              
       ++INCLUDE DDSPOOLD                                                       
         PRINT ON                                                               
*DDSPLWORKD                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDSPLWORKD                                                     
         PRINT ON                                                               
*ACGENBOTH                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
         PRINT ON                                                               
*ACGENFILE                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENFILE                                                      
         PRINT ON                                                               
T60BFFD  DSECT                                                                  
         ORG   CONTAGH                                                          
         EJECT                                                                  
       ++INCLUDE ACPROCED                                                       
TODAY    DS    CL3                                                              
EFFDATE  DS    PL3                                                              
USERKEY  DS    CL48                                                             
*                                                                               
PCSTAT   DC    XL1'00'                                                          
PCHOURS  EQU   X'80'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064ACPRO3E   09/12/02'                                      
         END                                                                    
