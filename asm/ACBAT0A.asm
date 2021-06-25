*          DATA SET ACBAT0A    AT LEVEL 048 AS OF 03/19/20                      
*PHASE T61B0AA                                                                  
*INCLUDE ACSRCHC                                                                
*INCLUDE CONVERT                                                                
*INCLUDE SRCHCALL                                                               
T61B0A   TITLE 'TYPE 10- MULTIPLE BILLABLE/NON-BILLABLE EXPENSE ENTRY'          
***********************************************************************         
*DEV  LVL DATE    TICKET       COMMENTS                               *         
*---- --- ------- ------------ ---------------------------------------*         
*JSAY 048 18MAR20 <DSRD-24576> UPDATE ORDER STATUS                    *         
***********************************************************************         
T61B0A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 PROGDX-PROGD,T61B0A,RR=R5,CLEAR=YES                              
         USING TWAD,RA                                                          
         USING WORKD,R9                                                         
         USING PROGD,RC                                                         
         ST    R5,RELOA                                                         
         ST    RB,BASERB                                                        
         L     RF,=V(CONVERT)                                                   
         A     RF,RELOA                                                         
         ST    RF,VCNVERT                                                       
         L     RF,=V(ACSRCHC)                                                   
         A     RF,RELOA                                                         
         ST    RF,VSEARCH                                                       
*                                                                               
         LHI   R1,PRVTAB-T61B0A                                                 
         A     R1,BASERB                                                        
         ST    R1,APRVTAB                                                       
         LA    R1,PROGD                                                         
         AHI   R1,EXCBLK-PROGD                                                  
         ST    R1,AEXCELD          SET POINTER TO EXCEL BLOCK                   
         LA    R1,PROGD                                                         
         AHI   R1,CATBLK-PROGD                                                  
         ST    R1,ACATD            SET POINTER TO CATCALL BLOCK                 
         LA    R1,PROGD                                                         
         AHI   R1,RECIOA-PROGD                                                  
         ST    R1,ARECIOA          SET ADDRESS OF TSAR RECORDS                  
         LA    R1,PROGD                                                         
         AHI   R1,RECOLD-PROGD                                                  
         ST    R1,ARECOLD                                                       
         LA    R1,PROGD                                                         
         AHI   R1,TAXIT-PROGD                                                   
         ST    R1,ATAXIT                                                        
         L     RF,ACOMFACS                                                      
         MVC   AXTRAINF,CXTRAINF-COMFACSD(RF)                                   
*                                                                               
         L     R7,ARECIOA                                                       
         USING RECSD,R7                                                         
*                                                                               
         LA    RF,ADTAB            RESOLVE TABLE ADDRESS                        
MUL3     XR    R1,R1                                                            
         ICM   R1,3,0(RF)          R1=DISPLACEMENT TO TABLE                     
         AR    R1,RB               R1=A(TABLE)                                  
         XR    R2,R2                                                            
         ICM   R2,3,2(RF)          R2=DISPLACEMENT INTO W/S                     
         LA    R2,PROGD(R2)        R2=LOCATION IN W/S                           
         STCM  R1,15,0(R2)         SAVE ADDRESS OF TABLE                        
         LA    RF,L'ADTAB(RF)                                                   
         CLI   0(RF),X'FF'                                                      
         BNE   MUL3                                                             
*&&DO                                                                           
*********************************************************************           
* TEMP CODE UNTIL TYPE 10 LIVE ON ADV FOR CANADA                    *           
*********************************************************************           
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN AGENCY                         
         BNE   TEMPX               NO,                                          
         L     RF,AXTRAINF                                                      
         USING XTRAINFD,RF                                                      
         TM    XIFLAG1,XITSTADV    TEST CONNECTED TO TST                        
         BO    TEMPX               YES - OK TO PROCESS                          
         LA    R2,BASSRVH                                                       
         ST    R2,FVADDR                                                        
         LHI   R0,AE$SECLK         'SECURITY LOCKOUT'                           
         STCM  R0,3,FVMSGNO                                                     
         J     EXIT57                                                           
TEMPX    DS    0H                                                               
         DROP  RF                                                               
*********************************************************************           
*&&                                                                             
         LA    R3,CSLSTCUR         SAVE CURRENT BATCH REFERENCE                 
         USING LSTTABD,R3                                                       
         MVC   CBATREF,LSTBREFN                                                 
         SR    R0,R0                                                            
         ICM   R0,3,LSTBITMA       ITEMS SO FAR                                 
         SR    RE,RE                                                            
         ICM   RE,3,LSTBDELI       LESS DELETED                                 
         SR    R0,RE                                                            
         ICM   RE,3,CSBMAXIT       MAX ALLOWED IN BATCH                         
         SR    RE,R0                                                            
         STCM  RE,3,MAXREM         MAX REMAINING AT ENTRY                       
         DROP  R3                                                               
*                                                                               
         LA    RF,TWAD             RESTORE SAVED VALUES                         
         AHI   RF,OSSAVE-TWAD                                                   
         MVC   STS(STSLNQ),0(RF)                                                
         CLI   TWATYPE,TWATYP10    TEST FIRST FOR TYPE10                        
         BNE   MUL4                YES                                          
         TM    PROCSW,PROCCLR      TEST FORCE CLEAR                             
         BO    MUL5                                                             
         CLC   SBATREF,CBATREF     TEST SAME BATCH                              
         BE    MUL6                                                             
*                                                                               
MUL4     MVI   FRSTSCRN,0                                                       
MUL5     XC    STS(STSLNQ),STS     CLEAR SAVED STORAGE                          
         MVI   TWATYPE,TWATYP10                                                 
         MVI   PROCSW,0                                                         
*                                                                               
MUL6     CLI   FRSTSCRN,0          TEST SAVED FIRST SCREEN                      
         BNE   *+10                YES,                                         
         MVC   FRSTSCRN,TWASCRN    SAVE FIRST SCREEN NUMBER                     
         MVC   SBATREF,CBATREF     SAVE BATCH REFERENCE                         
*                                                                               
         NI    BASRECH+1,X'FF'-(FATBPROT) UNPROTECT RECORD AND ACTION           
         NI    BASACTH+1,X'FF'-(FATBPROT)                                       
*                                                                               
         GOTO1 VDICTAT,DMCB,C'LL  ',ADCDDL,LDICT                                
*                                                                               
         BRAS  RE,SETSCR           SET SCREEN PARAMETERS                        
         MVI   PZERO,X'0C'         SET PACKED ZERO                              
         MVI   PNEG1,X'1D'         MINUS ONE                                    
         ZAP   ITMTAX,PZERO                                                     
         ZAP   ITMGST,PZERO                                                     
         ZAP   ITMPST,PZERO                                                     
         ZAP   ITMBASE,PZERO                                                    
         ZAP   MXAMT,=P'2000000000'    $20,000,000.00                           
         ZAP   MXBAT,=P'100000000000'   LIMIT 1 BILLION DOLLARS                 
         ZAP   MXNEG,=P'-9999999'       MAX NEG AMOUNT IF USING GST             
         MVC   WC99,=C'99'                                                      
         MVC   SXVND,=C'SX'                                                     
         NI    TWATSAR,X'FF'-(TWATSRES)  SET BUFFER NOT RESTORED                
         OI    PFKSW,PFKOK         SET ALLOW PF KEYS                            
*                                                                               
         TM    PROCSW,PROCNAR      HAS NARRATIVE BEEN ADDED                     
         BO    MUL7                YES                                          
         ZAP   TOTDLRS,PZERO       INITIALIZE TOTAL CASH                        
         ZAP   TAXDLRS,PZERO                                                    
         ZAP   PSTDLRS,PZERO                                                    
         ZAP   GSTBASE,PZERO                                                    
         ZAP   PSTBASE,PZERO                                                    
*                                                                               
         LHI   RF,NARLNQ           BUILD NARRATIVE RECORD                       
         STCM  RF,3,NARLN          LENGTH                                       
         LHI   RF,NARNUMQ                                                       
         STCM  RF,3,NARNUM         RECORD NUMBER(KEY)                           
         MVC   NARNAR1,SPACES      NARRATIVE 1                                  
         MVC   NARNAR2,SPACES      NARRATIVE 2                                  
         GOTOR TOTSAR,TSRINTQ      INITIALIZE                                   
         GOTOR TOTSAR,TSRADDQ      ADD A DUMMY NARRATIVE RECORD                 
*                                                                               
         LHI   RF,SAVDATAL         BUILD SAVDATA RECORD                         
         STCM  RF,3,SAVLN          LENGTH                                       
         LHI   RF,SAVNUMQ                                                       
         STCM  RF,3,SAVNUM         RECORD NUMBER(KEY)                           
         MVC   SAVREC,SPACES       SAVE DATA                                    
         GOTOR TOTSAR,TSRADDQ      ADD A DUMMY SAVDATA RECORD                   
         LHI   RF,SAVNUMQ                                                       
         STCM  RF,3,NREC                                                        
         OI    PROCSW,PROCNAR      SET NARRATIVE RECORD ADDED                   
*                                                                               
         BRAS  RE,GET2PL           GET VALUES TO 2P LEDGER                      
*                                                                               
MUL7     NI    PROCSW,X'FF'-(PROCDTL)                                           
         LHI   RF,NARNUMQ          GET DEFAULT NARRATIVE                        
         STCM  RF,3,NGET                                                        
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRGETQ      GET DEFAULT NARRATIVE RECORD                 
         MVC   DNARR1,NARNAR1      SAVE NARRATIVE                               
         MVC   DNARR2,NARNAR2                                                   
*                                                                               
         LHI   RF,SAVNUMQ          GET SAVDATA RECORD                           
         STCM  RF,3,NGET                                                        
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRGETQ      GET SAVED DATA RECORD                        
         MVC   SAVDATA(SAVDATAL),SAVREC   RESTORE CRITICAL DATA                 
*                                                                               
         BRAS  RE,SETUPR           SET UPPER SCREEN FIELD ADDRESSES             
*                                                                               
         L     R2,ATOTH                                                         
         MVC   FLD,SPACES                                                       
         BRAS  RE,MOVEFLD          CLEAR TOTAL FIELD                            
         TM    PROCSW,PROCLST      TEST LIST CONTROLLER MODE                    
         BNO   MUL9                                                             
         GOTOR LIST,DMCB,PROGD     LIST CONTROL                                 
         TM    PROCSW,PROCDTL      TEST PROCESS DETAIL                          
         BNO   *+8                                                              
         BAS   RE,EDT              CHECK THE DETAILS                            
         B     MUL23                                                            
*                                                                               
MUL9     TM    SCRSTA,SCRLISQ      TEST LIST SCREEN                             
         BO    MUL19               YES, NO CHANGES ALLOWED                      
         NI    PROCSW,X'FF'-PROCEDT TURNOFF EDIT IN PROGRESS                    
         BRAS  RE,SAVTOP           SAVE ANY CHANGED FIELDS                      
         BAS   RE,TSTNUM           SET NUMBER OF DETAIL LINES                   
         CLI   NITM,0              TEST ANY DETAIL                              
         BE    *+8                 NO,                                          
         OI    PROCSW,PROCEDT      SET EDIT IN PROGRESS                         
*                                                                               
         CLI   PFKEY,PFFISQ        TEST PF TO 'FIS'                             
         BE    MUL17                                                            
         TM    FLG,LASTDUP         TEST LAST MESSAGE WAS 'DUP INV'              
         BO    *+12                DON'T ALLOW SWAP                             
         CLI   PFKEY,PFSWAPQ       TEST PF 'SWAP KEY'                           
         BE    MUL17                                                            
         NI    FLG,X'FF'-(LASTDUP)                                              
*                                                                               
MUL11    GOTOR CLEAR,ACLRTAB       CLEAR NAME FIELDS                            
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         CLI   NITM,0              TEST ANY DETAILS                             
         BE    MUL17               YES, MUST EDIT                               
         BAS   RE,EDT              CHECK THE DETAILS                            
*                                                                               
MUL17    CLI   PFKEY,X'FF'                                                      
         BNE   *+8                                                              
         MVI   PFKEY,0                                                          
         CLI   PFKEY,0             TEST FOR ANY PFKEY                           
         BE    MUL25               NO                                           
*                                                                               
MUL19    BRAS  RE,VALPF                                                         
         TM    SCRSTA,SCRLISQ      TEST LIST SCREEN                             
         BNO   MUL20                                                            
         J     IMITEMS             'ITEM(S) XXX DISPLAYED...'                   
*                                                                               
MUL20    TM    PROCSW,PROCDTL      TEST PROCESS DETAIL                          
         BNO   *+8                                                              
         BAS   RE,EDT              EDIT AND POST                                
         TM    POSTSW,POSTNXT      TEST MORE POSTINGS                           
         BO    MUL19               YES, GO BACK                                 
*                                                                               
MUL23    TM    PFKSW,PFKPROC       PF KEY PROCESSD(UP/DOWN/SWAP) ?              
         BNO   MUL25                                                            
         ICM   R2,15,AINPH                                                      
         TM    PFKSW,PFKDELT                                                    
         JNO   IMITEMS             'ITEM(S) XXX DISPLAYED...'                   
         TM    SCRSTA,SCRSINQ      TEST SINGLE INPUT                            
         JO    IMITDEL                                                          
         SR    R1,R1                                                            
         ICM   R1,1,DITM           RELATIVE NUMBER OF DELETED ITEM              
         JZ    IMITDEL                                                          
         SR    RE,RE                                                            
         XR    RF,RF                                                            
         IC    RF,SCRLDL           LENGTH OF A LINE                             
         MR    RE,R1                                                            
         AR    R2,RF               GET CURSOR TO SAME LINE                      
         J     IMITDEL             OF DELETED ITEM                              
*                                                                               
MUL25    L     R2,AINPH                                                         
         TM    PROCSW,PROCEDT                                                   
         BNO   *+8                                                              
         L     R2,ADETH                                                         
         CLI   NITM,0              TEST ANY DETAILS                             
         JE    IMEREQF             'ENTER REQUIRED FIELDS...'                   
         TM    SCRSTA,SCRMINQ      TEST MULTIPLE INPUT                          
         JNO   MUL38               NO,                                          
*                                  GET TO FIRST BLANK LINE                      
         XR    R6,R6                                                            
         IC    R6,SCRMXD           R6=MAX LINES                                 
         L     R2,ADETH                                                         
         LHI   R0,MXCAP            TEST LAST ON SCREEN                          
         CLM   R0,3,NLST                                                        
         JNH   MUL35                                                            
*                                                                               
MUL31    BRAS  RE,SETLOW           SET ADCONS FOR LOWER                         
         XR    R0,R0                                                            
         LA    R3,LOWER                                                         
         LA    R5,NLOWER-1                                                      
*                                                                               
MUL33    ICM   RF,15,0(R3)                                                      
         BZ    MUL36                                                            
         TM    1(RF),FATBPROT      SKIP PROTECTED                               
         BO    MUL36                                                            
         CLI   5(RF),0             TEST FOR EMPTY FIELD                         
         BE    MUL36                                                            
         L     R2,ANEXT                                                         
         BCT   R6,MUL31                                                         
*                                                                               
MUL35    L     R2,ADETH            SCREEN IS FULL, CURSOR TO FIRST              
         J     MUL38                                                            
*                                                                               
MUL36    LA    R3,4(R3)                                                         
         BCT   R5,MUL33            BLANK LINE                                   
*                                                                               
MUL38    ZAP   BCDUB,TOTDLRS       TEST TOTAL DOLLARS                           
         AP    BCDUB,TAXDLRS                                                    
         AP    BCDUB,PSTDLRS                                                    
         OI    BCDUB+7,X'0F'                                                    
         CP    BCDUB,MXBAT          LIMIT 1 BILLION DOLLARS                     
         JL    IMIOKNX             'INPUT COMPLETE...'                          
         OI    POSTSW,POSTNOT      SET POSTING NOT ALLOWED                      
         L     R2,ADETH                                                         
         J     EMAMTHI             'AMOUNT TOO HIGH..'                          
*                                                                               
         EJECT                                                                  
**********************************************************************          
* TEST NUMBER OF DETAIL LINES WITH DATA                              *          
**********************************************************************          
TSTNUM   NTR1  ,                                                                
         LA    RF,MXCAP            GET MAX ALLOWED ON THIS SCREEN               
         AHI   RF,1                                                             
         XR    R0,R0                                                            
         ICM   R0,3,NFST                                                        
         SR    RF,R0                                                            
         STCM  RF,3,MAXBUF         MAX ON THIS SCREEN TO FILL BUFFER            
*                                                                               
         ICM   RF,3,NBUF           NUMBER IN BUFFER                             
         SR    R0,R0                                                            
         IC    R0,NITM             LESS NUMBER ON SCREEN                        
         SR    RF,R0                                                            
         SR    RE,RE                                                            
         ICM   RE,3,MAXREM         MAX REMAINING AT ENTRY                       
         SR    RE,RF               LESS NUMBER ALREADY ADDED                    
         AHI   RE,1                                                             
         STCM  RE,3,MAXBAT         MAX ON THIS SCREEN TO FILL BATCH             
*                                                                               
         XR    R4,R4                                                            
         XR    R6,R6                                                            
         IC    R6,SCRMXD           R6=MAX LINES                                 
         L     R2,ADETH                                                         
*                                                                               
TSTNUM3  BRAS  RE,SETLOW           SET ADCONS FOR LOWER                         
         XR    R0,R0                                                            
         LA    R3,LOWER                                                         
         LA    R5,NLOWER-1                                                      
*                                                                               
TSTNUM5  ICM   R2,15,0(R3)                                                      
         BZ    TSTNUM7                                                          
         TM    1(R2),FATBPROT      SKIP PROTECTED                               
         BO    TSTNUM7                                                          
         CLI   5(R2),0             TEST FOR EMPTY FIELD                         
         BE    TSTNUM7                                                          
         LA    R0,1                SET 'THIS LINE HAS DATA'                     
TSTNUM7  LA    R3,4(R3)                                                         
         BCT   R5,TSTNUM5                                                       
*                                                                               
         TM    SCRSTA,SCRTAXQ      TEST TAX SCREEN                              
         BNO   TSTNUM9                                                          
         ICM   RF,15,ACPJH         TEST JOB OR EXP DATA                         
         BZ    TSTNUM9             SCREEN ERROR                                 
         CLI   8(RF),C' '                                                       
         BH    TSTNUM9             YES, DATA IS ALLOWED                         
         ICM   R2,15,ABASH         DON'T ALLOW BASIS                            
         BZ    TSTNUM9                                                          
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
         ICM   R2,15,ALOCH         DON'T ALLOW LOCATION                         
         BZ    TSTNUM9                                                          
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
         ICM   R2,15,ATXWCH        DON'T ALLOW WORKCODE                         
         BZ    TSTNUM9                                                          
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
*                                                                               
TSTNUM9  AR    R4,R0               INCREMENT NUMBER OF LINES WITH DATA          
         ICM   R2,15,AAMTH                                                      
         CLM   R4,3,MAXBUF         TEST OVER MAX                                
         JH    EMMXCAP             'MAXIMUM NUMBER IS....                       
*                                                                               
         CLM   R4,3,MAXBAT         TEST MAX IN BATCH                            
         JNL   EMMAXRC             'MAXIMUM ENTRIES...'                         
*                                                                               
         L     R2,ANEXT                                                         
         LTR   R0,R0               TEST LAST WAS 'EMPTY LINE'                   
         BZ    TSTNUM11            YES, REST OF SCREEN MUST BE EMPTY            
         BCT   R6,TSTNUM3                                                       
         STC   R4,NITM             SCREEN IS FULL                               
         J     XIT                                                              
*                                  *HAVE ENCOUNTERED A BLANK LINE*              
TSTNUM11 CLI   NITM,0              TEST ANY PREVIOUS DATA                       
         BE    *+12                NO, CAN'T BE DELETE ERROR                    
         CLM   R4,1,NITM           TEST SAME NUMBER OF ITEMS                    
         BL    TSTNUM19            NO, MUST HAVE DELETED A LINE                 
         STC   R4,NITM                                                          
         BCT   R6,TSTNUM13                                                      
         J     XIT                 END OF SCREEN                                
*                                                                               
TSTNUM13 BRAS  RE,SETLOW           LOOK FOR DATA AFTER BLANK LINE               
         LA    R3,LOWER                                                         
         LA    R5,NLOWER-1                                                      
*                                                                               
TSTNUM15 ICM   R2,15,0(R3)                                                      
         BZ    TSTNUM17                                                         
         CLI   5(R2),0             MAKE SURE FIELD IS EMPTY                     
         JNE   EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
TSTNUM17 LA    R3,4(R3)                                                         
         BCT   R5,TSTNUM15                                                      
         L     R2,ANEXT                                                         
         B     TSTNUM11                                                         
*                                                                               
TSTNUM19 OC    NFST,NFST           TEST BLANK SCREEN                            
         BNZ   *+12                                                             
         MVI   NITM,0                                                           
         J     XIT                                                              
*                                                                               
         MVC   NGET,NFST           SET FOR REDISPLAY                            
         L     R2,AAMTH            R2=A(MISSING DATA)                           
         OI    PFKSW,PFKRDSP                                                    
         BRAS  RE,VALPF                                                         
         L     R2,ADETH                                                         
         TM    SCRSTA,SCRTAXQ      TEST TAX SCREEN                              
         JO    EMBASND             'BASIS CANNOT BE DELETED..'                  
         J     EMUPFKD             'USE PFKEY TO DELETE..'                      
         EJECT                                                                  
**********************************************************************          
* MANAGE EDIT OF DETAILS                                             *          
**********************************************************************          
EDT      NTR1  ,                                                                
         XR    R6,R6                                                            
         IC    R6,NITM             R6=NUMBER OF DETAIL LINES WITH DATA          
         L     R2,ADETH            R2=A(FIRST ITEM FIELD)                       
         MVC   NGET,NFST           START WITH FIRST                             
         B     EDT3                                                             
*                                                                               
EDT2     XR    R1,R1                                                            
         ICM   R1,3,NGET           INCREMENT NEXT LINE NUMBER                   
         BZ    *+8                 FOR 'NEW' DON'T ADD                          
         LA    R1,1(R1)                                                         
         STCM  R1,3,NGET                                                        
         L     R2,ANEXT                                                         
*                                                                               
EDT3     LA    RE,DETVALS          CLEAR DETAIL VALUES                          
         LA    RF,DETVALNQ                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ZAP   INAMNT,PZERO        INIT AMOUNT FIELDS                           
         ZAP   CDAMNT,PZERO                                                     
         MVI   ITMSTA,0            INIT STATUS                                  
*                                                                               
         MVI   ACTION,ADDRECQ      ASSUME ADD                                   
         CLC   NGET,NBUF           TEST NEW ITEM                                
         BH    EDT4                YES, ADD NEW ITEM                            
         OC    NGET,NGET                                                        
         BZ    EDT4                                                             
         OC    NBUF,NBUF           TEST FIRST TIME                              
         BZ    EDT4                                                             
         GOTOR TOTSAR,TSRGETQ      GET THE OLD RECORD                           
         MVC   ITMSTA,RECSTA       SAVE CURRENT STATUS                          
         MVI   ACTION,PUTRECQ      SET FOR PUTREC                               
*                                                                               
EDT4     BRAS  RE,SETLOW           SET LOWER ADCONS                             
         XR    RF,RF                                                            
         ICM   RF,3,SCRPROC        PROCESSING ROUTINE                           
         BNZ   *+6                                                              
         DC    H'0'                                                             
         A     RF,BASERB                                                        
         GOTOR (RF),DMCB,PROGD     EDIT DETAIL LINE                             
         CLI   ACTION,ADDRECQ                                                   
         BE    EDT5                                                             
         BRAS  RE,PUTLIN           PUT DETAIL LINE TO TSAR                      
         B     EDT9                                                             
*                                                                               
EDT5     GOTOR ADDLIN,DMCB,(R2)    ADD NEW ITEM                                 
         MVC   NLST,NBUF           LAST ON SCREEN IS LAST IN BUFFER             
         OC    NFST,NFST                                                        
         BNZ   *+10                                                             
         MVC   NFST,NBUF           SET FIRST                                    
*                                                                               
EDT9     BCT   R6,EDT2                                                          
*                                                                               
         TM    CTXSTAT,CTXSGSTI    TEST GST AMOUNT INPUT                        
         JNO   EDT11               NO,                                          
         ICM   R2,15,AGAMH         SET CURSOR TO GST                            
         BZ    *+12                                                             
         TM    1(R2),FATBPROT      IF NOT PROTECTED                             
         BZ    *+8                                                              
         ICM   R2,15,ADOCH                                                      
         CP    GSTBASE,PZERO                                                    
         JE    EMGSTAI             GST AMOUNT IS INVALID                        
*                                                                               
EDT11    TM    CTXSTAT,CTXSPSTI    TEST PST AMOUNT INPUT                        
         BNO   EDTX                NO,                                          
         ICM   R2,15,APAMH         SET CURSOR TO PST                            
         BZ    *+12                                                             
         TM    1(R2),FATBPROT      IF NOT PROTECTED                             
         BZ    *+8                                                              
         ICM   R2,15,ADOCH                                                      
         CP    PSTBASE,PZERO                                                    
         JE    EMPSTAI             PST AMOUNT IS INVALID                        
*                                                                               
EDTX     J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* CONSTANST AND LITERAL POOL                                         *          
**********************************************************************          
ADTAB    DS    0F                                                               
         DC    AL2(USATAB-T61B0A,AUSATAB-PROGD)                                 
         DC    AL2(CANTAB-T61B0A,ACANTAB-PROGD)                                 
         DC    AL2(UPRTAB-T61B0A,AUPRTAB-PROGD)                                 
         DC    AL2(PUNTAB-T61B0A,APUNTAB-PROGD)                                 
         DC    AL2(CLRTAB-T61B0A,ACLRTAB-PROGD)                                 
         DC    AL2(XJOTAB-T61B0A,AXJOTAB-PROGD)                                 
         DC    AL2(ORDTAB-T61B0A,AORDTAB-PROGD)                                 
         DC    AL2(EXOTAB-T61B0A,AEXOTAB-PROGD)                                 
         DC    AL2(RCDTAB-T61B0A,ARCDTAB-PROGD)                                 
         DC    AL2(PFKTAB-T61B0A,APFKEYS-PROGD)                                 
         DC    AL2(PFKMSK-T61B0A,APFKMSK-PROGD)                                 
         DC    AL2(DCDDL-T61B0A,ADCDDL-PROGD)                                   
         DC    X'FF'                                                            
*                                                                               
       ++INCLUDE ACPRVTAB                                                       
*                                                                               
UL2D     DC    C'2D'                                                            
UL2P     DC    C'2P'                                                            
NONC     DC    C'/NC'                                                           
*                                                                               
INVTMSG  DC    C'Total '                                                        
USETMSG  DC    C'(Tax '                                                         
*                                                                               
CGSTMSG  DC    C'G='                                                            
CPSTMSG  DC    C'P='                                                            
*                                                                               
ITMSMSG  DC    C'Items '                                                        
*                                                                               
PFKNFIS  DC    AL2(PFKMALL-(BPFKFIS))                                           
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE HEADLINE FIELDS                                           *          
**********************************************************************          
VALHED   NMOD1 0,**VALHED**,R8                                                  
         L     RC,0(R1)                                                         
         L     R2,ADOCH            TEST CLEAR                                   
         CLI   5(R2),5                                                          
         BNE   VALHED2                                                          
         CLC   8(5,R2),=C'CLEAR'                                                
         BNE   VALHED2                                                          
         GOTOR LOADSCRN,TWASCRN                                                 
         BRAS  RE,REINIT           RE-INTITIALIZE                               
         J     IMEREQF             'ENTER REQUIRED FIELDS...'                   
*                                                                               
*                                  * CHECK DUE DATE CHANGED                     
VALHED2  ICM   R2,15,ADUEH         IF VENDOR OR INVOICE DATE CHANGE             
         BZ    VALHED6                                                          
         TM    1(R2),FATBPROT      SKIP IF PROTECTED                            
         BO    VALHED6                                                          
         TM    4(R2),FINPVAL       TEST FIELD CHANGED                           
         BO    VALHED6                                                          
         NI    FLG,X'FF'-(AUTODUE)  TURNOFF AUTO DUE DATE                       
*                                                                               
VALHED6  BAS   RE,VALORD           ORDER NUMBER                                 
         BAS   RE,VALDOC           DOCUMENT NUMBER                              
         BAS   RE,VALDAT           DATE                                         
*                                                                               
         L     R2,AURGH            VALIDATE URGENT                              
         GOTO1 AFVAL,(R2)                                                       
         MVC   URGENT,FVIFLD                                                    
         BNE   VALHED7             NOTHING IN FIELD                             
         CLI   FVIFLD,C'U'         TEST FOR VALID INPUT                         
         BE    VALHED7             YES                                          
         J     EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
VALHED7  BAS   RE,VALCASH          CASH ACCOUNT                                 
         LA    RE,VENDVALS         CLEAR SAVED VENDOR VALUES                    
         LA    RF,VENDLNQS                                                      
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         ZAP   VENDDISC,PZERO     INIT DISCOUNT SAVES                           
         ZAP   XVNDDISC,PZERO                                                   
*                                                                               
         L     R2,AVENH           PRODUCTION VENDOR FIELD                       
         CLI   5(R2),0            IS THERE A PROD VENDOR?                       
         BE    VALHED8            NO                                            
*                                                                               
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         GOTO1 VSEARCH,DMCB,(4,(R2)),TWAD,(C'*',CPYSUPP),ACOMFACS,0             
         DROP  RF                                                               
*                                                                               
VALHD8   MVI   TYPE,C'P'          SET PROD VENDOR SWITCH                        
         BAS   RE,VEN             EDIT VENDOR AND CD FIELDS                     
         OI    4(R2),FINPVAL                                                    
         OI    POSTSW,POSTVEN      SET VENDOR POSTINGS                          
         TM    SCRSTA,SCRCANQ     TEST CANADIAN                                 
         BNO   *+8                                                              
         BAS   RE,VALCAN          VALIDATE CANADIAN                             
*                                                                               
VALHED8  TM    BCCPYST5,CPYSVEND   COPY THE VENDOR?                             
         BNO   *+8                 NO                                           
         BAS   RE,COPYVEN          COPY PROD VENDOR                             
         L     R2,AXVNH            TEST FOR AN EXPENSE VENDOR                   
         CLI   5(R2),0                                                          
         BE    VALHED9                                                          
*                                                                               
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         MVI   WORK,C'S'                                                        
         MVC   WORK+1(1),CPYSUPX                                                
         GOTO1 VSEARCH,DMCB,(4,(R2)),TWAD,(C'*',WORK),ACOMFACS,0                
         DROP  RF                                                               
*                                                                               
         MVI   TYPE,C'E'           NOTE EXPENSE VENDOR                          
         BAS   RE,VEN              NOW VERIFY EXP VENDOR                        
         OI    4(R2),FINPVAL                                                    
         OI    POSTSW,POSTEXP      SET EXPENSE POSTINGS                         
         TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         BNO   *+8                                                              
         BAS   RE,VALCAN           VALIDATE CANADIAN                            
         B     VALHED10                                                         
*                                                                               
VALHED9  TM    BCCPYST2,CPYSVENR   IS VENDOR REQUIRED?                          
         BZ    VALHED10            NO                                           
         CLI   VENDACCT,0          WAS PROD VENDOR ENTERED?                     
         BNE   VALHED10            YES                                          
         CLI   XVNDACCT,0          WAS EXP VENDOR ENTERED?                      
         BNE   VALHED10            YES                                          
         L     R2,AVENH            POINT TO PROD VENDOR FIELD                   
         J     EMPEVIR             'PROD OR EXPENSE VENDOR REQUIRED'            
*                                                                               
VALHED10 L     R2,AXVNH                                                         
         GOTO1 ANARRSCN,DMCB,(2,ANARH),0                                        
         MVC   NARR,BOELEM                                                      
         MVC   NARRLEN,BOHALF1                                                  
         TM    SCRSTA,SCRMINQ      TEST MULTI INPUT SCREEN                      
         JNO   VALHED11                                                         
         L     RF,ANARH                                                         
         MVC   DNARR1,8(RF)        SAVE DEFAULT NARRATIVE                       
         L     RF,ANA2H                                                         
         MVC   DNARR2,8(RF)                                                     
         LHI   RF,NARNUMQ                                                       
         STCM  RF,3,NGET                                                        
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRGETQ      GET DEFAULT NARRATIVE RECORD                 
         MVC   NARNAR1,DNARR1      SAVE NEW NARRATIVE                           
         MVC   NARNAR2,DNARR2                                                   
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRPUTQ      SAVE NEW TO TSAR                             
*                                                                               
VALHED11 CLI   LONGINVL,0          ANY LONG INVOICE ?                           
         JE    XIT                 NO, SKIP CHECK DUP                           
         LA    R3,VENDACCT         PRODUCTION VENDOR                            
         CLI   0(R3),0                                                          
         BE    VALHED12                                                         
         LA    RF,DOCDATE          USE DATE FOR CHECK DUP                       
         XC    WORK(L'INVPDAT),WORK                                             
         LA    RF,WORK             NO DATE CHECK                                
         GOTOR CHKINV,DMCB,2(R3),3(R3),LONGINV,0(RF)                            
         JE    VALHED12            NOT A DUPLICATE                              
         OI    DUPSW,DUPPROD       DUPLICATE ON PRODUCTION                      
         MVC   DUPPRDK,IOKEYSAV    SAVE KEY OF DUPLICATE                        
*                                                                               
VALHED12 LA    R3,XVNDACCT         OR EXPENSE VENDOR                            
         CLI   0(R3),0                                                          
         BE    VALHED13                                                         
         LA    RF,DOCDATE          USE DATE FOR CHECK DUP                       
         XC    WORK(L'INVPDAT),WORK                                             
         LA    RF,WORK             NO DATE CHECK                                
         GOTOR CHKINV,DMCB,2(R3),3(R3),LONGINV,0(RF)                            
         JE    VALHED13            NOT A DUPLCATE                               
         OI    DUPSW,DUPEXPN       DUPLICATE ON EXPENSE                         
         MVC   DUPEXPK,IOKEYSAV    SAVE KEY OF DUPLICATE                        
*                                                                               
*                                  ** DUPLICATE INVOICE **                      
VALHED13 TM    DUPSW,DUPPROD+DUPEXPN                                            
         BZ    VALHED15                                                         
         OI    DUPSW,DUPINV        SET DUPLICATE INVOICE                        
         LA    RF,DUPPRDK                                                       
         TM    DUPSW,DUPPROD                                                    
         BO    *+8                                                              
         LA    RF,DUPEXPK                                                       
         MVC   DUPINVK,0(RF)       SAVE KEY OF DUPLICATE                        
         L     R2,ADOCH                                                         
*                                                                               
         ICM   R3,15,AOVRH         TEST OVERRIDE FIELD                          
         JZ    EMDUPIT             NONE, DUPLICATE ITEM                         
*                                                                               
         L     RF,AOVRTH           'OVERWRITE' TAB                              
         TM    1(RF),FATBLOW       TEST STILL ZERO-INTENSITY                    
         BO    VALHED14            YES, MUST DISPLAY FIELD                      
         CLI   5(R3),0                                                          
         JE    EMDUPIT                                                          
         L     R2,AOVRH                                                         
         CLI   8(R2),C'Y'          MUST INPUT A 'Y'                             
         JNE   EMINVIF                                                          
         OI    DUPSW,DUPOVR        OVERRIDE                                     
         B     VALHED17                                                         
*                                                                               
VALHED14 MVI   BYTE,FCONOVR        SET OVERRIDE FIELD NUMBER                    
         LA    RF,BYTE                                                          
         GOTO1 VSECRET,DMCB,('SECPFLDP',ASECBLK),(RF),0                         
         CLI   0(R1),SECPYES                                                    
         JNE   EMDUPIT             'DUPLICATE INVOICE...'                       
         NI    1(R3),X'FF'-(FATBPROT)   UNPROTECT OVERRIDE FIELD                
         L     R3,AOVRTH           R3=A(OVERRIDE TAG FIELD)                     
         NI    1(R3),X'FF'-(FATBLOW)                                            
         OI    1(R3),FATBHIGH                                                   
         J     EMDUPIT             DUPLICATE ITEM                               
*                                                                               
*                                  ** NOT A DUPLICATE **                        
VALHED15 ICM   R3,15,AOVRH         TEST OVERRIDE FIELD                          
         JZ    VALHED17            NONE, DUPLICATE ITEM                         
         TM    1(R3),FATBPROT      TEST PROTECTED                               
         JO    VALHED17            YES,                                         
         MVI   8(R3),C' '                                                       
         OI    1(R3),FATBPROT      PROTECT OVERRIDE FIELD                       
         L     R3,AOVRTH           R3=A(OVERRIDE TAG FIELD)                     
         OI    1(R3),FATBLOW                                                    
VALHED17 BAS   RE,VALDUE           DUE DATE                                     
*                                                                               
VALHEDX  J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE ORDER NUMBER                                              *          
**********************************************************************          
VALORD   NTR1  ,                                                                
         L     R2,AORDH                                                         
         TM    4(R2),FINPVAL                                                    
         JO    XIT                                                              
         CLI   5(R2),0             ANY ORDER NUMBER ?                           
         BNE   VALORD1             YES,                                         
         XC    ORDRNUM,ORDRNUM     CLEAR ORDER NUMBER                           
         OC    LASTORD,LASTORD     ANY OLD ORDER ?                              
         BNZ   VALORD35                                                         
         OI    4(R2),FINPVAL       SET VALIDATED                                
         J     XIT                                                              
*                                                                               
VALORD1  OC    NBUF,NBUF                                                        
         JNZ   EMINVIF                                                          
         XC    WORK,WORK                                                        
         XC    BCPARTSW,BCPARTSW                                                
         LA    R3,WORK                                                          
         USING SCANBLKD,R3                                                      
         GOTO1 SCANNER,DMCB,(R2),(3,(R3))                                       
         CLI   4(R1),0                                                          
         JE    EMINVIF            'INVALID INPUT...'                            
         CLI   4(R1),2                                                          
         JH    EMINVIF            'INVALID INPUT...'                            
         CLI   SC1STLEN,6         NOT MORE THAN SIX                             
         JH    EMINVIF                                                          
         CLI   SC1STLEN,1         AT LEAST ONE                                  
         JL    EMINVIF                                                          
         CLI   SC2NDLEN,0         CAN'T HAVE RIGHT SIDE                         
         JNE   EMINVIF                                                          
         MVI   ORDRNUM,C'0'                                                     
         MVC   ORDRNUM+1(L'ORDRNUM-1),ORDRNUM                                   
         LA    RF,ORDRNUM+L'ORDRNUM                                             
         XR    R1,R1                                                            
         IC    R1,SC1STLEN        GET INPUT LEGNTH                              
         SR    RF,R1              RIGHT ALIGN                                   
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),SC1STFLD                                                 
*                                                                               
         LA    R3,SCBLKLQ(R3)                                                   
         CLI   SC1STLEN,0         TEST SECOND FIELD                             
         BE    VALORD2            NONE                                          
         CLI   SC2NDLEN,0         CAN'T HAVE RIGHT SIDE                         
         JNE   EMINVIF                                                          
         XR    R1,R1                                                            
         IC    R1,SC1STLEN        TEST PARTIAL                                  
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         CLC   SC1STFLD(0),=C'PART'                                             
         JNE   EMINVIF                                                          
         MVI   BCPARTSW,C'Y'                                                    
*                                                                               
VALORD2  OC    LASTORD,LASTORD                                                  
         BZ    VALORD3                                                          
         CLC   LASTORD,ORDRNUM    SAME ORDER ?                                  
         BNE   VALORD35           MUST POST OLD  FIRST                          
         J     XIT                YES, DON'T REPROCESS                          
*                                                                               
VALORD3  LA    R4,IOKEY           BUILD ORDER KEY                               
         USING ORDRECD,R4                                                       
         XC    ORDKEY,ORDKEY                                                    
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,ORDRNUM                                                  
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'ORDKEY),IOKEYSAV                                         
         JNE   EMORDNF            'ORDER NOT FOUND...'                          
         L     R4,AIO1                                                          
         TM    ORDRSTAT,ORDSDEL+ORDSFMCH+ORDSLDEL                               
         JNZ   EMOFMCH           'ORDER IS FULLY MATCH...'                      
*                                                                               
         CLI   ORDKSEQ,ORDKEXTN    TEST EXTENSION                               
         JE    EMORDNF             YES, ERROR                                   
         TM    ORDRSTA2,ORDSEXEX   TEST EXTENSION EXISTS                        
         BZ    VALORD3X            NO,                                          
         TM    ORDRSTA2,ORDSAPPR   TEST APPROVED                                
         BNZ   VALORD3X            YES                                          
         TM    ORDRSTAT,ORDGDRCV   TEST GOODS RECEIVED                          
         BNZ   VALORD3X            YES                                          
         J     EMORTNF             "ORDER TRANSACTION NOT ON FILE"              
*                                                                               
VALORD3X DS    0H                                                               
         GOTOR CLEAR,AORDTAB       CLEAR ORDER FIELDS-TOP                       
         ZAP   ORDRINV,PZERO       AMOUNT INVOICED                              
         ZAP   ORDRAMT,PZERO       AMOUNT OF ORDER                              
         XC    NORDR,NORDR                                                      
         XC    AEXOEL,AEXOEL                                                    
         XC    ASCMEL,ASCMEL                                                    
         XR    R0,R0                                                            
         LA    R5,ORDRFST          FIND EXPENSE ORDER ELEMENT                   
         USING EXOELD,R5                                                        
VALORD4  CLI   EXOEL,0                                                          
         BE    VALORD7                                                          
         CLI   EXOEL,EXOELQ                                                     
         BNE   *+8                                                              
         ST    R5,AEXOEL           SAVE ADDRESS OF EXPENSE ELEMENT              
         USING SCMELD,R5                                                        
         CLI   SCMEL,SCMELQ                                                     
         BNE   VALORD5                                                          
         TM    ORDRSTA2,ORDSEXEX   TEST EBUYER NARRATIVE                        
         BZ    VALORD5                                                          
         CLI   SCMTYPE,SCMTOMOC    TEST ORDER MATCHING                          
         BNE   VALORD5             NO,                                          
         ST    R5,ASCMEL           SAVE ADDRESS OF NARRATIVE                    
*                                                                               
VALORD5  IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VALORD4                                                          
         DROP  R5                                                               
*                                                                               
VALORD7  LA    R5,ORDRFST                                                       
         USING OAMELD,R5                                                        
         XR    R0,R0                                                            
         XR    RF,RF                                                            
         XR    R3,R3                                                            
         USING ORDEL,R3                                                         
VALORD9  CLI   0(R5),OAMELQ        TEST AMOUNT ELEMENT                          
         BNE   VALORD9A                                                         
         TM    OAMSTAT,OAMSXTRA    IGNORE EXTRA ELEMENTS                        
         BNZ   VALORD9A                                                         
         AHI   RF,1                COUNT NUMBER OF AMOUNT ELEMENT               
VALORD9A CLI   0(R5),ORDELQ        ORDER ELEMENT                                
         BNE   VALORD10                                                         
         LR    R3,R5               SAVE ADDRESS OF ORDER                        
         TM    ORDSTAT-ORDELD(R5),ORDSMNUP                                      
         JNZ   EMOFMCH           'ORDER IS FULLY MATCH...'                      
VALORD10 CLI   0(R5),0                                                          
         BE    VALORD11                                                         
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VALORD9                                                          
*                                                                               
VALORD11 LTR   R3,R3               TEST ORDER ELEMENT FOUND                     
         JZ    EMORDNF             ERROR: ORDER NOT FOUND                       
         XR    RE,RE                                                            
         ICM   RE,3,NBUF           NUMBER IN BUFFER                             
         AR    RE,RF               PLUS NUMBER ON ORDER                         
         LA    R0,MXCAP                                                         
         ICM   R2,15,AORDH                                                      
         CR    RE,R0               TEST OVER MAX                                
         JH    EMMXCAP             'MAXIMUM NUMBER IS....                       
*                                                                               
VALORD13 CLC   ORDACCU(2),=C'SJ'                                                
         BNE   VALORD15            MUST BE EXPENSE ORDER                        
         MVI   TYPE,C'P'                                                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ORDSUPA),ORDSUPA                                           
         CLC   BCCPYSUP,ORDSUPU                                                 
         BE    *+14                                                             
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(14),ORDSUPU                                                
         L     R2,AVENH            PRODUCTION VENDOR FIELD                      
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISVEN                                                   
         OI    6(R2),FOUTMOD                                                    
         LR    R5,R3                                                            
         B     VALORD17                                                         
*                                                                               
VALORD15 MVI   TYPE,C'E'                                                        
         MVC   FLD,SPACES                                                       
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(14),ORDSUPU   EXPENSE SUPPLIER                             
         L     R2,AXVNH                                                         
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISXVN                                                   
         OI    6(R2),FOUTMOD                                                    
*                                                                               
         LA    R5,ORDRFST                                                       
         USING OAMELD,R5                                                        
*                                                                               
VALORD17 CLI   0(R5),0             TEST EOR                                     
         BE    VALORD29                                                         
         CLI   0(R5),OAMELQ        TEST AMOUNT ELEMENT                          
         BE    VALORD21                                                         
VALORD19 XR    R0,R0                                                            
         IC    R0,1(R5)                                                         
         AR    R5,R0                                                            
         B     VALORD17                                                         
*                                                                               
VALORD21 TM    OAMSTAT,OAMSXTRA    IGNORE EXTRA ELEMENTS                        
         BNZ   VALORD19                                                         
         L     R2,ADETH                                                         
         BRAS  RE,ERASE            CLEAR BOTTOM OF SCREEN                       
         BRAS  RE,SETLOW           SET ADCONS FOR LOWER                         
*                                                                               
         AP    ORDRINV,OAMIVAL     AMOUNT INVOICED                              
         AP    ORDRAMT,OAMAMNT     AMOUNT OF ORDER                              
         ZAP   INAMNT,OAMAMNT                                                   
*                                                                               
         SP    INAMNT,OAMIVAL      LESS AMOUNT INVOICED                         
         CLC   OAMLAST,TODAYP                                                   
         BNE   *+10                                                             
         SP    INAMNT,OAMTVAL      LESS: AMOUNT INVOICED TODAY                  
*                                                                               
         CLI   TYPE,C'E'                                                        
         BE    VALORD23                                                         
         L     R2,ACLIH            CLIENT CODE                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(3),ORDACCA                                                   
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISCLI                                                   
         OI    6(R2),X'01'                                                      
*                                                                               
         L     R2,APROH            PRODUCT CODE                                 
         MVC   FLD,SPACES                                                       
         MVC   FLD(3),ORDACCA+3                                                 
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISPRO                                                   
         OI    6(R2),FOUTMOD                                                    
*                                                                               
         L     R2,AJOBH            JOB CODE                                     
         MVC   FLD,SPACES                                                       
         MVC   FLD(6),ORDACCA+6                                                 
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISJOB                                                   
         OI    6(R2),FOUTMOD                                                    
*                                                                               
         L     R2,AWKCH            WORKCODE                                     
         MVC   FLD,SPACES                                                       
         MVC   FLD(2),OAMWORK                                                   
         TM    OAMSTAT,OAMSNOCM                                                 
         BNO   *+10                                                             
         MVC   FLD+2(3),=C'/NC'    NON-COMMISSIONABLE                           
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISWKC                                                   
         OI    6(R2),FOUTMOD                                                    
*                                                                               
VALORD23 L     R2,AAMTH            AMOUNT                                       
         MVC   FLD,SPACES                                                       
         ZAP   BCDUB,OAMAMNT       ORDER AMOUNT                                 
         SP    BCDUB,OAMIVAL       LESS: AMOUNT INVOICED                        
         CLC   OAMLAST,TODAYP                                                   
         BNE   *+10                                                             
         SP    BCDUB,OAMTVAL       LESS: AMOUNT INVOICED TODAY                  
         CURED BCDUB,(12,FLD),2,MINUS=YES,ALIGN=LEFT                            
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
         OI    6(R2),FOUTMOD                                                    
*                                                                               
         CLI   TYPE,C'P'                                                        
         BE    VALORD27                                                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ORDACCA),ORDACCA  EXPENSE ACCOUNT                          
         CLC   ORDACCU(2),=C'SE'                                                
         BE    *+14                                                             
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(14),ORDACCU                                                
         L     R2,AXACH                                                         
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'MISEXA                                                   
         OI    6(R2),FOUTMOD                                                    
*                                                                               
         ICM   R5,15,AEXOEL        TEST EXPENSE DETAIL                          
         BZ    VALORD27            NO ELEMENT,                                  
         L     R3,AEXOTAB                                                       
*                                                                               
VALORD25 MVC   FLD,SPACES                                                       
         XR    R1,R1                                                            
         IC    R1,4(R3)            LENGTH OF DATA                               
         BCTR  R1,0                                                             
         XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISPLACEMENT TO DATA                         
         AR    RF,R5                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),0(RF)        DATA TO FIELD                                
         XR    RF,RF                                                            
         ICM   RF,3,0(R3)          GET ADDRESS OF SCREEN FIELD                  
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)                                                      
         BRAS  RE,MOVEFLD          MOVE DATA TO SCREEN                          
         OI    6(R2),FOUTMOD                                                    
         LA    R3,L'EXOTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         BNE   VALORD25                                                         
*                                                                               
VALORD27 L     R2,ADETH                                                         
         TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         BNO   VALORD28                                                         
         ZAP   ITMBASE,INAMNT                                                   
         ZAP   ITMGST,PZERO                                                     
         ZAP   ITMPST,PZERO                                                     
*                                                                               
VALORD28 GOTOR ADDLIN,DMCB,(R2)    ADD NEW ITEM                                 
         OC    NORDR,NORDR         TEST FIRST FOR ORDER                         
         BNZ   *+10                                                             
         MVC   NORDR,NBUF          SAVE NUMBER OF FIRST                         
         B     VALORD19                                                         
*                                                                               
VALORD29 OC    NORDR,NORDR         TEST ORDER DETAILS?                          
         JZ    EMORDNF                                                          
         ICM   R5,15,ASCMEL                                                     
         BZ    VALORD34                                                         
         USING SCMELD,R5                                                        
         LA    R1,L'DNARR1+L'DNARR2  R1= LENGTH OF FIELD                        
         SR    RF,RF                                                            
         IC    RF,SCMLN                                                         
         SHI   RF,SCMLN1Q            RF=LENGTH OF DATA                          
         CR    R1,RF                 GET SHORTEST LENGTH                        
         BL    *+6                                                              
         LR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DNARR1(0),SCMNARR                                                
         LHI   RF,NARNUMQ                                                       
         STCM  RF,3,NGET                                                        
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRGETQ      GET DEFAULT NARRATIVE RECORD                 
         MVC   NARNAR1,DNARR1      SAVE NEW NARRATIVE                           
         MVC   NARNAR2,DNARR2                                                   
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRPUTQ      SAVE NEW TO TSAR                             
*                                                                               
VALORD34 OI    PFKSW,PFKRDSP       FORCE RE-DISPLAY                             
         MVC   NGET,NORDR          FROM FIRST ORDER RECORD                      
         BRAS  RE,VALPF                                                         
*                                                                               
         L     R2,AORDH                                                         
         OI    4(R2),FINPVAL       SET VALIDATED                                
         MVC   LASTORD,ORDRNUM     SAVE LAST ORDER                              
         L     R2,ADOCH                                                         
         CLI   5(R2),0                                                          
         JE    IMODDEI            'ORDER DETAIL DISPLAYED..'                    
         J     XIT                                                              
*                                                                               
VALORD35 MVC   FLD,SPACES          MUST REDISPALY OLD                           
         MVC   FLD(L'LASTORD),LASTORD                                           
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'LASTORD                                                  
         OI    6(R2),FOUTMOD                                                    
         J     EMCCORD              CAN'T CHANGE ORDER NUMBER - POST..          
*                                                                               
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* VALIDATE DOCUMENT NUMBER FIELD                                     *          
**********************************************************************          
VALDOC   NTR1  ,                                                                
         XC    REFNUM,REFNUM                                                    
         XC    LONGINV,LONGINV                                                  
         MVI   FVMINL,1            REQUIRE SOME INPUT                           
         L     R2,ADOCH                                                         
         GOTO1 AFVAL,(R2)                                                       
         JNE   EMMISIF                                                          
*                                                                               
         LA    R3,TEMP                                                          
         USING SCANBLKD,R3                                                      
         GOTO1 SCANNER,DMCB,FVIHDR,(3,(R3)),=C',=,,'                            
         CLI   4(R1),0             TEST FOR INVALID SYNTAX                      
         JE    EMINVIF             'INVALID INPUT FIELD'                        
         CLI   4(R1),2                                                          
         JH    EMINVIF                                                          
         BE    VALDOC3             BRANCH IF TWO COMPONENTS                     
*                                                                               
         CLI   SC1STLEN,L'LONGINV  TEST NUMBER TOO LONG                         
         JH    EMINVIF                                                          
         MVC   REFNUML,SC1STLEN    SAVE REFERENCE                               
         MVC   REFNUM,SC1STFLD                                                  
         MVC   LONGINVL,SC1STLEN   SAVE LONG INVOICE NUMBER                     
         MVC   LONGINV,SC1STFLD                                                 
         BAS   RE,TESTAN           TEST ALPHA/NUMERIC                           
         CLI   SC1STLEN,L'REFNUM   TEST MORE THAN SIX                           
         BNH   VALDOCX             NO,                                          
         XR    RF,RF                                                            
         IC    RF,SC1STLEN         USE LAST SIX FOR REFERENCE                   
         SHI   RF,L'REFNUM                                                      
         LA    RF,SC1STFLD(RF)                                                  
         MVI   REFNUML,L'REFNUM                                                 
         MVC   REFNUM,0(RF)                                                     
         J     VALDOCX                                                          
*                                                                               
VALDOC3  LA    R0,2                ** TWO COMPONENTS **                         
         LA    R4,LONGINV          FIRST IS LONG                                
         LA    R5,LONGINVL                                                      
         MVI   FVINDX,1                                                         
         CLI   SC1STLEN,L'LONGINV  TEST NUMBER TOO LONG                         
         JH    EMINVIF                                                          
         CLI   SC1STLEN,L'REFNUM   TEST MIN LENGTH                              
         JNH   EMINVIF                                                          
         B     VALDOC9                                                          
*                                                                               
VALDOC7  LA    R4,REFNUM           SECOND IS REFERENCE                          
         LA    R5,REFNUML                                                       
         MVI   FVINDX,2                                                         
         CLI   SC1STLEN,L'REFNUM   TEST LENGTH                                  
         JH    EMINVIF                                                          
*                                                                               
VALDOC9  BAS   RE,TESTAN           TEST ALPHA/NUMERIC                           
         SR    RF,RF                                                            
         IC    RF,SC1STLEN         SAVE LENGTH                                  
         STC   RF,0(R5)                                                         
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R4),SC1STFLD    AND NUMBER                                   
*                                                                               
         LA    R3,SCBLKLQ(R3)                                                   
         BCT   R0,VALDOC7                                                       
*                                                                               
VALDOCX  OC    REFNUM,SPACES       BOTH ARE SPACES FILLED                       
         OC    LONGINV,SPACES                                                   
         J     XIT                                                              
*                                                                               
TESTAN   STM   RE,R6,SVREG         TEST FIELD IS ALPHA/NUMERIC                  
         XR    R0,R0                                                            
         IC    R0,SC1STLEN                                                      
         LA    RF,SC1STFLD                                                      
TESTAN3  LA    RE,TESTANT                                                       
TESTAN5  CLC   0(1,RF),0(RE)                                                    
         JH    EMINVIF             'INVALID INPUT FIELD'                        
         CLC   0(1,RF),1(RE)                                                    
         BNL   TESTAN9                                                          
         LA    RE,2(RE)                                                         
         CLI   0(RE),C' '                                                       
         BNE   TESTAN5                                                          
         J     EMINVIF                                                          
*                                                                               
TESTAN9  LA    RF,1(RF)                                                         
         BCT   R0,TESTAN3                                                       
         LM    RE,R6,SVREG                                                      
         BR    RE                                                               
*                                                                               
TESTANT  DC    C'90ZSRJIA '                                                     
*                                                                               
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE TRANSACTION DATE FIELD                                    *          
**********************************************************************          
VALDAT   NTR1  ,                                                                
         L     R2,ADATH            DATE FIELD                                   
         CLI   5(R2),0                                                          
         BNE   VALDDAT2                                                         
         GOTOR VDATCON,DMCB,(5,0),(0,WORK) DEFAULT TO CURRENT DATE              
         B     VALDDAT4                                                         
*                                                                               
VALDDAT2 GOTO1 DATVAL,DMCB,(0,8(R2)),WORK  EDIT DATE INPUT                      
         OC    DMCB(4),DMCB                                                     
         JZ    EMINDAT             'INVALID DATE'                               
*                                                                               
VALDDAT4 GOTO1 DATCON,DMCB,(0,WORK),(1,DOCDATE) PACK IT Y/M/D                   
         CLC   DOCDATE,BCTDATL                                                  
         JL    EMDOPSP             'DATE OUTSIDE PERMITTED SPAN                 
         CLC   DOCDATE,BCTDATH                                                  
         JH    EMDOPSP                                                          
         OI    4(R2),FINPVAL                                                    
         GOTO1 DATCON,DMCB,(1,DOCDATE),(8,8(R2))                                
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE DUE DATE FIELD                                            *          
**********************************************************************          
VALDUE   NTR1  ,                                                                
         ICM   R2,15,ADUEH         DUE DATE FIELD                               
         JZ    XIT                                                              
         XC    DODATE,DODATE                                                    
         TM    FLG,AUTODUE                                                      
         BO    VALDUE3                                                          
         CLI   5(R2),0                                                          
         BE    VALDUE3                                                          
         B     VALDUE9                                                          
*                                                                               
VALDUE3  CLC   DODATEP,DODATEX     TEST DUE DATE THE SAME                       
         BNE   VALDUE5             NO,                                          
         OC    DODATEP,DODATEP     TEST BOTH ZERO                               
         JZ    XIT                 YES,  NO DEFAULT                             
         LA    R3,DODATEP          GET DEFAULT                                  
         B     VALDUE7                                                          
*                                                                               
VALDUE5  LA    R3,DODATEP          GET DEFAULT                                  
         OC    DODATEX,DODATEX                                                  
         BZ    VALDUE7                                                          
         LA    R3,DODATEX                                                       
         OC    DODATEP,DODATEP                                                  
         BZ    VALDUE7                                                          
         XC    FLD,FLD                                                          
         BRAS  RE,MOVEFLD                                                       
         J     EMDIFDU             DIFFERENT DO DATES                           
*                                                                               
VALDUE7  GOTOR DATCON,DMCB,(2,0(R3)),(8,8(R2))                                  
         OI    FLG,AUTODUE         SET AUTO DUE DATE                            
*                                                                               
VALDUE9  GOTO1 DATVAL,DMCB,(0,8(R2)),WORK                                       
         OC    DMCB(4),DMCB                                                     
         JZ    EMINDAT             'INVALID DATE'                               
*                                                                               
         GOTO1 DATCON,DMCB,(0,WORK),(1,FULL) PACK IT Y/M/D                      
         GOTO1 (RF),(R1),,(8,8(R2))                                             
         CLC   FULL(3),DOCDATE                                                  
         JL    EMDOPSP                                                          
         CLC   FULL(3),BCTDATL                                                  
         JL    EMDOPSP             'DATE OUTSIDE PERMITTED SPAN                 
         CLC   FULL(3),BCTDATH                                                  
         JH    EMDOPSP                                                          
         GOTO1 DATCON,DMCB,(0,WORK),(2,DODATE)                                  
         OI    4(R2),FINPVAL                                                    
         J     XIT                                                              
         EJECT                                                                  
***********************************************************************         
* VALIDATE CASH ACCOUNT FIELD                                         *         
***********************************************************************         
VALCASH  NTR1  ,                                                                
         XC    CASHACCT(51),CASHACCT  VALIDATE CASH ACCT                        
         L     R2,ACSHH                                                         
         XR    R3,R3                                                            
         ICM   R3,1,5(R2)         LENGTH OF CASH ACCT                           
         JZ    XIT                                                              
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         MVC   KEY+1(2),=C'SC'     SET DEFAULT CASH U/L                         
         BCTR  R3,0                ADJUST LENGTH FOR EX                         
*                                                                               
         LA    R1,8(R2)                                                         
         CLI   0(R1),C'*'          TEST OVERRIDE                                
         BNE   VALCASH3            NO,                                          
         CHI   R3,3                                                             
         JL    EMINVIF             'INVALID INPUT FIELD'                        
         LA    R1,1(R1)                                                         
         LA    RF,CASHLIST        LIST OF OVERRIDES FOR CASH                    
*                                                                               
VALCASH1 CLI   0(RF),X'FF'        END OF LIST                                   
         JE    EMINACP            'INVALID ACCOUNT FOR POSTING '                
         CLC   0(2,R1),0(RF)      INPUT TO LIST                                 
         BE    VALCASH2                                                         
         LA    RF,2(RF)           NEXT VALID ENTRY TO CREDIT                    
         B     VALCASH1                                                         
*                                                                               
VALCASH2 MVC   KEY+1(2),0(R1)     2 CHAR U/L FROM INPUT                         
         SHI   R3,3               SUBTRACT *U/L FROM INPUT LEN                  
         LA    R1,2(R1)           POINT TO ACCT INPUT                           
*                                                                               
VALCASH3 EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R1)      CASH ACCT CODE                               
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
         MVC   CASHACCT,ACCODE     CASH ACCT #                                  
         MVC   CASHNAME,ACNAME     CASH ACCT NAME                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACNAME),ACNAME                                             
         ICM   R2,15,ACSHNH                                                     
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         L     R2,AVCDH                                                         
         CLI   5(R2),0             IS THERE A PROD CD?                          
         JNE   EMCDALL             YES - FLAG ERROR                             
         L     R2,AXCDH                                                         
         CLI   5(R2),0             IS THERE AN EXP CD?                          
         JNE   EMCDALL             'NO CASH DISCOUNT ALLOWED'                   
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EDIT VENDOR FIELD                                                  *          
**********************************************************************          
VEN      NTR1  ,                                                                
         XR    R3,R3                                                            
         IC    R3,5(R2)           LENGTH OF CREDIT ACCOUNT INPUT                
         LA    R0,15                                                            
         CR    R3,R0                                                            
         BNL   *+6                                                              
         LR    R0,R3                                                            
*                                                                               
         XR    R3,R3                                                            
         LA    RF,8(R2)                                                         
VEN4     CLC   0(2,RF),=C' <'                                                   
         BE    VEN6                                                             
         LA    R3,1(R3)                                                         
         LA    RF,1(RF)                                                         
         BCT   R0,VEN4                                                          
VEN6     LTR   R3,R3                                                            
         JNP   EMMISIF                                                          
         BCTR  R3,0                ADJUST LENGTH OF ACC. FOR EX                 
         MVC   VENDIN,SPACES                                                    
         EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   VENDIN(0),8(R2)        SAVE VENDOR INPUT                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'VENDIN),VENDIN                                             
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         LA    R1,8(R2)                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),COMPANY                                                   
         CLI   8(R2),C'*'         UNIT LEDGER INPUT OVERRIDE                    
         BE    VEN10              YES                                           
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         CLI   TYPE,C'P'          IS IT PROD VENDOR?                            
         BNE   VEN8               NO                                            
         MVC   KEY+1(2),CPYSUPP   ASSUME UNLESS OVERRIDDEN                      
         B     VEN18                                                            
*                                                                               
VEN8     MVC   KEY+1(1),=C'S'                                                   
         MVC   KEY+2(1),CPYSUPX   ASSUME UNLESS OVERRIDDEN                      
         B     VEN18                                                            
         DROP  RF                                                               
*                                                                               
VEN10    CLI   CASHACCT,0         IS THERE A CASH ACCOUNT?                      
         BNE   VEN12              YES - *SC NOT ALLOWED                         
         CLC   9(2,R2),=C'SC'     IS IT CASH ACCT?                              
         BE    VEN16                                                            
*                                                                               
VEN12    LA    RF,ADVCLIST        LIST OF OVERRIDES FOR PROD VEND               
         CLI   TYPE,C'P'          IS IT PRODUCTION VENDOR?                      
         BE    VEN14              YES                                           
         LA    RF,AGYCLIST        LIST OF OVERRIDES FOR EXP VEND                
*                                                                               
VEN14    CLI   0(RF),X'FF'        END OF LIST                                   
         JE    EMINACP            'INVALID ACCOUNT FOR POSTING '                
         CLC   9(2,R2),0(RF)      INPUT TO LIST                                 
         BE    VEN16                                                            
         LA    RF,2(RF)           NEXT VALID ENTRY TO CREDIT                    
         B     VEN14                                                            
*                                                                               
VEN16    MVC   KEY+1(2),9(R2)     2 CHAR U/L FROM INPUT                         
         SHI   R3,3               SUBTRACT *U/L FROM INPUT LEN                  
         LA    R1,11(R2)          POINT TO ACCT INPUT                           
VEN18    EX    R3,*+8                                                           
         B     *+10                                                             
         MVC   KEY+3(0),0(R1)     CREDIT ACCT TO KEY                            
*                                                                               
         CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   VEN20              NO                                            
         L     R1,AVCDH           CD FOR PROD VENDOR                            
         B     *+8                                                              
*                                                                               
VEN20    L     R1,AXCDH           CD FOR EXP VENDOR                             
         CLI   5(R1),0            INPUT                                         
         BE    VEN22              NO                                            
         CLI   CASHACCT,0         IS THERE A CASH ACCOUNT?                      
         JNE   EMCDALL            'NO CASH DISCOUNT ALLOWED'                    
*                                                                               
         CLI   8(R1),C'N'         IS THERE CASH DISCOUNT                        
         BE    VEN22              NO GO GET ACCT                                
         CLI   8(R1),C'Y'                                                       
         BE    VEN22                                                            
         LR    R2,R1                                                            
         J     EMINVIF            'INVALID INPUT FIELD'                         
*                                                                               
VEN22    MVC   RKEY,KEY           READ VENDOR FOR CD AND OTHER INFO             
         GOTO1 ARDHI,AIOAREA1                                                   
         L     RE,AIOAREA1                                                      
         CLC   KEYSAVE,0(RE)      WAS REC RETRIEVED                             
         BE    VEN24                                                            
         J     EMINACC            'INVALID ACCOUNT'                             
*                                                                               
VEN24    LA    R0,KEY             MOVE RECORD TO LOCAL STORAGE                  
         LA    R1,2000                                                          
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,GETVEN          GET VENDOR DATA                               
         BRAS  RE,GETDUE          GET THE DUE DATE                              
         TM    SCRSTA,SCRCANQ     TEST CANADIAN                                 
         BNO   *+8                                                              
         BAS   RE,VALCAN           VALIDATE CANADIAN                            
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
*                                                                               
* PRODUCTION VENDOR FINISH                                                      
*                                                                               
         CLI   TYPE,C'P'          IS THIS A PROD VENDOR?                        
         BNE   VEN36              NO                                            
         L     R2,AVENH                                                         
         CLI   5(R2),0            WAS PROD VENDOR INPUT?                        
         JE    XIT                NO                                            
         MVC   VENDACCT,ACCODE    SAVE PROD VENDOR KEY                          
         MVC   VENDNAME,ACNAME    SAVE PROD VENDOR NAME                         
         MVC   PRSTAT,ACBSTAT     SAVE STATUS BYTE                              
*                                                                               
         CLI   CASHACCT,0         IS THERE A CASH ACCT?                         
         BE    VEN34              NO - DON'T WANT 2C OR 27                      
         TM    PRSTAT,ACBSVEND    DO WE NEED 2C FOR PROD VENDOR?                
         BZ    VEN34              NO.                                           
         MVC   KEY(15),VENDACCT   ACCT #                                        
         MVC   KEY+1(2),=C'2C'    U/L                                           
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         MVI   PCONSULT,C'Y'      SET 2C SWITCH                                 
         MVC   P2CNUM,ACCODE                                                    
         MVC   P2CNAM,ACNAME                                                    
         MVC   KEY+1(14),=CL14'27999'  BUILD 27 ACCT                            
         GOTOR AGETACC,DMCB,KEY,(R6)                                            
         JNE   EXIT                                                             
         MVC   PROTROL,ACCODE                                                   
         MVC   PROTROLN,ACNAME                                                  
*                                                                               
VEN34    BAS   RE,NAMADR           SET NAME /ADDRESS                            
         ICM   R2,15,AVENH         DISPLAY CODE/NAME/ADDRESS                    
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
*                                                                               
* EXPENSE VENDOR FINISH                                                         
*                                                                               
VEN36    L     R2,AXVNH           WAS EXP VENDOR INPUT?                         
         CLI   5(R2),0                                                          
         JE    XIT                                                              
         MVC   XVNDACCT,ACCODE    EXP VENDOR ACCT #                             
         MVC   XVNDNAME,ACNAME    EXP VENDOR NAME                               
         MVC   EXSTAT,ACBSTAT     EXP 2C BIT (X'04')                            
         MVI   V29SW,0                                                          
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCULA,XVNDACCT  EXPENSE BVENDOR                               
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         JNE   EMINACC            'INVALID ACCOUNT...'                          
         L     R3,AIO1                                                          
         LA    R3,ACTRFST                                                       
         XR    RE,RE                                                            
         DROP  R4                                                               
*                                                                               
         USING RSTELD,R3          STATUS DSECT                                  
VEN38    CLI   0(R3),0            IS IT END OF RECORD?                          
         BE    VEN44                                                            
         CLI   0(R3),RSTELQ       IS IT X'30'?                                  
         BE    VEN40                                                            
         IC    RE,1(R3)                                                         
         AR    R3,RE                                                            
         B     VEN38                                                            
*                                                                               
VEN40    TM    RSTSTAT3,RSTSCAIV  DO WE NEED VENDOR AS CONTRA?                  
         BZ    VEN44              NO.                                           
         MVI   V29SW,C'Y'         YES.                                          
         DROP  R3                                                               
*                                                                               
VEN44    MVC   KEY(15),IOKEY      SAVE KEY                                      
         CLI   CASHACCT,0         IS THERE A CASH ACCT?                         
         BE    VEN48              NO - DON'T WANT 2C OR 27                      
         TM    EXSTAT,ACBSVEND    TEST NEED 2C VENDOR                           
         BZ    VEN48              NO.                                           
*                                                                               
         MVC   KEY(15),XVNDACCT                                                 
         MVC   KEY+1(2),=C'2C'                                                  
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         MVI   ECONSULT,C'Y'                                                    
         MVC   E2CNUM,ACCODE                                                    
         MVC   E2CNAM,ACNAME                                                    
         MVC   KEY+1(14),=CL14'27999'   BUILD 27                                
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
*                                                                               
         MVC   EXPTROL,ACCODE                                                   
         MVC   EXPTROLN,ACNAME                                                  
*                                                                               
VEN48    BAS   RE,NAMADR           SET NAME /ADDRESS                            
         ICM   R2,15,AXVNH         DISPLAY CODE/NAME/ADDRESS                    
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* FORMAT NAME/ADDRESS FOR DISPLAY                                    *          
**********************************************************************          
NAMADR   LR    R0,RE                                                            
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
         LA    R3,BOELEM                                                        
         MVC   0(L'VENDIN,R3),VENDIN                                            
         LA    R3,L'VENDIN+2(R3)                                                
         MVI   0(R3),C'<'                                                       
         LA    RE,VENDNAME                                                      
         CLI   TYPE,C'P'                                                        
         BE    *+8                                                              
         LA    RE,XVNDNAME                                                      
         MVC   1(VNDRLNQ,R3),0(RE)                                              
         LA    R3,VNDRLNQ(R3)                                                   
         CLI   0(R3),C' '                                                       
         BH    *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         MVI   1(R3),C'>'                                                       
         MVI   2(R3),C'<'                                                       
         LA    R3,3(R3)                                                         
         MVC   0(ADDRLNQ,R3),ADRLIN1                                            
         LA    R3,ADDRLNQ(R3)                                                   
         CLI   0(R3),C' '                                                       
         BH    *+10                                                             
         BCTR  R3,0                                                             
         B     *-10                                                             
         MVI   1(R3),C'>'                                                       
         GOTO1 VSQUASH,DMCB,BOELEM,L'BOELEM                                     
         MVC   FLD,BOELEM                                                       
         LA    R3,VENDLONG         SAVE LONGER VERSION FOR LIST                 
         CLI   TYPE,C'P'                                                        
         BE    *+8                                                              
         LA    R3,XVNDLONG                                                      
         MVC   0(L'VENDLONG,R3),BOELEM                                          
         LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* COPY THE PRODUCTION VENDOR TO EXPENSE VENDOR                       *          
**********************************************************************          
COPYVEN  NTR1  ,                                                                
         L     R1,AVENH                                                         
         GOTO1 AFVAL                                                            
         JNE   XIT                 NOTHING TO COPY                              
         MVC   FLD,FVIFLD                                                       
         L     R2,AXVNH                                                         
         TM    FVIHDR+4,FINPVAL    TEST IF PRODUCTION VENDOR CHANGED            
         BZ    *+12                YES                                          
         CLI   5(R2),0             TEST FOR EMPTY EXPENSE VENDOR                
         JNE   XIT                 NO-ASSUME ITS OK NOW                         
         MVC   5(1,R2),FVILEN      SET NEW INPUT LENGTH                         
         NI    4(R2),X'FF'-(FINPVAL)                                            
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* GET VENDOR RECORD DATA                                             *          
**********************************************************************          
GETVEN   NTR1  ,                                                                
         XC    ADEXEL,ADEXEL                                                    
         MVC   ADRLIN1,SPACES      ADDRESS LINE ONE                             
         MVI   ADRLIN1L,0                                                       
         LA    R4,IOAREA           FIND SOME VENDOR DATA                        
*                                                                               
GETVEN2  CLI   0(R4),0                                                          
         BNE   GETVEN6             DIDN'T FIND DISCOUNT ELEMENT                 
         CLI   TYPE,C'P'           IS THIS A PROD VENDOR?                       
         BNE   GETVEN4             NO                                           
         L     R2,AVCDH                                                         
         J     XIT                 SKIP CD FOR PROD VENDOR                      
*                                                                               
GETVEN4  L     R2,AXCDH                                                         
         J     XIT                 SKIP CD FOR EXP VENDOR                       
*                                                                               
GETVEN6  CLI   0(R4),RATEDSCQ      IS THIS A DIS ELEM?                          
         BE    GETVEN10            YES                                          
         CLI   0(R4),ITCELQ        TEST FOR INPUT TAX DEFAULT                   
         BE    GETVEN16                                                         
         CLI   0(R4),ADRELQ        TEST ADDRESS ELEMENT                         
         BE    GETVEN18                                                         
         CLI   0(R4),DEXELQ        TEST DUE DATE ELEMENT                        
         BNE   GETVEN8                                                          
         ST    R4,ADEXEL           SAVE ADDRESS OF DUE ELEMENT                  
*                                                                               
GETVEN8  XR    R3,R3                                                            
         IC    R3,1(R4)                                                         
         AR    R4,R3                                                            
         B     GETVEN2                                                          
*                                                                               
GETVEN10 CLI   CASHACCT,0          TEST FOR CASH ACCOUNT                        
         BNE   GETVEN8             YES-NO CASH DISCOUNT ALLOWED                 
*                                                                               
         XR    R0,R0                                                            
         ICM   R0,3,2(R4)          TEST ANY CD                                  
         BZ    GETVEN8                                                          
         CVD   R0,DUB                                                           
         ZAP   PDISC,DUB                                                        
         CLI   TYPE,C'P'           IS THIS A PROD VENDOR?                       
         BNE   GETVEN12            NO                                           
         LA    R5,VENDDISC                                                      
         L     R6,AVCDNH                                                        
         L     R3,AVCDH                                                         
         B     GETVEN14            OUTPUT CD                                    
*                                                                               
GETVEN12 LA    R5,XVNDDISC                                                      
         L     R6,AXCDNH                                                        
         L     R3,AXCDH                                                         
*                                                                               
GETVEN14 CURED (P3,PDISC),(6,8(R6)),2                                           
         CLI   8(R3),C'N'          TEST WANT CD                                 
         BE    GETVEN8             NO,                                          
         CLI   8(R3),C'Y'                                                       
         BE    GETVEN15            YES,                                         
         CLI   8(R3),C' '                                                       
         BNH   GETVEN15            NO INPUT IS A YES                            
         LR    R2,R3                                                            
         J     EMINVIF             'INVALID INPUT...'                           
*                                                                               
GETVEN15 ZAP   0(L'VENDDISC,R5),DUB                                             
         B     GETVEN8                                                          
*                                                                               
         USING ITCELD,R4                                                        
GETVEN16 CLC   DOCDATE,ITCEFFD     TEST TRANS DATE >= EFF DATE                  
         BL    GETVEN8                                                          
         LA    RE,VENDCTX          SET POINTER TO DEFAULT TAX                   
         CLI   TYPE,C'P'           TEST FOR PRODUCTION VENDOR                   
         BE    *+8                 YES                                          
         LA    RE,XVNDCTX                                                       
         USING CTD,RE              TEST IF WE ALREADY HAVE DEFAULT              
         OC    ITCPROV,ITCPROV     TEST PST                                     
         BNZ   *+14                YES,                                         
         MVC   CTAGTY,ITCTYPE      SAVE GST TYPE                                
         B     GETVEN8                                                          
         MVC   CTAPTY,ITCTYPE      SAVE PST TYPE                                
         MVC   CTAPRV,ITCPROV           PROVINCE                                
         B     GETVEN8                                                          
         DROP  RE                                                               
*                                                                               
         USING ADRELD,R4                                                        
GETVEN18 MVC   ADRLIN1,ADRADD1     ADDRESS LINE 1                               
         GOTO1 VSQUASH,DMCB,ADRLIN1,L'ADRLIN1                                   
         MVC   ADRLIN1L,7(R1)                                                   
         SR    R0,R0                                                            
         IC    R0,ADRNUM           NUMBER OF LINES                              
         SHI   R0,1                                                             
         BNP   GETVEN8             ONLY ONE LINE                                
         LA    RF,ADRADD2          SAVE REST OF ADDRESS                         
         LA    R1,VENDADR2                                                      
         CLI   TYPE,C'P'                                                        
         BE    *+8                                                              
         LA    R1,XVNDADR2                                                      
         MVC   0(L'VENDADR2,R1),0(RF)                                           
         LA    R1,L'VENDADR2(R1)                                                
         LA    RF,L'ADRADD2(RF)                                                 
         BCT   R0,*-14                                                          
         B     GETVEN8                                                          
         EJECT                                                                  
**********************************************************************          
* VALIDATE CANADIAN FIELDS                                           *          
**********************************************************************          
VALCAN   NTR1  ,                                                                
         GOTO1 VDICTAT,DMCB,C'L',DICI,DICO                                      
*                                                                               
         ZAP   GSTAMNT,PZERO                                                    
         ZAP   PSTAMNT,PZERO                                                    
         OC    LSTGAM,LSTGAM       TEST FIRST TIME                              
         BNZ   *+16                NO,                                          
         ZAP   LSTGAM,PZERO        YES, INITIALIZE                              
         ZAP   LSTPAM,PZERO                                                     
*                                                                               
         L     R2,AGTYH            R2=A(GST TYPE FIELD)                         
         CLC   LSTGTY,8(R2)        TEST CHANGED TYPE                            
         BE    *+8                                                              
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
         MVC   LSTGTY,8(R2)                                                     
*                                                                               
         L     R2,APTYH            R2=A(PST TYPE FIELD)                         
         CLC   LSTPTY,8(R2)        TEST CHANGED TYPE                            
         BE    *+8                                                              
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
         MVC   LSTPTY,8(R2)                                                     
*                                                                               
         L     R2,APRVH                                                         
         CLC   LSTPRV,8(R2)        TEST CHANGED PROVINCE                        
         BE    *+8                                                              
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
         MVC   LSTPRV,8(R2)                                                     
*                                                                               
         L     R5,AVATBLK                                                       
         USING VTCD,R5                                                          
         XC    VTCD(VTCLNQ),VTCD   SET GST BLOCK                                
         MVI   VTCACTN,VTCAIVAL    SET VALIDATE INPUT TAX TYPE                  
         MVC   VTCCPY,COMPANY      COMPANY                                      
         MVC   VTCCOMF,ACOMFACS    COMFACS                                      
         MVC   VTCINVD,DOCDATE     INVOICE DATE                                 
*                                                                               
         USING CTD,R3                                                           
         LA    R3,VENDCTX          GET DEFAULT TYPE FOR PROD                    
         CLI   VENDACCT,0                                                       
         BE    *+8                                                              
         BAS   RE,VALCTX           VERIFY THRU VATICAN                          
*                                                                               
         LA    R3,XVNDCTX          GET DEFAULT TYPE FOR EXPENSE                 
         CLI   XVNDACCT,0                                                       
         BE    *+8                                                              
         BAS   RE,VALCTX                                                        
*                                                                               
         L     R2,AGAMH            GET GST INPUT AMOUNT                         
         ZAP   GSTAMNT,PZERO                                                    
         CLI   5(R2),0                                                          
         BE    VALCAN11                                                         
         GOTOR VALAMT,GSTAMNT                                                   
         OI    CTXSTAT,CTXSGSTI    SET GST INPUT                                
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
*                                                                               
VALCAN11 L     R2,APAMH            GET PST INPUT AMOUNT                         
         ZAP   PSTAMNT,PZERO                                                    
         CLI   5(R2),0                                                          
         BE    VALCAN13                                                         
         GOTOR VALAMT,PSTAMNT                                                   
         OI    CTXSTAT,CTXSPSTI    SET PST INPUT                                
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
*                                                                               
VALCAN13 DS    0H                                                               
         CP    GSTAMNT,LSTGAM      TEST CHANGED GST AMOUNT                      
         BE    *+8                                                              
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
         ZAP   LSTGAM,GSTAMNT                                                   
         CP    PSTAMNT,LSTPAM      TEST CHANGED GST AMOUNT                      
         BE    *+8                                                              
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
         ZAP   LSTPAM,PSTAMNT                                                   
*                                                                               
         ICM   R2,15,AGONH         GROSS OR NET                                 
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         BE    VALCAN15                                                         
         TM    CTXSTAT,CTXSPSTI+CTXSGSTI                                        
         BNZ   *+8                                                              
         J     EMMISIF                                                          
         MVI   8(R2),TXGNETQ       DEFAULT IS NET                               
*                                                                               
VALCAN15 MVC   TXGON,8(R2)                                                      
         CLI   LSTGON,0            TEST FIRST TIME                              
         BE    *+18                                                             
         CLC   LSTGON,TXGON        TEST CHANGED GROSS/NET                       
         BE    *+8                                                              
         OI    CTXSTAT,CTXRECAL    SET TO RECALCULATE ALL                       
         MVC   LSTGON,TXGON                                                     
         CLI   TXGON,TXGGRSQ       TEST GROSS                                   
         JE    XIT                                                              
         CLI   TXGON,TXGNETQ       NET                                          
         JE    XIT                                                              
         CLI   TXGON,TXGNONQ       OR NON                                       
         JNE   EMINVIF                                                          
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE THRU VATICAN                                              *          
**********************************************************************          
VALCTX   NTR1  ,                                                                
         L     R2,AGTYH            R2=A(GST TYPE FIELD)                         
         MVC   CTUGTY,8(R2)        SAVE INPUT VALUE                             
         CLI   8(R2),TXGNONQ       TEST NO TAX                                  
         BE    VALCTX1                                                          
         CLI   8(R2),C' '                                                       
         BH    *+14                                                             
         MVC   CTUGTY,CTAGTY       USE DEFAULT AS UPPER                         
         B     VALCTX1                                                          
*                                                                               
         STCM  R2,15,VTCAFLDH      USE INPUT TYPE                               
         XC    VTCPRV,VTCPRV                                                    
         GOTOR VATICAN,VTCD        CALL VATICAN FOR ** GST **                   
         JNE   EMINVGT             INVALID TYPE                                 
*                                                                               
         TM    VTCINDS,VTCINA      TEST TAX NOT APPLICABLE                      
         JO    EMGSTNA                                                          
*                                                                               
VALCTX1  L     R2,APTYH            R2=A(PST TYPE FIELD)                         
         STCM  R2,15,VTCAFLDH                                                   
         MVC   BCBYTE1,8(R2)       SAVE INPUT                                   
         CLI   8(R2),C' '          TEST PST TYPE INPUT                          
         BH    *+10                NO, USE DEFAULT                              
         MVC   8(1,R2),CTAPTY      USE ACCOUNT LEVEL                            
         MVC   CTUPTY,8(R2)        SAVE UPPER SCREEN                            
*                                                                               
         CLI   8(R2),TXGNONQ       TEST NO TAX                                  
         BNE   VALCTX3                                                          
         L     R2,APRVH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF             CAN'T HAVE PROVINCE                          
         B     VALCTX9                                                          
*                                                                               
VALCTX3  L     R2,APRVH                                                         
         MVC   BCHALF,8(R2)                                                     
         CLI   8(R2),C' '                                                       
         BH    VALCTX4                                                          
         CLI   CTAPRV,C' '         TEST DEFAULT PROVINCE                        
         BH    *+12                                                             
         OI    CTXSTAT,CTXNOPRV    SET 'NO PROVINCE'                            
         B     VALCTX9                                                          
*                                                                               
         MVC   8(2,R2),CTAPRV      USE ACCOUNT LEVEL                            
*                                                                               
VALCTX4  L     RE,APRVTAB          VALIDATE PROVINCE CODE                       
VALCTX5  CLC   0(2,RE),8(R2)                                                    
         BE    VALCTX7                                                          
         LA    RE,L'PRVTAB(RE)                                                  
         CLI   0(RE),EOT                                                        
         BNE   VALCTX5                                                          
*                                                                               
         L     RF,APTYH                                                         
         MVC   8(1,RF),BCBYTE1     RESTORE INPUT TYPE                           
         J     EMPROVX             'PROVINCE DOES NOT EXIST'                    
*                                                                               
VALCTX7  MVC   VTCPRV,0(RE)                                                     
         GOTOR VATICAN,VTCD        CALL VATICAN FOR ** PST **                   
         MVC   CTUPRV,VTCPRV       SAVE UPPER SCREEN PROVINCE                   
*                                                                               
VALCTX8  L     R2,APRVH                                                         
         MVC   8(2,R2),BCHALF      RESTORE INPUT PROVINCE                       
         L     R2,APTYH                                                         
         MVC   8(1,R2),BCBYTE1     RESTORE INPUT TYPE                           
         JNE   EMINVPT             INVALID TYPE                                 
*                                                                               
VALCTX9  DS    0H                                                               
         J     XIT                                                              
         DROP  R5,R3                                                            
         EJECT                                                                  
ADVCLIST DS    0H    PRD. U/L'S VALID TO CREDIT                                 
         DC    C'SXSWSYSBSA'       SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
*                                                                               
AGYCLIST DS    0H    EXP. U/L'S VALID TO CREDIT                                 
         DC    C'SVSWSXSYSBSASFSL' SPACE OK-CASH NOT ALLOWED                    
         DC    X'FF'                                                            
*                                                                               
CASHLIST DS    0H     OVERRIDES OF CASH ACCOUNT                                 
         DC    C'SB'                                                            
         DC    C'SG'                                                            
         DC    X'FF'                                                            
*                                                                               
DICI     DS    0X                                                               
         DCDDL AC#PST,3                                                         
         DCDDL AC#QST,3                                                         
         DCDDL AC#ONT,3                                                         
         DCDDL AC#HST,3                                                         
         DC    AL1(EOT)                                                         
         DROP  R4                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS A DETAIL LINE                                              *          
**********************************************************************          
PROC     NMOD1 0,**PROC**,R8                                                    
         L     RC,0(R1)                                                         
         L     R2,AWKCH            IS THERE A WORKCODE ?                        
         CLI   5(R2),0             TEST FOR NO INPUT                            
         BE    PROC10              NO, ASSUME EXPENSE ITEM                      
         MVI   TYPE,C'P'           NOTE PRODUCTION ITEM                         
*                                                                               
PROC4    L     R2,AVENH                                                         
         CLI   VENDACCT,0          IS THERE AN PROD VENDOR?                     
         BNE   PROC9               YES                                          
         TM    BCCPYST2,CPYSVENR   IS PROD VEND REQUIRED?                       
         JO    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         JE    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
*                                                                               
PROC9    BAS   RE,PRODET           YES, EDIT LINE FOR ADV. EXP.                 
         B     PROC20                                                           
*                                                                               
PROC10   L     R2,AXVNH                                                         
         CLI   XVNDACCT,0          IS THERE AN EXP VENDOR?                      
         BNE   PROC15              YES                                          
         TM    BCCPYST2,CPYSVENR   IS PROD VEND REQUIRED?                       
         JO    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
         CLI   CASHACCT,0          IS THERE A CASH ACCT                         
         JE    EMVMEXP             'VENDOR INPUT MISSING FOR THIS ...'          
*                                                                               
PROC15   MVI   TYPE,C'E'                                                        
         BAS   RE,EXPDET           EDIT LINE FOR AGY. EXP.                      
*                                                                               
PROC20   LA    R3,LOWER            R3=A(FIELD POINTER)                          
         LA    R0,NLOWER-1         R0=LOOP COUNTER                              
*                                                                               
PROC22   ICM   R2,15,0(R3)                                                      
         BZ    PROC24                                                           
         TM    1(R2),FATBPROT                                                   
         BO    PROC24                                                           
         OI    4(R2),FINPVAL                                                    
PROC24   LA    R3,4(R3)                                                         
         BCT   R0,PROC22                                                        
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* PRODUCTION DETAIL ITEM                                             *          
**********************************************************************          
PRODET   NTR1  ,                                                                
         L     R2,AJOBH                                                         
         L     R2,APROH                                                         
         L     R2,ACLIH                                                         
         TM    4(R2),FINPVAL       TEST IF FIELD CHANGED                        
         BO    PRODET1                                                          
*                                                                               
         L     R1,APROH                                                         
         NI    4(R1),X'FF'-(FINPVAL) TURN OFF PREVIOUS VALID                    
         L     R1,AJOBH                                                         
         NI    4(R1),X'FF'-(FINPVAL)                                            
*                                                                               
PRODET1  BRAS  RE,DUP              TEST FOR DUPLICATION                         
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT...'                           
         BAS   RE,VALCLI                                                        
         OI    4(R2),FINPVAL       SET VALID                                    
*                                                                               
         L     R2,APROH                                                         
         TM    4(R2),FINPVAL       TEST IF FIELD CHANGED                        
         BO    PRODET3                                                          
         L     R1,AJOBH                                                         
         NI    4(R1),X'FF'-(FINPVAL)                                            
*                                                                               
PRODET3  BRAS  RE,DUP                                                           
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT...'                           
         BAS   RE,VALPRO                                                        
         OI    4(R2),FINPVAL                                                    
*                                                                               
         L     R2,AJOBH                                                         
         BRAS  RE,DUP                                                           
         BAS   RE,VALJOB                                                        
*                                                                               
         OI    4(R2),FINPVAL                                                    
         GOTO1 AGETJOB,DMCB,JOBACCT  CALL GETOPT                                
         ORG   *-2                                                              
         L     RE,AWKCH                                                         
         LA    RE,8(RE)                                                         
         CLI   0(RE),C'*'                                                       
         BE    *+12                                                             
         ST    RE,4(R1)                                                         
         MVI   0(R1),X'80'         PASSING WORKCODE                             
         BASR  RE,RF                                                            
*                                                                               
         GOTO1 AVALGOB             TEST GOBATCH OPTION                          
         JNE   EXIT                ERROR                                        
*                                                                               
         L     R2,AWKCH            =A(CURRENT WORK CODE INPUT)                  
         BRAS  RE,DUP                                                           
         GOTOR VALWC,DMCB,AWKCH                                                 
*                                                                               
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
         L     R5,AGOXBLK                                                       
         USING GOXBLKD,R5                                                       
         MVC   TAXOFF,GOEFFOFC     GET TAX OFFICE                               
         MVC   TAXWKC,GOTXWC       AND WORK CODE                                
         MVI   PASSCD,C'Y'                                                      
         NI    ITMSTA,X'FF'-ITMXJOB                                             
         TM    ACOPSTAT,ACOXJOB    IS THIS AN X-JOB?                            
         BNO   *+12                                                             
         OI    ITMSTA,ITMXJOB      SET X-JOB                                    
         B     PRODET7             LEAVE PASSCD AS Y                            
*                                                                               
         MVI   PASSCD,C'N'                                                      
         CLI   COCDPASS,C'N'       IS COMP. KEEPING ALL CD'S                    
         BE    *+10                YES                                          
         MVC   PASSCD,GOCLICD                                                   
         DROP  R4,R5                                                            
*                                                                               
PRODET7  L     R2,ACOFH            CREDIT OFFICE                                
         BRAS  RE,DUP                                                           
         BAS   RE,COFF                                                          
*                                                                               
         L     R2,AAMTH                                                         
         BRAS  RE,DUP                                                           
         GOTOR VALAMT,INAMNT                                                    
         ICM   R3,15,ABASH         TEST BASIS ON SCREEN                         
         BZ    PRODET10            NO,                                          
         CLI   5(R3),0             TEST BASIS INPUT                             
         BNE   PRODET9             YES,                                         
         MVC   FLD,SPACES          DISPLAY AMOUNT IN BASIS FIELD                
         XR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         STC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R2)                                                     
         ICM   R2,15,ABASH         TEST BASIS ON SCREEN                         
         BRAS  RE,MOVEFLD          MOVE AMOUNT TO BASIS                         
*                                                                               
PRODET9  ICM   R2,15,ABASH         TEST BASIS ON SCREEN                         
         GOTOR VALAMT,BASAMNT                                                   
*                                                                               
PRODET10 BAS   RE,XJOB             X-JOB FIELDS EDIT                            
         CLI   CHECK,C' '          CHECKING AMOUNT ?                            
         BE    PRODET20            NO                                           
         MVC   WORK(2),WRKCODE                                                  
         ZAP   WORK+2(6),INAMNT                                                 
         GOTO1 AWRKVAL,DMCB,WORK                                                
         JH    EMAEEWC             'AMOUNT EXCEEDS ESTIMAT...'                  
*                                                                               
PRODET20 TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         BNO   PRODET25                                                         
         TM    POSTSW,POSTVEN+POSTEXP TEST ANY VENDORS                          
         BZ    PRODET25               NO, SKIP GST/PST                          
*                                                                               
         GOTOR CTXR,DMCB,PROGD     GET GST/PST                                  
         B     PRODET27                                                         
*                                                                               
PRODET25 CLI   AGYCTRY,CTRYCAN     TEST CANADIAN AGENCY                         
         BE    PRODET27            YES, NO USE TAX                              
         OC    ABASH,ABASH         ANY TAX BASIS FIELDS ?                       
         BZ    PRODET27            NO,SKIP TAX EDIT                             
         GOTOR USET,DMCB,PROGD     GET USE TAX                                  
*                                                                               
PRODET27 CP    VENDDISC,PZERO      SAVED DISCOUNT                               
         BE    PRODET29            NONE,                                        
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   PRODET29            YES - NO CD ALLOWED                          
         ZAP   PL13,INAMNT         CALCULATE CD                                 
         CLI   TXGON,TXGGRSQ       TEST CANADIAN - GROSS                        
         BNE   *+16                                                             
         SP    PL13,ITMGST         CD IS LESS GST AND PST                       
         SP    PL13,ITMPST                                                      
         MP    PL13,VENDDISC                                                    
         SRP   PL13,64-4,5         ROUNDED DIVIDE BY 10,000                     
         ZAP   CDAMNT,PL13                                                      
*                                                                               
PRODET29 CLI   PASSCD,C'N'         PASSING CD TO CLIENT                         
         BNE   PRODET31            YES - REDUCE PAYABLE                         
         CP    CDAMNT,PZERO        TEST ANY CD                                  
         BE    PRODET31            NO, SKIP ACCOUNT CHECK                       
         BAS   RE,VALCD            FIND/VALIDATE CASH DISCOUNT A/C              
*                                                                               
PRODET31 TM    POSTSW,POSTBUF      TEST TIME TO POST                            
         JNO   XIT                 SET GOOD RETURN                              
         GOTOR PPOST,DMCB,PROGD                                                 
         OC    LASTORD,LASTORD     TEST ORDER NUMBER INPUT                      
         BZ    *+8                                                              
         BAS   RE,UPDORD           UPDATE THE ORDER                             
*                                                                               
         XR    R0,R0                                                            
         IC    R0,NPOST            COUNT NUMBER POSTED                          
         AHI   R0,1                                                             
         STC   R0,NPOST                                                         
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* EXPENSE DETAIL ITEM                                                *          
**********************************************************************          
EXPDET   NTR1  ,                                                                
         XC    CLIPROF,CLIPROF                                                  
         XC    PRODPROF,PRODPROF                                                
         XC    JOBPROF,JOBPROF                                                  
         ICM   R2,15,AWKCH                                                      
         BZ    EXPD01                                                           
         CLI   5(R2),0             TEST WORKCODE INPUT                          
         BE    EXPD01              NO,                                          
         ICM   R3,15,AJOBH         YES, THEN MUST HAVE A JOB                    
         JZ    EMINVIF                                                          
         CLI   5(R3),0                                                          
         JE    EMINVIF                                                          
*                                                                               
EXPD01   BAS   RE,EDEXP                                                         
         L     R2,ADOFH                                                         
         BRAS  RE,DUP                                                           
*                                                                               
         CLC   8(2,R2),=C'++'      SUPPLY THE OFFICE?                           
         BNE   EXPD12              NO                                           
         TM    BCCPYST5,CPYSNCST   YES, ON NEW COST?                            
         BO    EXPD02              YES                                          
*                                                                               
         MVI   COSTSW,C'N'                                                      
         TM    BCCPYST1,CPYSCOST   TEST COST ACCOUNTING                         
         BZ    EXPD04                                                           
         CLI   COSTANAL,C' '                                                    
         BE    EXPD04                                                           
         MVI   COSTSW,C'Y'                                                      
         B     EXPD04                                                           
*                                                                               
EXPD02   L     R1,ACATD                                                         
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,POSTACC                                                  
         MVC   CATDPT,DEPT                                                      
         GOTO1 VCATCALL                                                         
         MVC   COSTSW,CATPST                                                    
         DROP  R1                                                               
*                                                                               
EXPD04   CLI   COSTSW,C'Y'         TEST FOR ANALYZED BY CLIENT                  
         JNE   EMNAWTT             'NOT ALLOWED WITH THIS TYPE'                 
         L     R2,ACLIH            IF ++ VALID, WE NEED THE CLIENT              
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT FIELD'                        
         BAS   RE,VALCLI                                                        
         OI    4(R2),FINPVAL                                                    
*                                                                               
         L     R2,APROH            AND THE PRODUCT                              
         CLI   5(R2),0                                                          
         JE    EMMISIF             'MISSING INPUT FIELD'                        
         BAS   RE,VALPRO                                                        
         OI    4(R2),FINPVAL                                                    
*                                                                               
         GOTOR APRFMRGE                                                         
         LA    R4,PROFILE                                                       
         USING PPRELD,R4                                                        
         MVC   PRDOFF,PPRGAOFF     GET CLIENT/PRODUCT OFFICE CODE               
         L     R2,ADOFH            AND MOVE TO SCREEN                           
         MVC   FLD(L'PRDOFF),PRDOFF                                             
         BRAS  RE,MOVEFLD                                                       
         MVC   FLD(L'PRDOFF),PRDOFF                                             
         BRAS  RE,MOVEFLD                                                       
*                                                                               
EXPD12   BAS   RE,FOFF                                                          
*                                                                               
         L     R2,ACOFH                                                         
         BRAS  RE,DUP                                                           
         BAS   RE,COFF                                                          
*                                                                               
         L     R2,AAOFH                                                         
         BRAS  RE,DUP                                                           
         BAS   RE,AOFF                                                          
*                                                                               
         L     R2,ADPTH                                                         
         BRAS  RE,DUP                                                           
         BAS   RE,DEP              EDIT DEPARTMENT/VALIDATE ACCOUNTS            
*                                                                               
         TM    BCCPYST5,CPYSNCST   TEST NEW COST ACCOUNTING                     
         BO    EXPD14              YES                                          
         MVI   COSTSW,C'N'                                                      
         TM    BCCPYST1,CPYSCOST   TEST COST ACCOUNTING                         
         BZ    EXPD20                                                           
         CLI   COSTANAL,C' '                                                    
         BE    EXPD20                                                           
         MVI   COSTSW,C'Y'                                                      
         B     EXPD18                                                           
*                                                                               
EXPD14   L     R1,ACATD                                                         
         USING CATD,R1                                                          
         XC    CATD(CATLNQ),CATD                                                
         MVC   CATDMGR,DATAMGR                                                  
         MVC   CATSEAC,POSTACC                                                  
         MVC   CATOFF,ANAOFF                                                    
         CLI   ANAOFF,C' '                                                      
         BH    *+10                                                             
         MVC   CATOFF,FINOFF                                                    
         MVC   CATDPT,DEPT                                                      
         GOTO1 VCATCALL                                                         
         MVC   COSTSW,CATPST                                                    
         L     R2,AXACH                                                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'ACTKCULA),CATACC3 EXTRACT KEY FOR ERROR                    
         CLI   CATERR,0                                                         
         JNE   EMIANAL             'INVALID ANALYSIS ACCOUNT'                   
*                                                                               
         CLI   CATPST,C'N'         TEST TO POST                                 
         BE    EXPD18                                                           
         MVC   COSTANAL,CATCDE     SAVE CATEGORY CODE                           
         MVC   CR13NUM,CATACC3     AND 13 ACCOUNT                               
         DROP  R1                                                               
*                                                                               
EXPD18   CLI   COSTSW,C'Y'         TEST FOR COST ACCOUNTING                     
         BNE   EXPD20                                                           
         BAS   RE,COST             VALIDATE COST ACCOUNTING ACCOUNTS            
         L     R2,ACLIH            REQUIRE CLIENT INPUT                         
         CLI   5(R2),0             TEST FOR INPUT                               
         JE    EMMISIF             'MISSING INPUT'                              
*                                                                               
         TM    BCCPYST5,CPYSEXPP   TEST IF PRODUCT ALSO REQUIRED                
         BZ    EXPD20                                                           
         L     R2,APROH            YES-MAKE SURE ITS THERE                      
         CLI   5(R2),0             TEST FOR INPUT                               
         BNE   EXPD20                                                           
         J     EMMISIF             'MISSING INPUT'                              
*                                                                               
EXPD20   L     R2,APERH                                                         
         BRAS  RE,DUP                                                           
         TM    ANLSTA,ANLSTF       TEST FOR STAFF ANALYSIS                      
         BO    EXPD22                                                           
         CLI   5(R2),0             NO STAFF ANALYSIS--FORCE                     
         JNE   EMANFST             'ACCOUNT NOT FLAGGED FOR STAFF'              
*                                                                               
EXPD22   TM    ANLSTA,ANLSTF       TEST FOR STAFF ANALYSIS                      
         BO    EXPD24              YES                                          
         CLI   COSTSW,C'Y'         TEST COST ACCOUNTING                         
         BE    EXPD24              YES                                          
*                                                                               
         L     R2,ACLIH            THEN NO INPUT TO CLI/PRO                     
         CLI   5(R2),0                                                          
         JNE   EMINVIF             'INVALID INPUT FIELD'                        
         L     R2,APROH                                                         
         CLI   5(R2),0                                                          
         JE    EXPD26                                                           
         J     EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
EXPD24   L     R2,ACLIH                                                         
         CLI   5(R2),0                                                          
         BE    EXPD25                                                           
         BRAS  RE,DUP                                                           
         BAS   RE,VALCLI                                                        
*                                                                               
EXPD25   L     R2,APROH                                                         
         CLI   5(R2),0                                                          
         BE    EXPD26              NO PRODUCT                                   
         L     R3,ACLIH            TEST CLIENT IS INPUT                         
         CLI   5(R3),0                                                          
         BNE   *+8                 YES                                          
         J     EMINVIF             NO, PROD IS INVALID WITHOUT CLIENT           
         BRAS  RE,DUP                                                           
         BAS   RE,VALPRO                                                        
*                                                                               
EXPD26   L     R2,AJOBH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF             'INVALID INPUT FIELD'                        
*                                                                               
         CLI   COSTSW,C'Y'         TEST FOR COST ACCOUNTING                     
         BE    EXPD28              YES-MUST GET 1C ACCOUNT                      
         TM    ANLSTA,ANLSTF       TEST STAFF ANALYSIS                          
         BNO   EXPD30              NO-SKIP 1C ACCOUNT FETCH                     
         OC    CLICODE,CLICODE     TEST IF AT LEAST CLIENT INPUT                
         BZ    EXPD30              NO-SKIP 1C ACCOUNT FETCH                     
*                                                                               
EXPD28   GOTOR APRFMRGE                                                         
         XR    R6,R6               NO MORE PROFILES                             
         LA    R4,PROFILE          FIND 1/C ACCOUNT                             
         USING PPRELD,R4                                                        
         MVC   COSTNUM,PPRCOST                                                  
         BAS   RE,SET1C            HANDLE ANY OVERRIDES TO 1C A/C               
         MVC   KEY,SPACES                                                       
         MVC   KEY(15),COSTNUM                                                  
*                                                                               
         ICM   R2,15,AJOBH         CURSOR TO JOB                                
         CLI   5(R2),0             TEST ANY JOB                                 
         JNE   *+8                 YES,                                         
         ICM   R2,15,APROH         CURSOR TO PRODUCT                            
         CLI   5(R2),0             TEST ANY PRODUCT                             
         JNE   *+8                 YES,                                         
         ICM   R2,15,ACLIH         NO, USE CLIENT                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
         MVC   COSTNUM,ACCODE                                                   
         MVC   COSTNAME,ACNAME                                                  
         DROP  R4                                                               
*                                                                               
EXPD30   TM    ANLSTA,ANLSTF       TEST FOR STAFF ANALYSIS                      
         BNO   *+8                                                              
         BAS   RE,STF              YES-HANDLE STAFF AND 29 A/C                  
*                                                                               
         L     R2,AAMTH                                                         
         BRAS  RE,DUP              EDIT FOR REPEAT                              
         GOTOR VALAMT,INAMNT                                                    
         ZAP   TRANSAMT,INAMNT                                                  
         ICM   R3,15,ABASH         TEST BASIS ON SCREEN                         
         BZ    EXPD32              NO,                                          
         CLI   5(R3),0             TEST BASIS INPUT                             
         BNE   EXPD31              YES,                                         
         MVC   FLD,SPACES          DISPLAY AMOUNT IN BASIS FIELD                
         XR    R1,R1                                                            
         IC    R1,5(R2)                                                         
         STC   R1,5(R3)                                                         
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R2)                                                     
         ICM   R2,15,ABASH         TEST BASIS ON SCREEN                         
         BRAS  RE,MOVEFLD          MOVE AMOUNT TO BASIS                         
*                                                                               
EXPD31   ICM   R2,15,ABASH         TEST BASIS ON SCREEN                         
         GOTOR VALAMT,BASAMNT                                                   
*                                                                               
EXPD32   TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         BNO   EXPD34                                                           
         TM    POSTSW,POSTVEN+POSTEXP TEST ANY VENDORS                          
         BZ    EXPD34                 NO, SKIP GST/PST                          
         GOTOR CTXR,DMCB,PROGD     GET GST/PST                                  
         B     EXPD35                                                           
*                                                                               
EXPD34   CLI   AGYCTRY,CTRYCAN     TEST CANADIAN AGENCY                         
         BE    EXPD35              YES, NO USE TAX                              
         OC    ABASH,ABASH                                                      
         BZ    EXPD35                                                           
         GOTOR USET,DMCB,PROGD                                                  
*                                                                               
EXPD35   CP    XVNDDISC,PZERO      SAVED DISCOUNT                               
         BE    EXPD37                                                           
         ZAP   PL13,INAMNT         CALCULATE CD                                 
         CLI   TXGON,TXGGRSQ       TEST CANADIAN - GROSS                        
         BNE   *+16                                                             
         SP    PL13,ITMGST         CD IS LESS GST AND PST                       
         SP    PL13,ITMPST                                                      
         MP    PL13,XVNDDISC                                                    
         SRP   PL13,64-4,5          ROUNDED DIVIDE BY 10,000                    
         ZAP   CDAMNT,PL13                                                      
         BAS   RE,VALECD           VALIDATE EXPENSE CD ACCOUNT                  
*                                                                               
EXPD37   TM    POSTSW,POSTBUF      TEST TIME TO POST                            
         JNO   XIT                 SET GOOD RETURN                              
         GOTOR EPOST,DMCB,PROGD                                                 
         OC    LASTORD,LASTORD     TEST ORDER NUMBER INPUT                      
         BZ    *+8                                                              
         BAS   RE,UPDORD           UPDATE THE ORDER                             
         XR    R0,R0                                                            
         IC    R0,NPOST            COUNT NUMBER PSOTED                          
         AHI   R0,1                                                             
         STC   R0,NPOST                                                         
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE CLIENT FIELD                                              *          
**********************************************************************          
VALCLI   NTR1  ,                                                                
         L     R2,ACLIH                                                         
         BRAS  RE,GETCLI                                                        
         CLI   TYPE,C'E'           TEST EXPENSE TRANSACTION                     
         BE    VALCLI3             YES-SKIP LOCKED/SECURITY TEST                
         TM    ACBSTAT,ACBSLOCK    TEST IF LOCKED                               
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
*                                                                               
         CLC   TWAACCS(2),SPACES TEST ANY OLD SECURITY                          
         BNH   VALCLI3             NO                                           
         CLI   TWAACCS,C'*'        TEST FOR SINGLE OFFICE                       
         BE    VALCLI3             YES                                          
         CLI   TWAACCS,C'$'        TEST FOR OFFICE LIST                         
         BE    VALCLI3                                                          
         CLC   TWAACCS(2),FVIFLD   MATCH ON CLIENT CODE                         
         JE    EMSECLK             'SECURITY LOCKOUT'                           
*                                                                               
VALCLI3  MVC   FLD,SPACES                                                       
         MVC   FLD(L'CLINAME),CLINAME                                           
         ICM   R2,15,ACLINH        CLIENT NAME                                  
         BZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         LA    R1,FFTELEM          BUILD FFTEL                                  
         USING FFTELD,R1                                                        
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,FFTLN1Q+L'FFTDLEN+L'FFTCLPRA                               
         MVI   FFTTYPE,FFTTCLPR                                                 
         MVI   FFTSEQ,0                                                         
         MVI   FFTDLEN,L'FFTCLPRA                                               
         MVC   FFTCLAC,CLIPRO+3                                                 
         OC    FFTCLPRA,SPACES                                                  
         J     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE PRODUCT FIELD                                             *          
**********************************************************************          
VALPRO   NTR1  ,                                                                
         L     R2,APROH                                                         
         BRAS  RE,GETPRO                                                        
         CLI   TYPE,C'E'           TEST EXPENSE ITEM                            
         BE    VALPRO3                                                          
         TM    ACBSTAT,ACBSLOCK    TEST LOCKED IF PRODUCTION                    
         JO    EMACTLK                                                          
*                                                                               
VALPRO3  MVC   FLD,SPACES                                                       
         MVC   FLD(L'PRONAME),PRONAME                                           
         ICM   R2,15,APRONH        PRODUCT NAME                                 
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         LA    R1,FFTELEM          UPDATE FFTEL                                 
         USING FFTELD,R1                                                        
         MVC   FFTPRAC,CLIPRO+6                                                 
         OC    FFTCLPRA,SPACES                                                  
         J     XIT                                                              
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE JOB FIELD                                                 *          
**********************************************************************          
VALJOB   NTR1  ,                                                                
         L     R2,AJOBH                                                         
         BRAS  RE,GETJOB                                                        
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSCLSE                                                 
         JO    EMACTCL             'ACCOUNT IS CLOSED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'JOBNAME),JOBNAME                                           
         ICM   R2,15,AJOBNH        JOB NAME                                     
         BZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* SET OPTION DEFAULTS AND VALIDATE XJOB FIELDS                       *          
**********************************************************************          
XJOB     NTR1  ,                                                                
         TM    ITMSTA,ITMXJOB      TEST FOR XJOB                                
         BO    XJOB1               YES                                          
*                                                                               
         L     R2,AXACH            FOR REGULAR JOB, DO NOT ALLOW INPUT          
         CLI   5(R2),0             IN XJOB FIELDS                               
         JNE   EMINVIF             'INVALID INPUT...'                           
*                                                                               
         L     R2,ADOFH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
*                                                                               
         L     R2,AAOFH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
*                                                                               
         L     R2,ADPTH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
*                                                                               
         L     R2,APERH                                                         
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
         J     XIT                                                              
*                                                                               
XJOB1    L     R2,AXACH                                                         
         BRAS  RE,DUP                                                           
         L     R4,AGOXBLK                                                       
         USING GOXBLOCK,R4                                                      
         L     R2,ADOFH                                                         
         CLI   5(R2),0                                                          
         BE    XJOB2                                                            
         CLC   8(2,R2),=C'++'                                                   
         JE    EMNAWTT             'NOT ALLOWED WITH THIS TYPE'                 
*                                                                               
XJOB2    L     R2,ADOFH            FINANCIAL OFFICE                             
         BRAS  RE,DUP                                                           
         CLI   5(R2),0                                                          
         BNE   XJOB3               SOMETHING INPUT                              
         MVI   5(R2),2             SET LENGTH                                   
         MVC   8(2,R2),GOAWOFOF    OPT MAINT OFFICE                             
         OC    8(2,R2),8(R2)                                                    
         BNZ   XJOB3                                                            
         MVC   8(2,R2),PRDOFF      OR PROD OFFICE                               
XJOB3    BAS   RE,FOFF                                                          
*                                                                               
         NI    FLG,X'FF'-(NOANLOFC)                                             
         L     R2,AAOFH            ANALYSIS  OFFICE                             
         BRAS  RE,DUP                                                           
         CLI   5(R2),0                                                          
         BNE   XJOB5               SOMETHING INPUT                              
         MVI   5(R2),2             SET LENGTH                                   
         OC    GOAWOAOF,GOAWOAOF                                                
         BZ    XJOB4                                                            
         MVC   8(2,R2),GOAWOAOF    OPT MAINT OFFICE                             
         B     XJOB5                                                            
*                                                                               
XJOB4    MVC   8(2,R2),FINOFF      USE FINANCIAL OFFICE                         
         OI    FLG,NOANLOFC        DON'T DISPLAY ANALYSIS OFFICE                
XJOB5    BAS   RE,AOFF                                                          
*                                                                               
         L     R2,ADPTH                                                         
         BRAS  RE,DUP                                                           
         CLI   5(R2),0             TEST IF DEPARTMENT INPUT                     
         BNE   XJOB6               YES                                          
*                                                                               
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWODEP),GOAWODEP                                         
         MVC   5(1,R2),BCDPTLEN                                                 
         BRAS  RE,MOVEFLD                                                       
*                                                                               
XJOB6    L     R2,APERH                                                         
         BRAS  RE,DUP                                                           
         CLI   5(R2),0                                                          
         BNE   XJOB8                                                            
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GOAWOSTF),GOAWOSTF                                         
         BRAS  RE,MOVEFLD                                                       
         MVI   5(R2),L'GOAWOSTF                                                 
*                                                                               
XJOB8    L     R2,AXACH                                                         
         CLI   5(R2),0                                                          
         BNE   XJOB10                                                           
*                                                                               
         OC    GOAWOA,GOAWOA       TEST FOR WRITE-OFF ACCOUNT                   
         BZ    XJOB10              NONE                                         
         MVC   FLD,SPACES                                                       
         CLC   GOAWOA(2),=C'SE'    TEST FOR EXPENSE LEDGER                      
         BE    *+18                YES                                          
         MVI   FLD,C'*'                                                         
         MVC   FLD+1(L'GOAWOA),GOAWOA                                           
         B     *+10                                                             
         MVC   FLD(L'GOAWOA-2),GOAWOA+2 ONLY NEED A/C CODE FOR SE               
         BRAS  RE,MOVEFLD                                                       
*                                                                               
XJOB10   L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         LR    RE,R3                                                            
         LA    RF,EXCELNQ                                                       
         XR    R1,R1               CLEAR EXCEL BLOCK                            
         MVCL  RE,R0                                                            
*                                                                               
         MVI   EXCACT,EXCAVAL      VALIDATE CALL                                
         MVC   EXCAEXP,AXACH       PASS A(EXPENSE A/C HEADER)                   
         MVC   EXCAANO,AAOFH                                                    
         MVC   EXCADEP,ADPTH                                                    
         MVC   EXCASTF,APERH                                                    
         ST    R9,EXCAGWS                                                       
         GOTO1 VEXCEL,EXCELD                                                    
         JNE   XJOBERR                                                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'EXCSENM),EXCSENM  SE ACCOUNT NAME                          
         ICM   R2,15,AXACNH                                                     
         BZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         ICM   R2,15,ADPTH         TEST DEPARTMENT INPUT                        
         CLI   8(R2),C' '                                                       
         BNH   XJOB12              NO, SKIP NAME DISPLAY                        
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'EXC2DNM),EXC2DNM  DEPARTMENT NAME                          
         ICM   R2,15,ADPTNH                                                     
         BZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
XJOB12   MVC   FLD,SPACES                                                       
         MVC   FLD(L'EXC2PNM),EXC2PNM PERSON NAME                               
         ICM   R2,15,APERNH                                                     
         BZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
XJOB14   TM    FLG,NOANLOFC        TEST "DON'T DISPLAY ANALYSIS OFFICE"         
         JNO   XYES                                                             
         MVC   FLD,SPACES          CLEAR OFFICE & NAME                          
         ICM   R2,15,AAOFH         ANALYSIS  OFFICE                             
         BRAS  RE,MOVEFLD                                                       
         ICM   R2,15,AAOFNH        ANALYSIS OFFICE NAME                         
         JZ    XYES                                                             
         BRAS  RE,MOVEFLD                                                       
         J     XYES                                                             
*                                                                               
XJOBERR  TM    FLG,NOANLOFC        TEST "DON'T DISPLAY ANALYSIS OFFICE"         
         BNO   XJOBERR3                                                         
         MVC   FLD,SPACES          CLEAR OFFICE & NAME                          
         ICM   R2,15,AAOFH         ANALYSIS  OFFICE                             
         BRAS  RE,MOVEFLD                                                       
         ICM   R2,15,AAOFNH        ANALYSIS OFFICE NAME                         
         JZ    XJOBERR3                                                         
         BRAS  RE,MOVEFLD                                                       
*                                                                               
XJOBERR3 ICM   R2,15,FVADDR                                                     
         OC    EXCERRF,EXCERRF                                                  
         JZ    EXIT                                                             
         ICM   R2,15,EXCERRF       SET CURSOR TO CORRECT FIELD                  
         J     EXIT                                                             
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE FINANCIAL (DEBIT) OFFICE FIELD                            *          
**********************************************************************          
FOFF     NTR1  ,                                                                
         L     R2,ADOFH                                                         
         MVC   FVMAXL,BCOFFLEN                                                  
*                                                                               
FOFF1    GOTO1 AFVAL,(R2)                                                       
         BE    FOFF4               SOMETHING IN FIELD                           
         JH    EXIT                INPUT DATA TOO LONG                          
*                                                                               
         CLI   TYPE,C'E'           NO INPUT-TEST FOR EXPENSE                    
         BNE   FOFF3               NO                                           
         TM    BCCPYST1,CPYSOROE   TEST OFFICE REQUIRED                         
         JZ    XIT                 NO                                           
         J     EMMISIF             'MISSING INPUT FIELD'                        
*                                                                               
FOFF3    J     XIT                                                              
*                                                                               
FOFF4    CLI   TYPE,C'E'           TEST FOR EXPENSE                             
         BE    FOFF6               YES                                          
         TM    ITMSTA,ITMXJOB      ITS PRODUCTION-TEST FOR XJOB                 
         BO    FOFF6               YES-ITS OK TO HAVE VALUE IN FIELD            
         MVC   FLD,SPACES          CLEAR THE FIELD OUT                          
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
*                                                                               
FOFF6    BRAS  RE,GETOFF                                                        
         MVC   FINOFF,ACOFFC       SET FINANCIAL OFFICE                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'OFFNAME),OFFNAME                                           
         ICM   R2,15,ADOFNH                                                     
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE ANALYSIS OFFICE FIELD                                     *          
**********************************************************************          
AOFF     NTR1  ,                                                                
         L     R2,AAOFH                                                         
         MVC   FVMAXL,BCOFFLEN                                                  
*                                                                               
         GOTO1 AFVAL,(R2)                                                       
         BE    AOFF4               SOMETHING IN FIELD                           
         JH    EXIT                INPUT DATA TOO LONG                          
         J     XIT                                                              
*                                                                               
AOFF4    BRAS  RE,GETOFF                                                        
         MVC   ANAOFF,ACOFFC       SET ANALYSIS OFFICE                          
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'OFFNAME),OFFNAME                                           
         ICM   R2,15,AAOFNH                                                     
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE CREDIT OFFICE FIELD                                       *          
**********************************************************************          
COFF     NTR1  ,                                                                
         L     R2,ACOFH                                                         
         MVC   FVMAXL,BCOFFLEN                                                  
         GOTO1 AFVAL,(R2)                                                       
         JH    EXIT                INPUT DATA TOO LONG                          
         JL    XIT                 EXIT FOR NO INPUT                            
*                                                                               
         BRAS  RE,GETOFF                                                        
         MVC   CRDOFF,ACOFFC       SET CREDIT OFFICE                            
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'OFFNAME),OFFNAME                                           
         ICM   R2,15,ACOFNH                                                     
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE EXPENSE ACCOUNT CODE                                      *          
**********************************************************************          
EDEXP    NTR1  ,                                                                
         L     R2,AXACH                                                         
         BRAS  RE,DUP              TEST FOR DUPLICATION                         
         MVI   FVMINL,1                                                         
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         CLI   FVIFLD,C'*'         REPEAT FEATURE                               
         BNE   EDEXP10                                                          
         XR    RE,RE                                                            
         IC    RE,FVILEN                                                        
         CH    RE,=H'1'                                                         
         BNH   EDEXP8              MORE THAN 2 INDICATES DEFAULT ACT            
*                                                                               
EDEXP2   LA    RF,DLIST            A(LIST OF VALID U/L'S TO DEBIT)              
EDEXP4   CLI   0(RF),X'FF'         END OF LIST                                  
         JE    EMINACP                                                          
         CLC   FVIFLD+1(2),0(RF)   INPUT TO LIST                                
         BE    EDEXP6                                                           
         LA    RF,2(RF)                                                         
         B     EDEXP4                                                           
*                                                                               
EDEXP6   LR    RF,RE                                                            
         MVC   ACTKUNT(2),FVIFLD+1 DEFAULT U/L TO KEY                           
         SHI   RF,3                DEDUCT FOR *UL                               
         LTR   RF,RF                                                            
         JM    EMINACP                                                          
         LA    R5,FVIFLD+3         A(REST OF DFLT ACCT)                         
         B     EDEXP15                                                          
*                                                                               
EDEXP8   BRAS  RE,DUP              EDIT FOR REPEAT                              
         GOTO1 AFVAL,(R2)                                                       
         XR    RE,RE                                                            
         IC    RE,FVILEN           LEN FOR DFLT ACT                             
         CLI   8(R2),C'*'          CHK IF ITS DFLT ACT INPUT                    
         BE    EDEXP2              NO                                           
*                                                                               
EDEXP10  MVC   ACTKUNT(2),=C'SE'   U/L DEBIT                                    
         XR    RF,RF                                                            
         IC    RF,FVILEN           INPUT LEN                                    
         LA    R5,FVIFLD                                                        
*                                                                               
EDEXP15  BCTR  RF,0                SUB 1 FOR EXECUTE                            
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   ACTKACT(0),0(R5)    EXP ACC TO KEY                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   POSTACC,ACCODE                                                   
         MVC   POSTACCN,ACNAME                                                  
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'ACNAME),ACNAME                                             
         ICM   R2,15,AXACNH                                                     
         BZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         MVI   ANLSTA,0            CLEAR ANALYSIS SWITCH                        
*                                                                               
         TM    BCCPYST1,CPYSOROE   MANDATORY OFFICE                             
         BZ    *+8                                                              
         OI    ANLSTA,ANLOFF                                                    
*                                                                               
         TM    ACBSTAT,ACBSPERS    PERSONAL EXPENSE (STAFF=Y)                   
         BZ    *+8                                                              
         OI    ANLSTA,ANLSTF                                                    
*                                                                               
         TM    ACBSTAT,ACBSDEPT    DEPT EXPENSE (DEPT=Y)                        
         BZ    *+8                                                              
         OI    ANLSTA,ANLDEP                                                    
*                                                                               
         MVC   COSTANAL,SPACES                                                  
         OC    COSTANAL(1),ACCTCOST  (ANALYSIS=)                                
*                                                                               
         GOTO1 VHELLO,DMCB,(C'G',=C'ACCOUNT'),('RSTELQ',AIO1),0                 
         CLI   12(R1),0            TEST IF ELEMENT FOUND                        
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R6,12(R1)                                                        
         USING RSTELD,R6                                                        
         MVC   POSTCNTR,RSTCCTR    COST CENTER                                  
         MVC   POSTCPOS,RSTCCTRR   COST POSITION                                
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         JL    XIT                                                              
         L     R2,AXACH                                                         
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         XR    R6,R6                                                            
         IC    R6,5(R2)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         JM    EMINVIF             'INVALID INPUT FIELD'                        
         J     XIT                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE DEPARTMENT ANALYSIS ACCOUNTS                              *          
**********************************************************************          
DEP      NTR1  ,                                                                
         L     R2,ADPTH                                                         
         MVC   FVMAXL,BCDPTLEN     SET MAXIMUM LENGTH                           
         MVI   FVMINL,1            SET SOME INPUT REQUIRED                      
         TM    ANLSTA,ANLDEP       TEST DEPARTMENT ANALYSIS                     
         BO    DEP2                YES                                          
         TM    ANLSTA,ANLSTF       TEST STAFF ANALYSIS                          
         BNO   *+12                NO                                           
         CLI   LEV2P,1             TEST 1 LEVEL 2P LEDGER                       
         BH    DEP2                NO                                           
         MVI   FVMINL,0            DEPARTMENT IS NOT REQUIRED                   
*                                                                               
DEP2     MVC   BYTE,FVMINL         SAVE REQUIRED VALUE                          
         GOTO1 AFVAL,(R2)                                                       
         JH    EXIT                                                             
         BE    DEP3                SOMETHING WAS INPUT                          
         CLI   BYTE,1              TEST IF REQUIRED                             
         JE    EXIT                YES-EXIT WITH ERROR                          
         J     XIT                 NO-EXIT ITS OK                               
*                                                                               
DEP3     CLI   BYTE,1              TEST FIELD IS REQUIRED                       
         JNE   EMANFDP             'ACCOUNT NOT FLAGGED FOR DEPART...'          
         MVC   DEPT,FVIFLD                                                      
         TM    ANLSTA,ANLDEP       TEST DEPARTMENT ANALYSIS                     
         JNO   XIT                 NO-EXIT WITHOUT VALIDATING ACCOUNTS          
*                                                                               
         BRAS  RE,GETDPT                                                        
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   DEPNAME,ACNAME                                                   
         MVC   DEPNUM,ACCODE                                                    
         MVC   DEPSTFN,ACNAME                                                   
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'DEPNAME),DEPNAME                                           
         ICM   R2,15,ADPTNH                                                     
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING ACTKEY,R4                                                        
         MVC   ACTKEY,SPACES          VALIDATE 28 ACCOUNT                       
         MVC   ACTKCULA,POSTACC       SET EXPENSE ACCOUNT                       
         MVC   ACTKUNT(2),=C'28'                                                
         ICM   R2,15,AXACH                                                      
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   CRDSNUM,ACCODE                                                   
         MVC   CRDSNAME,ACNAME                                                  
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE COST ACCOUNTING ACCOUNTS                                  *          
**********************************************************************          
COST     NTR1  ,                   VALIDATE 1P ACCOUNT                          
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'1P'   SET UP 1/P ACCOUNT                           
         TM    BCCPYST5,CPYSNCST   TEST NEW COST ACCOUNTING                     
         BZ    *+14                NO                                           
         MVC   ACTKACT,=C'999999999999'                                         
         B     COST4                                                            
*                                                                               
         LA    RF,ACTKACT                                                       
         TM    ANLSTA,ANLOFF                                                    
         BNO   COST2                                                            
         LA    RE,ANAOFF           USE AOF, IF WE HAVE ONE                      
         CLI   ANAOFF,C' '                                                      
         BH    *+8                                                              
         LA    RE,FINOFF           ELSE, USE FINANCIAL OFFICE                   
         XR    R1,R1                                                            
         IC    R1,BCOFFLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(RE)       OFFICE                                       
         LA    RF,1(R1,RF)                                                      
*                                                                               
COST2    LA    R3,=C'9999'         DEFAULT DEPT                                 
         TM    ANLSTA,ANLDEP                                                    
         BNO   *+8                                                              
         LA    R3,DEPT             OR REAL DEPT                                 
*                                                                               
         XR    R1,R1                                                            
         IC    R1,BCDPTLEN                                                      
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),0(R3)                                                    
         LA    RF,1(R1,RF)                                                      
         MVC   0(1,RF),COSTANAL                                                 
*                                                                               
COST4    GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   CRCNUM,ACCODE                                                    
         MVC   CRCNAME,ACNAME                                                   
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* VALIDATE STAFF ANALYSIS ACCOUNTS                                   *          
**********************************************************************          
STF      NTR1  ,                   EXTRACT STAFF CODE                           
         L     R2,APERH                                                         
         MVC   FVMAXL,STAFFL       SET MAXIMUM LENGTH                           
         MVI   FVMINL,1            REQUIRE SOMETHING                            
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
         MVC   STAFF,FVIFLD                                                     
*                                                                               
         BRAS  RE,GETSTF                                                        
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   STAFFNUM,ACCODE                                                  
         MVC   STAFFNAM,ACNAME                                                  
         MVC   DEPSTFN,ACNAME                                                   
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'STAFFNAM),STAFFNAM                                         
         ICM   R2,15,APERNH                                                     
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         LA    R4,KEY                                                           
         USING ACTKEY,R4                                                        
         MVC   ACTKEY,SPACES       VALIDATE 29 ACCOUNT KEY                      
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKUNT(2),=C'29'   SET UP 2/9 ACCOUNT                           
         MVC   ACTKACT,=12C'9'     DEFAULT IS 999999 ETC.                       
*                                                                               
         OC    COSTNUM,COSTNUM                                                  
         BZ    *+10                                                             
         MVC   ACTKACT,COSTNUM+3   OR USE COSTING                               
         ICM   R2,15,APROH         CURSOR TO PRODUCT                            
         CLI   5(R2),0             TEST ANY PRODUCT                             
         JNE   *+8                 YES,                                         
         ICM   R2,15,ACLIH         NO, USE CLIENT                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   CRPSNUM,ACCODE                                                   
         MVC   CRPSNAME,ACNAME                                                  
         J     XIT                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE PRODUCTION CASH DISCOUNT INCOME ACCOUNT                   *          
**********************************************************************          
VALCD    NTR1  ,                                                                
         MVC   KEY,SPACES                                                       
         LA    R4,IOKEY                                                         
         USING PMDRECD,R4                                                       
         MVC   PMDKEY,SPACES                                                    
         MVI   PMDKTYP,PMDKTYPQ    READ MEDIA RECORD                            
         MVC   PMDKCPY,COMPANY                                                  
         MVC   PMDKMED,JOBACCT+9   1ST BYTE OF JOB IS MEDIA CODE                
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'PMDKEY),IOKEYSAV                                         
         JNE   EMMMR               'MISSING MEDIA RECORD'                       
*                                                                               
         L     R4,AIO1                                                          
         LA    R4,PMDRFST                                                       
         XR    R0,R0                                                            
*                                                                               
         USING PMDELD,R4                                                        
VALCD3   CLI   0(R4),PMDELQ                                                     
         BE    VALCD5                                                           
         CLI   0(R4),0                                                          
         BNE   *+6                                                              
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALCD3                                                           
*                                                                               
VALCD5   MVC   KEY(L'PMDCSHD),PMDCSHD                                           
         CLC   KEY,SPACES         TEST CASH DISCOUNT BY MEDIA                   
         BH    VALCD10            YES                                           
*                                                                               
         LA    R4,IOKEY                                                         
         USING LDGRECD,R4                                                       
         MVC   LDGKEY,SPACES       READ LEDGER RECORD                           
         MVC   LDGKCPY,COMPANY                                                  
         MVC   LDGKUNT(2),=C'SJ'   PRODUCTION LEDGER                            
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'LDGKEY),IOKEYSAV                                         
         JNE   EMMSJL              'MISSING SJ LEDGER'                          
*                                                                               
         L     R4,AIO1                                                          
         LA    R4,LDGRFST                                                       
         XR    R0,R0                                                            
*                                                                               
         USING LDGELD,R4                                                        
VALCD6   CLI   0(R4),LDGELQ                                                     
         BE    VALCD7                                                           
         CLI   0(R4),0                                                          
         BNE   *+6                MISSING LEDGER ELEMENT                        
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALCD6                                                           
*                                                                               
VALCD7   LA    R3,KEY                                                           
         USING ACTRECD,R3                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,LDGCDSC                                                  
         CLC   ACTKULA,SPACES                                                   
         BH    *+10                                                             
         MVC   ACTKULA,=CL14'SIMD' USE DEFAULT                                  
*                                                                               
VALCD10  L     R2,AAMTH            A(CURRENT AMOUNT HEADER)                     
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   SIMDAC,ACCODE                                                    
         MVC   SIMDACN,ACNAME                                                   
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE EXPENSE CASH DISCOUNT INCOME ACCOUNT                      *          
**********************************************************************          
VALECD   NTR1  ,                                                                
         L     R2,AAMTH            DEFAULT COMMISSION ACCT                      
         LA    R4,IOKEY                                                         
         USING LDGRECD,R4                                                       
         MVC   LDGKEY,SPACES         READ LEDGER RECORD                         
         MVC   LDGKCPY,COMPANY                                                  
         MVI   LDGKUNT,C'S'                                                     
         LA    RF,COMPEL                                                        
         USING CPYELD,RF                                                        
         MVC   LDGKLDG,CPYSUPX     EXPENSE VENDOR FROM CO REC                   
         DROP  RF                                                               
*                                                                               
         L     R2,AXVNH                                                         
         CLI   8(R2),C'*'                                                       
         BNE   *+10                                                             
         MVC   LDGKUNT(2),9(R2)    OVERRIDE UNIT/LEDGER                         
*                                                                               
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'LDGKEY),IOKEYSAV                                         
         JNE   EMMEL               'MISSING EXPENSE LEDGER RECORD'              
         L     R4,AIO1                                                          
         LA    R4,LDGRFST                                                       
         XR    R0,R0                                                            
*                                                                               
         USING LDGELD,R4                                                        
VALECD3  CLI   0(R4),LDGELQ                                                     
         BE    VALECD5                                                          
         CLI   0(R4),0                                                          
         BNE   *+6                MISSING LEDGER ELEMENT                        
         DC    H'0'                                                             
         IC    R0,1(R4)                                                         
         AR    R4,R0                                                            
         B     VALECD3                                                          
*                                                                               
VALECD5  LA    R3,KEY                                                           
         USING ACTRECD,R3                                                       
         MVC   ACTKCPY,COMPANY                                                  
         MVC   ACTKULA,LDGCDSC                                                  
         CLC   ACTKULA,SPACES                                                   
         BH    *+10                                                             
         MVC   ACTKULA,=CL14'SIMD' USE DEFAULT                                  
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK                                                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP                                                          
         MVC   SIMDAC,ACCODE                                                    
         MVC   SIMDACN,ACNAME                                                   
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
**********************************************************************          
* 1C ACCOUNT OVERRIDE                                                *          
**********************************************************************          
SET1C    NTR1  ,                                                                
         OC    POSTCNTR,SPACES                                                  
         LA    R3,COSTNUM+7        R3=A(REPLACEMENT POSITION POINTER)           
         CLI   POSTCPOS,0          TEST COST POSITION OVERRIDE                  
         BE    SET1C2              NO                                           
*                                                                               
         XR    R3,R3                                                            
         IC    R3,POSTCPOS         YES                                          
         LA    R3,COSTNUM+2(R3)                                                 
*                                                                               
SET1C2   LA    R1,POSTCNTR         R1=A(OVERRIDE COST CENTER)                   
         LA    RE,L'POSTCNTR                                                    
*                                                                               
SET1C4   CLI   0(R1),C' '          TEST FOR OVERRIDE CHARACTER                  
         BE    *+10                NO                                           
         MVC   0(1,R3),0(R1)                                                    
         LA    R1,1(R1)                                                         
         LA    R3,1(R3)                                                         
         BCT   RE,SET1C4                                                        
         J     XIT                                                              
         EJECT                                                                  
**********************************************************************          
* UPDATE THE ORDER RECORD                                            *          
**********************************************************************          
UPDORD   NTR1  ,                                                                
         LA    R4,IOKEY           BUILD ORDER KEY                               
         USING ORDRECD,R4                                                       
         XC    ORDKEY,ORDKEY      READ FOR UPDATE                               
         MVI   ORDKTYP,ORDKTYPQ                                                 
         MVC   ORDKCPY,COMPANY                                                  
         MVC   ORDKORD,LASTORD                                                  
         GOTO1 AIO,IOHIUP+IOACCMST+IO1                                          
         CLC   IOKEY(L'ORDKEY),IOKEYSAV                                         
         BE    *+6                                                              
         DC    H'0'                NO ORDER RECORD                              
         L     R4,AIO1                                                          
         LA    R3,ORDRFST                                                       
         USING OAMEL,R3                                                         
         XR    R0,R0                                                            
*                                                                               
UPDORD2  CLI   OAMEL,0                                                          
         JE    UPDORD14                                                         
         CLI   OAMEL,OAMELQ                                                     
         BE    UPDORD10                                                         
         CLI   OAMEL,ORDELQ                                                     
         BE    UPDORD6                                                          
*                                                                               
UPDORD4  IC    R0,OAMLN                                                         
         AR    R3,R0                                                            
         B     UPDORD2                                                          
*                                                                               
         USING ORDELD,R3                                                        
UPDORD6  CLI   ORDLN,ORDLN3Q                                                    
         BL    UPDORD7                                                          
         OI    ORDSTAT2,ORDSSTAT                                                
         OI    ORDRSTA2,ORDSSTAT                                                
UPDORD7  CLI   BCPARTSW,C'Y'      IS IT PARTIAL MATCH                           
         BE    UPDORD8                                                          
         OI    ORDSTAT,ORDSMNUP   SET FULLY MATCHED                             
         B     UPDORD4                                                          
UPDORD8  OI    ORDSTAT,ORDSPART   SET PARTIAL MATCH                             
         B     UPDORD4                                                          
*                                                                               
         USING OAMELD,R3                                                        
UPDORD10 CLC   WRKCODE,SPACES      TEST PRODUCTION INVOICE                      
         BH    UPDORD11            YES                                          
         CLC   OAMWORK,SPACES      NO BUT IS THE ORDER PRODUCTION               
         BE    UPDORD12            NO, OK TO UPDATE                             
         B     UPDORD4             YES ADD SPACES FILLED WORKCODE               
UPDORD11 CLC   OAMWORK,WRKCODE     MATCH WORKCODE                               
         BNE   UPDORD4             YES, OK TO UPDATE                            
*                                                                               
UPDORD12 AP    OAMTVAL,INAMNT      UPDATE AMOUNT                                
         SP    OAMTVAL,CDAMNT                                                   
         MVC   OAMLAST,TODAYP      SET DATE                                     
         LLC   RE,OAMIPND          INCREMENT PENDING COUNT                      
         AHI   RE,1                                                             
         STC   RE,OAMIPND                                                       
         J     UPDORD16                                                         
*                                                                               
UPDORD14 OI    ORDRSTA2,ORDSSTAT                                                
         TM    ORDRSTA2,ORDSEXEX   ONLY ADD EXTRA ELEMENT FOR                   
         BZ    UPDORD16               BRANDOCEAN ORDERS                         
         LA    R3,BOELEM                                                        
         XC    BOELEM,BOELEM                                                    
         MVI   OAMEL,OAMELQ                                                     
         MVI   OAMLN,OAMLN2Q                                                    
         MVI   OAMSTAT,OAMSXTRA                                                 
         ZAP   OAMAMNT,=P'0'                                                    
         ZAP   OAMINUM,=P'0'                                                    
         ZAP   OAMIVAL,=P'0'                                                    
         ZAP   OAMTVAL,INAMNT      UPDATE AMOUNT                                
         SP    OAMTVAL,CDAMNT                                                   
         MVC   OAMLAST,TODAYP      SET DATE                                     
         MVI   OAMIPND,1                                                        
         MVC   OAMWORK,WRKCODE                                                  
         OC    OAMWORK,OAMWORK                                                  
         BNZ   *+10                                                             
         MVC   OAMWORK,SPACES                                                   
         LA    RF,=CL8'ADD=CODE'                                                
         GOTO1 VHELLO,DMCB,(C'P',=C'ACCMST'),AIO1,BOELEM,(RF)                   
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
UPDORD16 GOTO1 AIO,IOPUT+IOACCMST+IO1                                           
         J     XIT                                                              
         DROP  R3,R4                                                            
         EJECT                                                                  
*                                                                               
* U/L'S VALID TO DEBIT                                                          
DLIST    DS    0H                                                               
         DC    C'SASBSFSLSC'                                                    
         DC    X'FF'                                                            
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB,R8                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
* EDIT TAX SCREEN DETAIL                                             *          
**********************************************************************          
USET     NMOD1 0,**USET**,R8                                                    
         L     RC,0(R1)                                                         
         NI    ITMSTA,X'FF'-ITMTAXD SET TO NO TAX                               
         L     R2,ATAXH            CLEAR TAX                                    
         MVC   FLD,SPACES                                                       
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         TM    SCRSTA,SCRSINQ      TEST DETAIL SCREEN                           
         BO    USET2               YES,                                         
         ICM   R3,15,ACPJH                                                      
         CLI   8(R3),C' '          CLIENT OR EXPENSE ACCOUNT                    
         BH    USET2               YES,                                         
         L     R2,ALOCH            NO,  LOC/BASIS/WC ARE NOT ALLOWED            
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
USET1    L     R2,ATXWCH                                                        
         CLI   5(R2),0                                                          
         JNE   EMINVIF                                                          
         J     XIT                                                              
*                                                                               
USET2    L     R2,ALOCH                                                         
         CLI   5(R2),0             TEST LOCALITY INPUT                          
         BE    USET1               NO, THAN BASIS/WC ARE NOT ALLOWED            
*                                                                               
         L     R2,ABASH                                                         
         CLI   5(R2),0             TEST BASIS INPUT                             
         JE    EMMISIF                                                          
         GOTOR VALAMT,BASAMNT      VALIDATE BASIS                               
*                                                                               
         L     R2,ATXWCH           TAX WORKCODE                                 
         MVC   TAXWKC,8(R2)                                                     
         CLI   5(R2),0             TEST TAX CODE INPUT                          
         BNE   USET5               YES,                                         
         J     EMMISIF             NO                                           
*                                                                               
USET5    BRAS  RE,DUP                                                           
         GOTOR VALWC,DMCB,(X'80',ATXWCH)  GET THE WORKCODE                      
*                                                                               
         L     R2,ALOCH                                                         
         BRAS  RE,DUP                                                           
         BRAS  RE,VALLOC           VALIDATE LOCALITY                            
         L     R2,ATAXH                                                         
         MVC   FLD,SPACES          DISPLAY TAX AMOUNT                           
         CURED TAXAMNT,(12,FLD),2,MINUS=YES,ALIGN=LEFT                          
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         OI    ITMSTA,ITMTAXD      SET ITEM HAS TAX                             
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB,R8                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
* EDIT CANADIAN TAX SCREEN DETAIL                                    *          
**********************************************************************          
CTXR     NMOD1 0,**CTXR**,R8                                                    
         L     RC,0(R1)                                                         
         L     R5,AVATBLK                                                       
         USING VTCD,R5                                                          
*                                                                               
         ZAP   ITMBASE,INAMNT                                                   
         CLI   ACTION,ADDRECQ      TEST ACTION 'ADD'                            
         BE    CTXR03              YES, CAN'T GET RECORD                        
         CLI   ACTION,RECALCQ      TEST RECALCULATE                             
         BE    CTXR03              YES, ALREADY HAVE RECORD                     
         GOTOR TOTSAR,TSRGETQ      GET THE OLD RECORD                           
         TM    SCRSTA,SCRTAXQ      TEST TAX SCREEN                              
         BNO   CTXR03              NO, ALREADY HAVE TYPE                        
         MVI   TYPE,C'P'           SET FOR PRODUCTION                           
         CLI   RECWKC,C' '         TEST WORKCODE PRESENT                        
         BH    CTXR03              YES,                                         
         MVI   TYPE,C'E'           NO, MUST BE EXPENSE                          
*                                                                               
         USING CTD,R6                                                           
CTXR03   MVC   VTCOFFC,CLIOFFIC    ASSUME CLIENT OFFICE                         
         LA    R6,VENDCTX                                                       
         CLI   TYPE,C'P'           TEST PRODUCTION                              
         BE    *+14                                                             
         MVC   VTCOFFC,CRDOFF      USE CREDIT OFFICE                            
         LA    R6,XVNDCTX                                                       
         MVC   ITMGTY,CTUGTY       SET FROM UPPER SCREEN                        
         MVC   ITMPTY,CTUPTY                                                    
         MVC   ITMPRV,CTUPRV                                                    
*                                                                               
         CLI   ACTION,RECALCQ      TEST RECALCULATE                             
         BNE   *+10                                                             
         MVC   VTCOFFC,RECOFF      USE SAVED OFFICE                             
*                                                                               
         CLI   ACTION,ADDRECQ      TEST NEW RECORD                              
         BNE   CTXR09              NO,                                          
         ICM   R2,15,ABASH         TEST BASIS ON SCREEN                         
         BZ    CTXR27              NO,                                          
         CLI   5(R2),0             TEST BASIS INPUT                             
         BNE   CTXR05                                                           
         MVC   FLD,SPACES                                                       
         CURED INAMNT,(L'RECBAS,FLD),2,MINUS=YES,ALIGN=LEFT                     
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
CTXR05   GOTOR VALAMT,ITMBASE      VALIDATE BASIS                               
         B     CTXR27                                                           
*                                                                               
CTXR09   CLI   RECGTO,C' '                                                      
         BNH   *+10                                                             
         MVC   ITMGTY,RECGTO       SET FROM RECORD OVERRIDES                    
         CLI   RECPTO,C' '                                                      
         BNH   *+10                                                             
         MVC   ITMPTY,RECPTO                                                    
         CLI   RECPVO,C' '                                                      
         BNH   *+10                                                             
         MVC   ITMPRV,RECPVO                                                    
         MVC   VTCOFFC,RECOFF      OFFICE                                       
         GOTOR GETAMT,DMCB,(L'RECBAS,RECBAS)                                    
         ZAP   ITMBASE,CASHRTN                                                  
         ZAP   OLDBASE,ITMBASE                                                  
         CLI   ACTION,RECALCQ      TEST RECALCULATE                             
         BE    CTXR27                                                           
*                                                                               
         GOTOR GETAMT,DMCB,(L'RECAMT,RECAMT) GET OLD AMOUNT                     
         ZAP   OLDAMNT,CASHRTN     SAVE OLD AMOUNT                              
         ICM   R2,15,ABASH         TEST OVERRIDE(BASIS) ON SCREEN               
         BNZ   CTXR11              YES,                                         
         CP    INAMNT,OLDAMNT      TEST AMOUNT CHANGED                          
         BE    CTXR27              NO,                                          
         CP    OLDAMNT,OLDBASE     TEST OLD AMT = OLD BASIS                     
         BNE   CTXR27              NO, CAN'T CHANGE BASIS                       
         ZAP   ITMBASE,INAMNT      SET BASIS=ITEM AMOUNT                        
         B     CTXR27                                                           
*                                                                               
CTXR11   CLI   5(R2),0             TEST BASIS INPUT                             
         JE    EMMISIF                                                          
         GOTOR VALAMT,ITMBASE      VALIDATE BASIS                               
         CP    OLDBASE,ITMBASE     TEST BASIS CHANGED                           
         BNE   CTXR13              YES,                                         
         OC    AAMTH,AAMTH         TEST AMOUNT ON SCREEN                        
         BZ    CTXR13              NO,                                          
         CP    OLDAMNT,OLDBASE     TEST OLD AMT = OLD BASIS                     
         BNE   CTXR13              NO,                                          
         CP    INAMNT,OLDAMNT      TEST AMOUNT CHANGED                          
         BE    CTXR13              NO,                                          
         ZAP   ITMBASE,INAMNT      SET BASIS=ITEM AMOUNT                        
         MVC   FLD,SPACES                                                       
         CURED ITMBASE,(L'RECBAS,FLD),2,MINUS=YES,ALIGN=LEFT                    
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
*                                                                               
CTXR13   MVC   ITMGTY,CTUGTY       SET UPPER SCREEN                             
         L     R2,AGTOH            TEST GST OVERRIDE                            
         STCM  R2,15,VTCAFLDH      SET ADDRESS OF OVERRIDE                      
         CLI   8(R2),C' '                                                       
         BNH   CTXR15                                                           
         MVC   ITMGTY,8(R2)                                                     
         CLI   8(R2),TXGNONQ                                                    
         BE    CTXR15                                                           
         XC    VTCPRV,VTCPRV                                                    
         GOTOR VATICAN,VTCD                                                     
         JNE   EMINVGT                                                          
*                                                                               
CTXR15   MVC   ITMPTY,CTUPTY       SET UPPER SCREEN                             
         L     R2,APTOH            TEST PST TYPE                                
         STCM  R2,15,VTCAFLDH      SET ADDRESS OF DEFAULT TYPE                  
         MVI   BYTE,C' '           SET NO OVERRIDES                             
         CLI   8(R2),C' '                                                       
         BNH   CTXR19                                                           
         MVC   ITMPTY,8(R2)                                                     
         CLI   8(R2),TXGNONQ                                                    
         BE    CTXR27                                                           
         MVI   BYTE,C'O'           SET OVERRIDE FLAG                            
*                                                                               
CTXR19   MVC   ITMPRV,CTUPRV       SET DEFAULT PROVINCE                         
         L     R2,APVOH            TEST OVERRIDE PROVINCE                       
         CLI   8(R2),C' '                                                       
         BNH   CTXR25                                                           
         MVC   ITMPRV,8(R2)                                                     
         MVI   BYTE,C'O'           SET 'OVERRIDE' FLAG                          
*                                                                               
         L     RE,APRVTAB          VALIDATE PROVINCE CODE                       
CTXR23   CLC   0(2,RE),8(R2)                                                    
         BE    CTXR25                                                           
         LA    RE,L'PRVTAB(RE)                                                  
         CLI   0(RE),EOT                                                        
         BNE   CTXR23                                                           
         J     EMPROVX                                                          
*                                                                               
CTXR25   CLI   BYTE,C'O'           TEST ANY PST OVERRIDES                       
         BNE   CTXR27                                                           
         ICM   R2,15,APTOH         SET PST TYPE FIELD                           
         STCM  R2,15,VTCAFLDH                                                   
         MVC   VTCPRV,ITMPRV       SET OVERRIDE PROVINCE CODE                   
         GOTOR VATICAN,VTCD                                                     
         JNE   EMINVPT                                                          
*                                                                               
CTXR27   DS    0H                                                               
         ZAP   ITMGST,PZERO                                                     
         ZAP   ITMPST,PZERO                                                     
         MVC   ITMTXRA,SPACES                                                   
*                                                                               
         XC    FLDH,FLDH           SET DUMMY FIELD & HEADER FOR VATICAN         
         MVI   FLDH,9                                                           
         MVI   FLDH+5,1                                                         
         MVC   FLD,SPACES                                                       
         LA    R2,FLDH                                                          
         STCM  R2,15,VTCAFLDH                                                   
*                                                                               
         XC    VTCPRV,VTCPRV       GET GST DATA                                 
         MVC   FLD(1),ITMGTY                                                    
         GOTOR VATICAN,VTCD                                                     
         CLI   ITMGTY,TXGNONQ      TEST 'NO TAX'                                
         BNE   *+16                                                             
         XC    VTCRATE,VTCRATE     CLEAR RATE                                   
         XC    VTCINDS,VTCINDS     CLEAR INDICATOR                              
         MVC   GSTDATA,VTCEFFD     SAVE DATE/RATE/ACT/ACT-NAME ETC..            
*                                                                               
         MVC   VTCPRV,ITMPRV       GET PST DATA                                 
         MVC   FLD(1),ITMPTY                                                    
         CLI   VTCPRV,C' '         TEST PROVINVCE CODE                          
         BNH   CTXR28              NONE, - NO PROVINCE TAX                      
         GOTOR VATICAN,VTCD                                                     
         CLI   ITMPTY,TXGNONQ                                                   
         BNE   *+16                                                             
CTXR28   XC    VTCRATE,VTCRATE     CLEAR RATE                                   
         XC    VTCINDS,VTCINDS     CLEAR INDICATOR                              
         MVC   PSTDATA,VTCEFFD     SAVE DATE/RATE/ACT/ACT-NAME ETC..            
         DROP  R5                                                               
*                                                                               
         CLI   ACTION,RECALCQ                                                   
         BE    CTXR33                                                           
         CLI   ACTION,ADDRECQ      TEST NEW RECORD                              
         BE    CTXR29                                                           
*                                  SUBTRACT OLD BASIS                           
         BRAS  RE,ADJBASE                                                       
*                                                                               
         USING VTCEFFD,R5                                                       
CTXR29   LA    R5,GSTDATA          ADD NEW BASIS                                
         CLI   ITMGTY,TXGNONQ      TEST EXCLUDE THIS ITEM                       
         BE    CTXR31              YES,                                         
         OC    VTCRATE,VTCRATE     TEST ZERO TAX RATE                           
         BZ    CTXR31              YES                                          
         AP    GSTBASE,ITMBASE                                                  
*                                                                               
CTXR31   DS    0H                                                               
         LA    R5,PSTDATA                                                       
         CLI   ITMPTY,TXGNONQ                                                   
         BE    CTXR33                                                           
         OC    VTCRATE,VTCRATE                                                  
         BZ    CTXR33                                                           
         AP    PSTBASE,ITMBASE                                                  
*                                                                               
CTXR33   NI    ITMSTA,X'FF'-(ITMGROS)                                           
         CLI   TXGON,TXGNONQ       TEST NO TAX                                  
         JE    XIT                                                              
         CLI   TXGON,TXGGRSQ       TEST INPUT IS GROSS                          
         JNE   *+8                                                              
         OI    ITMSTA,ITMGROS                                                   
*                                                                               
         TM    CTXSTAT,CTXSGSTI    TEST INPUT GST                               
         BO    CTXR43              YES,                                         
*                                                                               
         TM    ITMSTA,ITMGROS      TEST INPUT IS GROSS                          
         JNO   CTXR39                                                           
*                                  ** CALCULATE NET FROM GROSS **               
         TM    CTXSTAT,CTXSPSTI    TEST INPUT PST                               
         JNO   CTXR35                                                           
         BAS   RE,ALLOPST          GET ALLOCATED PST                            
         ZAP   NETGST,ITMBASE                                                   
         SP    NETGST,ITMPST                                                    
         B     CTXR37                                                           
*                                                                               
CTXR35   DS    0H                                                               
         CLC   ITMPRV,=C'PQ'       FOR PQ PROVINCE                              
         JNE   CTXR36                                                           
         CLC   DOCDATE,=X'B30101'  AND DATE AFTER JAN01/13                      
         JL    CTXR36                                                           
         LA    R5,GSTDATA                                                       
         XR    RF,RF                                                            
         ICM   RF,3,VTCRATE        TEST ANY GST                                 
         CVD   RF,DUB                                                           
         ZAP   PL8,DUB             GST RATE                                     
         LA    R5,PSTDATA                                                       
         ICM   RF,3,VTCRATE        TEST ANY PST                                 
         CVD   RF,DUB              PST RATE                                     
         AP    DUB,PL8            GST RATE + PST RATE COMBINED                  
         ZAP   PL16,ITMBASE       ITMBASE = GROSS = NET+GST+PST                 
         BRAS  RE,CALCNETS        SPECIAL NET CALCULATION                       
         ZAP   ITMNET,PL8           NET ONLY                                    
         J     CTXR40             SPECIAL PST CALC AS NET                       
*                                                                               
CTXR36   LA    R5,PSTDATA                                                       
         ZAP   PL16,ITMBASE                                                     
         BRAS  RE,CALCNET          CALCULATE NET                                
         ZAP   NETGST,PL8          SAVE (NET+GST)                               
         ZAP   ITMPST,ITMBASE      GET ITEM PST                                 
         SP    ITMPST,NETGST       GROSS MINUS (NET+GST)                        
*                                                                               
CTXR37   LA    R5,GSTDATA                                                       
         ZAP   PL16,NETGST         GET NET FROM (NET +GST)                      
         BRAS  RE,CALCNET                                                       
         ZAP   NETAMT,PL8          SAVE TRUE NET                                
         ZAP   ITMGST,NETGST       (NET+GST)-TRUE NET = GST                     
         SP    ITMGST,NETAMT                                                    
         B     CTXR49                                                           
*                                                                               
*                                  ** INPUT IS NET **                           
CTXR39   DS    0H                                                               
         ZAP   PL16,ITMBASE                                                     
CTXR40   TM    ITMSTA,ITMGROS      TEST INPUT IS GROSS                          
         JNO   *+10                                                             
         ZAP   PL16,ITMNET         NET FROM GROSS                               
         LA    R5,GSTDATA          GET GST                                      
         BRAS  RE,CALCTAX          CALCULATE GST                                
         ZAP   ITMGST,PL8                                                       
         TM    CTXSTAT,CTXSPSTI    TEST INPUT PST                               
         JNO   CTXR41                                                           
         BAS   RE,ALLOPST          GET ALLOCATED PST                            
         B     CTXR49                                                           
*                                                                               
CTXR41   LA    R5,PSTDATA                                                       
         ZAP   PL16,ITMBASE        NET                                          
*                                                                               
         CLC   ITMPRV,=C'PQ'       FOR PQ PROVINCE                              
         JNE   CTXR42                                                           
         CLC   DOCDATE,=X'B30101'  AND DATE AFTER JAN01/13                      
         JNL   *+10                DO NOT ADD GST TO NET                        
CTXR42   AP    PL16,ITMGST         PLUS GST                                     
         TM    ITMSTA,ITMGROS      TEST INPUT IS GROSS                          
         JNO   *+10                                                             
         ZAP   PL16,ITMNET         NET FROM GROSS                               
         BRAS  RE,CALCTAX          CALCULATE PST                                
         ZAP   ITMPST,PL8                                                       
         B     CTXR49                                                           
*                                                                               
CTXR43   DS    0H                  ** GST AMOUNT INPUT **                       
         LA    R5,GSTDATA          GET GST                                      
         OC    VTCRATE,VTCRATE     TEST ANY RATE                                
         BZ    CTXR44              NONE, NO TAX                                 
         CLI   ITMGTY,TXGNONQ      TEST EXCLUDE THIS ITEM                       
         BE    CTXR44                                                           
         CP    GSTBASE,PZERO                                                    
         BE    CTXR44                                                           
*                                                                               
         ZAP   PL16,ITMBASE        THIS ITEM AMOUNT                             
         MP    PL16,GSTAMNT        X INPUT GST                                  
         SRP   PL16,4,0                                                         
         ZAP   PL8,GSTBASE         DIVIDE BY BASE                               
         DP    PL16,PL8            GET GST AMOUNT                               
         SRP   PL16(8),64-4,5                                                   
         ZAP   ITMGST,PL16(8)                                                   
*                                                                               
CTXR44   TM    CTXSTAT,CTXSPSTI    TEST INPUT PST                               
         JO    CTXR47                                                           
         ZAP   PL16,ITMBASE                                                     
         LA    R5,PSTDATA                                                       
         OC    VTCRATE,VTCRATE     TEST ANY RATE                                
         BZ    CTXR49              NONE, NO TAX                                 
         CLI   ITMPTY,TXGNONQ      TEST EXCLUDE THIS ITEM                       
         BE    CTXR49                                                           
         CLI   TXGON,TXGNETQ       TEST INPUT IS NET                            
         JE    CTXR45              YES,                                         
         BRAS  RE,CALCNET          CALCULATE NET                                
         ZAP   NETGST,PL8          SAVE (NET+GST)                               
         ZAP   ITMPST,ITMBASE      TOTAL                                        
         SP    ITMPST,NETGST       MINUS, LESS (NET+GST)                        
         J     CTXR49                                                           
*                                                                               
CTXR45   AP    PL16,ITMGST         FOR NET INPUT ADD GST TO (NET+GST)           
         BRAS  RE,CALCTAX          CALCULATE PST                                
         ZAP   ITMPST,PL8                                                       
         B     CTXR49                                                           
*                                                                               
CTXR47   BAS   RE,ALLOPST                                                       
*                                                                               
CTXR49   LA    R5,GSTDATA                                                       
         MVC   ITMGSTR,VTCRATE                                                  
         LA    R5,PSTDATA                                                       
         MVC   ITMPSTR,VTCRATE                                                  
         BRAS  RE,TXRAL            BUILD TAX RATE/AMOUNT LINE(S)                
         CLI   ACTION,RECALCQ      TEST RECALCULATE TAX                         
         BE    CTXR53                                                           
         ICM   R2,15,AGSTH         SET RATE\AMOUNT(DETAIL SCREEN)               
         BZ    CTXR51                                                           
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'GSTRL),GSTRL                                               
         BRAS  RE,MOVEFLD                                                       
*                                                                               
CTXR51   ICM   R2,15,APSTH                                                      
         BZ    CTXR53                                                           
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'PSTRL),PSTRL                                               
         BRAS  RE,MOVEFLD                                                       
*                                                                               
CTXR53   DS    0H                                                               
         ICM   R2,15,ATXRAH        SET RATE\AMOUNT                              
         BZ    CTXR55                                                           
         CLI   ACTION,RECALCQ      TEST RECALCULATE TAX                         
         BE    CTXR55              YES, DON'T DISPLAY                           
*                                                                               
         MVC   FLD,SPACES          MOVE TAX DETAIL TO SCREEN                    
         MVC   FLD(L'ITMTXRA),ITMTXRA                                           
         BRAS  RE,MOVEFLD                                                       
*                                                                               
CTXR55   DS    0H                                                               
         J     XIT                                                              
         DROP  R6                                                               
*                                                                               
ALLOPST  DS    0H                  ALLOCATED PST                                
         LR    R0,RE                                                            
         LA    R5,PSTDATA                                                       
         OC    VTCRATE,VTCRATE     TEST ANY RATE                                
         BZ    ALLOPSTX            NONE, NO TAX                                 
         CP    PSTBASE,PZERO       TEST ANY BASIS                               
         BZ    ALLOPSTX                                                         
         CLI   ITMPTY,TXGNONQ      TEST EXCLIDE THIS ITEM                       
         BE    ALLOPSTX                                                         
         ZAP   PL16,ITMBASE        THIS ITEM AMOUNT                             
         MP    PL16,PSTAMNT        X INPUT PST                                  
         SRP   PL16,4,0                                                         
         ZAP   PL8,PSTBASE         DIVIDE BY BASE                               
         DP    PL16,PL8                                                         
         SRP   PL16(8),64-4,5                                                   
         ZAP   ITMPST,PL16(8)                                                   
ALLOPSTX LR    RE,R0                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* CALCULATE TAX                                                      *          
*  R5=A(GST/PST DATA)                                                *          
*  BASIS IS IN PL16                                                  *          
*  RETURN TAX IN PL8                                                 *          
**********************************************************************          
CALCTAX  LR    R0,RE                                                            
         ZAP   PL8,PZERO                                                        
         XR    RF,RF                                                            
         ICM   RF,3,VTCRATE                                                     
         BZ    CALCXIT                                                          
         CVD   RF,DUB                                                           
         MP    PL16,DUB                                                         
         SRP   PL16,64-5,5          RATE IS 3 DECIMAL                           
         B     CALCTX20                                                         
CALCTX20 ZAP   PL8,PL16                                                         
         B     CALCXIT                                                          
*                                                                               
*                                                                               
**********************************************************************          
* CALCULATE NET AMOUNT FROM GROSS                                    *          
*  DUB=GST RATE + PST RATE                                           *          
*  GROSS IS IN PL16                                                  *          
*  RETURN NET IN PL8                                                 *          
**********************************************************************          
CALCNETS LR    R0,RE                                                            
         ZAP   PL8,PL16            IF NO, TAX NET=GROSS                         
         MP    PL16,=P'1000000'                                                 
         AP    DUB,=P'10000'                                                    
         MP    PL16,=P'10'                                                      
         AP    DUB,=P'90000'                                                    
         DP    PL16,DUB                                                         
         ZAP   PL8,PL16(8)         NET                                          
         SRP   PL8,64-2,5          ROUNDED                                      
*                                                                               
CALCXITS LR    RE,R0                                                            
         BR    RE                                                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         EJECT                                                                  
**********************************************************************          
* CALCULATE NET AMOUNT FROM GROSS                                    *          
*  R5=A(GST/PST DATA)                                                *          
*  GROSS IS IN PL16                                                  *          
*  RETURN NET IN PL8                                                 *          
**********************************************************************          
CALCNET  LR    R0,RE                                                            
         ZAP   PL8,PL16            IF NO, TAX NET=GROSS                         
         MP    PL16,=P'1000000'                                                 
         XR    RF,RF                                                            
         ICM   RF,3,VTCRATE        TEST ANY PST                                 
         BZ    CALCXIT             NO, TAX                                      
         CVD   RF,DUB                                                           
         AP    DUB,=P'10000'                                                    
         MP    PL16,=P'10'                                                      
         AP    DUB,=P'90000'                                                    
         DP    PL16,DUB                                                         
         ZAP   PL8,PL16(8)         NET                                          
         SRP   PL8,64-2,5          ROUNDED                                      
*                                                                               
CALCXIT  LR    RE,R0                                                            
         BR    RE                                                               
         J     XIT                                                              
         DROP  R5                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB,R8                                                            
         EJECT                                                                  
**********************************************************************          
* NAME LIST SCREEN                                                   *          
**********************************************************************          
LIST     NMOD1 0,**LIST**                                                       
         L     RC,0(R1)                                                         
         MVC   TXHMSG,TXHMSGC                                                   
         TM    PROCSW,PROCLST      TEST RETURNING FROM PRIOR LIST               
         BO    LIST05                                                           
         BAS   RE,LISDIS           LIST DISPLAY                                 
         J     XIT                                                              
*                                                                               
LIST05   CLI   PFKEY,PFRTNQ        TEST RETURN                                  
         BNE   LIST07                                                           
         BRAS  RE,SAVTOP                                                        
         GOTOR LOADSCRN,RTRNSCR    LOAD RETURN SCREEN                           
         BRAS  RE,RESTOP           RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         OI    PFKSW,PFKRDSP       FORCE RE-DISPLAY                             
         NI    PROCSW,X'FF'-(PROCLST+PROCTOP)                                   
         MVC   NGET,RTRNFST        SET RETURN NUMBER                            
         TM    SCRSTA,SCRSINQ+SCRTAXQ TEST NEED NAMES                           
         BZ    *+8                                                              
         OI    PROCSW,PROCDTL      SET TO PROCESS DETAIL (FOR NAMES)            
         BRAS  RE,VALPF                                                         
         J     XIT                                                              
*                                                                               
LIST07   CLI   PFKEY,PFUPQ         TEST PF UP                                   
         BNE   LIST09                                                           
         TM    VPFSW,VPFUP                                                      
         BNO   LISTERR                                                          
         B     LIST19                                                           
*                                                                               
LIST09   CLI   PFKEY,PFDWNQ        TEST PF DOWN                                 
         BNE   LIST11                                                           
         TM    VPFSW,VPFDWN                                                     
         BNO   LISTERR                                                          
         B     LIST19                                                           
*                                                                               
LIST11   CLI   PFKEY,PFLFTQ        TEST LEFT                                    
         BNE   LIST13                                                           
         CLI   LIST#,1                                                          
         BE    LISTERR                                                          
         XR    R1,R1                                                            
         ICM   R1,1,LIST#                                                       
         BP    *+6                                                              
         DC    H'0'                                                             
         BCTR  R1,0                                                             
         STC   R1,LIST#                                                         
         B     LIST15                                                           
*                                                                               
LIST13   CLI   PFKEY,PFRHTQ        TEST RIGHT                                   
         BNE   LIST17                                                           
         CLC   LIST#,SCRLISMX                                                   
         BE    LISTERR                                                          
         XR    R1,R1                                                            
         IC    R1,LIST#                                                         
         LA    R1,1(R1)                                                         
         STC   R1,LIST#                                                         
*                                                                               
LIST15   MVC   NGET,NFST           RE-DISPLAY SAME ITEMS                        
         OI    PFKSW,PFKRDSP                                                    
         B     LIST19                                                           
*                                                                               
LIST17   DS    0H                                                               
         B     LISTERR                                                          
*                                                                               
LIST19   NI    PROCSW,X'FF'-(PROCLST)                                           
         BRAS  RE,VALPF                                                         
         OI    PROCSW,PROCLST      SET LIST DISPLAY                             
         J     XIT                                                              
*                                                                               
LISTERR  L     R2,ADETH            FIRST SELECT FIELD                           
         J     EMMIVPFK                                                         
         EJECT                                                                  
**********************************************************************          
* LIST DISPLAY                                                       *          
**********************************************************************          
LISDIS   NTR1  ,                                                                
         TM    PROCSW,PROCTOP      TEST TOP ALREADY SET                         
         BO    LISDIS02                                                         
         OI    PROCSW,PROCTOP                                                   
         MVC   LISVEN,VENDLONG                                                  
         MVC   LISXVN,XVNDLONG                                                  
         LA    R2,LISVEN2H                                                      
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
         LA    RF,BOELEM                                                        
         MVC   0(L'VENDADR2,RF),VENDADR2                                        
         LA    RF,L'VENDADR2+1(RF)                                              
         MVC   0(L'VENDADR3,RF),VENDADR3                                        
         LA    RF,L'VENDADR3+1(RF)                                              
         MVC   0(L'VENDADR4,RF),VENDADR4                                        
         LA    RF,L'VENDADR4+1(RF)                                              
         GOTO1 VSQUASH,DMCB,BOELEM,L'BOELEM                                     
         MVC   FLD,BOELEM                                                       
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         LA    R2,LISXVN2H                                                      
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
         LA    RF,BOELEM                                                        
         MVC   0(L'XVNDADR2,RF),XVNDADR2                                        
         LA    RF,L'XVNDADR2+1(RF)                                              
         MVC   0(L'XVNDADR3,RF),XVNDADR3                                        
         LA    RF,L'XVNDADR3+1(RF)                                              
         MVC   0(L'XVNDADR4,RF),XVNDADR4                                        
         LA    RF,L'XVNDADR4+1(RF)                                              
         GOTO1 VSQUASH,DMCB,BOELEM,L'BOELEM                                     
         MVC   FLD,BOELEM                                                       
         BRAS  RE,MOVEFLD                                                       
*                                                                               
LISDIS02 MVC   LISHEAD,SPACES      CLEAR HEADLINE/UNDER LINE                    
         MVC   LISUNDR,SPACES                                                   
         MVI   LISHEAD,C'#'        SET ITEM # AND AMOUNT IN HEADLINE            
         MVC   LISUNDR(2),=C'--'                                                
         LA    R5,LISHEAD+8                                                     
         MVC   0(L'LD@AMT,R5),LD@AMT                                            
         LA    RF,L'LD@AMT-1(R5)                                                
         LA    R1,L'LD@AMT                                                      
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         BCTR  R1,0                                                             
         LA    R5,LISUNDR+8                                                     
         MVI   0(R5),C'-'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R5),0(R5)                                                    
*                                                                               
LSTNM    L     R2,ALISH            CLEAR DETAIL LINE                            
         MVC   FLD,SPACES                                                       
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         LA    R2,8(R2)            DISPLAY ITEM # AND AMOUNT                    
         USING LISD,R2                                                          
         CURED (B2,NGET),(L'LISITM,LISITM),0,ALIGN=LEFT                         
         GOTOR GETAMT,DMCB,(L'RECAMT,RECAMT)                                    
         CURED CASHRTN,(L'LISAMT,LISAMT),2,MINUS=YES                            
*                                                                               
         LA    R3,LISTAB           FIND DISPLAY LIST                            
         TM    SCRSTA,SCRCANQ      TEST CANADA                                  
         BNO   *+8                                                              
         LA    R3,LISTABC                                                       
LISDIS03 CLC   LIST#,0(R3)                                                      
         BE    LISDIS05                                                         
         SR    R0,R0                                                            
         IC    R0,1(R3)                                                         
         AR    R3,R0                                                            
         CLI   0(R3),X'FF'                                                      
         BNE   LISDIS03                                                         
         DC    H'0'                BAD LIST NUMBER                              
*                                                                               
LISDIS05 LA    R3,2(R3)            R3 TO FIRST COLUMN ITEM IN LIST              
         LA    R5,LISCPJ-LISD                                                   
LISDIS06 LA    R4,COLIST                                                        
         USING COLD,R4                                                          
LISDIS07 CLC   0(1,R3),COLEQU      FIND COLUMN NUMBER                           
         BE    LISDIS09                                                         
         LA    R4,COLNQ(R4)                                                     
         CLI   0(R4),X'FF'                                                      
         BNE   LISDIS07                                                         
         DC    H'0'                BAD COLUMN NUMBER                            
*                                                                               
LISDIS09 XR    R1,R1                                                            
         IC    R1,COLDLEN          R1=LENGTH OF HEADING                         
         BCTR  R1,0                                                             
         XR    RF,RF                                                            
         ICM   RF,3,COLDICT                                                     
         LA    RF,PROGD(RF)                                                     
         LA    R6,LISHEAD                                                       
         AR    R6,R5                                                            
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R6),0(RF)       HEADING TO SCREEN                            
         LA    RF,0(R1,R6)         FIND END OF HEADING                          
         CLI   0(RF),C' '                                                       
         BH    *+10                                                             
         BCTR  RF,0                                                             
         BCT   R1,*-10                                                          
         BCTR  R1,0                                                             
         LA    R6,LISUNDR          SET UNDERLINE                                
         AR    R6,R5                                                            
         MVI   0(R6),C'-'                                                       
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,R6),0(R6)                                                    
         ST    R2,ASTARTIM         SAVE ADDRESS OF                              
*                                                                               
LISDIS11 NI    FLG,X'FF'-(RTNLIST)   TURN OFF 'RETURN TO LIST' FLAG             
         MVC   FLD,SPACES                                                       
         XR    RF,RF                                                            
         ICM   RF,3,COLROUT        RE = DISPLACEMENT TO ROUTINE                 
         AR    RF,RB                                                            
*                                                                               
         BASR  RE,RF               GO TO DISPLAY ROUTINE                        
         LA    RF,LISD             START OF LINE                                
         AR    RF,R5               PLUS CURRENT DISPLACEMENT                    
         XR    R1,R1                                                            
         IC    R1,COLWDTH          R1= WIDTH OF FIELD                           
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,RF),FLD         MOVE TO OUTPUT FIELD                         
         TM    FLG,RTNLIST         TEST RETURN FOR NEXT                         
         BNO   LISDIS13                                                         
         XR    R1,R1                                                            
         IC    R1,SCRLDL                                                        
         AR    R2,R1               BUMP TO NEXT LINE                            
         B     LISDIS11                                                         
*                                                                               
LISDIS13 L     R2,ASTARTIM         RESTORE START OF ITEM                        
         LA    R5,3(R1,R5)         BUMP R5 TO NEXT COLUMN                       
         LA    R3,1(R3)            NEXT COLUMN NUMBER IN LIST                   
         CLI   0(R3),X'FF'         TEST END OF LIST                             
         BNE   LISDIS06            NO,                                          
         J     XIT                 ALL DONE                                     
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
***********************************************************************         
* LIST DISPLAY ROUTINES                                               *         
***********************************************************************         
CPJR     NTR1  ,                   GET CLIENT/PRODUCT/JOB                       
         OC    RECCLI,SPACES                                                    
         OC    RECPRO,SPACES                                                    
         OC    RECJOB,SPACES                                                    
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,RECCLI                                                   
         OC    ACTKACT,SPACES                                                   
         CLC   ACTKACT,SPACES                                                   
         JE    XIT                 NO DATA IN THIS COLUMN                       
         GOTOR AGETACC,DMCB,KEY,0                                               
         MVC   FLD(L'ACTKACT),ACTKACT                                           
         MVC   FLD+L'ACTKACT+1(L'ACNAME),ACNAME                                 
         B     SQUASHR                                                          
         DROP  R4                                                               
*                                                                               
WRKR     NTR1  ,                   GET WORKCODE NAME                            
         OC    RECWKC,SPACES                                                    
         CLI   RECWKC,C' '                                                      
         JNH   XIT                                                              
         MVC   FLD(L'RECWKC),RECWKC                                             
         GOTOR AGETWC,FLD                                                       
         BE    *+6                                                              
         DC    H'0'                                                             
         MVC   FLD+L'RECWKC+1(L'WCODESC),WORK                                   
         B     SQUASHR                                                          
*                                                                               
EXPR     NTR1  ,                   GET EXPENSE NAME                             
         OC    RECXAC,SPACES                                                    
         CLI   RECXAC,C' '                                                      
         JNH   XIT                                                              
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),=C'SE'   ASSUME DEFAULT EXPENSE ACCOUNT               
         LA    R1,L'RECXAC-4                                                    
         LA    R2,RECXAC                                                        
         LA    R3,ACTKACT                                                       
         CLI   RECXAC,C'*'         TEST OVERRIDE                                
         BNE   EXPR03              NO,                                          
         AHI   R1,2                YES- INPUT IS *UL                            
         LA    R2,1(R2)                                                         
         LA    R3,ACTKUNT                                                       
*                                                                               
EXPR03   EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R2)       ACCOUNT OR UL-ACC TO KEY                     
         MVC   FLD(L'ACTKULA),0(R3)                                             
         GOTOR AGETACC,DMCB,KEY,0                                               
         MVC   FLD+L'ACTKULA+1(L'ACNAME),ACNAME                                 
         B     SQUASHR                                                          
         DROP  R4                                                               
*                                                                               
DOFR     NTR1  ,                   GET DEBIT OFFICE NAME                        
         OC    RECDOF,SPACES                                                    
         CLI   RECDOF,C' '                                                      
         JNH   XIT                                                              
         MVC   FLD(L'RECDOF),RECDOF                                             
         B     OFFR                                                             
*                                                                               
COFR     NTR1  ,                   GET CREDIT OFFICE NAME                       
         OC    RECCOF,SPACES                                                    
         CLI   RECCOF,C' '                                                      
         JNH   XIT                                                              
         MVC   FLD(L'RECCOF),RECCOF                                             
         B     OFFR                                                             
*                                                                               
AOFR     NTR1  ,                   GET ANALYSIS OFFICE NAME                     
         OC    RECAOF,SPACES                                                    
         CLI   RECAOF,C' '                                                      
         JNH   XIT                                                              
         MVC   FLD(L'RECAOF),RECAOF                                             
*                                                                               
OFFR     MVC   FVIFLD,FLD                                                       
         GOTOR GETOFF                                                           
         MVC   FLD+L'RECDOF+1(L'OFFNAME),OFFNAME                                
         B     SQUASHR                                                          
*                                                                               
DPTR     NTR1  ,                   GET DEPARTMENT NAME                          
         OC    RECDPT,SPACES                                                    
         OC    RECAOF,SPACES                                                    
         CLI   RECDPT,C' '                                                      
         JNH   XIT                                                              
         MVC   ANAOFF,RECAOF       MIGHT NEED THE ANALYSIS OFFICE               
         CLI   ANAOFF,C' '                                                      
         JH    *+10                                                             
         MVC   ANAOFF,RECDOF                                                    
         MVC   DEPT,SPACES                                                      
         MVC   DEPT(L'RECDPT),RECDPT                                            
         BRAS  RE,GETDPT                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FLD(L'RECDPT),RECDPT                                             
         MVC   FLD+L'RECDPT+1(L'ACNAME),ACNAME                                  
         B     SQUASHR                                                          
*                                                                               
STFR     NTR1  ,                    GET STAFF NAME                              
         OC    RECPER,SPACES                                                    
         OC    RECAOF,SPACES                                                    
         OC    RECDPT,SPACES                                                    
         CLI   RECPER,C' '                                                      
         JNH   XIT                                                              
         MVC   ANAOFF,RECAOF       MIGHT NEED THE ANALYSIS OFFICE               
         CLI   ANAOFF,C' '                                                      
         JH    *+10                                                             
         MVC   ANAOFF,RECDOF                                                    
         MVC   DEPT,SPACES                                                      
         MVC   DEPT(L'RECDPT),RECDPT                                            
         MVC   STAFF,SPACES                                                     
         MVC   STAFF(L'RECPER),RECPER                                           
         BRAS  RE,GETSTF                                                        
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   FLD(L'RECPER),RECPER                                             
         MVC   FLD+L'RECPER+1(L'ACNAME),ACNAME                                  
         B     SQUASHR                                                          
*                                                                               
TAXR     NTR1  ,                   TAX ROUTINES                                 
         CLI   RECLOCN,0           TEST NUMBER OF TAX ENTRIES                   
         JE    XIT                                                              
         ICM   R5,15,ANXTTAX       NEXT TAX ITEM                                
         BNZ   TAXR3                                                            
         GOTOR GETAMT,DMCB,(L'RECBAS,RECBAS)                                    
         ZAP   BASAMNT,CASHRTN                                                  
         LA    R2,RECLOCH                                                       
         STCM  R2,15,ALOCH         SET A(LOCATION HEADER)                       
         BRAS  RE,VALLOC           BUILD TAX TABLE                              
         XC    ALOCH,ALOCH                                                      
*                                                                               
         L     R5,ATAXIT                                                        
         USING TXD,R5                                                           
TAXR3    XC    ANXTTAX,ANXTTAX     ASSUME THIS IS LAST                          
         MVI   BOELEM,C' '                                                      
         MVC   BOELEM+1(L'BOELEM-1),BOELEM                                      
         LA    RF,BOELEM                                                        
         MVC   0(L'TXLOC,RF),TXLOC                                              
         LA    RF,L'TXLOC+1(RF)                                                 
         MVC   0(L'TXLOCN,RF),TXLOCN                                            
         LA    RF,L'TXLOCN+1(RF)                                                
         MVI   0(RF),C'('                                                       
         LA    RF,1(RF)                                                         
         MVC   0(L'TXACC-1,RF),TXACC+1                                          
         LA    RF,L'TXACC(RF)                                                   
         MVC   0(L'TXACCN,RF),TXACCN                                            
         LA    RF,L'TXACCN(RF)                                                  
         MVI   0(RF),C')'                                                       
         GOTOR VSQUASH,DMCB,BOELEM,(0,L'BOELEM)                                 
         MVC   FLD,BOELEM                                                       
         LA    R5,TXLNQ(R5)                                                     
         CLI   0(R5),0             TEST ANOTHER TAX ITEM                        
         JE    XIT                 NO,                                          
         ST    R5,ANXTTAX          SET ADDRESS OF NEXT                          
         OI    FLG,RTNLIST         SET RETURN TO TAX ROUTINE                    
         J     XIT                                                              
         DROP  R5                                                               
*                                                                               
SQUASHR  GOTOR VSQUASH,DMCB,FLD,(0,L'FLD)                                       
         J     XIT                                                              
         EJECT                                                                  
LICPJQ   EQU   1                   CLIENT/PRODUCT/JOB                           
LIWRKQ   EQU   2                   WORKCODE                                     
LIEXPQ   EQU   3                   EXPENSE                                      
LIDOFQ   EQU   4                   DEBIT OFFICE                                 
LICOFQ   EQU   5                   CREDIT OFFICE                                
LIAOFQ   EQU   6                   ANALYSIS OFFICE                              
LIDPTQ   EQU   7                   DEPARTMENT                                   
LIPERQ   EQU   8                   PERSON                                       
LITAXQ   EQU   9                   TAX                                          
*                                                                               
LISTAB   DS    0X                                                               
LIST1    DC    AL1(1,LIST2-LIST1),AL1(LICPJQ,LIWRKQ),X'FF'                      
LIST2    DC    AL1(2,LIST3-LIST2),AL1(LIEXPQ,LIDOFQ,LICOFQ),X'FF'               
LIST3    DC    AL1(3,LIST4-LIST3),AL1(LIAOFQ,LIDPTQ,LIPERQ),X'FF'               
LIST4    DC    AL1(4,LIST5-LIST4),AL1(LITAXQ),X'FF'                             
LIST5    DC    X'FF'                                                            
*                                                                               
LISTABC  DS    0X                  CANADA                                       
LISTC1   DC    AL1(1,LISTC2-LISTC1),AL1(LICPJQ,LIWRKQ),X'FF'                    
LISTC2   DC    AL1(2,LISTC3-LISTC2),AL1(LIEXPQ,LIDOFQ,LICOFQ),X'FF'             
LISTC3   DC    AL1(3,LISTC4-LISTC3),AL1(LIAOFQ,LIDPTQ,LIPERQ),X'FF'             
LISTC4   DC    X'FF'                                                            
*                                                                               
COLIST   DS    0X                                                               
         DC    AL1(LICPJQ,L'LISCPJ),AL2(CPJR-LIST)                              
         DC    AL1(L'LD@CPJ),AL2(LD@CPJ-PROGD)                                  
*                                                                               
         DC    AL1(LIWRKQ,L'LISWRK),AL2(WRKR-LIST)                              
         DC    AL1(L'LD@WKC),AL2(LD@WKC-PROGD)                                  
*                                                                               
         DC    AL1(LIEXPQ,L'LISEXP),AL2(EXPR-LIST)                              
         DC    AL1(L'LD@EXPA),AL2(LD@EXPA-PROGD)                                
*                                                                               
         DC    AL1(LIDOFQ,L'LISDOF),AL2(DOFR-LIST)                              
         DC    AL1(L'LD@DOF),AL2(LD@DOF-PROGD)                                  
*                                                                               
         DC    AL1(LICOFQ,L'LISCOF),AL2(COFR-LIST)                              
         DC    AL1(L'LD@COF),AL2(LD@COF-PROGD)                                  
*                                                                               
         DC    AL1(LIAOFQ,L'LISAOF),AL2(AOFR-LIST)                              
         DC    AL1(L'LD@AOF),AL2(LD@AOF-PROGD)                                  
*                                                                               
         DC    AL1(LIDPTQ,L'LISDPT),AL2(DPTR-LIST)                              
         DC    AL1(L'LD@DPT),AL2(LD@DPT-PROGD)                                  
*                                                                               
         DC    AL1(LIPERQ,L'LISPER),AL2(STFR-LIST)                              
         DC    AL1(L'LD@PER),AL2(LD@PER-PROGD)                                  
*                                                                               
         DC    AL1(LITAXQ,L'LISTAX),AL2(TAXR-LIST)                              
         DC    AL1(L'TXHMSG),AL2(TXHMSG-PROGD)                                  
         DC    X'FF'                                                            
*                                                                               
TXHMSGC  DC    C'TAX LOCALITY (TAX VENDOR)'                                     
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE AND PROCESS PF KEYS                                       *          
**********************************************************************          
VALPF    NTR1  BASE=*,LABEL=*                                                   
         TM    PFKSW,PFKRDSP       TEST FORCE RE-DISPLAY                        
         BO    PFNXT                                                            
*                                                                               
         L     R3,APFKEYS                                                       
         USING PFKTD,R3                                                         
VALPF03  CLC   PFKEY,PFKTNUM       MATCH NUMBER TO TABLE                        
         BNE   VALPF07                                                          
         TM    CSINDSL2,CSIPFAPP   TEST APP WILL HANDLE PF KEYS                 
         BZ    VALPF05             NO, GO TO ROUTINE                            
         XR    R1,R1                                                            
         IC    R1,PFKTVBI          SET VALIDATION BIT                           
         LA    RF,VPFSW            SET VALIDATION BYTE                          
         CLI   PFKTVBY,PFKTVBY0                                                 
         BE    *+8                                                              
         LA    RF,VPFSW1                                                        
         EX    R1,*+8              TEST PFKEY VALID                             
         B     *+8                                                              
         TM    0(RF),0                                                          
         BNO   VALPF07                                                          
*                                                                               
VALPF05  XR    RF,RF               GO TO ROUTINE                                
         ICM   RF,3,PFKTROU                                                     
         BZ    VALPF07             NO ROUTINE-MUST BE LIST ENTRY                
         AR    RF,RB                                                            
         BR    RF                                                               
*                                                                               
VALPF07  LA    R3,PFKTLNQ(R3)      TRY NEXT ENTRY                               
         CLI   0(R3),X'FF'                                                      
         BNE   VALPF03                                                          
*                                                                               
PFERR    L     R2,ADETH                                                         
         J     EMMIVPFK            'INVALID PFKEY...'                           
         DROP  R3                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'SWAP'                                              *          
**********************************************************************          
PFSWAP   CLI   SCRSWAP,0           TEST SWAP SCREEN                             
         JE    XIT                 NONE, IGNORE IT                              
         OC    NBUF,NBUF                                                        
         BZ    PFSWAP3                                                          
*                                                                               
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFSWAP3                                                          
         XR    R0,R0                                                            
         ICM   R1,15,BCACUR        R1=A(CURSOR)                                 
         ICM   RF,15,ADETH                                                      
         SR    R1,RF               LESS A(FIRST DETAIL)                         
         BM    PFSWAP3             CURSOR BEFORE DETAIL LINE                    
         XR    R3,R3                                                            
         IC    R3,SCRLDL           LENGTH OF DETAIL LINE                        
         DR    R0,R3                                                            
         ICM   R3,3,NFST           R3=# OF FIRST                                
         AR    R3,R1               PLUS LINE NUMBER OF SELECTED LINE            
         CLM   R3,3,NBUF           TEST VS. MAX IN BUFFER                       
         BH    PFSWAP3                                                          
         STCM  R3,3,NFST           SET NUMBER TO DISPLAY                        
*                                                                               
PFSWAP3  BRAS  RE,SAVTOP                                                        
         GOTOR LOADSCRN,SCRSWAP    LOAD 'SWAP' SCREEN                           
         TM    PROCSW,PROCEDT      TEST FIRST TIME SWAP                         
         BNO   PFSWAP5             YES,                                         
         BRAS  RE,RESTOP           RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
*                                                                               
PFSWAP5  L     R2,AORDH                                                         
         OC    NBUF,NBUF           TEST FIRST TIME                              
         JZ    IMEREQF             'ENTER REQUIRED FIELDS..'                    
*                                                                               
         TM    SCRSTA,SCRSINQ+SCRTAXQ TEST NEED NAMES                           
         BZ    *+8                                                              
         OI    PROCSW,PROCDTL      SET TO PROCESS DETAIL (FOR NAMES)            
         MVC   NGET,NFST           GET FIRST ON SCREEN                          
         B     PFNXT                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'UP'                                                *          
**********************************************************************          
PFUP     OC    NBUF,NBUF           TEST ANYTHING IN BUFFER                      
         BZ    PFERR               NO, ERROR                                    
*                                                                               
         XR    R1,R1                                                            
         ICM   R1,3,NFST           NUMBER OF FIRST ITEM                         
         BNZ   *+8                                                              
         ICM   R1,3,NBUF           GET THE MAX                                  
         XR    RF,RF                                                            
         IC    RF,SCRMXD                                                        
         TM    SCRSTA,SCRLISQ      TEST LIST SCREEN                             
         BNO   *+8                 NO,                                          
         LA    RF,1                YES, BACKUP ONE                              
         SR    R1,RF               LESS MAX                                     
         BP    *+8                 TEST TOO LOW                                 
         LA    R1,1                USE FIRST                                    
         STCM  R1,3,NGET                                                        
         TM    SCRSTA,SCRLISQ      TEST LIST SCREEN                             
         BNO   PFNXT               NO,                                          
*                                                                               
         XR    R0,R0                                                            
         XR    R3,R3                                                            
         IC    R3,SCRMXD                                                        
PFUP03   GOTOR TOTSAR,TSRGETQ      GET THE RECORD                               
         XR    RF,RF                                                            
         ICM   RF,1,RECLOCN        NUMBER OF TAX LINES NEEDED                   
         BNZ   *+8                                                              
         LA    RF,1                                                             
         AR    R0,RF               COUNT NUMBER OF LINES NEEDED                 
         CR    R0,R3               LINES NEEDED VS. MAX ALLOWABLE               
         BH    PFUP05              TOO MANY SELECTED                            
         XR    R1,R1                                                            
         ICM   R1,3,NGET           LOOK BACK ONE MORE                           
         SHI   R1,1                                                             
         BNP   PFNXT               ALREADY AT FIRST                             
         STCM  R1,3,NGET           GET PREVIOUS RECORD                          
         B     PFUP03                                                           
*                                                                               
PFUP05   XR    R1,R1               GET LAST RECORD THAT FIT                     
         ICM   R1,3,NGET           LOOK BACK ONE MORE                           
         AHI   R1,1                                                             
         STCM  R1,3,NGET                                                        
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'DOWN'                                              *          
**********************************************************************          
PFDWN    XR    R3,R3                                                            
         ICM   R3,1,NITM           NUMBER OF ITEMS ON SCREEN                    
         BZ    PFERR                                                            
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,NLST           LAST                                         
         BZ    *+8                                                              
         LA    R3,1(R3)            PLUS 1                                       
         STCM  R3,3,NGET                                                        
         CLC   NGET,NBUF           TEST AGAINST TOTAL                           
         BNH   PFNXT                                                            
*                                                                               
         XC    NGET,NGET           CLEAR LIST                                   
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFNXT               YES, OK TO SHOW BLANK SCREEN                 
         MVC   NGET,NLST           ALWAYS DISPLAY LAST                          
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'LIST'                                              *          
**********************************************************************          
PFLIST   OC    NBUF,NBUF           TEST ANYTHING IN BUFFER                      
         BZ    PFERR               NO, ERROR                                    
         MVC   RTRNSCR,SCRNUM      SAVE SCREEN NUMBER                           
         MVC   RTRNFST,NFST        SAVE NUMBER OF FIRST ON SCREEN               
         MVI   NLINL,0             SET LINES USED TO ZERO                       
*                                                                               
         BRAS  RE,SAVTOP                                                        
         MVI   BYTE,LISSCRNQ                                                    
         GOTOR LOADSCRN,BYTE       LOAD LIST SCREEN                             
         BRAS  RE,RESTOP           RESTORE THE TOP                              
         LA    R1,1                                                             
         STCM  R1,3,NGET                                                        
         MVI   LIST#,1                                                          
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'DELETE'                                            *          
**********************************************************************          
PFDEL    OC    NBUF,NBUF                                                        
         BZ    PFERR                                                            
*                                                                               
         MVC   NGET,NFST           SET FOR SINGLE                               
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFDEL3                                                           
         XR    R0,R0                                                            
         ICM   R1,15,BCACUR        R1=A(CURSOR)                                 
         ICM   RF,15,ADETH                                                      
         SR    R1,RF               LESS A(FIRST DETAIL)                         
         BM    PFERR               CURSOR BEFORE DETAIL LINE                    
         XR    R3,R3                                                            
         IC    R3,SCRLDL           LENGTH OF DETAIL LINE                        
         DR    R0,R3                                                            
         ICM   R3,3,NFST           R3=# OF FIRST                                
         AR    R3,R1               PLUS LINE NUMBER OF SELECTED LINE            
         CLM   R3,3,NBUF           TEST VS. MAX IN BUFFER                       
         BH    PFERR                                                            
         STCM  R3,3,NGET           SET NUMBER TO DELETE                         
         STC   R1,DITM             SAVE LINE NUMBER DELETED                     
*                                                                               
PFDEL3   BRAS  RE,DELLIN           DELETE LINE                                  
         XC    GSTXN,GSTXN                                                      
         ZAP   GSTXA,PZERO         CLEAR ROUNDING ADJUSTMENTS                   
         XC    PSTXN,PSTXN                                                      
         ZAP   PSTXA,PZERO                                                      
         OI    PFKSW,PFKDELT       SET DELETE FLAG                              
         OI    CTXSTAT,CTXRECAL                                                 
         MVC   NGET,NFST                                                        
         CLC   NGET,NBUF                                                        
         BNH   *+10                                                             
         XC    NGET,NGET                                                        
         OC    NBUF,NBUF           TEST BUFFER EMPTY                            
         BNZ   PFNXT               NO, DISPLAY FIRST                            
         XC    NGET,NGET           BUFFER EMPTY, SET FOR 'ENTER REQ..'          
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* SET UP NEXT SCREEN                                                 *          
**********************************************************************          
PFNXT    NI    PFKSW,X'FF'-(PFKRDSP)                                            
         BRAS  RE,ERASE            ** CLEAR THE SCREEN **                       
         XC    NLST,NLST                                                        
         XC    NFST,NFST                                                        
         MVI   NITM,0                                                           
         MVI   NLINL,0                                                          
         OC    NGET,NGET                                                        
         BNZ   PFNXT1                                                           
         BRAS  RE,DFLTNAR          SET DEFAULT NARRATIVE                        
         L     R2,ADETH            FIRST DETAIL LINE                            
         TM    PFKSW,PFKDELT                                                    
         JO    IMITDEL             'ITEM DELETED'                               
         J     IMEREQF             'ENTER REQUIRED...'                          
*                                                                               
PFNXT1   XR    R3,R3                                                            
         IC    R3,SCRMXD           MAX DETAIL LINE                              
         CLM   R3,3,NBUF           OR MAX IN BUFFER                             
         BL    *+8                                                              
         ICM   R3,3,NBUF                                                        
         XR    R0,R0                                                            
         L     R2,ADETH            FIRST DETAIL LINE                            
*                                                                               
PFNXT3   GOTOR TOTSAR,TSRGETQ      ** DISPLAY NEXT SET **                       
         BRAS  RE,SETLOW           SET ADCONS                                   
         TM    SCRSTA,SCRLISQ      TEST LIST SCREEN                             
         BNO   PFNXT5                                                           
         CLI   RECLOCN,2           TEST NUMBER OF LOCALITY TAX ITEMS            
         BL    PFNXT4              ONE OR LESS IS OK                            
         XR    RF,RF                                                            
         ICM   RF,1,SCRMXD         MAX ON SCREEN                                
         XR    RE,RE                                                            
         ICM   RE,1,NLINL          LESS NUMBER ALREADY ON                       
         SR    RF,RE                                                            
         ICM   RE,1,RECLOCN        LESS NUMBER NEEDED                           
         SR    RF,RE                                                            
         BM    PFNXT9              FORCE END OF SCREEN                          
*                                                                               
PFNXT4   GOTOR LIST,DMCB,PROGD     LIST CONTROLLER                              
         XR    RF,RF                                                            
         ICM   RF,1,RECLOCN        NUMBER OF TAX LINES USED                     
         BNZ   *+8                                                              
         LA    RF,1                                                             
         XR    R1,R1                                                            
         IC    R1,NLINL            UPDATE SCREEN COUNT                          
         AR    R1,RF                                                            
         STC   R1,NLINL                                                         
         CLI   RECLOCN,2           TEST NUMBER OF LOCALITY TAX ITEMS            
         BL    PFNXT6              ONE OR LESS IS OK                            
         XR    R1,R1                                                            
         IC    R1,RECLOCN                                                       
         BCTR  R1,0                                                             
*                                                                               
PFNXT4A  DS    0H                  BUMP TO NEXT AVAILABLE LINE                  
         L     R2,ANEXT                                                         
         BRAS  RE,SETLOW           SET ADCONS                                   
         BCT   R1,PFNXT4A                                                       
         B     PFNXT6                                                           
*                                                                               
PFNXT5   BRAS  RE,DSPREC           DISPLAY RECORD                               
PFNXT6   OC    NFST,NFST           TEST FIRST                                   
         BNZ   *+10                                                             
         MVC   NFST,NGET           SAVE NUMBER OF FIRST                         
         MVC   NLST,NGET           AND LAST                                     
         AHI   R0,1                COUNT NUMBER ON SCREEN                       
         STC   R0,NITM                                                          
         BCT   R3,*+8                                                           
         B     PFNXT9                                                           
*                                                                               
         CLC   NLINL,SCRMXD        TEST LIST MAX                                
         BNL   PFNXT9                                                           
         L     R2,ANEXT            POINT TO NEXT LINE                           
PFNXT7   XR    R1,R1                                                            
         ICM   R1,3,NGET           INCREMENT 'GET' NUMBER                       
         LA    R1,1(R1)                                                         
         STCM  R1,3,NGET                                                        
         CLC   NGET,NBUF           TEST FIND OF BUFFER                          
         BNH   PFNXT3                                                           
         CLI   NITM,0              TEST ANYTHING FOUND                          
         BNE   PFNXT9                                                           
         XC    NLST,NLST                                                        
         XC    NFST,NFST                                                        
         XC    NGET,NGET                                                        
         BRAS  RE,DFLTNAR          DISPLAY DEFAULT NARRATIVE                    
         L     R2,AINPH            FIRST DETAIL LINE                            
         TM    PFKSW,PFKDELT                                                    
         JO    IMITDEL             'ITEM DELETED'                               
         J     IMEREQF             'ENTER REQUIRED...'                          
*                                                                               
PFNXT9   OI    PFKSW,PFKPROC       PF KEY REQUEST PROCESSED                     
         TM    SCRSTA,SCRSINQ      TEST SINGLE ITEM SCREEN                      
         BNO   *+8                                                              
         OI    PROCSW,PROCDTL      SET TO PROCESS DETAIL (FOR NAMES)            
         J     XIT                'ITEM(S) XXX DISPLAYED...'                    
         EJECT                                                                  
**********************************************************************          
* SWITCH TO TAX                                                      *          
**********************************************************************          
PFTAX    TM    SCRSTA,SCRTAXQ      TEST ALREADY HAVE TAX SCREEN                 
         JO    EMMIVPFK            'INVALID PFKEY...                            
*                                                                               
         MVC   INVSCRN,SCRNUM      SAVE CURRENT SCREEN                          
         BRAS  RE,SAVTOP                                                        
         GOTOR LOADSCRN,SCRTAXS    LOAD TAX SCREEN                              
         J     PFINV3                                                           
                                                                                
**********************************************************************          
* SWITCH TO INVOICE                                                  *          
**********************************************************************          
PFINV    BRAS  RE,SAVTOP                                                        
         GOTOR LOADSCRN,INVSCRN    RESTORE INVOICE SCREEN                       
*                                                                               
PFINV3   BRAS  RE,SETLOW                                                        
         BRAS  RE,RESTOP           RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
         LA    R1,1                START AT FIRST                               
         OC    NBUF,NBUF                                                        
         BNZ   *+6                                                              
         XR    R1,R1               UNLESS NOTHING IN BUFFER                     
         STCM  R1,3,NGET                                                        
         B     PFNXT                                                            
         EJECT                                                                  
**********************************************************************          
* SWAP TO $FIS                                                       *          
**********************************************************************          
PFFIS    GOTOR TOTSAR,TSRSAVQ      SAVE BUFFER                                  
         LA    RF,TWAD             SAVE SOME DATA IN TWA                        
         AHI   RF,OSSAVE-TWAD                                                   
         MVC   0(STSLNQ,RF),STS                                                 
*                                                                               
         L     R3,ACOMFACS         PASS DUPLICATE KEY                           
         USING COMFACSD,R3                                                      
         LA    RF,INVPDA-INVPKEY                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',DUPINVK,(RF),GLVAINVP                     
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         LA    R2,XCTL             BUILD CONTROL ELEMENT                        
         USING GLVXCTLD,R2                                                      
         MVI   GLVXCODE,GLVXCODQ                                                
         MVI   GLVXLEN,GLVXLENQ                                                 
         MVC   GLVXFRSY,=C'ACC'                                                 
         MVC   GLVXFRPR,=C'INP'                                                 
         MVC   GLVXTOSY,=C'ACC'                                                 
         MVC   GLVXTOPR,=C'FIS'                                                 
         OI    GLVXFLG1,GLV1SEPS                                                
         GOTO1 CGLOBBER,DMCB,=C'PUTD',GLVXFRSY,22,GLVXCTL                       
         CLI   8(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         DROP  R2,R3                                                            
         EJECT                                                                  
**********************************************************************          
* PROCESS PF KEY 'POST'                                              *          
**********************************************************************          
PFPOST   XR    R3,R3                                                            
         ICM   R3,3,NBUF           NUMBER OF ITEMS IN BUFFER                    
         BZ    PFERR                                                            
         TM    POSTSW,POSTBUF+POSTNXT                                           
         BNZ   PFPOST5             NOT FIRST TIME                               
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         BO    PFPOST3                                                          
*                                  ** LOAD SINGLE SCREEN **                     
         BRAS  RE,SAVTOP                                                        
         GOTOR LOADSCRN,SCRSWAP    LOAD NEW SCREEN                              
         BRAS  RE,RESTOP           RESTORE THE TOP                              
         GOTOR VALHED,DMCB,PROGD   VALIDATE HEADING INFO                        
*                                                                               
PFPOST3  XC    NGET,NGET           FIRST TIME                                   
*                                                                               
PFPOST5  OI    POSTSW,POSTBUF      SET POST SWITCH                              
         NI    POSTSW,X'FF'-(POSTNXT) TURN OFF 'NEXT'                           
         OI    PROCSW,PROCDTL      USING DETAIL SCREEN                          
*                                                                               
         L     R2,ADETH            R2=A(DETAIL LINE)                            
         XR    R0,R0                                                            
         IC    R0,SCRMXD           MAX DETAIL LINES                             
         MVI   NITM,0              NUMBER ON SCREEN                             
         XC    NFST,NFST                                                        
*                                                                               
PFPOST7  XR    R1,R1                                                            
         ICM   R1,3,NGET           INCREMENT LINE NUMBER                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,NGET                                                        
         CLC   NGET,NBUF           TEST LAST DISPLAYED                          
         BH    PFPOST9             YES, ALL DONE                                
         OC    NFST,NFST                                                        
         BNZ   *+8                                                              
         STCM  R1,3,NFST           SAVE FIRST ON SCREEN                         
         GOTOR TOTSAR,TSRGETQ      GET NEXT LINE                                
         BRAS  RE,ERASE            CLEAR BOTTOM OF SCREEN                       
         BRAS  RE,SETLOW           SET ADCONS                                   
         GOTOR CLEAR,ACLRTAB       CLEAR NAME FIELDS                            
         BRAS  RE,DSPREC           DISPLAY RECORD                               
         MVC   NLST,NGET                                                        
         XR    R1,R1                                                            
         IC    R1,NITM                                                          
         AHI   R1,1                                                             
         STC   R1,NITM             NUMBER ON SCREEN                             
         L     R2,ANEXT                                                         
         BCT   R0,PFPOST7          TEST END OF SCREEN                           
*                                                                               
PFPOST9  CLI   NITM,0              ANYTHING TO POST                             
         BE    PFPOST11            NO, ALL DONE                                 
         OI    POSTSW,POSTNXT      SET FOR NEXT                                 
         J     XIT                                                              
*                                                                               
PFPOST11 GOTOR LOADSCRN,FRSTSCRN   RESTORE ORIGINAL SCREEN                      
         ZAP   POSTAMT,TOTDLRS     SAVE POSTED DOLLARS                          
         BRAS  RE,REINIT           RE-INTITIALIZE                               
         BRAS  RE,TSTMAX           TEST REACHED CONTROL TOTALS                  
         J     IMPOSTD             'N ITEMS POSTED $99                          
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PRODUCTION POSTINGS                                                *          
**********************************************************************          
PPOST    NMOD1 0,**PPOST*                                                       
         L     RC,0(R1)                                                         
         CLC   RECNUM,GSTXN        TEST RECORD NUMBER FOR GST ADJ.              
         BNE   *+10                                                             
         AP    ITMGST,GSTXA        ADD ADJUSTMENT                               
         CLC   RECNUM,PSTXN        TEST RECORD NUMBER FOR PST ADJ.              
         BNE   *+10                                                             
         AP    ITMPST,PSTXA        ADD ADJUSTMENT                               
*                                                                               
         BRAS  RE,ELMDLD           BUILD DESCRIPTION ELEMENT                    
*                                  **ELEMENTS FOR DEBIT TO JOB**                
         TM    ITMSTA,ITMXJOB      TEST FOR XJOB                                
         BZ    PPOST03             NO                                           
         USING SPDELD,R8                                                        
         MVI   SPDEL,SPDELQ        ADD ELEMENT WITH REAL CONTRA                 
         MVI   SPDLN,SPDLN1Q+L'ACTKCULA-1                                       
         MVC   SPDACCS(L'ACTKCULA-1),CASHACCT                                   
         OC    VENDACCT,VENDACCT   TEST FOR VENDOR                              
         BZ    *+18                NO, USE CASH                                 
         TM    BCCPYST3,CPYSCA22   TEST TO USE CASH AS CAC(EXPENSE BIT)         
         BO    *+10                YES                                          
         MVC   SPDACCS(L'ACTKCULA-1),VENDACCT+1 NO-USE VENDOR                   
         XR    R0,R0                                                            
         IC    R0,SPDLN                                                         
         AR    R8,R0                                                            
*                                                                               
PPOST03  BRAS  RE,ELMORD           ADD ORDER NUMBER ELEMENT                     
         CLI   PASSCD,C'N'         PASS CD TO CLIENT ?                          
         BE    *+8                                                              
         BRAS  RE,ELMDLH           BUILD/ADD CD ELEMENT                         
*                                                                               
         TM    ITMSTA,ITMXJOB      TEST FOR XJOB                                
         BZ    PPOST05             NO                                           
         USING SCIELD,R8                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSJXP                                                 
         ZAP   SCIAMNT,INAMNT      CARRY FINANCIAL POSTING AMT                  
         CP    CDAMNT,PZERO        IS THERE A DISCOUNT?                         
         BE    *+18                NO                                           
         CLI   CASHACCT,0          YES, IS THERE A CASH ACCOUNT?                
         BNE   *+10                YES, CD NOT ALLOWED                          
         SP    SCIAMNT,CDAMNT                                                   
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         BNE   PPOST04                                                          
         CLI   TXGON,TXGGRSQ       CANADIAN - GROSS                             
         BNE   PPOST04                                                          
         SP    SCIAMNT,ITMGST                                                   
         SP    SCIAMNT,ITMPST                                                   
PPOST04  LA    R8,SCILN1Q(R8)                                                   
*                                                                               
PPOST05  BRAS  RE,ELMTRL                                                        
         CLI   CASHACCT,0          IS THERE A CASH ACCOUNT?                     
         BNE   PPOST07             YES, NO PAYABLE POSTING THEN                 
*                                                                               
         LA    RF,PRODLIST                                                      
         CLC   VENDACCT+1(2),0(RF)                                              
         BE    *+20                                                             
         CLI   0(RF),X'FF'                                                      
         BE    PPOST07                                                          
         LA    RF,2(RF)                                                         
         B     *-22                                                             
*                                                                               
         MVC   PAYACC,VENDACCT                                                  
         BRAS  RE,ELMPAK           PAYABLE ACCOUNT ELEMENT                      
                                                                                
PPOST07  BRAS  RE,ELMFFL           ADD FFTEL FOR CLIENT/PROD                    
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER                      
*                                                                               
*                                  ** BUILD 69 DEBIT ELEMENT **                 
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSANAL,WRKCODE                                                 
         MVC   DLPSDBAC,JOBACCT    DEBIT JOB - SJ                               
         MVC   DLPSDBNM,JOBNAME                                                 
         MVC   DLPSCRAC,VENDACCT   CONTRA = PROD VENDOR                         
         MVC   DLPSCRNM,VENDNAME                                                
*                                                                               
         CLI   VENDACCT,0          IS THERE A VENDOR?                           
         BE    PPOST09             NO - MUST USE CASH                           
         CLI   CASHACCT,0          DO WE HAVE CASH ACCT?                        
         BE    PPOST11             NO.                                          
         TM    BCCPYST2,CPYSCACA   CONTRA A/C ON JOB BT3 IS CASH A/C            
         BZ    PPOST11             NO                                           
*                                                                               
PPOST09  MVC   DLPSCRAC,CASHACCT   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,CASHNAME   CASH ACCT NAME.                              
*                                                                               
PPOST11  TM    ITMSTA,ITMXJOB      TEST XJOB                                    
         BZ    PPOST13                                                          
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVC   DLPSCRAC,EXCSEAC    SET CAC=EXPENSE JOB                          
         MVC   DLPSCRNM,EXCSENM                                                 
         DROP  R3                                                               
*                                                                               
PPOST13  MVI   DLPSTYPE,0                                                       
         TM    ITMSTA,ITMNONC      TEST NON-COMMISSIONABLE                      
         BNO   *+8                                                              
         OI    DLPSTYPE,DLPSTNCM   SET FOR NO COMMISSION                        
         ZAP   DLPSAMNT,INAMNT     NET                                          
         CLI   TXGON,TXGGRSQ       TEST CANADIAN - GROSS                        
         BNE   *+16                                                             
         SP    DLPSAMNT,ITMGST                                                  
         SP    DLPSAMNT,ITMPST                                                  
         CP    CDAMNT,PZERO        IS DISCOUNT = 0                              
         BE    PPOST14             YES                                          
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   PPOST14             YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         DO WE PASS CD ALONG?                         
         BE    PPOST14             NO                                           
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
*                                                                               
PPOST14  TM    ITMSTA,ITMXJOB      TEST XJOB                                    
         BZ    *+10                                                             
         ZAP   DLPSAMNT,PZERO      MAKE A ZERO FINANCIAL POSTING                
*                                                                               
         LA    R3,FFTWKELM         MAKE A FFTTT WORKCODE ELEMENT                
         USING FFTELD,R3                                                        
         XC    0(13,R3),0(R3)                                                   
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,13                                                         
         MVI   FFTTYPE,FFTTWRKC                                                 
         MVI   FFTDLEN,8                                                        
         MVC   FFTDATA(2),WRKCODE                                               
         ZAP   FFTDATA+2(6),DLPSAMNT                                            
         DROP  R3                                                               
*                                                                               
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
         EJECT                                                                  
**********************************************************************          
* ADD CANADIAN GST/PST POSTINGS                                      *          
**********************************************************************          
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         BNE   PPOST15             NO,                                          
         TM    POSTSW,POSTVEN+POSTEXP TEST ANY VENDORS                          
         BZ    PPOST15                NO, SKIP GST/PST                          
         GOTOR CPOST,DMCB,PROGD    GST/PST POSTINGS                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD ELEMENTS FOR CREDIT TO VENDOR                                *          
**********************************************************************          
*                                  ** CANADIAN TAX ELEMENTS **                  
         ZAP   DUB,ITMBASE                                                      
         CLI   ITMGTY,TXGNONQ      TEST 'NO TAX'                                
         BE    *+8                                                              
         BRAS  RE,ELMGTX           GST ELEMENT                                  
         AP    DUB,ITMPST                                                       
         CP    ITMPST,PZERO                                                     
         BE    *+8                                                              
         BRAS  RE,ELMPTX           PST ELEMENT                                  
*                                                                               
PPOST15  CLC   CASHACCT+1(2),=C'SR' TEST CASH OVERRIDE AN SR                    
         BNE   PPOST15X             NO                                          
         BRAS  RE,ELMOTH           ADD PRODUCT/JOB ELEMENT                      
         BRAS  RE,ELMMTD           ADD MEDIA ELEMENT                            
*                                                                               
         MVI   DLPSEL,DLPSEDRQ      MAKE IT A MINUS DEBIT                       
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CASHACCT   CASH ACCT NUMBER.                            
         MVC   DLPSDBNM,CASHNAME   CASH ACCT NAME.                              
         MVC   DLPSCRAC,JOBACCT    CONTRA CLI/PROD/JOB                          
         MVC   DLPSCRNM,JOBNAME                                                 
         MVC   DLPSANAL,CLIOFFIC                                                
         ZAP   DLPSAMNT,INAMNT                                                  
         MP    DLPSAMNT,PNEG1                                                   
         B     PPOST16A                                                         
*                                                                               
PPOST15X BRAS  RE,ELMOTH           ADD PRODUCT/JOB ELEMENT                      
         BRAS  RE,ELMDLH           ADD CD ELEMENT                               
         BRAS  RE,ELMTRL           ADD TRANSACTION ELEMENT                      
         BRAS  RE,ELMFFL           ADD FREEFORN TEXT                            
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,ELMDUE           ADD DUE DATE ELEMENT                         
         BRAS  RE,ELMWKC           ADD FFTTWRK                                  
*                                                                               
*                                  ** BUILD 6A CREDIT ELEMENT **                
         USING DLPOSTD,R8                                                       
PPOST16  MVI   DLPSEL,DLPSECRQ     CREDIT SUPPLIER                              
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVI   DLPSTYPE,0                                                       
         MVC   DLPSDBAC,CLIACCT    CONTRA = PROD VENDOR                         
         MVC   DLPSDBNM,CLINAME                                                 
         MVC   DLPSCRAC,VENDACCT   CR JOB - SJ                                  
         MVC   DLPSCRNM,VENDNAME                                                
         CLC   VENDACCT+1(2),SXVND                                              
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOBACCT    OR CLI/PRD/JOB FOR EXPENSE VENDORS           
         MVC   DLPSDBNM,JOBNAME                                                 
*                                                                               
         CLI   CASHACCT,0          IS THERE A CASH ACCOUNT?                     
         BE    *+22                NO.                                          
         MVC   DLPSDBAC+6(3),PROACCT+6       IF CASH WE NEED PROD               
         MVC   DLPSCRAC,CASHACCT   CASH ACCT NUMBER.                            
         MVC   DLPSCRNM,CASHNAME   CASH ACCT NAME.                              
*                                                                               
         MVC   DLPSANAL,CLIOFFIC                                                
         OC    CRDOFF,CRDOFF       TEST FOR CREDIT OFFICE                       
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF     YES-USE IT FOR CREDIT POSTING                
         ZAP   DLPSAMNT,INAMNT                                                  
         CLI   CASHACCT,0          IS THERE A CASH ACCOUNT?                     
         BNE   *+10                YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT                                                  
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         BNE   PPOST16A                                                         
         CLI   TXGON,TXGGRSQ       TEST CANADIAN - GROSS                        
         BE    PPOST16A                                                         
         AP    DLPSAMNT,ITMGST                                                  
         AP    DLPSAMNT,ITMPST                                                  
*                                                                               
*                                                                               
PPOST16A LR    R6,R8               SAVE A(CREDIT ELEMENT)                       
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* BUILD ELEMENTS FOR CREDIT TO INCOME                                *          
**********************************************************************          
         CP    CDAMNT,PZERO        DID VENDOR HAVE DISCOUNT                     
         BE    PPOST17             NO                                           
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   PPOST17             YES - NO CD ALLOWED                          
         CLI   PASSCD,C'N'         CD FOR SUPPLIER                              
         BNE   PPOST17                                                          
*                                                                               
         BRAS  RE,ELMTRL           ADD STATUS ELEMENT                           
         BRAS  RE,ELMFFL           ADD FREEFORM TEXT                            
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,ELMWKC           ADD FFTTWRK                                  
*                                                                               
         XR    R1,R1                                                            
         IC    R1,1(R6)            LENGTH OF DLPOST ELEMENT                     
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R8),0(R6)       RESTORE CREDIT ELEMENT                       
         MVI   DLPSEL,DLPSECRQ     MAKE SURE IT'S A CREDIT                      
         MVC   DLPSCRAC,SIMDAC     CREDIT TO INCOME                             
         MVC   DLPSCRNM,SIMDACN                                                 
         MVC   DLPSDBAC,PROACCT    CLI/PROD AS CONTRA                           
         MVC   DLPSDBNM,PRONAME                                                 
         ZAP   DLPSAMNT,CDAMNT                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
PPOST17  CLI   PCONSULT,C'Y'       SHOULD WE POST TO 2C?                        
         BNE   PPOST19             NO.                                          
*                                                                               
         BRAS  RE,ELMTRL           ADD STATUS ELEMENT                           
         BRAS  RE,ELMFFL           ADD FREEFORM TEXT                            
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,ELMWKC           ADD FFTTWRK                                  
*                                                                               
         USING DLPOSTD,R8          ** POST 2C **                                
         MVI   DLPSEL,DLPSEDCQ     DEBIT & CREDIT                               
         MVI   DLPSLEN,DLPSLNQ     LENGTH                                       
         MVC   DLPSDBAC,P2CNUM     DB ACCT #                                    
         MVC   DLPSDBNM,P2CNAM     DB ACCT NAME                                 
         MVC   DLPSCRAC,PROTROL    CR ACCT #                                    
         MVC   DLPSCRNM,PROTROLN   CR ACCT NAME                                 
         OI    DLPSTYPE,DLPSTSUB                                                
         MVC   DLPSANAL,CLIOFFIC   OFFICE                                       
         OC    CRDOFF,CRDOFF       TEST CREDIT OFFICE OVERRIDE                  
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF                                                  
         ZAP   DLPSAMNT,INAMNT                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
PPOST19  TM    ITMSTA,ITMXJOB      TEST XJOB                                    
         BZ    PPOST31             NO                                           
         BRAS  RE,ELMFFL           ADD FREEFORM TEST                            
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,ELMWKC           ADD FFTTWRK                                  
*                                                                               
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVI   EXCACT,EXCAPST      POST CALL                                    
         ST    R9,EXCAGWS                                                       
         ST    R8,EXCADAY                                                       
         MVC   EXCFINO,ANAOFF                                                   
*                                                                               
         MVC   EXCSECAC,VENDACCT   EXPENSE POSTING CAC                          
         MVC   EXCSECAN,VENDNAME   AND CAC NAME                                 
         CLI   VENDACCT,0          TEST FOR A VENDOR                            
         BE    PPOST21             NO-USE CASH ACCOUNT                          
         CLI   CASHACCT,0          TEST FOR CASH ACCOUNT                        
         BE    PPOST23             NO                                           
         TM    BCCPYST3,CPYSCA22   TEST EXPENSE BIT TO USE CASH AS CAC          
         BZ    PPOST23             NO                                           
*                                                                               
PPOST21  MVC   EXCSECAC,CASHACCT                                                
         MVC   EXCSECAN,CASHNAME                                                
*                                                                               
PPOST23  ZAP   EXCAMNT,INAMNT      SET POSTING AMOUNT                           
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         BNE   PPOST24                                                          
         CLI   TXGON,TXGGRSQ       TEST CANADIAN - GROSS                        
         BNE   PPOST24                                                          
         SP    EXCAMNT,ITMGST                                                   
         SP    EXCAMNT,ITMPST                                                   
*                                                                               
PPOST24  CP    CDAMNT,PZERO        TEST FOR CASH DISCOUNT                       
         BE    PPOST25             NO                                           
         CLI   CASHACCT,0          TEST POSTING TO CASH ACCOUNT                 
         BNE   PPOST25             YES                                          
         SP    EXCAMNT,CDAMNT      DEDUCT CD FROM POSTING                       
         ZAP   EXCDAMNT,CDAMNT                                                  
*                                                                               
PPOST25  MVC   EXCFINO,FINOFF                                                   
         GOTO1 VEXCEL,EXCELD                                                    
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R8,EXCADAY                                                       
*                                                                               
PPOST31  ZAP   ITMTAX,PZERO                                                     
         TM    RECSTA,ITMTAXD      TEST TAX                                     
         JNO   ELMEND              FINISH UP THE POSTIND                        
         GOTOR TPOST,DMCB,PROGD                                                 
         J     ELMEND                                                           
*                                                                               
PRODLIST DC    C'SWSXSYSV',X'FF'                                                
*                                                                               
         LTORG                                                                  
         DROP  R3,R8,RB                                                         
         EJECT                                                                  
**********************************************************************          
* EXPENSE POSTINGS                                                   *          
**********************************************************************          
EPOST    NMOD1 0,**EPOST*                                                       
         L     RC,0(R1)                                                         
         CLC   RECNUM,GSTXN        TEST RECORD NUMBER FOR GST ADJ.              
         BNE   *+10                                                             
         AP    ITMGST,GSTXA        ADD ADJUSTMENT                               
         CLC   RECNUM,PSTXN        TEST RECORD NUMBER FOR PST ADJ.              
         BNE   *+10                                                             
         AP    ITMPST,PSTXA        ADD ADJUSTMENT                               
*                                                                               
         BRAS  RE,ELMDLD           BUILD DESCRIPTION ELEMENT                    
         BRAS  RE,ELMDLH           BUILD/ADD CD ELEMENT                         
         BRAS  RE,ELMANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMORD           ADD ORDER NUMBER ELEMENT                     
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     **DEBIT EXPENSE ACCOUNT **                   
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC       DEBIT ACCOUNT - DEF EXP               
         MVC   DLPSCRAC(51),XVNDACCT      CONTRA ACCOUNT - DEF VENDOR           
         CLI   XVNDACCT,0          IS THERE A EXP VENDOR?                       
         BE    EPOST03             NO - MUST USE CASH                           
         CLI   CASHACCT,0          CASH ACCOUNT?                                
         BE    EPOST05             NO                                           
         TM    BCCPYST3,CPYSCA22   C/A ON SE POSTING IS CASH A/C                
         BZ    EPOST05             NO..                                         
*                                                                               
EPOST03  MVC   DLPSCRAC(51),CASHACCT      CASH ACCT # & NAME                    
*                                                                               
EPOST05  MVC   DLPSANAL,FINOFF                                                  
         ZAP   DLPSAMNT,INAMNT                                                  
         CP    CDAMNT,PZERO        IS DIC = 0                                   
         BE    EPOST07             YES                                          
         CLI   CASHACCT,0          DO WE HAVE A CASH ACCT?                      
         BNE   EPOST07             YES - NO CD ALLOWED                          
         TM    BCCPYST1,CPYSDISC   C/D ON EXPENSES TO INCOME ACCT               
         BNZ   EPOST07             YES                                          
         SP    DLPSAMNT,CDAMNT     NET LESS C.D                                 
*                                                                               
EPOST07  CLI   TXGON,TXGGRSQ       TEST CANADIAN - GROSS                        
         BNE   *+16                                                             
         SP    DLPSAMNT,ITMGST                                                  
         SP    DLPSAMNT,ITMPST                                                  
         ZAP   EXPOST,DLPSAMNT     SAVE EXPENSE POSTING                         
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* ADD CANADIAN GST/PST POSTINGS                                      *          
**********************************************************************          
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         BNE   EPOST09             NO,                                          
         TM    POSTSW,POSTVEN+POSTEXP TEST ANY VENDORS                          
         BZ    EPOST09                NO, SKIP GST/PST                          
         GOTOR CPOST,DMCB,PROGD    GST/PST POSTINGS                             
*                                                                               
         EJECT                                                                  
**********************************************************************          
* CREDIT VENDOR                                                      *          
**********************************************************************          
*                                  ** CANADIAN TAX ELEMENTS **                  
         ZAP   DUB,ITMBASE                                                      
         CLI   ITMGTY,TXGNONQ      TEST 'NO TAX'                                
         BE    *+8                                                              
         BRAS  RE,ELMGTX           GST ELEMENT                                  
         AP    DUB,ITMPST                                                       
         CP    ITMPST,PZERO                                                     
         BE    *+8                                                              
         BRAS  RE,ELMPTX           PST ELEMENT                                  
*                                  ** CREDIT VENDOR **                          
EPOST09  DS    0H                                                               
         BRAS  RE,ELMDLH           BUILD/ADD CD ELEMENT                         
         BRAS  RE,ELMANL           ANALYSIS OFFICE                              
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,ELMDUE           ADD DUE DATE ELEMENT                         
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ                                                  
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC(51),POSTACC        CONTRA ACCOUNT - EXP                 
         MVC   DLPSCRAC(51),XVNDACCT       CREDIT ACCOUNT - VENDOR              
         MVC   DLPSANAL,FINOFF                                                  
         OC    CRDOFF,CRDOFF       TEST FOR CREDIT OFFICE                       
         BZ    *+10                NO                                           
         MVC   DLPSANAL,CRDOFF                                                  
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BE    EPOST11             NO.                                          
         MVC   DLPSCRAC,CASHACCT   CASH ACCT #                                  
         MVC   DLPSCRNM,CASHNAME   CASH ACCT NAME                               
*                                                                               
EPOST11  ZAP   DLPSAMNT,INAMNT     NET                                          
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   *+10                YES - NO CD ALLOWED                          
         SP    DLPSAMNT,CDAMNT     NET - CD                                     
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         BNE   EPOST12                                                          
         CLI   TXGON,TXGGRSQ       TEST CANADIAN - GROSS                        
         BE    EPOST12                                                          
         AP    DLPSAMNT,ITMGST                                                  
         AP    DLPSAMNT,ITMPST                                                  
EPOST12  LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
         CP    CDAMNT,PZERO        DID VENDOR HAVE DISCOUNT                     
         BE    EPOST13             NO                                           
         CLI   CASHACCT,0          IS THERE A CASH ACCT?                        
         BNE   EPOST13             YES - NO CD ALLOWED                          
         TM    BCCPYST1,CPYSDISC   C/D ON EXPENSES TO INCOME ACCT               
         BZ    EPOST13             NO                                           
*                                                                               
         BRAS  RE,ELMANL           ANALYSIS OFFICE                              
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         MVI   DLPSEL,DLPSECRQ     **CREDIT INCOME ACCOUNT **                   
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,XVNDACCT                                                
         MVC   DLPSDBNM,XVNDNAME                                                
         MVC   DLPSCRAC,SIMDAC                                                  
         MVC   DLPSCRNM,SIMDACN                                                 
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,CDAMNT                                                  
         MVC   DLPSANAL,FINOFF                                                  
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 2P WITH 29 CONTRA POSTING                                          *          
**********************************************************************          
EPOST13  TM    ANLSTA,ANLSTF                                                    
         BNO   EPOST21                                                          
         BRAS  RE,ELMANL           ANALYSIS OFFICE                              
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     ** DEBIT STAFF **                            
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,STAFFNUM   DEBIT 2P                                     
         MVC   DLPSDBNM,STAFFNAM                                                
         MVC   DLPSCRNM,CRPSNAME   CONTRA 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,DLPSTSUB   SUBSIDIARY FROM HERE                         
         MVI   DLPSCRAC,C'*'       CONTRA IS *EXPENSE-CLIENT                    
         MVC   DLPSANAL,FINOFF                                                  
*                                                                               
         L     R2,AXACH            CREDIT SE ACCT                               
         XR    R1,R1                                                            
         IC    R1,5(R2)            INPUT LENGTH                                 
         BCTR  R1,0                FOR EX INST.                                 
*                                                                               
         CLI   8(R2),C'*'                                                       
         BNE   *+8                                                              
         SHI   R1,3                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),POSTACC+3                                          
*                                                                               
         CLI   TENO,X'F0'          LENGTH OF ACCT WANTED                        
         BL    EPOST15                                                          
         PACK  DUB,TENO                                                         
         CVB   R4,DUB                                                           
         MVC   DLPSCRAC+1(14),SPACES                                            
         L     R2,AXACH                                                         
         LA    R6,1(R1)            INPUT LENGTH                                 
         SR    R6,R4               IF DESIRED LEN GREATER THAN GIVEN            
         BNM   *+6                                                              
         DC    H'0'                SHOULD HAVE CAUGHT THIS EARLIER              
         LA    R3,8(R6,R2)         R3=A(DESIRED PORTION ACCT INPUT)             
         BCTR  R4,0                                                             
         EX    R4,*+8                                                           
         B     *+10                                                             
         MVC   DLPSCRAC+1(0),0(R3) = * +DESIRED PORTION ACCT INPUT              
         LR    R1,R4                                                            
*                                                                               
EPOST15  STC   R1,BYTE             LEN OF ACCT INPUT DESIRED                    
         LA    RF,DLPSCRAC+2(R1)                                                
         MVI   0(RF),C'-'                                                       
         LA    R1,DLPSCRAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),CRPSNUM+3   2/9 ACCT KEY                                 
         MVC   WORK(15),DLPSCRAC   SAVE FOR DEBIT SIDE OF CRD CLI EL            
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 29 WITH 2P CONTRA POSTING                                          *          
**********************************************************************          
         BRAS  RE,ELMANL           ANALYSIS OFFICE                              
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     ** CREDIT CLIENT **                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBNM,STAFFNAM   CONTRA 2P                                    
         MVC   DLPSCRNM,CRPSNAME   CREDIT 29                                    
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,DLPSTSUB   SUBSIDIARY FROM HERE                         
         MVC   DLPSDBAC(15),WORK   SAVED FROM DEBIT ELEMENT                     
         MVC   DLPSCRAC,CRPSNUM                                                 
         XR    R1,R1                                                            
         IC    R1,BYTE                                                          
         LA    R1,1(R1)                                                         
         LA    RF,DLPSDBAC+1(R1)                                                
         LA    R1,DLPSDBAC+14                                                   
         SR    R1,RF                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   1(0,RF),STAFFNUM+3  CONTRA IS *EXPENSE-STAFF                     
         MVC   DLPSANAL,FINOFF                                                  
*                                                                               
         CLI   V29SW,C'Y'          SHOULD CONTRA BE VENDOR?                     
         BNE   *+16                NO                                           
         MVC   DLPSDBAC,XVNDACCT   EXP VENDOR                                   
         MVC   DLPSDBNM,XVNDNAME   EXP VENDOR NAME                              
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 2D WITH 28 CONTRA POSTING  DR                                      *          
* 28 WITH 2D CONTRA POSTING  CR                                      *          
**********************************************************************          
EPOST21  TM    ANLSTA,ANLDEP                                                    
         BNO   EPOST25                                                          
         BRAS  RE,ELMANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     ** DEBIT/CREDIT **                           
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,DEPNUM     DEBIT 2D   CONTRA = 28                       
         MVC   DLPSDBNM,DEPNAME                                                 
         MVC   DLPSCRAC,CRDSNUM    CREDIT 28  CONTRA = 2D                       
         MVC   DLPSCRNM,CRDSNAME                                                
         ZAP   DLPSAMNT,EXPOST                                                  
         MVC   DLPSANAL,FINOFF                                                  
         OI    DLPSTYPE,DLPSTSUB   IN CASE WE MISSED IT ON ELEMENT 2            
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 2C WITH 27 CONTRA POSTING                                          *          
* 27 WITH 2C CONTRA POSTING                                          *          
**********************************************************************          
EPOST25  CLI   ECONSULT,C'Y'       DO WE NEED EXP 2C?                           
         BNE   EPOST29             NO.                                          
         BRAS  RE,ELMANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDCQ     ** DEBIT/CREDIT **                           
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,E2CNUM     ACCT #                                       
         MVC   DLPSDBNM,E2CNAM     ACCT NAME                                    
         MVC   DLPSCRAC,EXPTROL    27                                           
         MVC   DLPSCRNM,EXPTROLN   27                                           
         ZAP   DLPSAMNT,INAMNT     AMT                                          
         MVC   DLPSANAL,FINOFF                                                  
         OC    CRDOFF,CRDOFF       USE CREDIT OFFICE IF AVAILABLE               
         BZ    *+10                                                             
         MVC   DLPSANAL,CRDOFF                                                  
         OI    DLPSTYPE,DLPSTSUB                                                
         LA    R8,DLPSLNQ(R8)                                                   
         EJECT                                                                  
**********************************************************************          
* 1C WITH 1P CONTRA POSTING                                          *          
**********************************************************************          
EPOST29  CLI   COSTSW,C'Y'                                                      
         BNE   EPOST33                                                          
         BRAS  RE,ELMANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     ** DEBIT 1C **                               
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CONTRA  1C                                   
         MVC   DLPSCRNM,COSTNAME                                                
         MVC   DLPSDBAC,CRCNUM     DEBIT 1P                                     
         MVC   DLPSDBNM,CRCNAME                                                 
         ZAP   DLPSAMNT,EXPOST                                                  
         OI    DLPSTYPE,DLPSTSUB                                                
         MVC   DLPSANAL,FINOFF                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
         BRAS  RE,ELMANL           ANALYSIS OFFICE ELEMENT                      
         BRAS  RE,ELMFFL           FREEFORM TEXT                                
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     ** CREDIT CLIENT **                          
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSCRAC,COSTNUM    CR 1C                                        
         MVC   DLPSCRNM,COSTNAME                                                
*                                                                               
         MVC   DLPSDBAC(1),COMPANY                                              
         MVC   DLPSDBAC+1(2),=C'13'                                             
         MVC   DLPSDBAC+3(12),SPACES                                            
         MVC   DLPSDBAC+3(L'COSTANAL),COSTANAL                                  
         TM    BCCPYST5,CPYSNCST   NEW COST?                                    
         BZ    *+10                NO                                           
         MVC   DLPSDBAC,CR13NUM    YES, GET CONTRA FROM CATCALL                 
*                                                                               
         MVC   DLPSDBNM,CRCNAME                                                 
         OI    DLPSTYPE,DLPSTSUB                                                
         MVC   DLPSANAL,FINOFF                                                  
         ZAP   DLPSAMNT,EXPOST                                                  
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
EPOST33  ZAP   ITMTAX,PZERO                                                     
         TM    RECSTA,ITMTAXD      TEST TAX                                     
         JNO   ELMEND              FINISH UP THE POSTIND                        
         MVC   TAXOFF,CRDOFF                                                    
         CLI   TAXOFF,C' '                                                      
         BH    *+10                                                             
         MVC   TAXOFF,FINOFF                                                    
         GOTOR TPOST,DMCB,PROGD                                                 
         J     ELMEND                                                           
*                                                                               
         LTORG                                                                  
         DROP  R8,RB                                                            
*                                                                               
         EJECT                                                                  
**********************************************************************          
* TAX POSTINGS  - US                                                 *          
**********************************************************************          
TPOST    NMOD1 0,**TPOST*                                                       
         L     RC,0(R1)                                                         
         MVI   TAXCNT,0                                                         
*                                                                               
         L     R5,ATAXIT                                                        
         USING TXD,R5                                                           
TPOST3   DS    0H                                                               
         AP    ITMTAX,TXTAX                                                     
*                                                                               
         LA    R3,FFTTXELM         MAKE A FFTT TAX WC  ELEMENT                  
         USING FFTELD,R3                                                        
         XC    0(13,R3),0(R3)                                                   
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTLN,13                                                         
         MVI   FFTTYPE,FFTTWRKC                                                 
         MVI   FFTDLEN,8                                                        
         MVC   FFTDATA(2),TXWKC                                                 
         ZAP   FFTDATA+2(6),TXTAX                                               
         DROP  R3                                                               
*                                                                               
         TM    RECSTA,ITMEXPN      TEST EXPENSE ITEM                            
         BNO   *+8                                                              
         BRAS  RE,ELMTXWKC                                                      
         BRAS  RE,ELMLIN           LONG INVOICE NUMBER                          
         BRAS  RE,ELMSUT           SALES TAXES ELEMENT                          
         MVC   PAYACC,TXACC                                                     
         BRAS  RE,ELMPAK           ADD PAYABLE ACCOUNT ELEMENT                  
*                                                                               
         TM    ITMSTA,ITMXJOB      TEST FOR XJOB                                
         BNO   TPOST3A             NO                                           
         USING SPDELD,R8                                                        
         MVI   SPDEL,SPDELQ        ADD ELEMENT WITH REAL CONTRA                 
         MVI   SPDLN,SPDLN1Q+L'ACTKCULA-1                                       
         MVC   SPDACCS(L'ACTKCULA-1),CASHACCT                                   
         OC    VENDACCT,VENDACCT   TEST FOR VENDOR                              
         BZ    *+18                NO, USE CASH                                 
         TM    BCCPYST3,CPYSCA22   TEST TO USE CASH AS CAC(EXPENSE BIT)         
         BO    *+10                YES                                          
         MVC   SPDACCS(L'ACTKCULA-1),VENDACCT+1 NO-USE VENDOR                   
         LLC   R0,SPDLN                                                         
         AR    R8,R0                                                            
*                                                                               
         USING SCIELD,R8                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITSJXP                                                 
         ZAP   SCIAMNT,TXTAX       CARRY TAX AMOUNT                             
         LA    R8,SCILN1Q(R8)                                                   
*                                                                               
*                                  ** BUILD 69 DEBIT ELEMENT **                 
         USING DLPOSTD,R8                                                       
TPOST3A  MVI   DLPSEL,DLPSEDRQ     DEBIT JOB                                    
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,JOBACCT                                                 
         MVC   DLPSDBNM,JOBNAME                                                 
*                                                                               
         TM    ITMSTA,ITMXJOB      TEST XJOB                                    
         BNO   TPOST3B             NO,                                          
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVC   DLPSCRAC,EXCSEAC    SET CAC=EXPENSE JOB                          
         MVC   DLPSCRNM,EXCSENM                                                 
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,PZERO                                                   
         B     TPOST4A                                                          
         DROP  R3                                                               
*                                                                               
TPOST3B  TM    RECSTA,ITMEXPN      TEST EXPENSE ITEM                            
         BNO   TPOST4                                                           
         MVC   DLPSDBAC,POSTACC    DEBIT EXPENSE                                
         MVC   DLPSDBNM,POSTACCN                                                
         MVI   DLPSEL,DLPSEDCQ     DEBIT/CREDIT PAIR                            
*                                                                               
TPOST4   MVC   DLPSCRAC,TXACC      CONTRA - PAYABLE ACCOUNT                     
         MVC   DLPSCRNM,TXACCN                                                  
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,TXTAX                                                   
*                                                                               
TPOST4A  MVC   DLPSANAL,TXWKC                                                   
         CLC   DLPSDBAC+1(2),=C'SJ'                                             
         BE    *+10                                                             
         MVC   DLPSANAL,TAXOFF                                                  
         LA    R8,DLPSLNQ(R8)                                                   
         TM    RECSTA,ITMEXPN      TEST EXPENSE ITEM                            
         BO    TPOST5                                                           
*                                                                               
         TM    RECSTA,ITMXJOB                                                   
         BNO   TPOST4X                                                          
         BRAS  RE,ELMFFL           ADD FREEFORM TEST                            
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,ELMTXWKC                                                      
         L     R3,AEXCELD                                                       
         USING EXCELD,R3                                                        
         MVI   DLPSEL,DLPSEDRQ     DEBIT EXPENSE ACCOUNT                        
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,EXCSEAC                                                 
         MVC   DLPSDBNM,EXCSENM                                                 
         MVC   DLPSCRAC,VENDACCT                                                
         MVC   DLPSCRNM,VENDNAME                                                
                                                                                
         ZAP   DLPSAMNT,TXTAX      AMOUNT                                       
         MVC   DLPSANAL,ANAOFF     OFFICE                                       
*                                                                               
         LA    R8,DLPSLNQ(R8)                                                   
         DROP  R3                                                               
*                                                                               
TPOST4X  BRAS  RE,ELMOTH           ADD PRODUCT/JOB ELEMENT                      
         BRAS  RE,ELMSUT           SALES TAXES ELEMENT                          
         BRAS  RE,ELMLIN           ADD LONG INVOICE NUMBER ELEMENT              
         BRAS  RE,ELMTXWKC                                                      
*                                                                               
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSECRQ     CREDIT POSTING                               
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,CLIACCT    CONTRA ACCOUNT IS CLIENT                     
         MVC   DLPSDBNM,CLINAME                                                 
*                                                                               
         CLC   TXACC+1(2),SXVND                                                 
         BNE   *+16                                                             
         MVC   DLPSDBAC,JOBACCT    FOR SX IT'S CLI/PRD/JOB                      
         MVC   DLPSDBNM,JOBNAME                                                 
*                                                                               
         MVC   DLPSCRAC,TXACC      CREDIT ACCOUNT                               
         MVC   DLPSCRNM,TXACCN     CREDIT ACCOUNT NAME                          
*                                                                               
         MVI   DLPSTYPE,0          MAIN ACCOUNTING ENTRY                        
         ZAP   DLPSAMNT,TXTAX      AMOUNT                                       
         MVC   DLPSANAL,TAXOFF     OFFICE                                       
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
TPOST5   LA    R5,TXLNQ(R5)                                                     
         CLI   TXACC,0                                                          
         BE    TPOSTX                                                           
*                                                                               
         SR    R1,R1                                                            
         IC    R1,TAXCNT                                                        
         AHI   R1,1                                                             
         STC   R1,TAXCNT                                                        
         LA    R0,MXTXLQ           MAX TAX LOCALITIES                           
         CR    R1,R0                                                            
         BL    TPOST3                                                           
*                                                                               
TPOSTX   XIT1  REGS=(R8)                                                        
*                                                                               
         LTORG                                                                  
         DROP  R5,R8,RB                                                         
         EJECT                                                                  
**********************************************************************          
* TAX POSTINGS  - CANADA                                             *          
**********************************************************************          
CPOST    NMOD1 0,**CPOST*                                                       
         L     RC,0(R1)                                                         
*                                                                               
         CLI   ITMGTY,C'X'         IF TYPES X OR Z SKIP ZERO TEST               
         BE    CPOST1              MAKE ALL POSTINGS                            
         CLI   ITMGTY,C'Z'                                                      
         BE    CPOST1                                                           
         CLI   ITMPTY,C'X'                                                      
         BE    CPOST1                                                           
         CLI   ITMPTY,C'Z'                                                      
         BE    CPOST1                                                           
*                                                                               
         CP    ITMGST,PZERO        TEST ANY GST                                 
         BNE   *+14                YES,                                         
         CP    ITMPST,PZERO        TEST ANY PST                                 
         JE    CPOSTX              NO, SKIP TAX POSTINGS                        
*                                  ** POST GST **                               
CPOST1   LA    R5,GSTDATA                                                       
         USING VTCEFFD,R5                                                       
         ZAP   DUB,ITMBASE                                                      
         BAS   RE,ELMBAS           ADD BASIS ELEMENT                            
*                                                                               
*                                  ** BUILD 69 DEBIT ELEMENT **                 
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT GST ACCOUNT                            
         MVI   DLPSLEN,DLPSLNQ                                                  
         LA    R5,GSTDATA                                                       
         USING VTCEFFD,R5                                                       
         MVC   DLPSDBAC,VTCACT                                                  
         MVC   DLPSDBNM,VTCACTNM                                                
*                                                                               
         MVC   DLPSCRAC,VENDACCT   CONTRA - VENDOR                              
         MVC   DLPSCRNM,VENDNAME                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,ITMGST                                                  
         LA    RF,CLIOFFIC                                                      
         CLI   TYPE,C'P'                                                        
         JE    CPOST3                                                           
         MVC   DLPSCRAC,XVNDACCT   EXP - VENDOR                                 
         MVC   DLPSCRNM,XVNDNAME                                                
         LA    RF,FINOFF                                                        
         CLI   0(RF),C' '                                                       
         BH    CPOST3                                                           
         LA    RF,CRDOFF                                                        
*                                                                               
CPOST3   MVC   DLPSANAL,0(RF)      OFFICE CODE                                  
         CLI   DLPSANAL,C' '                                                    
         BH    *+6                                                              
         DC    H'0'                MISSING OFFICE                               
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
*                                  ** POST PST **                               
CPOST5   CP    ITMPST,PZERO        TEST ANY PST                                 
         BE    CPOSTX              NO TAX - SKIP POSTING                        
         LA    R5,PSTDATA                                                       
         ZAP   DUB,ITMBASE                                                      
         AP    DUB,ITMGST                                                       
         BAS   RE,ELMGTX           ADD TAX ELEMENT                              
         ZAP   DUB,ITMBASE                                                      
         CLC   ITMPRV,=C'PQ'       FOR PQ PROVINCE                              
         BNE   CPOST5A                                                          
         CLC   DOCDATE,=X'B30101'  AND DATE AFTER JAN01/13                      
         BNL   *+10                DO NOT ADD GST TO NET                        
CPOST5A  AP    DUB,ITMGST                                                       
         BAS   RE,ELMBAS           ADD BASIS ELEMENT                            
*                                                                               
*                                  ** BUILD 69 DEBIT ELEMENT **                 
         USING DLPOSTD,R8                                                       
         MVI   DLPSEL,DLPSEDRQ     DEBIT PST ACCOUNT                            
         MVI   DLPSLEN,DLPSLNQ                                                  
         MVC   DLPSDBAC,VTCACT                                                  
         MVC   DLPSDBNM,VTCACTNM                                                
*                                                                               
         MVC   DLPSCRAC,VENDACCT   CONTRA - VENDOR                              
         MVC   DLPSCRNM,VENDNAME                                                
         MVI   DLPSTYPE,0                                                       
         ZAP   DLPSAMNT,ITMPST                                                  
         LA    RF,CLIOFFIC                                                      
         CLI   TYPE,C'P'                                                        
         BE    CPOST6                                                           
         MVC   DLPSCRAC,XVNDACCT   EXP - VENDOR                                 
         MVC   DLPSCRNM,XVNDNAME                                                
         LA    RF,FINOFF                                                        
         CLI   0(RF),C' '                                                       
         BH    CPOST6                                                           
         LA    RF,CRDOFF                                                        
*                                                                               
CPOST6   MVC   DLPSANAL,0(RF)                                                   
         LA    R8,DLPSLNQ(R8)                                                   
*                                                                               
CPOSTX   XIT1  REGS=(R8)                                                        
*                                                                               
         LTORG                                                                  
         DROP  R5,R8,RB                                                         
         EJECT                                                                  
**********************************************************************          
* BUILD POSTING DESCRIPTION ELEMENT                                  *          
**********************************************************************          
ELMDLD   ST    RE,SAVERE                                                        
         LA    R2,IOAREA           CLEAR IO AREA                                
         LA    R3,L'IOAREA                                                      
         XR    R1,R1                                                            
         MVCL  R2,R0                                                            
         LA    R8,IOAREA+2                                                      
         USING DLDESCD,R8                                                       
         MVI   DLDSEL,DLDSELQ                                                   
         MVC   DLDSREF,REFNUM                                                   
         MVC   DLDSDATE,DOCDATE                                                 
         MVI   DLDSSBRF,0                                                       
         MVI   DLDSSTAT,0                                                       
         CLI   URGENT,C'U'                                                      
         JNE   *+8                                                              
         OI    DLDSSTAT,TRNSURG    URGENT                                       
*                                                                               
         GOTO1 ANARRSCN,DMCB,(2,ANARH),0                                        
         MVC   NARR,BOELEM                                                      
         MVC   NARRLEN,BOHALF1                                                  
*                                                                               
         XR    R1,R1               GET NARRATIVE LENGTH                         
         ICM   R1,3,NARRLEN                                                     
         JZ    *+6                                                              
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   DLDSNARR(0),NARR                                                 
         LA    R1,DLDSNARR-DLDESCD                                              
         AH    R1,NARRLEN                                                       
         STC   R1,DLDSLEN                                                       
         AR    R8,R1                                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
*                                                                               
         DROP  R8                                                               
         EJECT                                                                  
**********************************************************************          
* COMMON ELEMENT ROUTINES                                            *          
**********************************************************************          
         USING OTHELD,R8           BUILD 'OTHERS' ELEMENT                       
ELMOTH   MVI   OTHEL,OTHELQ                                                     
         MVI   OTHLN,OTHLN1Q                                                    
         MVC   OTHNUM(13),SPACES   PRODUCT AND JOB                              
         MVC   OTHNUM(6),PRODCODE                                               
         MVC   OTHNUM+6(6),JOBNUM                                               
         J     ELMXIT                                                           
*                                                                               
ELMDLH   CP    CDAMNT,PZERO        IS THERE DISCOUNT ?                          
         BER   RE                  NO,                                          
         CLI   CASHACCT,0          IS THERE A CASH ACCT ?                       
         BNER  RE                  YES - CD NOT ALLOWED                         
*                                                                               
         USING SCIELD,R8                                                        
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,SCITCDSC                                                 
         ZAP   SCIAMNT,CDAMNT     CD ELEMENT FOR JOB                            
         J     ELMXIT                                                           
*                                  BUILD/ADD FFTEL FOR CLI/PROD                 
ELMFFL   CLI   FFTELEM,0           DO WE HAVE A CLIENT AND PRODUCT?             
         BER   RE                  NO, DONE                                     
         LA    R3,FFTELEM                                                       
         USING FFTELD,R3                                                        
         XR    R1,R1                                                            
         IC    R1,FFTLN                                                         
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   0(0,R8),FFTELEM                                                  
         LA    R8,1(R1,R8)         R8 TO NEXT AREA                              
         BR    RE                                                               
         DROP  R3                                                               
*                                                                               
ELMTRL   TM    ITMSTA,ITMXJOB                                                   
         BZR   RE                                                               
         USING TRSELD,R8                                                        
         XC    TRSEL(TRSLNQ),TRSEL                                              
         MVI   TRSEL,TRSELQ        BUILD TRANSACTION STATUS ELEMENT             
         MVI   TRSLN,TRSLNQ                                                     
         OI    TRSSTAT3,TRSSXJOB   SET X-JOB BIT ON                             
         J     ELMXIT                                                           
*                                                                               
ELMANL   CLI   ANAOFF,C' '         ANALYSIS OFICE                               
         BNHR  RE                                                               
         CLC   ANAOFF,FINOFF                                                    
         BER   RE                                                               
         USING ANOELD,R8                                                        
         MVI   ANOEL,ANOELQ                                                     
         MVI   ANOLN,ANOLNQ                                                     
         MVI   ANOTYPE,ANOTPER                                                  
         MVC   ANOOFFC,ANAOFF                                                   
         J     ELMXIT                                                           
*                                                                               
ELMORD   OC    LASTORD,LASTORD     ORDER ELEMENT                                
         BZR   RE                                                               
         USING FFNELD,R8                                                        
         XC    FFNELD(FFNLN2Q),FFNELD                                           
         MVI   FFNEL,FFNELQ                                                     
         MVI   FFNLN,FFNLN2Q                                                    
         MVC   FFNONUM,LASTORD                                                  
         CLI   BCPARTSW,C'Y'       IS IT PART MATCH                             
         JNE   ELMXIT                                                           
         MVI   FFNSTAT,FFNSPRTQ    SET PARTIAL                                  
         J     ELMXIT                                                           
*                                                                               
ELMLIN   CLI   LONGINVL,0          LONG INVOICE NUMBER                          
         BER   RE                                                               
         USING FFTELD,R8                                                        
         XC    0(FFTDATA-FFTELD+L'LONGINV,R8),0(R8)                             
         MVI   FFTEL,FFTELQ                                                     
         MVI   FFTTYPE,FFTTINVN                                                 
         XR    R1,R1                                                            
         IC    R1,LONGINVL                                                      
         STC   R1,FFTDLEN                                                       
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   FFTDATA(0),LONGINV                                               
         AHI   R1,FFTDATA-FFTELD+1                                              
         STC   R1,FFTLN                                                         
         LA    R8,0(R1,R8)                                                      
         BR    RE                                                               
*                                                                               
         USING PAKELD,R8                                                        
ELMPAK   MVI   PAKEL,PAKELQ        PAYABLE ACCOUNT ELEMENT                      
         MVI   PAKLN,PAKLNQ                                                     
         MVC   PAKACC,PAYACC                                                    
         MVC   PAKOFF,CLIOFFIC                                                  
         OC    CRDOFF,CRDOFF                                                    
         JZ    *+10                                                             
         MVC   PAKOFF,CRDOFF                                                    
         MVC   PAKCON,CLIACCT                                                   
         CLC   VENDACCT+1(2),SXVND                                              
         JNE   *+10                                                             
         MVC   PAKCON,JOBACCT                                                   
         MVC   PAKDATE,DOCDATE                                                  
         MVC   PAKREF,REFNUM                                                    
         J     ELMXIT                                                           
*                                                                               
         USING TXD,R5                                                           
         USING SUTELD,R8            SALES/USE TAX ELEMENT                       
ELMSUT   XC    SUTEL(SUTLN2Q),SUTEL                                             
         MVI   SUTEL,SUTELQ                                                     
         MVI   SUTLN,SUTLN3Q                                                    
         MVC   SUTEFF,TXEFF                                                     
         MVC   SUTRTE,TXRTE                                                     
         MVC   SUTBAS,INAMNT                                                    
         MVC   SUTLOC,TXLOCK                                                    
         MVC   SUTTVNDR,VENDACCT+1 USE PROD VENDOR                              
         TM    RECSTA,ITMEXPN      TEST EXPENSE VENDOR                          
         JZ    *+10                                                             
         MVC   SUTTVNDR,XVNDACCT+1 USE EXP VENDOR                               
         J     ELMXIT                                                           
         DROP  R5                                                               
*                                                                               
         USING DUEELD,R8           DUE DATE ELEMENT                             
ELMDUE   OC    DODATE,DODATE                                                    
         BZR   RE                                                               
         MVI   DUEEL,DUEELQ                                                     
         MVI   DUELN,DUELNQ                                                     
         MVC   DUEDATE,DODATE                                                   
         J     ELMXIT                                                           
*                                                                               
         USING SCIELD,R8           BASIS ELEMENT                                
ELMBAS   MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN1Q                                                    
         MVI   SCITYPE,C'N'                                                     
         ZAP   SCIAMNT,DUB                                                      
         CLI   TXGON,TXGGRSQ       TEST GROSS                                   
         JNE   *+16                                                             
         SP    SCIAMNT,ITMGST                                                   
         SP    SCIAMNT,ITMPST                                                   
         J     ELMXIT                                                           
*                                                                               
         USING SCIELD,R8           GST TAX ELEMENT                              
ELMGTX   XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTAXP                                                 
         ZAP   SCIAMNT,ITMGST                                                   
         ZAP   SCIBASE,ITMBASE                                                  
         MVC   SCISUBPT,ITMGTY                                                  
         J     ELMXIT                                                           
*                                                                               
         USING SCIELD,R8           PST TAX ELEMENT                              
ELMPTX   XC    SCIEL(SCILN3Q),SCIEL                                             
         MVI   SCIEL,SCIELQ                                                     
         MVI   SCILN,SCILN3Q                                                    
         MVI   SCITYPE,SCITTQST                                                 
         ZAP   SCIAMNT,ITMPST                                                   
         ZAP   SCIBASE,ITMBASE                                                  
         AP    SCIBASE,ITMGST                                                   
         MVC   SCISUBPR,ITMPRV                                                  
         MVC   SCISUBPT,ITMPTY                                                  
         J     ELMXIT                                                           
*                                                                               
         USING MDTELD,R8                                                        
ELMMTD   XC    MDTEL(MDTLNQ),MDTEL                                              
         MVI   MDTEL,MDTELQ                                                     
         MVI   MDTLN,MDTLNQ                                                     
         MVI   MDTSYS,MDTSPROD                                                  
         MVC   MDTMED,JOBNUM                                                    
         MVC   MDTCLI(12),JOBACCT+3                                             
         MVC   MDTMOS,PMOS                                                      
         MVC   MDTDSCP,JOBNAME                                                  
         OC    MDTDSCP,SPACES                                                   
         ZAP   DUB,INAMNT                                                       
         MP    DUB,PNEG1                                                        
         CVB   R0,DUB                                                           
         STCM  R0,15,MDTGRS                                                     
         STCM  R0,15,MDTRECV                                                    
         STCM  R0,15,MDTNET                                                     
         J     ELMXIT                                                           
*                                                                               
ELMWKC   LA    R3,FFTWKELM                                                      
         J     *+8                                                              
ELMTXWKC LA    R3,FFTTXELM                                                      
         CLI   0(R3),0                                                          
         BER   RE                                                               
         USING FFTELD,R3                                                        
         LLC   R1,FFTLN                                                         
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   0(0,R8),0(R3)                                                    
         J     ELMXIT                                                           
         DROP  R3                                                               
*                                                                               
ELMXIT   XR    R1,R1                                                            
         IC    R1,1(R8)                                                         
         AR    R8,R1                                                            
         BR    RE                                                               
*                                                                               
         DROP  R8                                                               
         EJECT                                                                  
**********************************************************************          
* FINISH POSTING - ADD TO FILE                                       *          
**********************************************************************          
ELMEND   MVI   0(R8),0             MARK END                                     
         LA    R8,1(R8)            END ADDRESS                                  
         LA    R3,IOAREA           START ADDRESS                                
         SR    R8,R3                                                            
         STCM  R8,3,IOAREA         TOTAL LENGTH                                 
*                                                                               
         XC    BOWORK1,BOWORK1                                                  
         XR    R1,R1               DOCUMENT #                                   
         IC    R1,REFNUML                                                       
         BCTR  R1,0                                                             
         BASR  RF,0                                                             
         EX    R1,8(RF)                                                         
         J     *+10                                                             
         MVC   BOWORK1(0),REFNUM                                                
*                                                                               
         ZAP   BOPL61,INAMNT       AMOUNT                                       
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN                                
         JNE   ELMEND5             NO,                                          
         ZAP   ITMTAX,ITMGST       GET TAX TOTAL FOR ITEM                       
         AP    ITMTAX,ITMPST                                                    
         CLI   TXGON,TXGGRSQ       TEST GROSS                                   
         JE    *+10                YES, AMOUNT OK                               
         AP    BOPL61,ITMTAX       ADD TAX TO AMOUNT                            
*                                                                               
ELMEND5  GOTO1 AADDITE,BOPARM,(X'20',IOAREA),BOPL61,BOWORK1,0,0,ITMTAX          
         LA    R0,FVFOK            TEST ADD OK                                  
         CLM   R0,3,FVMSGNO                                                     
         JE    XIT                                                              
         DC    H'0'                                                             
         EJECT                                                                  
**********************************************************************          
* ADD A SCREEN LINE TO TSAR BUFFER                                   *          
**********************************************************************          
ADDLIN   NTR1  BASE=*,LABEL=*                                                   
         L     RE,ARECOLD          CLEAR RECORD AREA                            
         LA    RF,RECLNQ2                                                       
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         BRAS  RE,BLDREC           BUILD A TSAR RECORD                          
         XR    R1,R1               INCREMENT RECORD COUNT                       
         ICM   R1,3,NREC                                                        
         LA    R1,1(R1)                                                         
         STCM  R1,3,NREC                                                        
         MVC   RECNUM,NREC         SET RECORD NUMBER                            
         NI    ITMSTA,X'FF'-ITMEXPN                                             
         CLI   RECWKC,C' '                                                      
         JH    *+8                                                              
         OI    ITMSTA,ITMEXPN                                                   
         MVC   RECSTA,ITMSTA       SET STATUS                                   
         MVC   RECOFF,CLIOFFIC                                                  
         CLI   TYPE,C'P'                                                        
         JE    ADDLIN1                                                          
         MVC   RECOFF,FINOFF                                                    
         CLI   RECOFF,C' '                                                      
         BH    ADDLIN1                                                          
         MVC   RECOFF,CRDOFF                                                    
         CLI   RECOFF,C' '                                                      
         BH    ADDLIN1                                                          
         MVC   RECOFF,RECDOF                                                    
         CLI   RECOFF,C' '                                                      
         BH    ADDLIN1                                                          
*                                                                               
ADDLIN1  BRAS  RE,ACASH            ADD ITEM AMOUNTS                             
*                                                                               
         TM    SCRSTA,SCRMINQ      TEST MULTIPLE INPUT SCREEN                   
         JNO   ADDLIN3             NO                                           
         LA    R1,RECLNQ           YES, DON'T ADD COMMENTS                      
         STCM  R1,3,RECLEN                                                      
*                                                                               
ADDLIN3  GOTOR TOTSAR,TSRADDQ      ADD IT                                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* PUT A SCREEN LINE TO TSAR BUFFER                                   *          
**********************************************************************          
PUTLIN   NTR1  BASE=*,LABEL=*                                                   
         BRAS  RE,DCASH            DELETE 'OLD' CASH                            
*                                                                               
         LA    RE,RECSD                                                         
         LA    RF,RECLNQ2                                                       
         L     R0,ARECOLD                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE               SAVE OLD RECORD                              
         MVC   BCHALF,RECNUM       SAVE RECORD NUMBER                           
*                                                                               
         L     R4,ARECOLD                                                       
OLD      USING RECSD,R4                                                         
*                                                                               
         BRAS  RE,BLDREC           RE-BUILD A TSAR RECORD                       
*                                                                               
         MVC   RECNUM,BCHALF       RESTORE RECORD NUMBER                        
         NI    ITMSTA,X'FF'-ITMEXPN                                             
         CLI   RECWKC,C' '                                                      
         JH    *+8                                                              
         OI    ITMSTA,ITMEXPN                                                   
         MVC   RECSTA,ITMSTA                                                    
         MVC   RECOFF,OLD.RECOFF      YES, USE SAVED OFFICE                     
         TM    SCRSTA,SCRTAXQ+SCRCANQ TEST CANADIAN TAX                         
         JO    PUTLIN3                YES                                       
         MVC   RECOFF,CLIOFFIC                                                  
         CLI   TYPE,C'P'                                                        
         BE    PUTLIN3                                                          
         MVC   RECOFF,FINOFF                                                    
         CLI   RECOFF,C' '                                                      
         BH    PUTLIN3                                                          
         MVC   RECOFF,CRDOFF                                                    
*                                                                               
PUTLIN3  BRAS  RE,ACASH            ADD 'NEW' AMOUNTS                            
         TM    SCRSTA,SCRSINQ      TEST DETAIL SCREEN                           
         BO    PUTLIN5             YES, DETAIL IS ON SCREEN                     
*                                                                               
         MVC   RECNAR1,OLD.RECNAR1 RESTORE OLD NARRATIVE FOR DETAIL             
         MVC   RECNAR2,OLD.RECNAR2                                              
         MVC   RECLEN,OLD.RECLEN                                                
*                                                                               
PUTLIN5  GOTOR TOTSAR,TSRPUTQ      PUT IT BACK                                  
         LA    RE,OLD.RECSD        CLEAR OLD RECORD                             
         LA    RF,RECLNQ2                                                       
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB,OLD                                                           
         EJECT                                                                  
**********************************************************************          
* DELETE A LINE FROM TSAR BUFFER                                     *          
**********************************************************************          
DELLIN   NTR1  BASE=*,LABEL=*                                                   
         GOTOR TOTSAR,TSRGETQ      GET RECORD                                   
         BRAS  RE,DCASH            DELETE OLD CASH                              
*                                                                               
         TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         JNO   DELLIN3                                                          
         BRAS  RE,ADJBASE          ADJUST BASIS                                 
*                                                                               
DELLIN3  GOTOR TOTSAR,TSRDELQ      DELETE                                       
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ADD ITEM DOLLARS AND TAX                                           *          
**********************************************************************          
ACASH    NTR1  BASE=*,LABEL=*                                                   
         GOTOR GETAMT,DMCB,(L'RECAMT,RECAMT)                                    
         AP    TOTDLRS,CASHRTN     ADD CURRENT AMOUNT TO TOTAL                  
         TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         JO    ACASH3                                                           
*                                                                               
         TM    ITMSTA,ITMTAXD      TEST TAXED                                   
         JNO   XIT                                                              
         GOTOR GETAMT,DMCB,(L'RECTAX,RECTAX)                                    
         AP    TAXDLRS,CASHRTN     ADD CURRENT TAX TO TOTAL                     
         XR    R1,R1                                                            
         IC    R1,NTAXL            UPDATE NUMBER OF TAX LINES                   
         AHI   R1,1                                                             
         STC   R1,NTAXL                                                         
         J     XIT                                                              
*                                                                               
ACASH3   DS    0H                  ** CANADIAN **                               
         GOTOR GETAMT,DMCB,(L'RECBAS,RECBAS)                                    
         ZAP   PL8,CASHRTN          SAVE BASIS                                  
         GOTOR GETAMT,DMCB,(L'RECGAM,RECGAM)                                    
         AP    GSTDLRS,CASHRTN                                                  
         GOTOR GETAMT,DMCB,(L'RECPAM,RECPAM)                                    
         AP    PSTDLRS,CASHRTN                                                  
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* DELETE ITEM DOLLARS AND TAX                                        *          
**********************************************************************          
DCASH    NTR1  BASE=*,LABEL=*                                                   
         GOTOR GETAMT,DMCB,(L'RECAMT,RECAMT)                                    
         SP    TOTDLRS,CASHRTN     SUBTRACT OLD AMOUNT                          
         TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         JO    DCASH3                                                           
         TM    RECSTA,ITMTAXD      TEST TAX ITEM                                
         JNO   XIT                                                              
*                                                                               
         GOTOR GETAMT,DMCB,(L'RECTAX,RECTAX)                                    
         SP    TAXDLRS,CASHRTN     SUBTRACT TAX AMOUNT                          
         XR    RF,RF                                                            
         IC    RF,NTAXL            ADJUST NUMBER OF TAX LINES                   
         SHI   RF,1                                                             
         STC   RF,NTAXL                                                         
         J     XIT                                                              
*                                                                               
DCASH3   GOTOR GETAMT,DMCB,(L'RECBAS,RECBAS)                                    
         ZAP   PL8,CASHRTN         SAVE BASIS                                   
         GOTOR GETAMT,DMCB,(L'RECGAM,RECGAM)                                    
         SP    GSTDLRS,CASHRTN     SUBTRACT CURRENT GST FROM TOTAL              
         GOTOR GETAMT,DMCB,(L'RECPAM,RECPAM)                                    
         SP    PSTDLRS,CASHRTN     SUBTRACT CURRENT PST FROM TOTAL              
         J     XIT                                                              
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
***********************************************************************         
* ROUTINE TO CHECK FOR DUPLICATE INVOICE NUMBER                       *         
*                                                                     *         
* NTRY:- P1    = A(LEDGER CODE)                                       *         
*        P2    = A(SUPPLIER ACCOUNT)                                  *         
*        P3    = A(20 BYTE INVOICE NUMBER)                            *         
*        P4    = A(PWOS INVOICE DATE)                                 *         
* EXIT:- P1    = A(I/O AREA CONTAINING TRANSACTION RECORD - IF FOUND) *         
*        CC    = LOW IF A DRAFT TRANSACTION RECORD FOUND              *         
*              = EQUAL IF NO DUPLICATE INVOICE FOUND                  *         
*              = HIGH IF A LIVE TRANSACTION RECORD FOUND              *         
***********************************************************************         
                                                                                
CHKINV   NTR1  BASE=*,LABEL=*                                                   
         LA    R2,IOKEY                                                         
         USING INVPASD,R2          BUILD KEY OF INVOICE PASSIVE                 
         XC    INVPKEY,INVPKEY                                                  
         MVI   INVPTYP,INVPTYPQ                                                 
         MVC   INVPCPY,CUABIN                                                   
         LM    RE,RF,0(R1)                                                      
         MVC   INVPLDG,0(RE)                                                    
         MVC   INVPACT,0(RF)                                                    
         LM    RE,RF,8(R1)                                                      
         MVC   INVPINV,0(RE)                                                    
         MVC   INVPDAT,0(RF)                                                    
         L     R0,AIO1             RETURN A(I/O AREA) IN P1 FOR CALLER          
         ST    R0,0(R1)                                                         
         MVI   BYTE,0              SET NO DUPLICATE                             
*                                                                               
CHKINV02 GOTOR AIO,IOHI+IOACCDIR                                                
         BE    *+6                                                              
         DC    H'0'                                                             
CHKINV04 CLC   INVPKEY(INVPDAT-INVPKEY),IOKEYSAV                                
         BNE   CHKINV06                                                         
SAV      USING INVPKEY,IOKEYSAV                                                 
*&&DO                                                                           
         OC    SAV.INVPDAT,SAV.INVPDAT                                          
         BZ    *+14                                                             
         CLC   INVPDAT,SAV.INVPDAT                                              
         JNE   CHKINV06                                                         
*&&                                                                             
*                                                                               
         L     R0,AIO1                                                          
         ST    R0,IOADDR                                                        
         GOTOR AIO,IOGET+IOACCMST                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         USING TRNRECD,R2                                                       
         TM    TRNRSTAT,TRNSDELT   TEST THIS RECORD IS DELETED                  
         BNZ   *+16                                                             
         TM    TRNRSTAT,TRNSDRFT   TEST THIS IS A DRAFT TRANSACTION             
         JZ    XNO                 NO, ** LIVE DUPLICATE **                     
         MVI   BYTE,1              SET DRAFT DUPLICATE                          
         LA    R2,IOKEY                                                         
         GOTOR AIO,IOSEQ+IOACCDIR                                               
         BE    *+6                                                              
         DC    H'0'                                                             
         B     CHKINV04                                                         
*                                                                               
CHKINV06 CLI   BYTE,0                                                           
         JE    XYES                NO DUPLICATE                                 
         CLI   BYTE,2              SET LOW - DUPLICATE DRAFT                    
         J     XIT                                                              
         DROP  R2,SAV                                                           
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* GET DUE DATE - FROM HIGHER LEVEL ACCOUNTS                          *          
**********************************************************************          
GETDUE   NTR1  BASE=*,LABEL=*                                                   
         ICM   R4,15,ADEXEL        ACCOUNT LEVEL DUE ELEMENT                    
         BNZ   GETDUE15                                                         
         LA    R2,IOKEY                                                         
         USING LDGRECD,R2                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKEY(3),KEY       GET LEDGER RECORD                            
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'LDGKEY),IOKEYSAV                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R2,AIO1                                                          
         LA    R2,LDGRFST                                                       
         SR    R0,R0                                                            
*                                                                               
         USING ACLELD,R2           GET ACCOUNT LENGTH                           
GETDUE3  CLI   ACLEL,ACLELQ                                                     
         BE    GETDUE5                                                          
         CLI   ACLEL,0                                                          
         BNE   *+6                                                              
         DC    H'0'                MISSING ACCOUNT LENGTHS ELEMENT              
         IC    R0,ACLLN                                                         
         AR    R2,R0                                                            
         B     GETDUE3                                                          
*                                                                               
GETDUE5  SR    R0,R0                                                            
         LA    R2,ACLVALS                                                       
GETDUE7  CLI   ACLVLEN-ACLVALS(R2),12 FIND ACCOUNT LEVEL ELEMENT                
         BE    GETDUE9                                                          
         AHI   R0,1                                                             
         LA    R2,L'ACLVALS(R2)                                                 
         B     GETDUE7                                                          
*                                                                               
GETDUE9  LTR   R0,R0               TEST END OF ACCOUNT LEVELS                   
         JZ    XIT                 NO DUE ELEMENT                               
         SHI   R0,1                DECREMENT COUNT                              
         SHI   R2,L'ACLVALS        LOOK AT NEXT LOWER LEVEL                     
         SR    R1,R1               R1=LENGTH OF LEVEL                           
         IC    R1,ACLVLEN-ACLVALS(R2)                                           
         AHI   R1,2                                                             
*                                                                               
         LA    R4,IOKEY                                                         
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   ACTKEY(0),KEY       GET ACCOUNT RECORD                           
         GOTO1 AIO,IOHIGH+IOACCMST+IO2                                          
         CLC   IOKEY(L'ACTKEY),IOKEYSAV                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         L     R4,AIO2                                                          
         LA    R4,ACTRFST                                                       
         SR    RF,RF                                                            
*                                                                               
         USING DEXELD,R4                                                        
GETDUE11 CLI   DEXEL,DEXELQ                                                     
         BE    GETDUE15                                                         
         IC    RF,DEXLN                                                         
         AR    R4,RF                                                            
         CLI   0(R4),0                                                          
         BNE   GETDUE11                                                         
         B     GETDUE9                                                          
*                                                                               
GETDUE15 LA    R5,TEMP                                                          
         USING CONBLKD,R5                                                       
         XC    CONBLK(CONBLKL),CONBLK                                           
         MVI   CONACTN,CONAGETQ    TRANSLATE                                    
         MVI   CONFLD,CONFIDUE     INVOICE DUE DATE                             
         MVI   CONILEN,L'DEXVAL                                                 
         LA    RF,DEXVAL                                                        
         STCM  RF,15,CONIADD       INPUT ADDRESS                                
         LA    RF,DODATEP                                                       
         CLI   TYPE,C'P'                                                        
         BE    *+8                                                              
         LA    RF,DODATEX                                                       
         STCM  RF,15,CONOADD       OUTPUT ADDRESS                               
         MVC   CONCOMF,ACOMFACS                                                 
         MVC   CONIDATE,DOCDATE                                                 
         GOTOR VCNVERT,CONBLKD                                                  
         CLI   CONERR,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         J     XIT                                                              
*                                                                               
         DROP  R2,R4,R5                                                         
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* RECALCULATE TAX FOR ALL ITEMS                                      *          
*  NECESSARY IF A SPECFIC GST OR PST AMOUNT HAS BEEN INPUT           *          
**********************************************************************          
RECALC   NTR1  BASE=*,LABEL=*                                                   
         ZAP   GSTDLRS,PZERO       CLEAR THE GST                                
         ZAP   PSTDLRS,PZERO                 PST                                
         XC    GSTXN,GSTXN                                                      
         ZAP   GSTXA,PZERO         CLEAR ROUNDING ADJUSTMENTS                   
         XC    PSTXN,PSTXN                                                      
         ZAP   PSTXA,PZERO                                                      
*                                                                               
         XR    R3,R3                                                            
         ICM   R3,3,NBUF                                                        
         MVI   ACTION,RECALCQ                                                   
         LA    R0,1                                                             
*                                                                               
RECALC3  STCM  R0,3,NGET                                                        
         GOTOR TOTSAR,TSRGETQ      GET A RECORD                                 
         MVI   TYPE,C'P'           SET FOR PRODUCTION                           
         CLI   RECWKC,C' '         TEST WORKCODE PRESENT                        
         BH    *+8                 YES,                                         
         MVI   TYPE,C'E'           NO, MUST BE EXPENSE                          
*                                                                               
         GOTOR GETAMT,DMCB,(L'RECAMT,RECAMT)                                    
         ZAP   INAMNT,CASHRTN                                                   
         MVC   RECGAM,SPACES                                                    
         MVC   RECPAM,SPACES                                                    
*                                                                               
         MVC   GSTDATA+(VTCRATE-VTCEFFD)(L'VTCRATE),RECGSTR                     
         MVC   PSTDATA+(VTCRATE-VTCEFFD)(L'VTCRATE),RECPSTR                     
*                                                                               
         GOTOR CTXR,DMCB,PROGD     GET NEW GST/PST                              
*                                                                               
         MVC   RECTXRA,ITMTXRA                                                  
         CP    ITMGST,PZERO                                                     
         JE    RECALC5                                                          
         AP    GSTDLRS,ITMGST                                                   
         CURED ITMGST,(L'RECGAM,RECGAM),2,MINUS=YES                             
*                                                                               
RECALC5  CP    ITMPST,PZERO                                                     
         JE    RECALC7                                                          
         AP    PSTDLRS,ITMPST                                                   
         CURED ITMPST,(L'RECPAM,RECPAM),2,MINUS=YES                             
*                                                                               
RECALC7  GOTOR TOTSAR,TSRPUTQ      SAVE THE RECORD                              
         XR    R0,R0                                                            
         ICM   R0,3,NGET                                                        
         AHI   R0,1                GET NEXT RECORD                              
         BCT   R3,RECALC3                                                       
*                                                                               
         MVI   ACTION,C' '                                                      
*                                                                               
         TM    CTXSTAT,CTXSGSTI    TEST GST INPUT                               
         BNO   *+8                                                              
         BRAS  RE,GSTRND                                                        
*                                                                               
         TM    CTXSTAT,CTXSPSTI    TEST PST INPUT                               
         BNO   *+8                                                              
         BRAS  RE,PSTRND                                                        
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ADJUST GST FOR POSSIBLE ROUNDING ERRORS                            *          
**********************************************************************          
GSTRND   NTR1  BASE=*,LABEL=*                                                   
         ZAP   BCDUB,GSTAMNT       GST AMOUNT INPUT                             
         SP    BCDUB,GSTDLRS       LESS AMOUNT CALCULATED                       
         CP    BCDUB,PZERO         TEST ADJUSTMENT IS ZERO                      
         JE    XIT                 YES, NO ADJUST IS NECESSSARY                 
         ZAP   GSTXA,BCDUB         SAVE GST ROUNDING ADJUSTMENT                 
         OI    BCDUB+(L'BCDUB-1),X'0F'                                          
         CP    BCDUB,=P'100'       NOT MORE THAN $1.00                          
         BL    GSTRND1                                                          
         ICM   R2,15,AGAMH         SET CURSOR TO GST                            
         BZ    *+12                                                             
         TM    1(R2),FATBPROT      IF NOT PROTECTED                             
         BZ    *+8                                                              
         ICM   R2,15,ADOCH                                                      
         OI    CTXSTAT,CTXRECER    SET RECALC ERROR                             
         NI    CTXSTAT,X'FF'-(CTXOKX)                                           
         J     EMGSTAI             GST AMOUNT IS INVALID                        
*                                                                               
GSTRND1  XR    R3,R3                                                            
         ICM   R3,3,NBUF           GET LAST RECORD                              
*                                                                               
GSTRND3  STCM  R3,3,NGET                                                        
         GOTOR TOTSAR,TSRGETQ      GET A RECORD WITH GST                        
         GOTOR GETAMT,DMCB,(L'RECGAM,RECGAM)                                    
         CP    CASHRTN,PZERO       TEST ANY GST                                 
         BNE   GSTRND5             YES,                                         
         SHI   R3,1                NO, GET ANOTHER                              
         B     GSTRND3                                                          
*                                                                               
GSTRND5  MVC   GSTXN,RECNUM        SAVE RECORD NUMBER FOR ADJUSTMENT            
         ZAP   ITMGST,CASHRTN      SET GST FOR DISPLAY                          
         GOTOR GETAMT,DMCB,(L'RECPAM,RECPAM)                                    
         ZAP   ITMPST,CASHRTN      SET PST FOR DISPLAY                          
         MVC   ITMGSTR,RECGSTR     SET GST/PST RATES                            
         MVC   ITMPSTR,RECPSTR                                                  
         BRAS  RE,TXRAL            BUILD TAX RATE/AMOUNT LINE                   
         MVC   RECTXRA,ITMTXRA                                                  
         GOTOR TOTSAR,TSRPUTQ      PUT THE RECORD                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ADJUST PST FOR POSSIBLE ROUNDING ERRORS                            *          
**********************************************************************          
PSTRND   NTR1  BASE=*,LABEL=*                                                   
         ZAP   BCDUB,PSTAMNT       PST AMOUNT INPUT                             
         SP    BCDUB,PSTDLRS       LESS AMOUNT CALCULATED                       
         CP    BCDUB,PZERO         TEST ADJUSTMENT IS ZERO                      
         JE    XIT                 YES, NO ADJUST IS NECESSSARY                 
         ZAP   PSTXA,BCDUB         SAVE PST ROUNDING ADJUSTMENT                 
         OI    BCDUB+(L'BCDUB-1),X'0F'                                          
         CP    BCDUB,=P'100'       NOT MORE THAN $1.00                          
         BL    PSTRND1                                                          
         ICM   R2,15,APAMH         SET CURSOR TO PST                            
         BZ    *+12                                                             
         TM    1(R2),FATBPROT      IF NOT PROTECTED                             
         BZ    *+8                                                              
         ICM   R2,15,ADOCH                                                      
         OI    CTXSTAT,CTXRECER    SET RECALC ERROR                             
         NI    CTXSTAT,X'FF'-(CTXOKX)                                           
         J     EMPSTAI             PST AMOUNT IS INVALID                        
*                                                                               
PSTRND1  XR    R3,R3                                                            
         ICM   R3,3,NBUF           GET LAST RECORD                              
*                                                                               
PSTRND3  STCM  R3,3,NGET                                                        
         GOTOR TOTSAR,TSRGETQ      GET A RECORD WITH PST                        
         GOTOR GETAMT,DMCB,(L'RECPAM,RECPAM)                                    
         CP    CASHRTN,PZERO       TEST ANY GST                                 
         BNE   PSTRND5             YES,                                         
         SHI   R3,1                NO, GET ANOTHER                              
         B     PSTRND3                                                          
*                                                                               
PSTRND5  MVC   PSTXN,RECNUM        SAVE RECORD NUMBER FOR ADJUSTMENT            
         ZAP   ITMPST,CASHRTN      SET PST FOR DISPLAY                          
         GOTOR GETAMT,DMCB,(L'RECGAM,RECGAM)                                    
         ZAP   ITMGST,CASHRTN      SET GST FOR DISPLAY                          
         MVC   ITMGSTR,RECGSTR     SET GST/PST RATES                            
         MVC   ITMPSTR,RECPSTR                                                  
         BRAS  RE,TXRAL            BUILD TAX RATE/AMOUNT LINE                   
         MVC   RECTXRA,ITMTXRA                                                  
         GOTOR TOTSAR,TSRPUTQ      PUT THE RECORD                               
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* BUILD TAX DISPLAY LINE RATE/GST PST                                *          
**********************************************************************          
TXRAL    NTR1  BASE=*,LABEL=*                                                   
         MVC   GSTRL,SPACES        CLEAR GST/PST RATE LINES                     
         MVC   PSTRL,SPACES                                                     
         MVC   ITMTXRA,SPACES                                                   
         MVC   WORK,SPACES                                                      
         LA    R3,WORK             DISPLAY RATE\AMOUNT                          
         MVC   0(2,R3),=C'G='                                                   
         LA    R3,2(R3)                                                         
         XR    RF,RF                                                            
         ICM   RF,3,ITMGSTR        GST RATE                                     
         CVD   RF,DUB                                                           
         CURED DUB,(5,0(R3)),3,ALIGN=LEFT                                       
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         ZAP   BCDUB,ITMGST                                                     
         CLC   GSTXN,RECNUM        ADD ROUNDING ADJUST                          
         BNE   *+10                                                             
         AP    BCDUB,GSTXA                                                      
         CURED BCDUB,(9,0(R3)),2,MINUS=YES,ALIGN=LEFT                           
         AR    R3,R0                                                            
         LA    R4,WORK+2                                                        
         LR    R1,R3                                                            
         SR    R1,R4                                                            
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   GSTRL(0),0(R4)      SAVE RATE/AMOUNT                             
*                                                                               
         LA    R3,1(R3)                                                         
         MVC   0(2,R3),=C'P='                                                   
         LA    R3,2(R3)                                                         
         LR    R4,R3                                                            
         XR    RF,RF                                                            
         ICM   RF,3,ITMPSTR                                                     
         CVD   RF,DUB                                                           
         CURED DUB,(5,0(R3)),3,ALIGN=LEFT                                       
*                                                                               
         AR    R3,R0                                                            
         MVI   0(R3),C'/'                                                       
         LA    R3,1(R3)                                                         
         ZAP   BCDUB,ITMPST                                                     
         CLC   PSTXN,RECNUM        ADD ROUNDING ADJUST                          
         BNE   *+10                                                             
         AP    BCDUB,PSTXA                                                      
         CURED BCDUB,(9,0(R3)),2,MINUS=YES,ALIGN=LEFT                           
         AR    R3,R0                                                            
         LR    R1,R3                                                            
         SR    R1,R4               GET LENGTH OF GST DATA                       
         BCTR  R1,0                                                             
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   PSTRL(0),0(R4)                                                   
         MVC   ITMTXRA,WORK                                                     
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ADJUST GST/PST BASIS                                               *          
**********************************************************************          
ADJBASE  NTR1  BASE=*,LABEL=*                                                   
         GOTOR GETAMT,DMCB,(L'RECBAS,RECBAS)                                    
         ZAP   PL8,CASHRTN         SAVE BASIS AMOUNT                            
         GOTOR GETAMT,DMCB,(L'RECGAM,RECGAM)                                    
         CP    CASHRTN,PZERO       TEST ANY GST                                 
         BE    *+10                NO, THAN NO BASIS                            
         SP    GSTBASE,PL8         YES, REDUCE OLD BASIS                        
         GOTOR GETAMT,DMCB,(L'RECPAM,RECPAM)                                    
         CP    CASHRTN,PZERO       TEST ANY PST                                 
         BE    *+10                NO, THAN NO BASIS                            
         SP    PSTBASE,PL8         YES, REDUCE OLD BASIS                        
         J     XIT                                                              
         LTORG                                                                  
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* INTERFACE TO TSAR                                                  *          
**********************************************************************          
TOTSAR   NTR1  BASE=*,LABEL=*                                                   
         LA    R4,TSARBLK                                                       
         USING TSARD,R4                                                         
         TM    TWATSAR,TWATSRES    TEST ALREADY RESTORED                        
         BO    TOTSAR5                                                          
*                                                                               
         LR    R0,R1               SAVE ACTION                                  
         MVC   TSACOM,ACOMFACS     A(COMFACS)                                   
         LA    R1,L'RECKEY                                                      
         STC   R1,TSKEYL           SET KEY LENGTH                               
         LA    R1,RECLNQ2                                                       
         STCM  R1,3,TSRECL         SET MAX RECORD LENGTH                        
         L     R1,ARECIOA                                                       
         ST    R1,TSAREC           A(RECORD AREA)                               
         MVI   TSRECI,TSRVAR       SET VARIABLE LENGTH                          
         MVI   TSACTN,TSAINI       SET INITIALZE                                
         OI    TSINDS,TSIALLOC     SET TO ALLOCATE                              
         MVI   TSPAGN,TSNMAX                                                    
         CHI   R0,TSRSAVQ          TEST ACTION SAVE                             
         JE    XIT                 DON'T SAVE - NEVER RESTORED                  
         TM    TWATSAR,TWATSINI    TEST INITIALIZED                             
         BZ    TOTSAR3             NO, MUST INITIALIZED                         
         MVI   TSACTN,TSARES       SET RESTORE                                  
         MVC   TSPAGL,TWATSPGL     SET LOW PAGE                                 
         MVC   TSPAGN,TWATSPGN     SET NUMBER OF PAGES                          
*                                                                               
TOTSAR3  GOTO1 VTSAR,TSARD                                                      
         JE    *+6                                                              
         DC    H'0'                TSAR BUFFER ERROR                            
         MVC   TWATSPGL,TSPAGL                                                  
         MVC   TWATSPGN,TSPAGN                                                  
         OI    TWATSAR,TWATSINI+TWATSRES                                        
         LR    R1,R0                                                            
*                                                                               
TOTSAR5  SLL   R1,2                                                             
         B     *(R1)                                                            
*                                                                               
         B     TOTSADD             ADD                                          
         B     TOTSGET             GET                                          
         B     TOTSPUT             PUT                                          
         B     TOTSSAV             SAVE                                         
         B     TOTSDEL             DELETE                                       
         B     TOTSINT             RE-INITIALIZE                                
*                                                                               
         EJECT                                                                  
TOTSADD  MVI   TSACTN,TSAADD       SET ACTION ADD                               
         B     TOTSALL                                                          
*                                                                               
TOTSGET  MVI   TSACTN,TSAGET       SET ACTION GET                               
         L     RE,ARECIOA          CLEAR AREA                                   
         LA    RF,RECLNQ2                                                       
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
         BAS   RE,ADJNUM                                                        
         B     TOTSALL                                                          
*                                                                               
TOTSPUT  MVI   TSACTN,TSAPUT       SET ACTION PUT                               
         BAS   RE,ADJNUM                                                        
         B     TOTSALL                                                          
*                                                                               
TOTSSAV  MVI   TSACTN,TSASAV       SET ACTION SAVE                              
         B     TOTSALL                                                          
*                                                                               
TOTSDEL  MVI   TSACTN,TSADEL       SET ACTION DELETE                            
         BAS   RE,ADJNUM                                                        
         B     TOTSALL                                                          
*                                                                               
TOTSALL  GOTO1 VTSAR,TSARD                                                      
         CLI   TSERRS,0                                                         
         JE    *+6                                                              
         DC    H'0'                TSAR BUFFER ERROR                            
         XR    RF,RF                                                            
         ICM   RF,3,TSPRECN                                                     
         LA    R0,SAVNUMQ                                                       
         SR    RF,R0               REDUCE FOR EXTRA RECORDS                     
         STCM  RF,3,NBUF           SAVE NUMBER IN BUFFER                        
         J     XIT                                                              
*                                                                               
TOTSINT  MVI   TSACTN,TSAINI       SET INITIALZE                                
         MVI   TSINDS,TSIALLOC+TSIREUSE                                         
         GOTO1 VTSAR,TSARD                                                      
         JE    *+6                                                              
         DC    H'0'                TSAR BUFFER ERROR                            
         MVC   TWATSPGL,TSPAGL                                                  
         MVC   TWATSPGN,TSPAGN                                                  
         OI    TWATSAR,TWATSINI+TWATSRES                                        
         XC    NBUF,NBUF           CLEAR BUFFER                                 
         J     XIT                                                              
*                                                                               
*                           ADJUST NUMBER TO ALLOW FOR FIXED RECORDS            
ADJNUM   XR    R1,R1                                                            
         TM    NGET,RECSRQ         TEST SPECIAL RECORD                          
         JO    *+8                 YES, USE RECORD NUMBER                       
         LHI   R1,SAVNUMQ          NO, INCREMENT BY NUMBER OF SPECIALS          
         NI    NGET,X'FF'-(RECSRQ)   TURNOFF 'SPECIAL' FLAG                     
         XR    RF,RF                                                            
         ICM   RF,3,NGET                                                        
         AR    RF,R1               GET REAL RECORD NUMBER                       
         STCM  RF,3,TSRNUM         SET RECORD NUMBER                            
         BR    RE                                                               
         LTORG                                                                  
*                                                                               
         DROP  R4,RB                                                            
         EJECT                                                                  
**********************************************************************          
* CLEAR FIELDS                                                       *          
**********************************************************************          
CLEAR    NTR1  BASE=*,LABEL=*                                                   
         L     R3,0(R1)            R3=A(LIST OF FIELDS)                         
         MVC   FLD,SPACES                                                       
*                                                                               
CLEAR3   XR    RF,RF                                                            
         ICM   RF,3,0(R3)                                                       
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)                                                      
         JZ    CLEAR5                                                           
*                                                                               
         TM    4(R2),FINPTHIS      TEST IF FIELD INPUT                          
         JO    CLEAR5              YES                                          
         BRAS  RE,MOVEFLD          NO-CLEAR IT                                  
         MVI   5(R2),0                                                          
*                                                                               
CLEAR5   LA    R3,L'XJOTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   CLEAR3                                                           
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* ERASE THE LOWER SCREEN                                             *          
**********************************************************************          
ERASE    NTR1  BASE=*,LABEL=*                                                   
         L     R2,ADETH            R2=A(FIRST DETAIL HEADER)                    
         XR    R6,R6                                                            
         IC    R6,SCRMXD           SET MAX NUMBER OF DETAIL LINES               
*                                                                               
ERASE2   BRAS  RE,SETLOW                                                        
         MVC   FLD,SPACES                                                       
         LA    R3,LOWER                                                         
         LA    R0,NLOWER-1                                                      
*                                                                               
ERASE4   ICM   R2,15,0(R3)         R2=A(FIELD HEADER)                           
         JZ    ERASE6              NO FIELD ON THIS SCREEN                      
         BRAS  RE,MOVEFLD                                                       
*                                                                               
ERASE6   LA    R3,4(R3)                                                         
         JCT   R0,ERASE4                                                        
*                                                                               
         L     R2,ANEXT                                                         
         JCT   R6,ERASE2                                                        
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* TEST MAX ITEMS FOR AUTO CLOSE                                      *          
**********************************************************************          
TSTMAX   NTR1  BASE=*,LABEL=*                                                   
         LA    R3,CSLSTCUR                                                      
         USING LSTTABD,R3                                                       
         CLC   LSTBITMA,CSBMAXIT   TEST MAX FOR BATCH TYPE                      
         BNL   TSTMAX3             FORCE AUTO CLOSE                             
         CLC   LSTBITMA,LSTBITMC   ITEMS ADDED VS. CONTROL                      
         BNL   *+14                                                             
         CP    LSTBCSHA,LSTBCSHC   CASH ADDED VS. CONTROL                       
         JL    XIT                                                              
*                                                                               
         GOTOR ATSTBTY,=AL1(ACTACL) TEST AUTO CLOSE                             
         JNE   XIT                                                              
*                                                                               
TSTMAX3  OI    LSTBINDS,LSTBIAUT   SET AUTO CLOSE FROM ITEM/INPUT               
         LA    R1,=AL1(RECBAT,ACTCLO,0,0,0,0)                                   
         GOTOR ANTRSES,(R1)        BATCH/CLOSE                                  
         DROP  R3                                                               
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
*                                                                               
         EJECT                                                                  
**********************************************************************          
* REPEAT FEATURE ROUTINE-R2=A(CURRENT FLD HDR WITH '*' INPUT)        *          
**********************************************************************          
DUP      CLI   8(R2),C''''                                                      
         BNER  RE                                                               
         CLI   5(R2),1                                                          
         BNER  RE                                                               
DUP1     NTR1  BASE=*,LABEL=*                                                   
         LR    R1,R2               R1=A(SOURCE FIELD)                           
         XR    R4,R4                                                            
         IC    R4,SCRLDL            R4=L' OF DETAIL LINE                        
         SR    R1,R4               BACK UP TO PREVIOUS LINE                     
         L     R3,ADETH            R3=A(FIRST DETAIL LINE)                      
         CR    R1,R3               TEST IF USER IS ON FIRST LINE                
         JL    EMAPITL             'APOSTROPHE INPUT INVALID ON TOP..'          
*                                                                               
         XR    RF,RF                                                            
         ICM   RF,1,5(R1)                                                       
         JZ    EMNODUP             'INPUT IS NOT AVAILABLE TO REPEAT'           
         STC   RF,5(R2)                                                         
         MVC   FLD,SPACES                                                       
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   FLD(0),8(R1)        GET OLD DATA                                 
*                                                                               
         BRAS  RE,MOVEFLD          MOVE IN NEW DATA                             
         OI    6(R2),FOUTMOD                                                    
         J     XIT                                                              
*                                                                               
         LTORG                                                                  
*                                                                               
         DROP  RB                                                               
         EJECT                                                                  
**********************************************************************          
* MOVE DATA TO DISPLAY DATA                                          *          
**********************************************************************          
MOVEFLD  ST    RE,SAVERE                                                        
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),FATBXHDR      TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   8(0,R2),FLD                                                      
         L     RE,SAVERE                                                        
         BR    RE                                                               
                                                                                
**********************************************************************          
* RE-INITIALIZE                                                      *          
**********************************************************************          
REINIT   ST    RE,SAVERE                                                        
         NI    CSINDSL2,X'FF'-CSIPFAPP                                          
         GOTOR TOTSAR,TSRINTQ      INITIALIZE                                   
         GOTOR TOTSAR,TSRSAVQ      SAVE BUFFER                                  
         MVI   PROCSW,PROCCLR      SET TO CLEAR ON RETURN                       
         XC    STS(STSLNQ),STS                                                  
         ZAP   TOTDLRS,PZERO       CLEAR SAVED STORAGE                          
         ZAP   TAXDLRS,PZERO                                                    
         ZAP   PSTDLRS,PZERO                                                    
         XC    GSTXN,GSTXN                                                      
         ZAP   GSTXA,PZERO         CLEAR ROUNDING ADJUSTMENTS                   
         XC    PSTXN,PSTXN                                                      
         ZAP   PSTXA,PZERO                                                      
*                                                                               
         LA    RF,BASMSGH          RE-TRANSMIT SCREEN                           
         SR    R1,R1                                                            
REINIT3  CLI   0(RF),0                                                          
         JE    REINIT5                                                          
         IC    R1,0(RF)                                                         
         AR    RF,R1                                                            
         J     REINIT3                                                          
REINIT5  XC    0(3,RF),0(RF)       ERASE BEFORE                                 
         MVI   1(RF),01                                                         
*                                                                               
         L     R2,AORDH                                                         
         NI    4(R2),X'FF'-(FINPVAL)                                            
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* GET CLIENT RECORD                                                  *          
**********************************************************************          
GETCLI   STM   RE,R6,SVREG                                                      
         MVC   FVMAXL,BCCLILEN     MAXIMUM=CLIENT CODE LENGTH                   
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,FVIFLD      CLIENT CODE                                  
*                                                                               
         GOTOR AGETACC,DMCB,KEY,CLIPROF                                         
         JNE   EXIT                                                             
*                                                                               
         MVC   CLIACCT,ACCODE      EXTRACT CLIENT KEY                           
         MVC   CLINAME,ACNAME                                                   
         MVC   CLIPRO,ACCODE                                                    
         MVC   CLIPRON,ACNAME                                                   
         MVC   CLICODE,FVIFLD      SAVE CLIENT CODE                             
         J     LMX                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET PRODUCT RECORD                                                 *          
**********************************************************************          
GETPRO   STM   RE,R6,SVREG                                                      
         XR    RE,RE                                                            
         IC    RE,BCPROLEN         RE=L'CLIENT+L'PRODUCT                        
         XR    RF,RF                                                            
         IC    RF,BCCLILEN         RF=L'CLIENT                                  
         SR    RE,RF               RE=L'PRODUCT                                 
         STC   RE,FVMAXL                                                        
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,CLIACCT+(ACTKACT-ACTRECD)                                
         XR    R3,R3                                                            
         IC    R3,BCCLILEN                                                      
         LA    R3,ACTKACT(R3)      POINT R3 AT PRODUCT CODE POSITION            
         XR    R1,R1                                                            
         IC    R1,FVXLEN           R1=EXECUTE LENGTH                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R3),FVIFLD      EXTRACT PRODUCT CODE                         
*                                                                               
         GOTOR AGETACC,DMCB,KEY,PRODPROF                                        
         JNE   EXIT                                                             
*                                                                               
         MVC   PROACCT,ACCODE      EXTRACT PRODUCT KEY/NAME                     
         MVC   PRONAME,ACNAME                                                   
         MVC   CLIPRO,ACCODE                                                    
         MVC   CLIPRON,ACNAME                                                   
         MVC   PRODCODE,FVIFLD                                                  
         J     LMX                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET JOB RECORD                                                     *          
**********************************************************************          
GETJOB   STM   RE,R6,SVREG                                                      
         XR    RE,RE                                                            
         IC    RE,BCJOBLEN         RE=L'CLIENT+L'PRODUCT+L'JOB                  
         XR    RF,RF                                                            
         IC    RF,BCPROLEN         RF=L'CLIENT+L'PRODUCT                        
         SR    RE,RF               RE=L'JOB                                     
         STC   RE,FVMAXL                                                        
         MVI   FVMINL,1            YES-FORCE A MISSING INPUT FIELD              
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         LA    R4,KEY                                                           
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         MVC   ACTKUNT(2),BCCPYEL+(CPYPROD-CPYELD)                              
         MVC   ACTKACT,PROACCT+(ACTKACT-ACTRECD)                                
         XR    R3,R3                                                            
         IC    R3,BCPROLEN                                                      
         LA    R3,ACTKACT(R3)      POINT R3 AT JOB CODE POSITION                
         XR    R1,R1                                                            
         IC    R1,FVXLEN           R1=EXECUTE LENGTH                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R3),FVIFLD      EXTRACT JOB CODE                             
*                                                                               
         GOTOR AGETACC,DMCB,KEY,JOBPROF                                         
         JNE   EXIT                                                             
*                                                                               
         MVC   JOBACCT,ACCODE      EXTRACT JOB KEY/NAME                         
         MVC   JOBNAME,ACNAME                                                   
*                                                                               
         GOTO1 AMRGPRF                                                          
         LA    R6,PSCOMPPR                                                      
         USING PPRELD,R6                                                        
         MVC   CLICOST,PPRCOST     GET COST ACCOUTING POINTER (1C)              
         MVC   CLIOFFIC,PPRGAOFF   SAVE PRODUCTION OFFICE                       
         MVC   JOBNUM,FVIFLD       SAVE JOB CODE                                
         J     LMX                                                              
         DROP  R4,R6                                                            
         EJECT                                                                  
**********************************************************************          
* GET STAFF LEDGER VALUES                                            *          
**********************************************************************          
GET2PL   STM   RE,R6,SVREG                                                      
         LA    R4,IOKEY                                                         
         USING LDGRECD,R4                                                       
         MVC   LDGKEY,SPACES                                                    
         MVC   LDGKCPY,CUABIN                                                   
         LHI   R1,UL2P-T61B0A                                                   
         A     R1,BASERB                                                        
         MVC   LDGKUNT(2),0(R1)    GET 2P LEDGER VALUES                         
         GOTO1 AGETLDG                                                          
         JNE   LMX                                                              
*                                                                               
         ICM   R1,15,ACALDG                                                     
         USING LDGTABD,R1                                                       
         MVI   LEV2P,3             SET 2P LEDGER LEVEL                          
         XR    R5,R5                                                            
         IC    R5,LDGTLVB          GET DISPLACEMENT TO STAFF CODE               
         CLI   LDGTLVC,L'ACTKACT   TEST FOR 3 LEVEL LEDGER                      
         JE    GET2PL4             YES                                          
*                                                                               
         IC    R5,LDGTLVA                                                       
         MVI   LEV2P,2                                                          
         CLI   LDGTLVB,L'ACTKACT   TEST FOR 2 LEVEL LEDGER                      
         JE    GET2PL4                                                          
*                                                                               
         XR    R5,R5                                                            
         MVI   LEV2P,1                                                          
         CLI   LDGTLVA,L'ACTKACT                                                
         JE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
GET2PL4  LA    RE,L'ACTKACT                                                     
         SR    RE,R5                                                            
         STC   RE,STAFFL           SAVE L'STAFF CODE                            
         J     LMX                                                              
*                                                                               
         DROP  R1,R4                                                            
         EJECT                                                                  
**********************************************************************          
* GET DEPARTMENT ACCOUNT                                             *          
**********************************************************************          
GETDPT   STM   RE,R6,SVREG                                                      
         LA    R4,KEY              VALIDATE 2D ACCOUNT                          
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         LHI   R1,UL2D-T61B0A                                                   
         A     R1,BASERB                                                        
         MVC   ACTKUNT(2),0(R1)                                                 
         LA    R1,ACTKACT                                                       
*                                                                               
         TM    BCCPYST1,CPYSOROE    TEST OFFICE REQUIRED                        
         JNO   GETDPT3                                                          
         LA    R6,ANAOFF           USE ANALYSIS OFFICE                          
         CLI   ANAOFF,C' '         IS PRESENT                                   
         JH    *+8                                                              
         LA    R6,FINOFF           ELSE,  USE FINANCIAL                         
         XR    RF,RF                                                            
         IC    RF,BCOFFLEN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R6)       OFFICE                                       
         LA    R1,1(RF,R1)                                                      
*                                                                               
GETDPT3  XR    RF,RF                                                            
         IC    RF,BCDPTLEN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),DEPT        DEPARTMENT                                   
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         J     LMX                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET STAFF ACCOUNT                                                  *          
**********************************************************************          
GETSTF   STM   RE,R6,SVREG                                                      
         LA    R4,KEY              VALIDATE 2P ACCOUNT KEY                      
         USING ACTRECD,R4                                                       
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,COMPANY                                                  
         LHI   R1,UL2P-T61B0A                                                   
         A     R1,BASERB                                                        
         MVC   ACTKUNT(2),0(R1)                                                 
         LA    R1,ACTKACT                                                       
*                                                                               
         CLI   LEV2P,3             ONLY MOVE OFFICE FOR 3 LEVEL ACCTS           
         JL    GETSTF3                                                          
         LA    R6,ANAOFF           USE ANALYSIS OFFICE                          
         CLI   ANAOFF,C' '         IS PRESENT                                   
         JH    *+8                                                              
         LA    R6,FINOFF           ELSE,  USE FINANCIAL                         
         XR    RF,RF                                                            
         IC    RF,BCOFFLEN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),0(R6)       OFFICE TO KEY                                
         LA    R1,1(RF,R1)         BUMP TO NEXT SPOT IN KEY                     
*                                                                               
GETSTF3  CLI   LEV2P,2                                                          
         JL    GETSTF5                                                          
         XR    RF,RF                                                            
         IC    RF,BCDPTLEN                                                      
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),DEPT                                                     
         LA    R1,1(RF,R1)                                                      
*                                                                               
GETSTF5  XR    RF,RF                                                            
         IC    RF,STAFFL                                                        
         BCTR  RF,0                                                             
         BASR  RE,0                                                             
         EX    RF,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R1),STAFF       MOVE STAFF INTO KEY                          
*                                                                               
         GOTOR AGETACC,DMCB,KEY,0                                               
         J     LMX                                                              
*                                                                               
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* GET OFFICE SHORTNAME                                               *          
**********************************************************************          
GETOFF   STM   RE,R6,SVREG                                                      
         MVC   OFFNAME,SPACES                                                   
         GOTOR AVALOFFC,BOPARM,(X'80',FVIFLD)                                   
         JNE   EXIT                                                             
         LA    R4,IOKEY                                                         
         USING OFFRECD,R4                                                       
         MVC   OFFKEY,SPACES                                                    
         MVI   OFFKTYP,OFFKTYPQ                                                 
         MVC   OFFKCPY,CUABIN                                                   
         MVC   OFFKOFF,ACOFFC                                                   
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'OFFKEY),IOKEYSAV TEST FOUND OFFICE RECORD                
         JE    GETOFF3                  YES, DISPLAY NAME                       
*                                                                               
         USING ACTRECD,R4          GET NAME FROM 2D                             
         MVC   ACTKEY,SPACES                                                    
         MVC   ACTKCPY,CUABIN                                                   
         LHI   R1,UL2D-T61B0A                                                   
         A     R1,BASERB                                                        
         MVC   ACTKUNT(2),0(R1)                                                 
         MVC   ACTKACT(2),ACOFFC                                                
         GOTO1 AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'OFFKEY),IOKEYSAV TEST FOUND OFFICE RECORD                
         JE    GETOFF3                  YES, DISPLAY NAME                       
         JNE   EMOFCNF            'OFFICE NOT FOUND...'                         
*                                                                               
GETOFF3  L     R4,AIO1                                                          
         LA    R4,ACTRFST                                                       
         SR    R1,R1                                                            
*                                                                               
         USING NAMELD,R4                                                        
GETOFF5  CLI   NAMEL,0                                                          
         JE    LMX                                                              
         CLI   NAMEL,NAMELQ                                                     
         JE    GETOFF7                                                          
         IC    R1,NAMLN                                                         
         AR    R4,R1                                                            
         J     GETOFF5                                                          
*                                                                               
GETOFF7  IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   OFFNAME(0),NAMEREC                                               
         J     LMX                                                              
         DROP  R4                                                               
         EJECT                                                                  
**********************************************************************          
* VALIDATE LOCATION - BUILD TAX POSTING ENTRIES                      *          
**********************************************************************          
VALLOC   STM   RE,R6,SVREG                                                      
         ZAP   TAXAMNT,PZERO                                                    
         MVI   NTXLN,0             NUMBER OF TAX LINES                          
         L     R0,ATAXIT           CLEAR TAX ENTRIES                            
         LA    R1,L'TAXIT                                                       
         XR    RE,RE                                                            
         XR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R5,ATAXIT                                                        
         USING TXD,R5                                                           
         MVI   FVMINL,2                                                         
         GOTOR AFVAL,(R2)                                                       
         JNE   EXIT                                                             
         LA    RF,8(R2)                                                         
         XR    R0,R0                                                            
         IC    R0,5(R2)                                                         
*                                                                               
VALLOC1  SR    R6,R6               PARSE LOCALITY XX,XXXX                       
         LR    R1,RF                                                            
VALLOC2  CLI   0(R1),C','          TEST DELIMITER                               
         JE    VALLOC3             YES, PROCESS THIS PORTION                    
         LA    R1,1(R1)                                                         
         AHI   R6,1                R6=LENGTH OF THIS PORTION                    
         JCT   R0,VALLOC2                                                       
*                                                                               
VALLOC3  STM   RF,R1,SVRFR1        SAVE START, REMAIN LEN, COMMA                
*                                                                               
VALLOC4  DS    0H                                                               
         ICM   R2,15,ALOCH                                                      
         CHI   R6,2                                                             
         JL    EMINVIF                                                          
*                                                                               
         LA    R4,IOKEY                                                         
         USING SUTRECD,R4                                                       
         MVC   SUTKEY,SPACES       READ SALES/USE TAX RECORD                    
         MVI   SUTKTYP,SUTKTYPQ                                                 
         MVI   SUTKSUB,SUTKSUBQ                                                 
         MVC   SUTKCPY,COMPANY                                                  
         LR    R1,R6               REMAINING LOCATION LENGTH                    
         ICM   RF,15,SVRFR1        RESTORE START                                
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   SUTKLOC(0),0(RF)                                                 
*                                                                               
         GOTOR AIO,IOHIGH+IOACCMST+IO1                                          
         CLC   IOKEY(L'SUTKEY),IOKEYSAV                                         
         JNE   EMINACC             'INVALID ACCOUNT..                           
         L     R4,AIO1                                                          
         ZAP   TXRTE,PZERO                                                      
         ZAP   TXTAX,PZERO                                                      
         ZAP   TXBAS,PZERO                                                      
         MVC   TXWKC,TAXWKC                                                     
         MVC   WORK,SPACES                                                      
         LA    R3,SUTRFST                                                       
         SR    R1,R1                                                            
*                                                                               
         USING NAMELD,R3                                                        
VALLOC5  CLI   NAMEL,NAMELQ                                                     
         JNE   VALLOC6                                                          
         IC    R1,NAMLN                                                         
         SHI   R1,3                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   WORK(0),NAMEREC                                                  
         J     VALLOC9                                                          
*                                                                               
         USING SUTELD,R3                                                        
VALLOC6  CLI   SUTEL,SUTELQ                                                     
         JNE   VALLOC9                                                          
         CLI   SUTLN,SUTLN2Q                                                    
         JL    VALLOC7                                                          
         MVC   TXACC(1),COMPANY                                                 
         MVC   TXACC+1(L'SUTACC),SUTACC                                         
         MVC   TXLOCK,SUTKEY       SAVE LOCALITY CODE                           
         MVC   TXLOCN,WORK         LOCALITY NAME                                
*                                                                               
VALLOC7  CLC   SUTEFF,DOCDATE      TEST EFFECTIVE DATE                          
         JH    VALLOC9                                                          
         MVC   TXEFF,SUTEFF                                                     
         ZAP   TXRTE,SUTRTE                                                     
*                                                                               
VALLOC9  IC    R1,SUTLN                                                         
         AR    R3,R1                                                            
         CLI   0(R3),0                                                          
         JNE   VALLOC5                                                          
*                                                                               
         OC    TXRTE,TXRTE         TEST TAX RATE FOUND                          
         JZ    VALLOC15                                                         
         ZAP   PL13,BASAMNT        BASIS AMOUNT                                 
         MP    PL13,TXRTE          X RATE                                       
         SRP   PL13,64-6,5                                                      
         ZAP   TXTAX,PL13          SAVE TAX                                     
         AP    TAXAMNT,TXTAX                                                    
         XR    RF,RF                                                            
         IC    RF,NTXLN                                                         
         AHI   RF,1                                                             
         STC   RF,NTXLN                                                         
*                                                                               
         MVC   KEY,SPACES                                                       
         MVC   KEY(L'TXACC),TXACC VALIDATE POSTING ACCOUNT                      
         GOTOR AGETACC,DMCB,KEY,0                                               
         JNE   EXIT                                                             
         TM    ACBSTAT,ACBSLOCK                                                 
         JO    EMACTLK             'ACCOUNT IS LOCKED'                          
         TM    ACBSTAT,ACBSABAL                                                 
         JNO   EMINACP             'INVALID ACCOUNT FOR POSTING'                
*                                                                               
VALLOC11 LA    R5,TXLNQ(R5)                                                     
         SHI   R6,2                REDUCE LENGTH OF LOCATION                    
         JP    VALLOC4             GET HIGHER LEVEL                             
*                                                                               
         LM    RF,R1,SVRFR1        RESTORE START,LENGTH,COMMA                   
         LA    RF,1(R1)            RF PASSED DELIMITER                          
         SHI   R0,1                ADJUST REMAINING LENGTH                      
         JP    VALLOC1             LOOK FOR NEXT STRING                         
         J     LMX                                                              
*                                                                               
VALLOC15 XC    BCWORK,BCWORK                                                    
         LA    R0,L'SUTKLOC                                                     
         STC   R0,BCWORK                                                        
         MVC   BCWORK+1(L'SUTKLOC),SUTKLOC                                      
         J     EMMTXRT             'MISSING TAX RATE FOR...                     
         DROP  R3,R4,R5                                                         
         EJECT                                                                  
**********************************************************************          
* VALIDATE WORKCODE                                                  *          
*  R1=A(WORKCODE FIELD)                                              *          
**********************************************************************          
VALWC    STM   RE,R6,SVREG                                                      
         L     R2,0(R1)                                                         
         MVC   AWKCSAV,0(R1)                                                    
         MVC   FLD,SPACES                                                       
*                                                                               
         TM    AWKCSAV,X'80'       TEST TAX WORKCODE                            
         JO    VALWC1              YES                                          
         ICM   R2,15,AWKCNH        CLEAR WC NAME FIELD                          
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
VALWC1   L     R2,AWKCSAV                                                       
         MVI   FVMINL,2                                                         
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         CLC   8(2,R2),WC99                                                     
         JE    EMWC99              'W/C 99 NOT ALLOWED..                        
*                                                                               
         L     R4,AGOBLOCK                                                      
         USING GOBLOCKD,R4                                                      
*                                                                               
         LA    RF,GOUWLIST                                                      
         LA    R0,GOUWLSTN                                                      
VALWC2   CLC   FVIFLD(2),0(RF)     CHK FOR MATCH ON NON-BILLABLE WC'S           
         JE    EMINWRK             'INVALID WORKCODE'                           
         LA    RF,2(RF)                                                         
         JCT   R0,VALWC2                                                        
*                                                                               
         GOTOR AGETWC,FVIFLD       READ ANALYSIS RECORD FOR VALID W/C           
         JNE   EXIT                                                             
         TM    AWKCSAV,X'80'       TEST TAX WORKCOCE                            
         JO    LMX                                                              
         MVC   WRKNAME,SPACES                                                   
         MVC   WRKNAME(L'WCODESC),WORK                                          
         MVC   WRKCODE(2),FVIFLD                                                
*                                                                               
         MVC   FLD(L'WRKNAME),WRKNAME                                           
         ICM   R2,15,AWKCNH        DISPLAY WC NAME                              
         JZ    *+8                                                              
         BRAS  RE,MOVEFLD                                                       
*                                                                               
         L     R2,AWKCSAV                                                       
         L     R1,AIO2                                                          
         USING WCORECD,R1                                                       
         LA    R1,WCORFST                                                       
         USING WCOELD,R1                                                        
         SR    R0,R0                                                            
VALWC3   CLI   WCOEL,0             TEST EOR                                     
         JNE   *+6                                                              
         DC    H'0'                                                             
         CLI   WCOEL,WCOELQ        TEST WORK CODE ELEMENT                       
         JE    *+14                                                             
         IC    R0,WCOLN                                                         
         AR    R1,R0                                                            
         J     VALWC3                                                           
         LHI   RF,NONC-T61B0A                                                   
         A     RF,BASERB                                                        
         TM    WCOSTAT,WCOSNONC    TEST NON-COMMISSIONABLE                      
         JNO   VALWC4                                                           
         CLI   5(R2),2                                                          
         JNE   VALWC4                                                           
         MVI   5(R2),5                                                          
         MVC   10(L'NONC,R2),0(RF)  FORCE NON-COMMISSIONABLE INPUT              
         DROP  R1                                                               
*                                                                               
VALWC4   NI    ITMSTA,X'FF'-(ITMNONC)                                           
         CLI   5(R2),2                                                          
         JE    LMX                 ONLY WORKCODE                                
         LA    R1,10(R2)                                                        
*                                                                               
VALWC5   CLI   0(R1),C','                                                       
         JE    *+12                                                             
         CLI   0(R1),C'/'                                                       
         JNE   VALWCERR                                                         
         CLI   1(R1),C'N'          TEST NO COMMISSION                           
         JNE   VALWCERR                                                         
         CLI   5(R2),5                                                          
         JNE   *+14                                                             
         CLC   1(2,R1),1(RF)                                                    
         JNE   VALWCERR                                                         
         OI    ITMSTA,ITMNONC                                                   
         J     LMX                                                              
*                                                                               
VALWCERR L     R3,AINP             SET CURSOR TO ERROR                          
         USING TIOBD,R3                                                         
         LA    RF,8(R2)                                                         
         SR    R1,RF                                                            
         STC   R1,TIOBCURI                                                      
         LA    RF,TWAD                                                          
         SR    R2,RF                                                            
         STCM  R2,3,TIOBCURD                                                    
         OI    TIOBINDS,TIOBSETC                                                
         J     EMINVIF                                                          
         DROP  R4,R3                                                            
         EJECT                                                                  
**********************************************************************          
* VALIDATE AMOUNT FIELD                                              *          
*  R1=A(OUTPUT PACKED AMOUNT FIELD)                                  *          
**********************************************************************          
VALAMT   STM   RE,R6,SVREG                                                      
         LR    R3,R1               R3=A(PACKED AMOUNT FIELD)                    
         MVI   FVMINL,1            REQUIRE SOME INPUT                           
         GOTO1 AFVAL,(R2)                                                       
         JNE   EXIT                                                             
*                                                                               
         XR    R0,R0                                                            
         IC    R0,FVILEN           LENGTH OF AMOUNT INPUT                       
         GOTOR CASHVAL,DMCB,(X'82',FVIFLD),(R0)                                 
         CLI   0(R1),0                                                          
         JNE   EMINAMT             'INVALID AMOUNT'                             
         ZAP   0(L'INAMNT,R3),4(8,R1)  SAVE CURRENT AMOUNT                      
         ZAP   BCDUB,4(8,R1)                                                    
         OI    BCDUB+L'BCDUB-1,X'0F'                                            
         CP    BCDUB,MXAMT                                                      
         JH    EMINAMT             'INVALID AMOUNT'                             
*                                                                               
         MVC   FLD,SPACES          REDISPLAY AMOUNT                             
         ZAP   BCDUB,0(L'INAMNT,R3)                                             
         CURED BCDUB,(12,FLD),2,MINUS=YES,ALIGN=LEFT                            
         TM    1(R2),FATBPROT      TEST PROTECTED                               
         JO    *+12                                                             
         OI    6(R2),FOUTMOD                                                    
         STC   R0,5(R2)                                                         
         BRAS  RE,MOVEFLD                                                       
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* GET AMOUNT FIELDS FROM RECORD                                      *          
*  PARM 1  BYTE  0     LENGTH OF FIELD                               *          
*          BYTES 1-3   A(FIELD)                                      *          
*  RTRN    CASHRTN CONTAINS AMOUNT                                   *          
**********************************************************************          
GETAMT   STM   RE,R6,SVREG                                                      
         XR    R0,R0                                                            
         IC    R0,0(R1)                                                         
         L     R2,0(R1)                                                         
         ZAP   CASHRTN,PZERO                                                    
         XC    WORK,WORK                                                        
         LR    R1,R0               R1=LENGTH OF FIELD                           
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   WORK(0),0(R2)       GET AMOUNT INTO WORK                         
         LA    R1,WORK(R1)         R1=A(END OF AMOUNT)                          
*                                                                               
         CLI   0(R1),C' '          GET ACTUAL LENGTH                            
         JH    *+14                                                             
         BCTR  R1,0                                                             
         JCT   R0,*-10                                                          
         J     LMX                 NO AMOUNT - ZERO                             
*                                                                               
         GOTO1 CASHVAL,CASHPRM,(X'82',WORK),(R0)                                
         CLI   0(R1),0                                                          
         JE    *+6                                                              
         DC    H'0'                                                             
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* LOAD NEW SCREEN                                                    *          
*  R1=A(SCREEN NUMBER)                                               *          
**********************************************************************          
LOADSCRN ST    RE,SAVERE                                                        
         XR    R0,R0                                                            
         ICM   R0,1,0(R1)          SCREEN NUMBER                                
         JNZ   LOADS2                                                           
         ICM   R0,1,TWASCRN                                                     
         JNZ   LOADS2                                                           
         ICM   R0,1,FRSTSCRN                                                    
LOADS2   GOTO1 AOVRSCR,BOPARM,((R0),BASOLY2H)                                   
         JE    *+6                                                              
         DC    H'0'                                                             
         STC   R0,TWASCRN          SAVE SCREEN NUMBER                           
         BRAS  RE,SETSCR           SET SCREEN PARAMETERS                        
         BRAS  RE,SETUPR           SET NEW UPPER SCREEN FIELD ADDRESSES         
         MVI   NITM,0                                                           
         GOTO1 ABLDDET,0           REFRESH BATCH DETAILS                        
         L     RE,SAVERE                                                        
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* SET SCREEN DATA                                                    *          
**********************************************************************          
SETSCR   STM   RE,R6,SVREG                                                      
         L     R1,AUSATAB          ASSUME US                                    
         CLI   AGYCTRY,CTRYCAN     UNLESS CANADA                                
         JNE   *+8                                                              
         L     R1,ACANTAB                                                       
*                                                                               
SETSCR3  MVC   SCRD(SCRLNQ),0(R1)  MOVE SCREEN PARAMETERS                       
         CLC   SCRNUM,TWASCRN      TEST CORRECT SCREEN                          
         JE    SETSCR9                                                          
         LA    R1,SCRLNQ(R1)                                                    
         CLI   0(R1),X'FF'                                                      
         JNE   SETSCR3                                                          
         DC    H'0'                BAD SCREEN                                   
*                                                                               
SETSCR9  MVC   BCBYTE1,SCRSTA                                                   
         NI    BCBYTE1,SCRMINQ+SCRSINQ+SCRTAXQ+SCRLISQ                          
         L     RF,APFKMSK                                                       
         USING PFKMD,RF                                                         
*                                                                               
SETSCR11 CLC   BCBYTE1,PFKMSCTY    MATCH SCREEN TYPE                            
         JE    SETSCR13                                                         
         LA    RF,PFKMLNQ(RF)                                                   
         CLI   0(RF),X'FF'                                                      
         JNE   SETSCR11                                                         
         DC    H'0'                                                             
*                                                                               
SETSCR13 NC    CSAPFMSK,PFKMCREM   TURNOFF SOME OLD SETTINGS                    
         OC    CSAPFMSK,PFKMCSET   SET NEW PFKEYS                               
         ST    RF,ACPFKMSK         SAVE ADDRESS OF CURRENT ENTRY                
         J     LMX                                                              
         DROP  RF                                                               
         EJECT                                                                  
**********************************************************************          
* SET SCREEN ADCONS                                                  *          
**********************************************************************          
SETUPR   STM   RE,R6,SVREG                                                      
         LA    R1,NUPPER           NUMBER OF UPPER FIELDS                       
         SLL   R1,2                                                             
         LA    RF,UPPER                                                         
         BASR  RE,0                CLEAR UPPER ADDRESSES                        
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
         LA    R2,TWAD                                                          
         XR    R1,R1                                                            
         ICM   R1,3,SCRUPR                                                      
         A     R1,BASERB                                                        
         J     SETADDR                                                          
*                                                                               
SETLOW   STM   RE,R6,SVREG         AT ENTRY R2 POINTS TO CLIENT HEADER          
         LA    R1,NLOWER           NUMBER OF LOWER FIELDS                       
         SLL   R1,2                                                             
         BCTR  R1,0                                                             
         LA    RF,LOWER                                                         
         BASR  RE,0                CLEAR LOWER ADDRESSES                        
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         XC    0(0,RF),0(RF)                                                    
         XR    R1,R1                                                            
         ICM   R1,3,SCRLOW                                                      
         A     R1,BASERB                                                        
*                                                                               
SETADDR  CLI   0(R1),X'FF'         TEST FOR EOT                                 
         JE    LMX                                                              
         XR    RE,RE                                                            
         ICM   RE,3,0(R1)                                                       
         AR    RE,R2               GET ADDRESS OF SCREEN FIELD                  
         XR    RF,RF                                                            
         ICM   RF,3,2(R1)                                                       
         LA    RF,PROGD(RF)        GET ADDRESS OF ADCON                         
         ST    RE,0(RF)            SAVE ADDRESS OF FIELD HEADER                 
         LA    R1,L'MULTUPR(R1)                                                 
         J     SETADDR                                                          
         EJECT                                                                  
**********************************************************************          
* BUILD A TSAR RECORD FROM SCREEN DATA OR OLD RECORD                 *          
**********************************************************************          
BLDREC   STM   RE,R6,SVREG                                                      
         L     RE,ARECIOA          CLEAR IO AREA                                
         LA    RF,RECLNQ2                                                       
         XR    R1,R1                                                            
         MVCL  RE,R0                                                            
*                                                                               
         L     R3,ARCDTAB                                                       
BLDREC3  XR    R4,R4                                                            
         ICM   R4,3,0(R3)          DISPLACEMENT TO RECORD AREA                  
         L     R5,ARECIOA                                                       
         AR    R4,R5               R4=A(OUTPUT AREA)                            
         XR    R5,R5                                                            
         ICM   R5,3,2(R3)          DISPLACEMENT TO A(FIELD HEADER)              
         LA    R5,PROGD(R5)                                                     
         ICM   R2,15,0(R5)         R2=A(FIELD HEADER)                           
         JNZ   BLDREC5             FIELD IS ON THE SCREEN                       
*                                                                               
         XR    R1,R1                                                            
         IC    R1,4(R3)            SET R1 TO FIELD LENGTH                       
         BCTR  R1,0                                                             
         XR    R5,R5                                                            
         ICM   R5,3,0(R3)          DISPLACEMENT TO RECORD AREA                  
         L     RE,ARECOLD          R5=A(OLD RECORD)                             
         AR    R5,RE                                                            
         J     BLDREC7                                                          
*                                                                               
BLDREC5  LA    R5,8(R2)            R5=A(INPUT AREA)                             
         XR    R1,R1               GET LENGTH FROM HEADER                       
         IC    R1,0(R2)                                                         
         LA    RE,8+1                                                           
         TM    1(R2),FATBXHDR      TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
BLDREC7  BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R4),0(R5)       MOVE FIELD TO RECORD AREA                    
*                                                                               
         LA    R3,L'RCDTAB(R3)     NEXT RCDTAB ENTRY                            
         CLI   0(R3),X'FF'                                                      
         JNE   BLDREC3                                                          
*                                                                               
         LA    R1,RECLNQ                                                        
         CLC   RECNAR1,DNARR1      TEST DEFAULT NARRATIVE                       
         JNE   *+14                                                             
         CLC   RECNAR2,DNARR2                                                   
         JE    BLDREC9                                                          
         LA    R1,RECLNQ2                                                       
         CLC   RECNAR2,SPACES                                                   
         JH    BLDREC9                                                          
         LA    R1,RECLNQ1                                                       
*                                                                               
BLDREC9  STCM  R1,3,RECLEN         SET RECORD LENGTH                            
         TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         JO    BLDREC14            YES,                                         
*                                                                               
         L     R1,ARECOLD                                                       
         MVC   RECLOCN,RECLOCN-RECSD(R1)                                        
         MVC   RECLOCH,RECLOCH-RECSD(R1)                                        
         ICM   R2,15,ALOCH         SAVE TAX HEADER/DATA/TRAILER                 
         JZ    BLDREC10                                                         
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   RECLOCH(0),0(R2)    SAVE HEADER/DATA/TRAILER                     
         MVC   RECLOCN,NTXLN       SAVE NUBMER OF LINES                         
*                                                                               
BLDREC10 CLI   RECTAX,C' '         TEST ANY TAX - YET                           
         JH    BLDREC12            YES,                                         
         CLI   RECBAS,C' '         TEST BASIS                                   
         JH    BLDREC12            YES,                                         
         MVC   RECBAS,RECAMT       BASIS = AMOUNT                               
BLDREC12 J     LMX                                                              
*                                                                               
BLDREC14 MVC   RECTXRA,ITMTXRA     SPECIAL FOR CANADIAN                         
         MVC   RECGSTR,GSTDATA+(VTCRATE-VTCEFFD)                                
         MVC   RECPSTR,PSTDATA+(VTCRATE-VTCEFFD)                                
         CURED ITMBASE,(L'RECBAS,RECBAS),2,MINUS=YES,ALIGN=LEFT                 
         MVC   RECGAM,SPACES                                                    
         CP    ITMGST,PZERO                                                     
         JE    BLDREC15                                                         
         CURED ITMGST,(L'RECGAM,RECGAM),2,MINUS=YES                             
*                                                                               
BLDREC15 MVC   RECPAM,SPACES                                                    
         CP    ITMPST,PZERO                                                     
         JE    LMX                                                              
         CURED ITMPST,(L'RECPAM,RECPAM),2,MINUS=YES                             
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* MOVE DATA FROM TSAR RECORD TO SCREEN                               *          
**********************************************************************          
DSPREC   STM   RE,R6,SVREG                                                      
         L     R3,ARCDTAB                                                       
*                                                                               
DSPREC3  XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISPLACEMENT TO A(FIELD HEADER)              
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)         R2=A(FIELD HEADER)                           
         JZ    DSPREC5                                                          
         XR    RF,RF                                                            
         ICM   RF,3,0(R3)          DISPLACEMENT TO RECORD AREA                  
         L     RE,ARECIOA                                                       
         AR    RF,RE                                                            
*                                                                               
         XR    R1,R1                                                            
         IC    R1,0(R2)                                                         
*                                                                               
         LA    RE,8+1                                                           
         TM    1(R2),FATBXHDR      TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   8(0,R2),0(RF)       MOVE DATA TO SCREEN                          
*                                                                               
         LA    RE,8(R1,R2)                                                      
         LA    R1,1(R1)            SET INPUT LENGTH                             
         CLI   0(RE),C' '                                                       
         JH    *+10                                                             
         BCTR  RE,0                                                             
         JCT   R1,*-10                                                          
         TM    1(R2),FATBPROT                                                   
         JO    *+8                                                              
         STC   R1,5(R2)                                                         
*                                                                               
         NI    4(R2),X'FF'-(FINPVAL)                                            
*                                                                               
DSPREC5  LA    R3,L'RCDTAB(R3)     NEXT RCDTAB ENTRY                            
         CLI   0(R3),X'FF'                                                      
         JNE   DSPREC3                                                          
*                                                                               
         TM    SCRSTA,SCRMINQ+SCRTAXQ   TEST MULTIPLE OR TAX                    
         JNZ   DSPREC7             YES, USE DEFAULT NARRATIVE                   
         L     R3,ARECIOA                                                       
         LHI   RF,RECLNQ                                                        
         CLM   RF,3,RECLEN         TEST RECORD HAS NARRATIVE                    
         JNE   LMX                 YES, ALREADY DISPLAYED                       
*                                                                               
DSPREC7  BRAS  RE,DFLTNAR          DISPLAY DEFAULT NARRATIVE                    
         TM    SCRSTA,SCRTAXQ      TEST TAX                                     
         JNO   LMX                 NO,                                          
         ICM   R2,15,ACPJH         CLI/PROD/JOB/WC FIELD                        
         TM    RECSTA,ITMEXPN      TEST EXPENSE ITEM                            
         JNO   DSPREC9             NO,                                          
         MVC   FLD,SPACES          DISPLAY EXPENSE ACCOUNT                      
         MVC   FLD(L'RECXAC),RECXAC                                             
         BRAS  RE,MOVEFLD                                                       
         J     LMX                                                              
*                                                                               
DSPREC9  MVC   FLD,SPACES          DISPLAY EXPENSE ACCOUNT                      
         MVC   FLD(L'RECCPJ),RECCPJ                                             
         LA    R3,FLD                                                           
         TM    RECSTA,ITMNONC      SHOW NON-COMMISSIONABLE                      
         JNO   *+12                                                             
         MVI   16(R3),C'N'                                                      
         MVI   17(R3),C'C'                                                      
         TM    RECSTA,ITMXJOB      TEXT XJOB                                    
         JNO   *+8                 NO,                                          
         MVI   19(R3),C'X'         FLAG X-JOB                                   
DSPREC11 BRAS  RE,MOVEFLD                                                       
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* SAVE UPPER PORTION OF SCREEN                                       *          
**********************************************************************          
SAVTOP   STM   RE,R6,SVREG                                                      
         L     R3,AUPRTAB                                                       
SAVTOP3  XR    R2,R2                                                            
         ICM   R2,3,0(R3)          DISP. TO A(OF FIELD HEADER)                  
         LA    R2,PROGD(R2)                                                     
         ICM   R2,15,0(R2)         R2=A(FIELD HEADER)                           
         JZ    SAVTOP5                                                          
         XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISP. TO A(LENGTH & DATA FIELDS)             
         LA    RF,PROGD(RF)        RF=A(SAVED STORAGE AREA)                     
         XR    R1,R1                                                            
         IC    R1,0(R2)            LENGTH + HEADER                              
         LA    RE,8+1                                                           
         TM    1(R2),FATBXHDR      TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,8+8+1                                                         
         SR    R1,RE                                                            
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   1(0,RF),8(R2)       SAVE SCREEN INPUT                            
         MVC   0(1,RF),5(R2)       SAVE LENGTH                                  
         CLI   5(R2),0             TEST ANY DATA INPUT                          
         JE    *+8                                                              
         OI    PROCSW,PROCEDT      SET EDIT IN PROGRESS                         
         TM    4(R2),FINPVAL       TEST VALIDATED BIT                           
         JNO   *+8                                                              
         OI    0(RF),X'80'         SET FLAG FOR REBUILD                         
SAVTOP5  LA    R3,L'UPRTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   SAVTOP3                                                          
         J     LMX                                                              
         EJECT                                                                  
**********************************************************************          
* RESTORE TOP OF SCREEN                                              *          
**********************************************************************          
RESTOP   STM   RE,R6,SVREG                                                      
         L     R3,AUPRTAB                                                       
RESTOP3  XR    R2,R2                                                            
         ICM   R2,3,0(R3)          DISP. TO A(OF FIELD HEADER)                  
         LA    R2,PROGD(R2)                                                     
         ICM   R2,15,0(R2)         R2=A(FIELD HEADER)                           
         JZ    RESTOP4                                                          
         XR    RF,RF                                                            
         ICM   RF,3,2(R3)          DISP. TO A(LENGTH & DATA FIELDS)             
         LA    RF,PROGD(RF)        RF=A(SAVED STORAGE AREA)                     
         TM    0(RF),X'80'         TEST VALIDATED                               
         JNO   *+8                                                              
         OI    4(R2),FINPVAL       SET IN FIELD HEADER                          
         NI    0(RF),X'7F'                                                      
         XR    R1,R1                                                            
         ICM   R1,1,0(RF)                                                       
         JZ    RESTOP4                                                          
         STC   R1,FLDILEND(R2)                                                  
         BCTR  R1,0                                                             
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   FLDHDRL(0,R2),1(RF)       SET SCREEN INPUT                       
*                                                                               
RESTOP4  LA    R3,L'UPRTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   RESTOP3                                                          
*                                                                               
         TM    SCRSTA,SCRMINQ      TEST MULTIPLE INPUT                          
         JNO   RESTOP5             NO                                           
         BRAS  RE,DFLTNAR          DISPLAY DEFAULT NARRATIVE                    
*                                                                               
RESTOP5  ICM   R2,15,AOVRH         TEST OVERRIDE FIELD ON SCREEN                
         JZ    LMX                 NONE                                         
         CLI   OVRIDE,C'Y'         TEST OVERRIDE IN USE                         
         JNE   LMX                                                              
         NI    1(R2),X'FF'-(FATBPROT) UNPROTECT OVERRIDE FIELD                  
         L     R2,AOVRTH           R2=A(OVERRIDE TAG FIELD)                     
         NI    1(R2),X'FF'-(FATBLOW)                                            
         OI    1(R2),FATBHIGH                                                   
*                                                                               
LMX      LM    RE,R6,SVREG                                                      
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* DISPLAY DEFAULT NARRATIVE                                          *          
**********************************************************************          
DFLTNAR  LR    RF,RE                                                            
         ICM   R2,15,ANARH          DISPLAY DEFAULT NARRATIVE                   
         JZ    DFLTNARX                                                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'DNARR1),DNARR1                                             
         BRAS  RE,MOVEFLD                                                       
         ICM   R2,15,ANA2H                                                      
         JZ    DFLTNARX                                                         
         MVC   FLD,SPACES                                                       
         MVC   FLD(L'DNARR2),DNARR2                                             
         BRAS  RE,MOVEFLD                                                       
DFLTNARX LR    RE,RF                                                            
         BR    RE                                                               
         EJECT                                                                  
**********************************************************************          
* INFO AND ERROR ROUTINES                                            *          
**********************************************************************          
*                                  ** INFO MESSAGES **                          
*                                                                               
IMITEMS  XC    BCWORK,BCWORK                                                    
         LA    R3,BCWORK                                                        
         CURED (B2,NFST),(5,1(R3)),0,ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         LHI   R0,AI$ITEMD         'ITEM X OF XX DISPLAYED.....'                
         STCM  R0,3,BCHALF                                                      
         TM    SCRSTA,SCRSINQ      TEST SINGLE SCREEN                           
         JO    IMITEMS3                                                         
         LHI   R0,AI$ITMDS         'ITEM(S) X THRU XX DISPLAYED.....'           
         STCM  R0,3,BCHALF                                                      
*                                                                               
         CURED (B2,NLST),(5,1(R3)),0,ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
IMITEMS3 DS    0H                                                               
         CURED (B2,NBUF),(5,1(R3)),0,ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)                                                    
         L     R2,ADETH            CURSOR TO FIRST INPUT                        
         TM    SCRSTA,SCRMINQ      MULTI-LINE SCREEN                            
         JNO   IMITEMS9                                                         
         LHI   R0,MXCAP            MAX ITEMS DISPLAYED                          
         CLM   R0,3,NLST                                                        
         JNH   IMITEMS9                                                         
         SR    R3,R3                                                            
         IC    R3,SCRMXD           FIND FIRST BLANK LINE                        
         XR    R0,R0                                                            
         IC    R0,SCRLDL                                                        
IMITEMS5 CLI   8(R2),C' '                                                       
         JNH   IMITEMS9                                                         
         AR    R2,R0               CURSOR TO SECOND LINE                        
         JCT   R3,IMITEMS5                                                      
         L     R2,ADETH            CURSOR TO FIRST INPUT                        
IMITEMS9 STCM  R2,15,BOCURSOR                                                   
         J     IMVX                                                             
*                                                                               
IMPOSTD  LHI   R0,AI$ITMPD         'N ITEMS POSTED $99'                         
         STCM  R0,3,BCHALF                                                      
         XC    BCWORK,BCWORK                                                    
         LA    R3,BCWORK                                                        
         CURED (B1,NPOST),(3,1(R3)),0,ALIGN=LEFT                                
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         MVI   1(R3),C'$'                                                       
         CURED POSTAMT,(12,2(R3)),2,MINUS=YES,ALIGN=LEFT                        
         AHI   R0,2                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)                                                    
         L     R2,AORDH            CURSOR TO ORDER FIELD                        
         STCM  R2,15,BOCURSOR                                                   
         J     IMVX                                                             
*                                                                               
IMVX     MVI   BCBYTE1,GTMINF                                                   
         J     VMX                                                              
*                                                                               
IMEREQF  LHI   R0,AI$EREQF         'ENTER REQUIRED FIELDS..'                    
         J     IMX                                                              
*                                                                               
IMODDEI  LHI   R0,AI$ODDEI         'ORDER DETAILS DISPLAYED...'                 
         J     IMX                                                              
*                                                                               
IMIOKNX  LHI   R0,AI$IOKNX         'INPUT COMPLETE - ENTER NEXT'                
         J     IMX                                                              
*                                                                               
IMITDEL  LHI   R0,AI$ITDEL         'ITEM DELETED...'                            
         J     IMX                                                              
*                                                                               
IMNOMOD  LHI   R0,AI$NOMOD         'NO MORE DATA...'                            
         J     IMX                                                              
*                                                                               
IMDISNX  LHI   R0,AI$DISNX         'RECORD DISPLAYED HIT...'                    
         J     IMX                                                              
*                                                                               
IMX      MVI   FVOMTYP,GTMINF                                                   
         OI    CTXSTAT,CTXOKX      SET OK EXIT                                  
         J     EMX                                                              
         EJECT                                                                  
*                                  ** ERROR MESAGES **                          
EMMXCAP  LHI   R0,AE$MXCAP         'MAXIMUM CAPACITY IS %1 ITEMS'               
         STCM  R0,3,BCHALF                                                      
         XC    BCWORK,BCWORK                                                    
         LA    R3,BCWORK                                                        
         LA    R0,MXCAP                                                         
         CURED (R0),(3,1(R3)),0,ALIGN=LEFT                                      
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)                                                    
         J     EMVX                                                             
*                                                                               
EMMTXRT  LHI   R0,AE$MTXRT         'MISSING TAX RATE FOR ....                   
         J     EMVX                                                             
*                                                                               
EMWC99   LHI   R0,AE$WC99          'WC 99 NOT VALID'                            
         J     EMX                                                              
*                                                                               
EMMISIF  LHI   R0,AE$MISIF         'MISSING INPUT FIELD'                        
         J     EMXNPF                                                           
*                                                                               
EMMIVPFK LHI   R0,AE$IVPFK         'INVALID PFKEY'                              
         J     EMX                                                              
*                                                                               
EMVMEXP  LHI   R0,AE$VMEXP         'VENDOR INPUT MISSING FOR THIS...'           
         J     EMXNPF                                                           
*                                                                               
EMAEEWC  LHI   R0,AE$AEEWC         'AMOUNT EXCEEDS ESTIMATE...'                 
         J     EMX                                                              
*                                                                               
EMNAWTT  LHI   R0,AE$NAWTT         'NOT ALLOWED WITH THIS TYPE'                 
         J     EMX                                                              
*                                                                               
EMIANAL  LHI   R0,AE$IANAL         'INVALID ANALYSIS ACCOUNT'                   
         MVC   FVXTRA(14),KEY+1                                                 
         J     EMX                                                              
*                                                                               
EMANFST  LHI   R0,AE$ANFST         'ACCOUNT NOT FLAGGED FOR STAFF'              
         J     EMX                                                              
*                                                                               
EMINACP  LHI   R0,AE$INACP         'INVALID ACCOUNT FOR POSTING'                
         MVC   FVXTRA(14),KEY+1                                                 
         J     EMX                                                              
*                                                                               
EMACTLK  LHI   R0,AE$ACTLK         'ACCOUNT IS LOCKED'                          
         MVC   FVXTRA(14),KEY+1                                                 
         J     EMX                                                              
*                                                                               
EMSECLK  LHI   R0,AE$SECLK         'SECURITY LOCKOUT'                           
         J     EMX                                                              
*                                                                               
EMACTCL  LHI   R0,AE$ACTCL         'ACCOUNT IS CLOSED'                          
         J     EMX                                                              
*                                                                               
EMINVIF  LHI   R0,AE$INVIF         'INVALID INPUT FIELD'                        
         J     EMX                                                              
*                                                                               
EMANFDP  LHI   R0,AE$ANFDP         'ACCOUNT NOT FLAGGED FOR DEPT'               
         J     EMX                                                              
*                                                                               
EMMMR    LHI   R0,AE$MMR           'MISSING MEDIA RECORD'                       
         J     EMX                                                              
*                                                                               
EMMSJL   LHI   R0,AE$MSJL          'MISSING SJ LEDGER RECORD'                   
         J     EMX                                                              
*                                                                               
EMMEL    LHI   R0,AE$MEL           'MISSING EXPENSE LEDGER RECORD'              
         J     EMX                                                              
*                                                                               
EMINAMT  LHI   R0,AE$INAMT         'INVALID AMOUNT'                             
         J     EMX                                                              
*                                                                               
EMINDAT  LHI   R0,AE$INDAT         'INVALID DATE'                               
         J     EMXNPF                                                           
*                                                                               
EMINWRK  LHI   R0,AE$INWRK         'INVALID WORKCODE'                           
         J     EMX                                                              
*                                                                               
EMAPITL  LHI   R0,AE$APITL         'APOSTROPHE INPUT INVALID ON TOP.'           
         J     EMX                                                              
*                                                                               
EMNODUP  LHI   R0,AE$NODUP         'INPUT IS NOT AVAILABLE TO REPEAT'           
         J     EMX                                                              
*                                                                               
EMPEVIR  LHI   R0,AE$PEVIR         'PROD OR EXPENSE VENDOR REQUIRED'            
         J     EMXNPF                                                           
*                                                                               
EMCDALL  LHI   R0,AE$CDALL         'NO CASH DISC IF CASH ACCOUNT ...'           
         J     EMX                                                              
*                                                                               
EMINACC  LHI   R0,AE$INACC         'INVALID ACCOUNT'                            
         J     EMX                                                              
*                                                                               
EMDOPSP  LHI   R0,AE$DOPSP         'DATE OUTSIDE PERMITTED SPAN'                
         J     EMX                                                              
*                                                                               
EMORDNF  LHI   R0,AE$ORDNF         'ORDER NOT FOUND'                            
         J     EMX                                                              
*                                                                               
EMORTNF  LHI   R0,AE$ORTNF         'ORDER TRANSACTION NOT ON FILE'              
         J     EMX                                                              
*                                                                               
EMOFMCH  LHI   R0,AE$OFMCH         'ORDER IS FULLY MATCH...'                    
         J     EMX                                                              
*                                                                               
EMCCORD  LHI   R0,AE$CCORD         'CAN'T CHANGE ORDER NUMBER - POST..          
         J     EMX                                                              
*                                                                               
EMUPFKD  LHI   R0,AE$UPFKD         'USE PFKEY TO DELETE LINE...                 
         J     EMX                                                              
*                                                                               
EMBASND  LHI   R0,AE$BASND         'BASIS CANNOT BE DELETED...                  
         J     EMX                                                              
*                                                                               
EMDUPIT  OI    FLG,LASTDUP         SET FLAG FOR RETURN                          
         LHI   R0,AE$DUPIT         'DUPLICATE ITEM ON FILE                      
         TM    DUPSW,DUPPROD+DUPEXPN                                            
         JO    EMDIPEV                                                          
         TM    DUPSW,DUPEXPN                                                    
         JO    EMDIEV                                                           
         TM    DUPSW,DUPPROD                                                    
         JO    EMDIPV                                                           
         J     EMX                                                              
*                                                                               
EMDIPV   LHI   R0,AE$DIPV          'DUPLICATE ITEM ON PRODUCTION                
         J     EMX                                                              
*                                                                               
EMDIEV   LHI   R0,AE$DIEV          'DUPLICATE ITEM ON EXPENSE                   
         J     EMX                                                              
*                                                                               
EMDIPEV  LHI   R0,AE$DIPEV         'DUPLICATE ITEM ON PROD & EXPENSE            
         J     EMX                                                              
*                                                                               
EMDIFDU  LHI   R0,AE$DIFDU         'DIFFERENT DUE DATES. ENTER DUE              
         J     EMX                                                              
*                                                                               
EMNOTXE  LHI   R0,AE$NOTXE         'TAX NOT VALID ON EXPENSE JOB                
         J     EMX                                                              
*                                                                               
EMOFCNF  LHI   R0,AE$OFCNF         'OFFICE DOES NOT EXIST..                     
         J     EMX                                                              
*                                                                               
EMDSKER  LHI   R0,AE$DSKER         'UNRECOVERABLE DISK ERROR...'                
         J     EMX                                                              
*                                                                               
EMMAXRC  LHI   R0,AE$MAXRC         'MAXIMUM NUMBER OF ENTRIES REACHED.'         
         J     EMX                                                              
*                                                                               
EMAMTHI  LHI   R0,AE$AMTHI         'AMOUNT TOO HIGH'                            
         J     EMX                                                              
*                                                                               
EMINVGT  LHI   R0,AE$INVGT         'INVALID GST TYPE'                           
         J     EMX                                                              
*                                                                               
EMINVPT  LHI   R0,AE$INVPT         'INVALID PST TYPE'                           
         J     EMX                                                              
*                                                                               
EMGSTNA  LHI   R0,AE$GSTNA         'GST N/A'                                    
         J     EMX                                                              
*                                                                               
EMPSTNA  LHI   R0,AE$PSTNA         'PST N/A'                                    
         J     EMX                                                              
*                                                                               
EMPROVX  LHI   R0,AE$PROVX         'PROVINCE DOES NOT EXIT'                     
         J     EMX                                                              
*                                                                               
EMGSTAI  LHI   R0,AE$GSTAI         'GST AMOUNT IS INVALID'                      
         J     EMX                                                              
*                                                                               
EMPSTAI  LHI   R0,AE$PSTAI         'PST AMOUNT IS INVALID'                      
         J     EMX                                                              
*                                                                               
EMVX     MVI   BCBYTE1,GTMERR                                                   
         J     VMX                                                              
*                                                                               
EMXNPF   NI    PFKSW,X'FF'-(PFKOK) NO PFKEYS                                    
EMX      STCM  R0,3,FVMSGNO                                                     
         J     EXIT                                                             
*                                                                               
VMX      LA    R1,BCPARM           MESSAGES WITH '&1'                           
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,BCHALF      MESSAGE NUMBER                               
         MVC   GTMSYS,ASSYSO                                                    
         MVC   GTMTYP,BCBYTE1      'I' OR 'E'                                   
         LA    R0,BCWORK           SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         GOTO1 VGETTXT,(R1)        RESOLVE MESSAGE                              
         LA    R0,FVFSET           SET MESSAGE ALREADY SUPPLIED                 
         STCM  R0,3,FVMSGNO                                                     
         J     EXIT                                                             
         DROP  R1                                                               
         EJECT                                                                  
**********************************************************************          
* EXIT ROUTINES                                                      *          
**********************************************************************          
EXIT     OC    GSTXN,GSTXN                                                      
         JNZ   *+10                                                             
         ZAP   GSTXA,PZERO         CLEAR ROUNDING ADJUSTMENTS                   
         OC    PSTXN,PSTXN                                                      
         JNZ   *+10                                                             
         ZAP   PSTXA,PZERO                                                      
*                                                                               
         CLI   AGYCTRY,CTRYCAN     TEST CANADIAN AGENCY                         
         JNE   EXIT1               NO,                                          
         TM    POSTSW,POSTVEN+POSTEXP TEST ANY VENDORS                          
         JZ    EXIT1                  NO, SKIP GST/PST                          
         TM    CTXSTAT,CTXRECER    TEST RECALC ERROR                            
         JO    EXIT1                                                            
         TM    CTXSTAT,CTXRECAL+CTXOKX                                          
         JNO   EXIT1                                                            
         TM    CTXSTAT,CTXREEND    ALREADY DONE                                 
         JO    EXIT1                                                            
         LA    R0,1                TEST MORE THAN 1 ITEM                        
         CLM   R0,3,NBUF                                                        
         JNL   EXIT1                                                            
         SR    R0,R0               SAVE FIRST ON SCREEN                         
         ICM   R0,3,NFST           SAVE NUMBER OF FIRST                         
*                                                                               
         BRAS  RE,RECALC           RECALCULATE TAX FOR ALL                      
         OI    CTXSTAT,CTXREEND                                                 
         STCM  R0,3,NGET           SET TO RESTORE FIRST                         
         OI    PFKSW,PFKRDSP       FORCE REDISPLAY OF CURRENT ITEMS             
         BRAS  RE,VALPF                                                         
*                                                                               
EXIT1    ST    R2,FVADDR                                                        
*                                                                               
         ZAP   BCDUB,TOTDLRS       TEST TOTAL DOLLARS                           
         AP    BCDUB,TAXDLRS                                                    
         AP    BCDUB,PSTDLRS                                                    
         AP    BCDUB,GSTXA         ADD ROUNDING ADJUSTMENT                      
         AP    BCDUB,PSTXA         ADJUST TOTAL                                 
         OI    BCDUB+7,X'0F'                                                    
         CP    BCDUB,MXBAT         LIMIT 1 BILLION DOLLARS                      
         JL    *+8                                                              
         OI    POSTSW,POSTNOT      SET POSTING NOT ALLOWED                      
*                                                                               
         OC    LASTORD,LASTORD     TEST ANY ORDER                               
         JZ    EXIT3                                                            
         ICM   R2,15,AORDAH        AMOUNT OF ORDER                              
         JZ    EXIT3                                                            
         MVC   FLD,SPACES                                                       
         CURED ORDRAMT,(12,FLD),2,MINUS=YES,ALIGN=LEFT                          
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
         L     R2,AORDNH           NUMBER INVOICED                              
         MVC   FLD,SPACES                                                       
         CURED ORDRINV,(12,FLD),2,MINUS=YES,ALIGN=LEFT                          
         STC   R0,5(R2)            SET LENGTH                                   
         BRAS  RE,MOVEFLD                                                       
*                                                                               
EXIT3    LHI   RF,PFKNFIS-T61B0A   TURNOFF FIS                                  
         A     RF,BASERB                                                        
         MVC   BCHALF,0(RF)                                                     
         NC    CSAPFMSK,BCHALF                                                  
         SR    RF,RF                                                            
         TM    DUPSW,DUPINV        TEST DUPLICATE INVOICE                       
         JNO   *+8                                                              
         LHI   RF,BPFKFIS                                                       
         STCM  RF,3,BCHALF                                                      
         OC    CSAPFMSK,BCHALF     ALLOW FIS                                    
*                                                                               
         NI    CSINDSL2,X'FF'-CSIPFAPP ASSUME NO APPLICATION PK KEYS            
         TM    PROCSW,PROCEDT      TEST EDIT IN PROGRESS                        
         JO    EXIT5               YES,                                         
         L     RF,ACPFKMSK         SAVE ADDRESS OF CURRENT ENTRY                
         USING PFKMD,RF                                                         
         NC    CSAPFMSK,PFKMCREM   TURNOFF SOME OLD SETTINGS                    
         OC    CSAPFMSK,PFKMCSET   SET NEW PFKEYS                               
         J     EXIT47                                                           
*                                                                               
EXIT5    OI    BASRECH+1,FATBPROT  PROTECT RECORD AND ACTION                    
         OI    BASACTH+1,FATBPROT                                               
         OI    CSINDSL2,CSIPFAPP   SET APP WILL HANDLE PF KEYS                  
*                                                                               
         MVI   VPFSW,0             SET VALID PF KEY SWITCH                      
         MVI   VPFSW1,0                                                         
*                                                                               
         TM    PFKSW,PFKOK         TEST ALLOW PF KEYS                           
         JNO   EXIT11              ERROR, NO PFKEYS                             
*                                                                               
         L     RF,ACPFKMSK                                                      
         USING PFKMD,RF                                                         
         OC    VPFSW,PFKMSET       SET VALID PF KEYS                            
         OC    VPFSW1,PFKMSET1                                                  
         DROP  RF                                                               
*                                                                               
         OC    NBUF,NBUF           TEST ANYTHING IN BUFFER                      
         JNZ   EXIT9               YES,                                         
         NI    VPFSW,X'FF'-(VPFUP+VPFDWN+VPFDEL+VPFPOST)                        
         NI    VPFSW1,X'FF'-(VPFTAX+VPFLIS+VPFLFT+VPFRHT)                       
         J     EXIT11                                                           
*                                                                               
EXIT9    OI    VPFSW1,VPFLIS       ALLOW LIST                                   
         OI    VPFSW,VPFPOST       ALLOW POST                                   
         LA    R3,1                                                             
         CLM   R3,3,NFST           TEST FIRST ON SCREEN                         
         JE    *+8                 YES, DON'T ALLOW 'UP'                        
         OI    VPFSW,VPFUP         ALLOW UP                                     
         CLI   NITM,0              TEST ANY ON SCREEN                           
         JE    *+8                 NO, DON'T ALLOW DELETE                       
         OI    VPFSW,VPFDEL+VPFDWN ALLOW DELETE/DOWN                            
*                                                                               
         TM    SCRSTA,SCRSINQ+SCRLISQ   TEST SINGLE OR LIST                     
         JNZ   EXIT11                                                           
         CLC   NITM,SCRMXD         TEST SCREEN FULL                             
         JNL   *+8                 YES,                                         
         NI    VPFSW,X'FF'-(VPFDWN) NO, DON'T ALLOW DOWN                        
*                                                                               
EXIT11   LHI   R0,MXCAP            TEST MAX CAPACITY                            
         CLM   R0,3,NLST                                                        
         JH    *+8                                                              
         NI    VPFSW,X'FF'-(VPFDWN) NO, DON'T ALLOW DOWN                        
*                                                                               
         TM    DUPSW,DUPINV        TEST DUPLICATE INVOICE                       
         JZ    *+8                                                              
         OI    VPFSW,VPFFIS        ALLOW FIS                                    
*                                                                               
         TM    POSTSW,POSTNOT      TEST POSTING ALLOWED(AMOUNT HIGH)            
         JNO   *+8                                                              
         NI    VPFSW,X'FF'-VPFPOST DON'T ALLOW POST                             
         TM    DUPSW,DUPINV                                                     
         JNO   EXIT13                                                           
         TM    DUPSW,DUPOVR        TEST OVERRIDE                                
         JO    EXIT13                                                           
         NI    VPFSW,X'FF'-(VPFPOST+VPFSWAP) DON'T ALLOW POST/SWAP              
*                                                                               
EXIT13   CLI   SCRNUM,TISCRNQ      TEST TAX SCREEN                              
         JE    *+12                YES,                                         
         CLI   SCRNUM,CTISCRNQ     TEST CANADIAN                                
         JNE   *+8                 NO,                                          
         NI    VPFSW,X'FF'-VPFDEL  DON'T ALLOW DELETE                           
*                                                                               
         NI    PROCSW,X'FF'-(PROCLST)                                           
         TM    SCRSTA,SCRLISQ      TEST LIST SCREEN                             
         JNO   EXIT15              NO,                                          
         OI    PROCSW,PROCLST                                                   
         NI    VPFSW,(VPFUP+VPFDWN)                                             
         MVI   VPFSW1,VPFRTN                                                    
         CLI   LIST#,1                                                          
         JE    *+8                                                              
         OI    VPFSW1,VPFLFT       ALLOW LEFT                                   
         CLC   LIST#,SCRLISMX                                                   
         JE    *+8                                                              
         OI    VPFSW1,VPFRHT        ALLOW RIGHT                                 
         CLC   NLST,NBUF           TEST LAST ON SCREEN                          
         JL    EXIT15                                                           
         NI    VPFSW,X'FF'-(VPFDWN)                                             
*                                                                               
EXIT15   MVC   BOELEM(L'SPACES),SPACES   BUILD OUTPUT STRING                    
         TM    PFKSW,PFKOK         TEST ALLOW PF KEYS                           
         JNO   EXIT20              ERROR, NO PFKEYS                             
         L     R3,APFKEYS                                                       
         USING PFKTD,R3                                                         
         XR    R1,R1                                                            
         IC    R1,PFKTDLN                                                       
         BCTR  R1,0                                                             
         XR    RF,RF                                                            
         ICM   RF,3,PFKTDIC                                                     
         LA    RF,PROGD(RF)                                                     
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   BOELEM(0),0(RF)                                                  
         LA    R2,BOELEM                                                        
         CLI   0(R2),C' '                                                       
         JNH   *+12                                                             
         LA    R2,1(R2)                                                         
         J     *-12                                                             
         LA    R3,PFKTLNQ(R3)                                                   
*                                                                               
EXIT17   XR    R1,R1                                                            
         IC    R1,PFKTVBI          SET VALIDATION BIT                           
         LA    RF,VPFSW            SET VALIDATION BYTE                          
         CLI   PFKTVBY,PFKTVBY0                                                 
         JE    *+8                                                              
         LA    RF,VPFSW1                                                        
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+8                                                              
         TM    0(RF),0             TEST PF KEY VALID                            
         JNO   EXIT19                                                           
*                                                                               
         XR    R1,R1               GET PFKEY NUMBER                             
         IC    R1,PFKTNUM                                                       
         CVD   R1,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  DUB(2),DUB+6(2)                                                  
         LA    R1,1                                                             
         MVC   0(1,R2),DUB+1                                                    
         CLI   DUB,C'0'                                                         
         JE    *+14                                                             
         MVC   0(2,R2),DUB                                                      
         LA    R1,2                                                             
         AR    R2,R1                                                            
         MVI   0(R2),C'='                                                       
         LA    R2,1(R2)                                                         
*                                                                               
         XR    R1,R1               GET PFKEY NAME                               
         IC    R1,PFKTDLN                                                       
         BCTR  R1,0                                                             
         XR    RF,RF                                                            
         ICM   RF,3,PFKTDIC                                                     
         LA    RF,PROGD(RF)                                                     
         BASR  RE,0                                                             
         EX    R1,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,R2),0(RF)                                                    
         LA    R2,2(R1,R2)                                                      
*                                                                               
EXIT19   LA    R3,PFKTLNQ(R3)                                                   
         CLI   0(R3),X'FF'                                                      
         JNE   EXIT17                                                           
*                                                                               
EXIT20   LA    R0,L'BOELEM                                                      
         GOTO1 VSQUASH,DMCB,BOELEM,(R0)                                         
         MVC   FLD,BOELEM                                                       
         L     R2,APFKH                                                         
         BRAS  RE,MOVEFLD          MOVE PFKEYS TO OUTPUT LINE                   
         DROP  R3                                                               
*                                                                               
EXIT21   MVC   FLD,SPACES                                                       
         OC    NBUF,NBUF           TEST ANYTHING IN BUFFER                      
         JZ    EXIT47                                                           
         OC    NFST,NFST                                                        
         JZ    EXIT25                                                           
         XC    BCWORK,BCWORK       ** ITEMS MESSAGE **                          
         LA    R3,BCWORK                                                        
         CURED (B2,NFST),(5,1(R3)),0,ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         LHI   R0,AI$ITMOF         'ITEM X to XX .'                             
         STCM  R0,3,BCHALF                                                      
         TM    SCRSTA,SCRSINQ                                                   
         JO    EXIT23                                                           
         LHI   R0,AI$ITMTO         'ITEMS X TO XX OF                            
         STCM  R0,3,BCHALF                                                      
*                                                                               
         CURED (B2,NLST),(5,1(R3)),0,ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
*                                                                               
EXIT23   DS    0H                                                               
         CURED (B2,NBUF),(5,1(R3)),0,ALIGN=LEFT                                 
         AHI   R0,1                                                             
         STC   R0,0(R3)                                                         
         AR    R3,R0                                                            
         XC    0(2,R3),0(R3)                                                    
         LA    R1,BCPARM           MESSAGES WITH '&1'                           
         USING GETTXTD,R1                                                       
         XC    GTBLOCK,GTBLOCK                                                  
         MVC   GTMSGNO,BCHALF      MESSAGE NUMBER                               
         MVC   GTMSYS,ASSYSO                                                    
         MVI   GTMTYP,GTMINF       'I'                                          
         LA    R0,BCWORK           SET A(SUBSTITUTION PARAMETERS)               
         STCM  R0,7,GTASUBST                                                    
         MVC   WORK,SPACES                                                      
         LA    R5,WORK                                                          
         STCM  R5,7,GTAOUT                                                      
         LA    R0,L'MISTOT                                                      
         STC   R0,GTMAXL                                                        
         OI    GT1INDS,GT1OWRK                                                  
         GOTO1 VGETTXT,(R1)        RESOLVE MESSAGE                              
         DROP  R1                                                               
*                                                                               
         GOTO1 VSQUASH,DMCB,WORK,L'WORK                                         
         XR    R3,R3                                                            
         IC    R3,7(R1)            R3=LENGTH OF DATA                            
         ICM   RF,15,ATOTH                                                      
         XR    R1,R1                                                            
         IC    R1,0(RF)                                                         
         LA    RE,9                                                             
         TM    1(RF),FATBXHDR      TEST FOR EXTENDED FIELD                      
         JZ    *+8                                                              
         LA    RE,17                                                            
         SR    R1,RE               R1=LENGTH OF SCREEN FIELD                    
         LA    RF,FLD(R1)          RF=A(RIGHT SIDE OF FIELD)                    
         BCTR  R3,0                                                             
         SR    RF,R3               RF=A(START OF RIGHT SIDE DATA)               
         BASR  RE,0                                                             
         EX    R3,8(RE)                                                         
         J     *+10                                                             
         MVC   0(0,RF),WORK        MOVE RIGHT SIDE TO FLD                       
*                                                                               
EXIT25   LA    R5,FLD              INVOICE TOTAL  $999.99                       
         LHI   R1,INVTMSG-T61B0A                                                
         A     R1,BASERB                                                        
         MVC   0(L'INVTMSG,R5),0(R1)                                            
         LA    R5,L'INVTMSG(R5)                                                 
         TM    SCRSTA,SCRCANQ      TEST CANADIAN                                
         JO    EXIT27                                                           
         ZAP   BCDUB,TOTDLRS                                                    
         AP    BCDUB,TAXDLRS                                                    
         MVI   0(R5),C'$'                                                       
         LA    R5,1(R5)                                                         
         CURED BCDUB,(13,0(R5)),2,MINUS=YES,ALIGN=LEFT                          
         AR    R5,R0                                                            
         LA    R5,1(R5)                                                         
         ZAP   TOTITM,TOTDLRS                                                   
         J     EXIT31                                                           
*                                                                               
EXIT27   ZAP   TOTOVR,TOTDLRS      ** CANADIAN TOTALS **                        
         ZAP   TOTITM,TOTDLRS                                                   
         ZAP   TOTTAX,GSTDLRS       TAX TOTAL                                   
         AP    TOTTAX,PSTDLRS                                                   
         AP    TOTTAX,GSTXA        ADD ROUNDING ADJUSTMENT                      
         AP    TOTTAX,PSTXA                                                     
         CLI   TXGON,TXGGRSQ       TEST GROSS                                   
         JNE   *+10                                                             
         SP    TOTITM,TOTTAX       YES, GET NET ITEMS                           
         CLI   TXGON,TXGNETQ       TEST NET                                     
         JNE   *+10                                                             
         AP    TOTOVR,TOTTAX       YES, ADD TAX TO OVERALL                      
*                                                                               
*&&DO                                                                           
         CURED GSTBASE,(8,0(R5)),2,MINUS=YES,ALIGN=LEFT                         
         AR    R5,R0                                                            
         LA    R5,1(R5)                                                         
*&&                                                                             
                                                                                
         CP    TOTOVR,MXNEG                                                     
         JL    *+12                                                             
         MVI   0(R5),C'$'                                                       
         LA    R5,1(R5)                                                         
*                                                                               
         CURED TOTOVR,(13,0(R5)),2,MINUS=YES,ALIGN=LEFT                         
         AR    R5,R0                                                            
         LA    R5,1(R5)                                                         
         CP    TOTTAX,PZERO                                                     
         JE    EXIT35                                                           
         MVI   0(R5),C'('                                                       
         LA    R5,1(R5)                                                         
         CP    GSTDLRS,PZERO                                                    
         JE    EXIT29                                                           
         LHI   R1,CGSTMSG-T61B0A   (G=999.00 P=123.11/ITEMS $1234.00)           
         A     R1,BASERB                                                        
         MVC   0(L'CGSTMSG,R5),0(R1)                                            
         LA    R5,L'CGSTMSG(R5)                                                 
         ZAP   BCDUB,GSTDLRS                                                    
         AP    BCDUB,GSTXA                                                      
         CURED BCDUB,(12,0(R5)),2,MINUS=YES,ALIGN=LEFT                          
         AR    R5,0                                                             
*                                                                               
EXIT29   CP    PSTDLRS,PZERO                                                    
         JE    EXIT33                                                           
         CP    GSTDLRS,PZERO                                                    
         JE    *+8                                                              
         LA    R5,1(R5)                                                         
         LHI   R1,CPSTMSG-T61B0A                                                
         A     R1,BASERB                                                        
         MVC   0(L'CPSTMSG,R5),0(R1)                                            
         LA    R5,L'CPSTMSG(R5)                                                 
         ZAP   BCDUB,PSTDLRS                                                    
         AP    BCDUB,PSTXA                                                      
         CURED BCDUB,(12,0(R5)),2,MINUS=YES,ALIGN=LEFT                          
         AR    R5,0                                                             
         J     EXIT33                                                           
*                                                                               
EXIT31   CLI   NTAXL,0             ANY TAX ITEMS                                
         JE    EXIT35              NO                                           
         LHI   R1,USETMSG-T61B0A   (USE TAX $99.99 / ITEMS $99.99)              
         A     R1,BASERB                                                        
         MVC   0(L'USETMSG,R5),0(R1)                                            
         LA    R5,L'USETMSG(R5)                                                 
         MVI   0(R5),C'$'                                                       
         LA    R5,1(R5)                                                         
         CURED TAXDLRS,(12,0(R5)),2,MINUS=YES,ALIGN=LEFT                        
         AR    R5,0                                                             
*                                                                               
EXIT33   MVI   0(R5),C'/'                                                       
         LA    R5,1(R5)                                                         
         LHI   R1,ITMSMSG-T61B0A                                                
         A     R1,BASERB                                                        
         MVC   0(L'ITMSMSG,R5),0(R1)                                            
         LA    R5,L'ITMSMSG(R5)                                                 
*                                                                               
         MVI   0(R5),C'$'                                                       
         LA    R5,1(R5)                                                         
         CURED TOTITM,(13,0(R5)),2,MINUS=YES,ALIGN=LEFT                         
         AR    R5,0                                                             
         MVI   0(R5),C')'                                                       
*                                                                               
EXIT35   ICM   R2,15,ATOTH                                                      
         BRAS  RE,MOVEFLD          PUT #ITEMS MESSAGE                           
*                                                                               
EXIT37   TM    SCRSTA,SCRMINQ      TEST MULTIPLE SCREEN                         
         JO    EXIT47              YES, NEVER PROTECT UPPER                     
         TM    SCRSTA,SCRTAXQ      TEST TAX SCRREEN                             
         JO    EXIT41              YES, ALWAYS PROTECT                          
         XR    R3,R3               IS FIRST ITEM ON SCREEN ?                    
         ICM   R3,3,NFST                                                        
         CHI   R3,1                                                             
         JE    EXIT47              YES, UPPER CAN BE UNPROTECTD                 
*                                                                               
EXIT41   ICM   R2,15,AORDH         TEST UPPER FIELDS PROTECTED                  
         JZ    EXIT51                                                           
         TM    1(R2),FATBPROT                                                   
         JO    EXIT51              YES, OK TO EXIT                              
*                                                                               
         L     R3,APUNTAB          PROTECT UPPER SCREEN FIELDS                  
EXIT43   TM    0(R3),X'80'         TEST NARRATIVE FIELD                         
         JNO   *+12                NO,                                          
         TM    SCRSTA,SCRSINQ      TEST DETAIL SCREEN                           
         JO    EXIT45              YES, DON'T PROTECT                           
         XR    RF,RF                                                            
         ICM   RF,3,1(R3)                                                       
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)                                                      
         JZ    EXIT45                                                           
         OI    1(R2),FATBPROT                                                   
         NI    6(R2),X'FF'-(FOUTMOD)                                            
*                                                                               
EXIT45   LA    R3,L'PUNTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   EXIT43                                                           
         J     EXIT51                                                           
*                                                                               
EXIT47   ICM   R2,15,AORDH         TEST UPPER FIELDS PROTECTED                  
         JZ    EXIT51                                                           
         TM    1(R2),FATBPROT                                                   
         JNO   EXIT51                                                           
         L     R3,APUNTAB          UNPROTECT UPPER SCREEN FIELDS                
*                                                                               
EXIT49   XR    RF,RF                                                            
         ICM   RF,3,1(R3)                                                       
         LA    RF,PROGD(RF)                                                     
         ICM   R2,15,0(RF)                                                      
         JZ    *+8                                                              
         NI    1(R2),X'FF'-(FATBPROT)                                           
         LA    R3,L'PUNTAB(R3)                                                  
         CLI   0(R3),X'FF'                                                      
         JNE   EXIT49                                                           
*                                                                               
EXIT51   SR    R1,R1               TRANSMIT SCREEN                              
         LA    R2,BASMSGH                                                       
EXIT53   OI    FLDOINDD(R2),FOUTTRN                                             
         IC    R1,0(R2)                                                         
         AR    R2,R1                                                            
         CLI   0(R2),0                                                          
         JNE   EXIT53                                                           
*                                                                               
         OC    NBUF,NBUF                                                        
         JZ    EXIT55                                                           
         LHI   RF,SAVNUMQ                                                       
         STCM  RF,3,NGET                                                        
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRGETQ      GET SAVDATA RECORD                           
         MVC   SAVREC,SAVDATA      SAVE NEW DATA                                
         OI    NGET,RECSRQ                                                      
         GOTOR TOTSAR,TSRPUTQ      SAVE NEW TO TSAR                             
*                                                                               
EXIT55   GOTOR TOTSAR,TSRSAVQ      SAVE BUFFER                                  
         LA    RF,TWAD             SAVE SOME DATA IN TWA                        
         AHI   RF,OSSAVE-TWAD                                                   
         MVC   0(STSLNQ,RF),STS                                                 
*                                                                               
EXIT57   L     RD,BCSVRD           RETURN TO BASE                               
         L     RD,8(RD)                                                         
*                                                                               
XYES     CR    RB,RB                                                            
         J     XIT                                                              
XNO      LTR   RB,RB                                                            
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
**********************************************************************          
* SCREEN CONTROL - SEE SCRD                                          *          
**********************************************************************          
         DS    0F                                                               
USATAB   DS    0X                   ** US TABLES **                             
         DC    AL1(MISCRNQ,MXMULQ,LINLNQ,SCRMINQ)     MULTIPLE                  
         DC    AL1(SISCRNQ,TISCRNQ)                                             
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(MULTUPR-T61B0A,MULTLOW-T61B0A)                               
*                                                                               
         DC    AL1(SISCRNQ,MXSINQ,0,SCRSINQ)          DETAIL                    
         DC    AL1(MISCRNQ,TISCRNQ)                                             
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(SINGUPR-T61B0A,SINGLOW-T61B0A)                               
*                                                                               
         DC    AL1(TISCRNQ,MXUSEQ,USELNQ,SCRTAXQ)      TAX                      
         DC    AL1(SISCRNQ,TISCRNQ)                                             
         DC    AL1(0,0)                                                         
         DC    AL2(USET-T61B0A)                                                 
         DC    AL2(USEUPR-T61B0A,USELOW-T61B0A)                                 
*                                                                               
         DC    AL1(LISSCRNQ,MXLISQ,LISLNQ,SCRLISQ)      LIST                    
         DC    AL1(SISCRNQ,0)                                                   
         DC    AL1(4,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(LISUPR-T61B0A,LISLOW-T61B0A)                                 
         DC    X'FF'                                                            
*                                                                               
*                                   ** CANADIAN TABLES **                       
CANTAB   DC    AL1(CMISCRNQ,MXCMULQ,CLINLNQ,SCRMINQ+SCRCANQ)  MULTIPLE          
         DC    AL1(CSISCRNQ,CTISCRNQ)                                           
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(CMULTUPR-T61B0A,CMULTLOW-T61B0A)                             
*                                                                               
         DC    AL1(CSISCRNQ,MXCSINQ,0,SCRSINQ+SCRCANQ) DETAIL                   
         DC    AL1(CMISCRNQ,CTISCRNQ)                                           
         DC    AL1(0,0)                                                         
         DC    AL2(PROC-T61B0A)                                                 
         DC    AL2(CSINGUPR-T61B0A,CSINGLOW-T61B0A)                             
*                                                                               
         DC    AL1(CTISCRNQ,MXCTAXQ,CTAXLNQ,SCRTAXQ+SCRCANQ)  TAX               
         DC    AL1(CSISCRNQ,CTISCRNQ)                                           
         DC    AL1(0,0)                                                         
         DC    AL2(CTXR-T61B0A)                                                 
         DC    AL2(CTXUPR-T61B0A,CTXLOW-T61B0A)                                 
*                                                                               
         DC    AL1(LISSCRNQ,MXLISQ,LISLNQ,SCRLISQ)      LIST                    
         DC    AL1(SISCRNQ,0)                                                   
         DC    AL1(3,0)                                                         
         DC    AL2(0)                                                           
         DC    AL2(LISUPR-T61B0A,LISLOW-T61B0A)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* SCREEN TABLES                                                      *          
**********************************************************************          
*  MULTIPLE INPUT - UPPER                                                       
MULTUPR  DS    0F                                                               
         DC    AL2(MISORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(MISORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(MISORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(MISDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(MISDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(MISDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(MISURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(MISCSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(MISCSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(MISVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(MISVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(MISVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(MISXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(MISXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(MISXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(MISOVRTH-TWAD,AOVRTH-PROGD)                                  
         DC    AL2(MISOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(MISNARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(MISNAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(MISDTL-TWAD,ADETH-PROGD)                                     
         DC    AL2(MISDTL-TWAD,AINPH-PROGD)                                     
         DC    AL2(MISTOTH-TWAD,ATOTH-PROGD)                                    
         DC    AL2(MISPFKH-TWAD,APFKH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  MULTIPLE INPUT - LOWER                                                       
MULTLOW  DS    0F                                                               
         DC    AL2(MISAMTH-MISDTL,AAMTH-PROGD)                                  
         DC    AL2(MISCLIH-MISDTL,ACLIH-PROGD)                                  
         DC    AL2(MISPROH-MISDTL,APROH-PROGD)                                  
         DC    AL2(MISJOBH-MISDTL,AJOBH-PROGD)                                  
         DC    AL2(MISWKCH-MISDTL,AWKCH-PROGD)                                  
         DC    AL2(MISEXAH-MISDTL,AXACH-PROGD)                                  
         DC    AL2(MISDOFH-MISDTL,ADOFH-PROGD)                                  
         DC    AL2(MISCOFH-MISDTL,ACOFH-PROGD)                                  
         DC    AL2(MISAOFH-MISDTL,AAOFH-PROGD)                                  
         DC    AL2(MISDPTH-MISDTL,ADPTH-PROGD)                                  
         DC    AL2(MISPERH-MISDTL,APERH-PROGD)                                  
         DC    AL2(MISLIN2H-MISDTL,ANEXT-PROGD)                                 
         DC    X'FF'                                                            
                                                                                
*                                                                               
*  SINGLE INPUT - UPPER                                                         
SINGUPR  DS    0F                                                               
         DC    AL2(SISORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(SISORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(SISORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(SISDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(SISDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(SISDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(SISURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(SISCSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(SISCSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(SISVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(SISVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(SISVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(SISXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(SISXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(SISXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(SISOVRTH-TWAD,AOVRTH-PROGD)                                  
         DC    AL2(SISOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(SISNARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(SISNAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(SISDTL-TWAD,ADETH-PROGD)                                     
         DC    AL2(SISDTL-TWAD,AINPH-PROGD)                                     
         DC    AL2(SISTOTH-TWAD,ATOTH-PROGD)                                    
         DC    AL2(SISPFKH-TWAD,APFKH-PROGD)                                    
         DC    X'FF'                                                            
                                                                                
*  SINGLE INPUT - LOWER                                                         
SINGLOW  DS    0F                                                               
         DC    AL2(SISAMTH-SISDTL,AAMTH-PROGD)                                  
         DC    AL2(SISCLIH-SISDTL,ACLIH-PROGD)                                  
         DC    AL2(SISCLNH-SISDTL,ACLINH-PROGD)                                 
         DC    AL2(SISPROH-SISDTL,APROH-PROGD)                                  
         DC    AL2(SISPRNH-SISDTL,APRONH-PROGD)                                 
         DC    AL2(SISJOBH-SISDTL,AJOBH-PROGD)                                  
         DC    AL2(SISJONH-SISDTL,AJOBNH-PROGD)                                 
         DC    AL2(SISWKCH-SISDTL,AWKCH-PROGD)                                  
         DC    AL2(SISWKNH-SISDTL,AWKCNH-PROGD)                                 
         DC    AL2(SISEXAH-SISDTL,AXACH-PROGD)                                  
         DC    AL2(SISEXNH-SISDTL,AXACNH-PROGD)                                 
         DC    AL2(SISDOFH-SISDTL,ADOFH-PROGD)                                  
         DC    AL2(SISDONH-SISDTL,ADOFNH-PROGD)                                 
         DC    AL2(SISCOFH-SISDTL,ACOFH-PROGD)                                  
         DC    AL2(SISCONH-SISDTL,ACOFNH-PROGD)                                 
         DC    AL2(SISAOFH-SISDTL,AAOFH-PROGD)                                  
         DC    AL2(SISAONH-SISDTL,AAOFNH-PROGD)                                 
         DC    AL2(SISDPTH-SISDTL,ADPTH-PROGD)                                  
         DC    AL2(SISDPTNH-SISDTL,ADPTNH-PROGD)                                
         DC    AL2(SISPERH-SISDTL,APERH-PROGD)                                  
         DC    AL2(SISPERNH-SISDTL,APERNH-PROGD)                                
         DC    AL2(SISBASH-SISDTL,ABASH-PROGD)                                  
         DC    AL2(SISLOCH-SISDTL,ALOCH-PROGD)                                  
         DC    AL2(SISTAXH-SISDTL,ATAXH-PROGD)                                  
         DC    AL2(SISTXWCH-SISDTL,ATXWCH-PROGD)                                
         DC    X'FF'                                                            
                                                                                
*  USE TAX - UPPER                                                              
USEUPR   DS    0F                                                               
         DC    AL2(USEORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(USEORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(USEORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(USEDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(USEDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(USEDUEH-TWAD,ADUEH-PROGD)                                    
         DC    AL2(USEURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(USECSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(USECSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(USEVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(USEVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(USEVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(USEXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(USEXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(USEXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(USEOVRTH-TWAD,AOVRTH-PROGD)                                  
         DC    AL2(USEOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(USENARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(USENAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(USEDTL-TWAD,ADETH-PROGD)                                     
         DC    AL2(USEDTL-TWAD,AINPH-PROGD)                                     
         DC    AL2(USETOTH-TWAD,ATOTH-PROGD)                                    
         DC    AL2(USEPFKH-TWAD,APFKH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  USE TAX - LOWER                                                              
USELOW   DS    0F                                                               
         DC    AL2(USEBASH-USEDTL,ABASH-PROGD)                                  
         DC    AL2(USELOCH-USEDTL,ALOCH-PROGD)                                  
         DC    AL2(USECPJH-USEDTL,ACPJH-PROGD)                                  
         DC    AL2(USETXWCH-USEDTL,ATXWCH-PROGD)                                
         DC    AL2(USETAXH-USEDTL,ATAXH-PROGD)                                  
         DC    AL2(USELIN2H-USEDTL,ANEXT-PROGD)                                 
         DC    X'FF'                                                            
*                                                                               
*  LIST - UPPER                                                                 
LISUPR   DS    0F                                                               
         DC    AL2(LISDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(LISDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(LISVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(LISXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(LISHEADH-TWAD,AHEADH-PROGD)                                  
         DC    AL2(LISUNDRH-TWAD,AUNDRH-PROGD)                                  
         DC    AL2(LISDTL-TWAD,ADETH-PROGD)                                     
         DC    AL2(LISTOTH-TWAD,ATOTH-PROGD)                                    
         DC    AL2(LISPFKH-TWAD,APFKH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  LIST - LOWER                                                                 
LISLOW   DS    0F                                                               
         DC    AL2(LISSEL1H-LISDTL,ASELH-PROGD)                                 
         DC    AL2(LISLIN1H-LISDTL,ALISH-PROGD)                                 
         DC    AL2(LISSEL2H-LISDTL,ANEXT-PROGD)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* SCREEN TABLES - CANADIAN                                           *          
**********************************************************************          
*  MULTIPLE INPUT - UPPER                                                       
CMULTUPR DS    0F                                                               
         DC    AL2(CMIORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(CMIORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(CMIORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(CMIDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(CMIDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(CMIDUEH-TWAD,ADUEH-PROGD)                                    
*                                                                               
         DC    AL2(CMIGONH-TWAD,AGONH-PROGD)                                    
         DC    AL2(CMIGTYH-TWAD,AGTYH-PROGD)                                    
         DC    AL2(CMIGAMH-TWAD,AGAMH-PROGD)                                    
         DC    AL2(CMIPRVH-TWAD,APRVH-PROGD)                                    
         DC    AL2(CMIPTYH-TWAD,APTYH-PROGD)                                    
         DC    AL2(CMIPAMH-TWAD,APAMH-PROGD)                                    
*                                                                               
         DC    AL2(CMIURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(CMICSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(CMICSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(CMIVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(CMIVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(CMIVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(CMIXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(CMIXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(CMIXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(CMIOVRTH-TWAD,AOVRTH-PROGD)                                  
         DC    AL2(CMIOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(CMINARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(CMINAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(CMIDTL-TWAD,ADETH-PROGD)                                     
         DC    AL2(CMIDTL-TWAD,AINPH-PROGD)                                     
         DC    AL2(CMITOTH-TWAD,ATOTH-PROGD)                                    
         DC    AL2(CMIPFKH-TWAD,APFKH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  MULTIPLE INPUT - LOWER                                                       
CMULTLOW DS    0F                                                               
         DC    AL2(CMIAMTH-CMIDTL,AAMTH-PROGD)                                  
         DC    AL2(CMICLIH-CMIDTL,ACLIH-PROGD)                                  
         DC    AL2(CMIPROH-CMIDTL,APROH-PROGD)                                  
         DC    AL2(CMIJOBH-CMIDTL,AJOBH-PROGD)                                  
         DC    AL2(CMIWKCH-CMIDTL,AWKCH-PROGD)                                  
         DC    AL2(CMIEXAH-CMIDTL,AXACH-PROGD)                                  
         DC    AL2(CMIDOFH-CMIDTL,ADOFH-PROGD)                                  
         DC    AL2(CMICOFH-CMIDTL,ACOFH-PROGD)                                  
         DC    AL2(CMIAOFH-CMIDTL,AAOFH-PROGD)                                  
         DC    AL2(CMIDPTH-CMIDTL,ADPTH-PROGD)                                  
         DC    AL2(CMIPERH-CMIDTL,APERH-PROGD)                                  
         DC    AL2(CMILIN2H-CMIDTL,ANEXT-PROGD)                                 
         DC    X'FF'                                                            
                                                                                
*                                                                               
*  SINGLE INPUT - UPPER                                                         
CSINGUPR DS    0F                                                               
         DC    AL2(CSIORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(CSIORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(CSIORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(CSIDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(CSIDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(CSIDUEH-TWAD,ADUEH-PROGD)                                    
*                                                                               
         DC    AL2(CSIGONH-TWAD,AGONH-PROGD)                                    
         DC    AL2(CSIGTYH-TWAD,AGTYH-PROGD)                                    
         DC    AL2(CSIGAMH-TWAD,AGAMH-PROGD)                                    
         DC    AL2(CSIGSTH-TWAD,AGSTH-PROGD)                                    
         DC    AL2(CSIPRVH-TWAD,APRVH-PROGD)                                    
         DC    AL2(CSIPTYH-TWAD,APTYH-PROGD)                                    
         DC    AL2(CSIPAMH-TWAD,APAMH-PROGD)                                    
         DC    AL2(CSIPSTH-TWAD,APSTH-PROGD)                                    
*                                                                               
         DC    AL2(CSIURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(CSICSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(CSICSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(CSIVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(CSIVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(CSIVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(CSIXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(CSIXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(CSIXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(CSIOVRTH-TWAD,AOVRTH-PROGD)                                  
         DC    AL2(CSIOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(CSINARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(CSINAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(CSIDTL-TWAD,ADETH-PROGD)                                     
         DC    AL2(CSIDTL-TWAD,AINPH-PROGD)                                     
         DC    AL2(CSITOTH-TWAD,ATOTH-PROGD)                                    
         DC    AL2(CSIPFKH-TWAD,APFKH-PROGD)                                    
         DC    X'FF'                                                            
                                                                                
*  SINGLE INPUT - LOWER                                                         
CSINGLOW DS    0F                                                               
         DC    AL2(CSIAMTH-CSIDTL,AAMTH-PROGD)                                  
         DC    AL2(CSICLIH-CSIDTL,ACLIH-PROGD)                                  
         DC    AL2(CSICLNH-CSIDTL,ACLINH-PROGD)                                 
         DC    AL2(CSIPROH-CSIDTL,APROH-PROGD)                                  
         DC    AL2(CSIPRNH-CSIDTL,APRONH-PROGD)                                 
         DC    AL2(CSIJOBH-CSIDTL,AJOBH-PROGD)                                  
         DC    AL2(CSIJONH-CSIDTL,AJOBNH-PROGD)                                 
         DC    AL2(CSIWKCH-CSIDTL,AWKCH-PROGD)                                  
         DC    AL2(CSIWKNH-CSIDTL,AWKCNH-PROGD)                                 
         DC    AL2(CSIEXAH-CSIDTL,AXACH-PROGD)                                  
         DC    AL2(CSIEXNH-CSIDTL,AXACNH-PROGD)                                 
         DC    AL2(CSIDOFH-CSIDTL,ADOFH-PROGD)                                  
         DC    AL2(CSIDONH-CSIDTL,ADOFNH-PROGD)                                 
         DC    AL2(CSICOFH-CSIDTL,ACOFH-PROGD)                                  
         DC    AL2(CSICONH-CSIDTL,ACOFNH-PROGD)                                 
         DC    AL2(CSIAOFH-CSIDTL,AAOFH-PROGD)                                  
         DC    AL2(CSIAONH-CSIDTL,AAOFNH-PROGD)                                 
         DC    AL2(CSIDPTH-CSIDTL,ADPTH-PROGD)                                  
         DC    AL2(CSIDPTNH-CSIDTL,ADPTNH-PROGD)                                
         DC    AL2(CSIPERH-CSIDTL,APERH-PROGD)                                  
         DC    AL2(CSIPERNH-CSIDTL,APERNH-PROGD)                                
*                                                                               
         DC    AL2(CSIBASH-CSIDTL,ABASH-PROGD)                                  
         DC    AL2(CSIGTOH-CSIDTL,AGTOH-PROGD)                                  
         DC    AL2(CSIPTOH-CSIDTL,APTOH-PROGD)                                  
         DC    AL2(CSIPVOH-CSIDTL,APVOH-PROGD)                                  
         DC    X'FF'                                                            
*                                                                               
*  CANADIAN TAX - UPPER                                                         
CTXUPR   DS    0F                                                               
         DC    AL2(CTXORDH-TWAD,AORDH-PROGD)                                    
         DC    AL2(CTXORDAH-TWAD,AORDAH-PROGD)                                  
         DC    AL2(CTXORDNH-TWAD,AORDNH-PROGD)                                  
         DC    AL2(CTXDOCH-TWAD,ADOCH-PROGD)                                    
         DC    AL2(CTXDATH-TWAD,ADATH-PROGD)                                    
         DC    AL2(CTXDUEH-TWAD,ADUEH-PROGD)                                    
*                                                                               
         DC    AL2(CTXGONH-TWAD,AGONH-PROGD)                                    
         DC    AL2(CTXGTYH-TWAD,AGTYH-PROGD)                                    
         DC    AL2(CTXGAMH-TWAD,AGAMH-PROGD)                                    
         DC    AL2(CTXPRVH-TWAD,APRVH-PROGD)                                    
         DC    AL2(CTXPTYH-TWAD,APTYH-PROGD)                                    
         DC    AL2(CTXPAMH-TWAD,APAMH-PROGD)                                    
*                                                                               
         DC    AL2(CTXURGH-TWAD,AURGH-PROGD)                                    
         DC    AL2(CTXCSHH-TWAD,ACSHH-PROGD)                                    
         DC    AL2(CTXCSHNH-TWAD,ACSHNH-PROGD)                                  
         DC    AL2(CTXVENH-TWAD,AVENH-PROGD)                                    
         DC    AL2(CTXVCDH-TWAD,AVCDH-PROGD)                                    
         DC    AL2(CTXVCDDH-TWAD,AVCDNH-PROGD)                                  
         DC    AL2(CTXXVNH-TWAD,AXVNH-PROGD)                                    
         DC    AL2(CTXXCDH-TWAD,AXCDH-PROGD)                                    
         DC    AL2(CTXXCDDH-TWAD,AXCDNH-PROGD)                                  
         DC    AL2(CTXOVRTH-TWAD,AOVRTH-PROGD)                                  
         DC    AL2(CTXOVRH-TWAD,AOVRH-PROGD)                                    
         DC    AL2(CTXNARH-TWAD,ANARH-PROGD)                                    
         DC    AL2(CTXNAR2H-TWAD,ANA2H-PROGD)                                   
         DC    AL2(CTXDTL-TWAD,ADETH-PROGD)                                     
         DC    AL2(CTXDTL-TWAD,AINPH-PROGD)                                     
         DC    AL2(CTXTOTH-TWAD,ATOTH-PROGD)                                    
         DC    AL2(CTXPFKH-TWAD,APFKH-PROGD)                                    
         DC    X'FF'                                                            
*                                                                               
*  CANADIAN TAX - LOWER                                                         
CTXLOW   DS    0F                                                               
         DC    AL2(CTXBASH-CTXDTL,ABASH-PROGD)                                  
         DC    AL2(CTXCPJH-CTXDTL,ACPJH-PROGD)                                  
         DC    AL2(CTXGTOH-CTXDTL,AGTOH-PROGD)                                  
         DC    AL2(CTXPTOH-CTXDTL,APTOH-PROGD)                                  
         DC    AL2(CTXPVOH-CTXDTL,APVOH-PROGD)                                  
         DC    AL2(CTXTXRAH-CTXDTL,ATXRAH-PROGD)                                
         DC    AL2(CTXLIN2H-CTXDTL,ANEXT-PROGD)                                 
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* SAVED UPPER INPUT FIELDS (2 BYTE DISP TO FIELD, 2 BYTE DISP TO AREA)*         
***********************************************************************         
UPRTAB   DS    0F                                                               
         DC    AL2(AORDH-PROGD,ORDNUML-PROGD)                                   
         DC    AL2(ADOCH-PROGD,DOCNUML-PROGD)                                   
         DC    AL2(ADATH-PROGD,DOCDTEL-PROGD)                                   
         DC    AL2(ADUEH-PROGD,DUEDTEL-PROGD)                                   
         DC    AL2(AURGH-PROGD,URGENTL-PROGD)                                   
         DC    AL2(ACSHH-PROGD,CASHACL-PROGD)                                   
         DC    AL2(AVENH-PROGD,PRDVNDL-PROGD)                                   
         DC    AL2(AVCDH-PROGD,PRDVCDL-PROGD)                                   
         DC    AL2(AXVNH-PROGD,EXPVNDL-PROGD)                                   
         DC    AL2(AXCDH-PROGD,EXPVCDL-PROGD)                                   
         DC    AL2(AOVRH-PROGD,OVRIDEL-PROGD)                                   
         DC    AL2(AGONH-PROGD,GORNL-PROGD)                                     
         DC    AL2(AGTYH-PROGD,GTYPL-PROGD)                                     
         DC    AL2(AGAMH-PROGD,GAML-PROGD)                                      
         DC    AL2(APRVH-PROGD,PRVL-PROGD)                                      
         DC    AL2(APTYH-PROGD,PTYPL-PROGD)                                     
         DC    AL2(APAMH-PROGD,PAML-PROGD)                                      
         DC    X'FF'                                                            
***********************************************************************         
* UPPER INPUT FIELDS TO PROTECT/ UNPROTECT                            *         
***********************************************************************         
PUNTAB   DS    0XL3                                                             
         DC    X'00',AL2(AORDH-PROGD)                                           
         DC    X'00',AL2(ADOCH-PROGD)                                           
         DC    X'00',AL2(ADATH-PROGD)                                           
         DC    X'00',AL2(ADUEH-PROGD)                                           
         DC    X'00',AL2(AURGH-PROGD)                                           
         DC    X'00',AL2(ACSHH-PROGD)                                           
         DC    X'00',AL2(AVENH-PROGD)                                           
         DC    X'00',AL2(AVCDH-PROGD)                                           
         DC    X'00',AL2(AXVNH-PROGD)                                           
         DC    X'00',AL2(AXCDH-PROGD)                                           
         DC    X'00',AL2(AOVRH-PROGD)                                           
         DC    X'80',AL2(ANARH-PROGD)                                           
         DC    X'80',AL2(ANA2H-PROGD)                                           
*                                                                               
         DC    X'00',AL2(AGONH-PROGD)                                           
         DC    X'00',AL2(AGTYH-PROGD)                                           
         DC    X'00',AL2(AGAMH-PROGD)                                           
         DC    X'00',AL2(APRVH-PROGD)                                           
         DC    X'00',AL2(APTYH-PROGD)                                           
         DC    X'00',AL2(APAMH-PROGD)                                           
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TABLE OF SCREEN  FIELDS TO CLEAR                                    *         
***********************************************************************         
         DS    0F                                                               
CLRTAB   DS    0XL2                                                             
         DC    AL2(AORDAH-PROGD)   ORDER AMOUNT                                 
         DC    AL2(AORDNH-PROGD)   AMOUNT INVOICED AGAINST ORDER                
         DC    AL2(ACSHNH-PROGD)   CASH ACCOUNT NAME                            
         DC    AL2(AVCDNH-PROGD)   VENDOR CD AMOUNT                             
         DC    AL2(AXCDNH-PROGD)   EXPENSE VENDOR CD AMOUNT                     
         DC    AL2(ADOFNH-PROGD)   DEBIT OFFICE NAME                            
         DC    AL2(ACOFNH-PROGD)   CREDIT OFFICE NAME                           
         DC    AL2(AAOFNH-PROGD)   ANALYSIS OFFICE NAME                         
         DC    AL2(ADPTNH-PROGD)   DEPARTMENT NAME                              
         DC    AL2(APERNH-PROGD)   STAFF NAME                                   
         DC    AL2(ATAXH-PROGD)    TAX FIELD                                    
         DC    AL2(AGSTH-PROGD)    GST FIELD                                    
         DC    AL2(APSTH-PROGD)    GST FIELD                                    
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* XJOB FIELDS TO CLEAR                                                *         
***********************************************************************         
         DS    0F                                                               
XJOTAB   DS    0XL2                                                             
         DC    AL2(ADOFH-PROGD)    FINANCIAL OFFICE                             
         DC    AL2(AAOFH-PROGD)    ANALYSIS OFFICE                              
         DC    AL2(AXACH-PROGD)    EXPENSE ACCOUNT                              
         DC    AL2(ADPTH-PROGD)    DEPARTMENT                                   
         DC    AL2(APERH-PROGD)    STAFF                                        
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* ORDER FIELDS TO CLEAR                                               *         
***********************************************************************         
ORDTAB   DS    0F                                                               
         DC    AL2(AVENH-PROGD)    PRODUCTION VENDOR                            
         DC    X'FF'                                                            
                                                                                
***********************************************************************         
* ORDER FIELDS TO FILL FROM EXOELD(EXPENSE ORDERS)                    *         
***********************************************************************         
         DS    0F                                                               
EXOTAB   DS    0XL5                                                             
         DC    AL2(ACLIH-PROGD,EXOCLI-EXOELD),AL1(L'EXOCLI) CLI                 
         DC    AL2(APROH-PROGD,EXOPRO-EXOELD),AL1(L'EXOPRO) PRO                 
         DC    AL2(ADOFH-PROGD,EXODOF-EXOELD),AL1(L'EXODOF) DR OFF              
         DC    AL2(ACOFH-PROGD,EXOCOF-EXOELD),AL1(L'EXOCOF) CR OFF              
         DC    AL2(AAOFH-PROGD,EXOAOF-EXOELD),AL1(L'EXOAOF) AN OFF              
         DC    AL2(ADPTH-PROGD,EXODEP-EXOELD),AL1(L'EXODEP) DEPART              
         DC    AL2(APERH-PROGD,EXOPER-EXOELD),AL1(L'EXOPER) PERSON              
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY TABLE - SEE PFKTD                                             *         
***********************************************************************         
PFKTAB   DC    AL1(0,0,0),AL2(0)                                                
         DC    AL1(L'LD@PFK),AL2(LD@PFK-PROGD)                                  
*                                                                               
         DC    AL1(0,VPFUP,PFUPQ),AL2(PFUP-VALPF)                               
         DC    AL1(L'LD@UP),AL2(LD@UP-PROGD)                                    
*                                                                               
         DC    AL1(0,VPFDWN,PFDWNQ),AL2(PFDWN-VALPF)                            
         DC    AL1(L'LD@DOWN),AL2(LD@DOWN-PROGD)                                
*                                                                               
         DC    AL1(0,VPFPOST,PFPOSTQ),AL2(PFPOST-VALPF)                         
         DC    AL1(L'LD@POST),AL2(LD@POST-PROGD)                                
*                                                                               
         DC    AL1(0,VPFDEL,PFDELQ),AL2(PFDEL-VALPF)                            
         DC    AL1(L'LD@DEL),AL2(LD@DEL-PROGD)                                  
*                                                                               
         DC    AL1(0,VPFFIS,PFFISQ),AL2(PFFIS-VALPF)                            
         DC    AL1(L'LD@FIS),AL2(LD@FIS-PROGD)                                  
*                                                                               
         DC    AL1(1,VPFLIS,PFLISQ),AL2(PFLIST-VALPF)                           
         DC    AL1(L'LD@NAME),AL2(LD@NAME-PROGD)                                
*                                                                               
         DC    AL1(0,VPFDTL,PFSWAPQ),AL2(PFSWAP-VALPF)                          
         DC    AL1(L'LD@DTL),AL2(LD@DTL-PROGD)                                  
*                                                                               
         DC    AL1(0,VPFMUL,PFSWAPQ),AL2(PFSWAP-VALPF)                          
         DC    AL1(L'LD@MULT),AL2(LD@MULT-PROGD)                                
*                                                                               
         DC    AL1(1,VPFLFT,PFLFTQ),AL2(0)                                      
         DC    AL1(L'LD@LEFT),AL2(LD@LEFT-PROGD)                                
*                                                                               
         DC    AL1(1,VPFRHT,PFRHTQ),AL2(0)                                      
         DC    AL1(L'LD@RIGHT),AL2(LD@RIGHT-PROGD)                              
*                                                                               
         DC    AL1(1,VPFTAX,PFTAXQ),AL2(PFTAX-VALPF)                            
         DC    AL1(L'LD@TAX),AL2(LD@TAX-PROGD)                                  
*                                                                               
         DC    AL1(1,VPFINV,PFINVQ),AL2(PFINV-VALPF)                            
         DC    AL1(L'LD@INV),AL2(LD@INV-PROGD)                                  
*                                                                               
         DC    AL1(1,VPFRTN,PFRTNQ),AL2(0)                                      
         DC    AL1(L'LD@RETRN),AL2(LD@RETRN-PROGD)                              
*                                                                               
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* PFKEY MASK TABLE - SEE PFKMD                                        *         
***********************************************************************         
PFKMALL  EQU   X'FFFF'                                                          
*                                                                               
PFKMSK   DS    0XL7                PFKEY MASK TABLE                             
         DC    AL1(SCRMINQ)                                                     
         DC    AL2(PFKMALL-(BPFKMUL+BPFKINV))                                   
         DC    AL2(BPFKDTL+BPFKTAX)                                             
         DC    AL1(VPFDTL,VPFTAX)                                               
*                                                                               
         DC    AL1(SCRSINQ)                                                     
         DC    AL2(PFKMALL-(BPFKDTL+BPFKINV))                                   
         DC    AL2(BPFKMUL+BPFKTAX)                                             
         DC    AL1(VPFMUL,VPFTAX)                                               
*                                                                               
         DC    AL1(SCRTAXQ)                                                     
         DC    AL2(PFKMALL-(BPFKMUL+BPFKDTL+BPFKTAX))                           
         DC    AL2(BPFKINV)                                                     
         DC    AL1(0,VPFINV)                                                    
*                                                                               
         DC    AL1(SCRLISQ)                                                     
         DC    AL2(PFKMALL-(BPFKMUL+BPFKINV+BPFKTAX))                           
         DC    AL2(BPFKDTL)                                                     
         DC    AL1(VPFDTL,0)                                                    
         DC    X'FF'                                                            
         EJECT                                                                  
***********************************************************************         
* TSAR RECORD FIELD DISPLACEMENT TABLE                                *         
***********************************************************************         
RCDTAB   DS    0XL5                                                             
         DC    AL2(RECCLI-RECSD,ACLIH-PROGD),AL1(L'RECCLI)                      
         DC    AL2(RECPRO-RECSD,APROH-PROGD),AL1(L'RECPRO)                      
         DC    AL2(RECJOB-RECSD,AJOBH-PROGD),AL1(L'RECJOB)                      
         DC    AL2(RECWKC-RECSD,AWKCH-PROGD),AL1(L'RECWKC)                      
         DC    AL2(RECAMT-RECSD,AAMTH-PROGD),AL1(L'RECAMT)                      
         DC    AL2(RECXAC-RECSD,AXACH-PROGD),AL1(L'RECXAC)                      
         DC    AL2(RECDOF-RECSD,ADOFH-PROGD),AL1(L'RECDOF)                      
         DC    AL2(RECCOF-RECSD,ACOFH-PROGD),AL1(L'RECCOF)                      
         DC    AL2(RECAOF-RECSD,AAOFH-PROGD),AL1(L'RECAOF)                      
         DC    AL2(RECDPT-RECSD,ADPTH-PROGD),AL1(L'RECDPT)                      
         DC    AL2(RECPER-RECSD,APERH-PROGD),AL1(L'RECPER)                      
*                                                                               
         DC    AL2(RECCPJC-RECSD,ACLIH-PROGD),AL1(L'RECCPJC)                    
         DC    AL2(RECCPJP-RECSD,APROH-PROGD),AL1(L'RECCPJP)                    
         DC    AL2(RECCPJJ-RECSD,AJOBH-PROGD),AL1(L'RECCPJJ)                    
         DC    AL2(RECCPJW-RECSD,AWKCH-PROGD),AL1(L'RECCPJW)                    
         DC    AL2(RECBAS-RECSD,ABASH-PROGD),AL1(L'RECBAS)                      
         DC    AL2(RECLOC-RECSD,ALOCH-PROGD),AL1(L'RECLOC)                      
         DC    AL2(RECTAX-RECSD,ATAXH-PROGD),AL1(L'RECTAX)                      
         DC    AL2(RECTXWC-RECSD,ATXWCH-PROGD),AL1(L'RECTXWC)                   
*                                                                               
         DC    AL2(RECGTO-RECSD,AGTOH-PROGD),AL1(L'RECGTO)                      
         DC    AL2(RECPTO-RECSD,APTOH-PROGD),AL1(L'RECPTO)                      
         DC    AL2(RECPVO-RECSD,APVOH-PROGD),AL1(L'RECPVO)                      
         DC    AL2(RECTXRA-RECSD,ATXRAH-PROGD),AL1(L'RECTXRA)                   
*                                                                               
         DC    AL2(RECNAR1-RECSD,ANARH-PROGD),AL1(L'RECNAR1)                    
         DC    AL2(RECNAR2-RECSD,ANA2H-PROGD),AL1(L'RECNAR2)                    
         DC    X'FF'                                                            
         EJECT                                                                  
**********************************************************************          
* LOCAL DICTIONARY LIST                                              *          
**********************************************************************          
DCDDL    DS    0XL4                                                             
         DCDDL AC#AMT,L'LD@AMT                                                  
         DCDDL AC#CLIPK,L'LD@CPJ                                                
         DCDDL AC#WC,L'LD@WKC                                                   
         DCDDL AC#EXPA,L'LD@EXPA                                                
         DCDDL AC#RSDOF,L'LD@DOF                                                
         DCDDL AC#RSCOF,L'LD@COF                                                
         DCDDL AC#ANOFF,L'LD@AOF                                                
         DCDDL AC#DPT,L'LD@DPT                                                  
         DCDDL AC#PRSN,L'LD@PER                                                 
         DCDDL AC#PFK,L'LD@PFK                                                  
         DCDDL AC#DTL,L'LD@DTL                                                  
         DCDDL AC#MULT,L'LD@MULT                                                
         DCDDL AC#UP,L'LD@UP                                                    
         DCDDL AC#DOWN,L'LD@DOWN                                                
         DCDDL AC#DEL,L'LD@DEL                                                  
         DCDDL AC#POST,L'LD@POST                                                
         DCDDL AC#CLEAR,L'LD@CLEAR                                              
         DCDDL AC#FIS,L'LD@FIS                                                  
         DCDDL AC#TAX,L'LD@TAX                                                  
         DCDDL AC#INV,L'LD@INV                                                  
         DCDDL AC#NAME,L'LD@NAME                                                
         DCDDL AC#LEFT,L'LD@LEFT                                                
         DCDDL AC#RIGHT,L'LD@RIGHT                                              
         DCDDL AC#RETRN,L'LD@RETRN                                              
         DC    AL1(0)                                                           
         EJECT                                                                  
**********************************************************************          
* WORKING STORAGE                                                    *          
**********************************************************************          
PROGD    DSECT                                                                  
MISCRNQ  EQU   X'F8'               MULTIPLE ITEM SCREEN                         
SISCRNQ  EQU   X'F5'               SINGLE ITEM SCREEN                           
TISCRNQ  EQU   X'B0'               TAX INPUT SCREEN                             
CMISCRNQ EQU   X'FB'               CANADIAN MULTIPLE ITEM SCREEN                
CSISCRNQ EQU   X'F3'               CANADIAN SINGLE ITEM SCREEN                  
CTISCRNQ EQU   X'B1'               CANADIAN TAX INPUT SCREEN                    
LISSCRNQ EQU   X'B2'               LIST SCREEN                                  
*                                                                               
LINLNQ   EQU   (MISLIN2H-MISDTL)   LENGTH OF DETAIL LINE                        
USELNQ   EQU   (USELIN2H-USEDTL)   LENGTH OF USE TAX LINE                       
LISLNQ   EQU   (LISSEL2H-LISDTL)   LENGTH OF LIST LINE                          
CLINLNQ  EQU   (CMILIN2H-CMIDTL)   LENGTH OF DETAIL LINE(CANADIAN)              
CTAXLNQ  EQU   (CTXLIN2H-CTXDTL)   LENGTH OF USE TAX LINE(CANADIAN)             
*                                                                               
MXMULQ   EQU   (MISTABH-MISDTL)/(LINLNQ)     MAX DETAIL LINES                   
MXUSEQ   EQU   (USETABH-USEDTL)/(USELNQ)     MAX TAX LINES                      
MXLISQ   EQU   (LISTOTH-LISDTL)/(LISLNQ)     MAX LIST LINES                     
MXCMULQ  EQU   (CMITABH-CMIDTL)/(CLINLNQ)    MAX DETAIL LINES(CAN)              
MXCTAXQ  EQU   (CTXTABH-CTXDTL)/(CTAXLNQ)    MAX TAX LINES(CAN)                 
*                                                                               
MXSINQ   EQU   1                            MAX ON SINGLE DETAIL                
MXCSINQ  EQU   1                            SINGLE DETAILS(CAN)                 
MXTXLQ   EQU   8                   MAX TAX LOCALITIES                           
*                                  PFKEY EQUATES                                
PFUPQ    EQU   1                    UP                                          
PFDWNQ   EQU   2                    DOWN                                        
PFPOSTQ  EQU   3                    POST                                        
PFDELQ   EQU   4                    DELETE                                      
PFFISQ   EQU   6                    FIS                                         
PFLISQ   EQU   7                    LIST                                        
PFSWAPQ  EQU   9                    DETAIL/MULTIPLE                             
PFLFTQ   EQU   9                    LEFT                                        
PFRHTQ   EQU   10                   RIGHT                                       
PFTAXQ   EQU   10                   TAX                                         
PFINVQ   EQU   11                   INVOICE                                     
PFRTNQ   EQU   11                   RETURN                                      
*                                                                               
FCONDUE  EQU   1                   FIELD SECURITY - DUE DATE                    
FCONOVR  EQU   2                                  - OVERRIDE                    
*                                                                               
MXCAP    EQU   120                 MAX BUFFER CAPACITY                          
VNDRLNQ  EQU   20                  LENGTH OF NAME DISPLAY                       
ADDRLNQ  EQU   26                  LENGTH OF ADDRESS DISPLAY                    
*                                                                               
SCRD     DS    0F                  SCREEN DATA                                  
SCRNUM   DS    XL1                 SCREEN NUMBER                                
SCRMXD   DS    XL1                 MAX NUMBER OF DETAIL LINES                   
SCRLDL   DS    XL1                 LENGTH OF DETAIL LINE                        
SCRSTA   DS    XL1                 STATUS                                       
SCRMINQ  EQU   X'80'                MULTIPLE INPUT                              
SCRSINQ  EQU   X'40'                SINGLE INPUT                                
SCRCANQ  EQU   X'20'                CANADIAN SCREEN                             
SCRTAXQ  EQU   X'10'                TAX SCREEN                                  
SCRLISQ  EQU   X'08'                LIST SCREEN                                 
SCRSWAP  DS    AL1                 SWAP SCREEN CODE                             
SCRTAXS  DS    AL1                 TAX SCREEN CODE                              
SCRLISMX DS    XL1                 NUMBER OF SCREENS IN LIST                    
         DS    XL1                 N/D                                          
SCRPROC  DS    AL2                 PROCESSING ROUTINE                           
SCRUPR   DS    AL2                 DISP. TO TABLE OF UPPER FIELDS               
SCRLOW   DS    AL2                 DISP. TO TABLE OF LOWER FIELDS               
SCRLNQ   EQU   *-SCRD                                                           
*                                                                               
BASERB   DS    F                                                                
RELOA    DS    F                                                                
VCNVERT  DS    A                                                                
VSEARCH  DS    A                                                                
AEXCELD  DS    A                                                                
ACATD    DS    A                                                                
ARECIOA  DS    A                                                                
ARECOLD  DS    A                                                                
ATAXIT   DS    A                                                                
AXTRAINF DS    A                                                                
*                                                                               
AUSATAB  DS    A                   A(US TABLES)                                 
ACANTAB  DS    A                   A(CANADIAN TABLES)                           
AUPRTAB  DS    A                   A(HEADER TABLE)                              
APUNTAB  DS    A                   A(PROCTECT/UNPROTECT)                        
ACLRTAB  DS    A                   A(UPPER NAME FIELDS)                         
AXJOTAB  DS    A                   A(XJOB FIELDS)                               
AORDTAB  DS    A                   A(ORDER FIELDS)                              
AEXOTAB  DS    A                   A(EXPENSE ORDER FIELDS)                      
ARCDTAB  DS    A                   A(RECORD TABLE)                              
APFKEYS  DS    A                   A(PFKEY TABLE)                               
APFKMSK  DS    A                   A(PFKEY MASK)                                
ADCDDL   DS    A                   A(LOCATE DICTIONARY LIST)                    
APRVTAB  DS    A                   A(PROVINCE TABLE)                            
*                                                                               
UPPER    DS    0A                  ** UPPER FIELDS **                           
AHEADH   DS    A                   A(LIST HEAD LINE)                            
AUNDRH   DS    A                   A(UNDERLINE LINE)                            
AORDH    DS    A                   A(ORDER NUMBER)                              
AORDAH   DS    A                   A(ORDER AMOUNT)                              
AORDNH   DS    A                   A(ORDER NUMBER OF INVOICES)                  
ADOCH    DS    A                   A(DOCUMENT FIELD)                            
ADATH    DS    A                   A(TRANSACTION DATE)                          
ADUEH    DS    A                   A(DUE DATE)                                  
*                                                                               
AGONH    DS    A                   A(GROSS OR NET)                              
AGTYH    DS    A                   A(GST TYPE)                                  
AGAMH    DS    A                   A(GST INPUT AMOUNT)                          
AGSTH    DS    A                   A(GST ITEM RATE\AMOUNT)                      
APRVH    DS    A                   A(PROVINCE)                                  
APTYH    DS    A                   A(PST TYPE)                                  
APAMH    DS    A                   A(PST INPUT AMOUNT)                          
APSTH    DS    A                   A(PST ITEM RATE\AMOUNT)                      
*                                                                               
AURGH    DS    A                   A(URGENT)                                    
ACSHH    DS    A                   A(CASH ACCOUNT)                              
ACSHNH   DS    A                   A(CASH ACCOUNT NAME)                         
AVENH    DS    A                   A(PRODUCTION VENDOR)                         
AVCDH    DS    A                   A(VENDOR CASH DISCOUNT)                      
AVCDNH   DS    A                   A(VENDOR CASH DISCOUNT NAME)                 
AXVNH    DS    A                   A(EXPENSE VENDOR)                            
AXCDH    DS    A                   A(EXPENSE VENDOR CD)                         
AXCDNH   DS    A                   A(EXPENSE VENDOR CD NAME)                    
AOVRTH   DS    A                   A(OVERRIDE TAG FIELD)                        
AOVRH    DS    A                   A(OVERRIDE FIELD)                            
ANARH    DS    A                   A(FIRST NARRATIVE)                           
ANA2H    DS    A                   A(SECOND NARRATIVE)                          
ASELH    DS    A                   A(SELECT FIELD)                              
ADETH    DS    A                   A(FIRST DETAIL LINE)                         
AINPH    DS    A                   A(FIRST UNPROTECTED DETAIL)                  
ATOTH    DS    A                   A(TOTALS)                                    
APFKH    DS    A                   A(PFKEY LINE)                                
NUPPER   EQU   (*-UPPER)/4                                                      
*                                                                               
LOWER    DS    0A                  ** LOWER FIELDS **                           
ALISH    DS    A                   A(LIST LINE)                                 
         ORG   ALISH                                                            
AAMTH    DS    A                   A(AMOUNT)                                    
ACLIH    DS    A                   A(CLIENT)                                    
ACLINH   DS    A                   A(CLIENT NAME)                               
APROH    DS    A                   A(PRODUCT)                                   
APRONH   DS    A                   A(PRODUCT NAME)                              
AJOBH    DS    A                   A(JOB)                                       
AJOBNH   DS    A                   A(JOB NAME)                                  
AWKCH    DS    A                   A(W/C)                                       
AWKCNH   DS    A                   A(WORKCODE NAME)                             
ABASH    DS    A                   A(BASIS)                                     
ACPJH    DS    A                   A(CPJ)                                       
AXACH    DS    A                   A(EXPENSE A/C)                               
AXACNH   DS    A                   A(EXPENSE A/C NAME)                          
ADOFH    DS    A                   A(DEBIT/FINANCIAL OFFICE)                    
ADOFNH   DS    A                   A(DEBIT OFFICE NAME)                         
ACOFH    DS    A                   A(CREDIT OFFICE)                             
ACOFNH   DS    A                   A(CREDIT OFFICE NAME)                        
AAOFH    DS    A                   A(ANALYSIS OFFICE)                           
AAOFNH   DS    A                   A(ANALYSIS OFFICE NAME)                      
ADPTH    DS    A                   A(DEPARTMENT CODE)                           
ADPTNH   DS    A                   A(DEPARTMENT NAME)                           
APERH    DS    A                   A(PERSON/STAFF CODE)                         
APERNH   DS    A                   A(PERSON/STAFF NAME)                         
*                                                                               
AGTOH    DS    A                   A(GST OVERRIDE)                              
APTOH    DS    A                   A(PST OVERRIDE)                              
APVOH    DS    A                   A(PROVINCE OVERRIDE)                         
ATXRAH   DS    A                   A(TAX RATE\AMOUNT)                           
*                                                                               
ATAXH    DS    A                   A(TAX AMOUNT)                                
ALOCH    DS    A                   A(LOCALITY)                                  
ATXWCH   DS    A                   A(TAX WORKCODE)                              
*                                                                               
ANEXT    DS    A                                                                
NLOWER   EQU  (*-LOWER)/4                                                       
*                                                                               
ACPFKMSK DS    A                  A(CURRENT PFKEY MASK ENTRY)                   
ADEXEL   DS    A                  A(DUE ELEMENT)                                
AEXOEL   DS    A                  A(EXPENSE ORDER DETAIL)                       
ASCMEL   DS    A                  A(NARRATIVE ELEMENT)                          
*                                                                               
SAVERE   DS    F                                                                
SVREG    DS    9F                  SAVE REGISTERS RE,R6                         
SVRFR1   DS    3F                  SAVE RF,R0,R1                                
AWKCSAV  DS    F                                                                
ASTARTIM DS    A                   A(START OF ITEM ON LIST SCREEN)              
ANXTTAX  DS    A                   A(NEXT TAX ITEM IN TABLE)                    
*                                                                               
CASHPRM  DS    3F                  FOR CASHVAL CALL                             
         ORG   CASHPRM+4                                                        
CASHRTN  DS    PL8                                                              
*                                                                               
DUPPRDK  DS    XL(L'INVPKEY)                                                    
DUPEXPK  DS    XL(L'INVPKEY)                                                    
*                                                                               
CBATREF  DS    CL(L'LSTBBREF)      BATCH REFERENCE                              
PZERO    DS    PL1                                                              
PNEG1    DS    PL1                                                              
MXAMT    DS    PL8                                                              
MXBAT    DS    PL8                                                              
MXNEG    DS    PL8                                                              
WC99     DS    CL2                                                              
SXVND    DS    CL2                                                              
BYTE     DS    CL1                                                              
NPOST    DS    XL1                 NUMBER POSTED                                
NTXLN    DS    XL1                 NUMBER OF TAX LINES                          
TXHMSG   DS    CL(L'TXHMSGC)       TAX HEADLINE MESSAGE FOR LSIT                
*                                                                               
PL8      DS    PL8                                                              
PL13     DS    PL13                                                             
PL16     DS    PL16                                                             
*                                                                               
NETGST   DS    PL6                 NET +GST                                     
NETAMT   DS    PL6                                                              
ITMTAX   DS    PL6                                                              
ITMGST   DS    PL6                                                              
ITMPST   DS    PL6                                                              
ITMBASE  DS    PL6                                                              
ITMGSTR  DS    XL2                                                              
ITMPSTR  DS    XL2                                                              
ITMNET   DS    PL6                                                              
GSTRL    DS    CL(L'CSIGST)                                                     
PSTRL    DS    CL(L'CSIPST)                                                     
*                                                                               
BASAMNT  DS    PL6                 BASIS AMOUNT                                 
TAXAMNT  DS    PL6                 TAX AMOUNT                                   
         ORG   TAXAMNT                                                          
GSTAMNT  DS    PL6                 GST INPUT TAX                                
PSTAMNT  DS    PL6                 PST INPUT TAX                                
*                                                                               
TOTOVR   DS    PL8                 OVERALL TOTAL                                
TOTITM   DS    PL8                 ITEMS TOTAL                                  
TOTTAX   DS    PL8                 TAX TOTAL                                    
*                                                                               
OFFNAME  DS    CL36                OFFICE NAME                                  
VENDIN   DS    CL15                VENDOR INPUT                                 
*                                                                               
XCTL     DS    XL24                GLOBBER CONTROL ELEMENT                      
*                                                                               
ACTION   DS    CL1                                                              
ADDRECQ  EQU   C'A'                ADD A RECORD                                 
PUTRECQ  EQU   C'P'                PUT A RECORD                                 
RECALCQ  EQU   C'R'                RECALCULATE GST/PST                          
*                                                                               
POSTSW   DS    XL1                 POSTING CONTROL                              
POSTBUF  EQU   X'80'                POST ITEMS IN BUFFER                        
POSTNXT  EQU   X'40'                RETURN TO POST NEXT                         
POSTNOT  EQU   X'20'                POST NOT ALLOED                             
POSTVEN  EQU   X'08'                VENDOR POSTINGS                             
POSTEXP  EQU   X'04'                EXPENSE POSTINGS                            
POSTPEN  EQU   X'02'                POST PENDING COUNT                          
*                                                                               
PFKSW    DS    XL1                 PF KEY CONTROL                               
PFKPROC  EQU   X'80'                PF KEY HAS BEEN PROCESSED                   
PFKDELT  EQU   X'40'                DELETE HAS BEEN PROCESSED                   
PFKRDSP  EQU   X'20'                FORCE A REDISPLAY                           
PFKOK    EQU   X'10'                NO ERRORS - ALLOW PF KEYS                   
*                                                                               
DUPSW    DS    XL1                 DUPLICATE CONTROL SWITCH                     
DUPINV   EQU   X'80'                THIS IS A DUPLICATE INVOICE                 
DUPOVR   EQU   X'40'                DUPLICATE OVERRIDE                          
DUPPROD  EQU   X'20'                DUPLICATE ON PRODUCTION VENDOR              
DUPEXPN  EQU   X'10'                DUPLICATE ON EXPENSE VENDOR                 
*                                                                               
CASHACCT DS    CL15                SC CASH ACCT #                               
CASHNAME DS    CL36                SC CASH ACCT NAME                            
*                                                                               
DITM     DS    XL1                 RELATIVE NUMBER OF DELETED ITEM              
REFNUML  DS    XL1                 LENGTH OF REFERENCE                          
REFNUM   DS    CL6                 REFERENCE                                    
LONGINVL DS    XL1                 LENGTH OF LONG INVOICE NUMBER                
LONGINV  DS    CL20                LONG INVOICE NUMBER                          
DOCDATE  DS    XL3                 INVOICE DATE                                 
DODATEP  DS    XL2                 DUE DATE - PRODCUTION                        
DODATEX  DS    XL2                 DUE DATE - EXPENSE                           
DODATE   DS    XL2                 DUE DATE                                     
ADRLIN1  DS    CL(L'ADRADD1)       ADDRESS LINE 1                               
ADRLIN1L DS    XL1                 LENGTH OF ADDRESS LINE 1                     
*                                                                               
VENDVALS DS    0C                  ** SAVED VENDOR VALUES **                    
VENDACCT DS    CL15                PROD VENDOR KEY                              
VENDNAME DS    CL36                PROD VENDOR NAME                             
VENDDISC DS    PL3                 CD VAL FOR PROD VENDS                        
*                                                                               
VENDCTX  DS    CL(CTLNQ)           PRODUCTION VENDOR CANADIAN TAX               
*                                                                               
VENDADR2 DS    CL(L'ADRADD2)       VENDOR ADDRESS LINE 2                        
VENDADR3 DS    CL(L'ADRADD3)                           3                        
VENDADR4 DS    CL(L'ADRADD4)                           4                        
VENDLONG DS    CL(L'LISVEN)        LONGER <NAME><ADDR> FOR LIST                 
*                                                                               
PAYACC   DS    CL15                PAYABLE ACCOUNT                              
PDISC    DS    PL3                                                              
*                                                                               
XVNDACCT DS    CL15                EXP VENDOR KEY                               
XVNDNAME DS    CL36                EXP VENDOR NAME                              
XVNDDISC DS    PL3                 CD VAL FOR EXP VENDS                         
*                                                                               
XVNDCTX  DS    CL(CTLNQ)           EXPENSE VENDOR CANADIAN TAX                  
*                                                                               
XVNDADR2 DS    CL(L'ADRADD2)       EXP VENDOR ADDRESS LINE 2                    
XVNDADR3 DS    CL(L'ADRADD3)                               3                    
XVNDADR4 DS    CL(L'ADRADD4)                               4                    
XVNDLONG DS    CL(L'LISVEN)        LONGER <NAME><ADDR> FOR LIST                 
*                                                                               
PCONSULT DS    CL1                 1099(2C) VENDOR SWITCH PROD VENDOR.          
ECONSULT DS    CL1                 1099(2C) VENDOR SWITCH EXP VENDOR.           
PRSTAT   DS    CL1                 PROD VENDOR 1099 STATUS                      
EXSTAT   DS    CL1                 EXP VENDOR 1099 STATUS                       
V29SW    DS    CL1                 EXPENSE VENDOR CONTRA.                       
*                                                                               
P2CNUM   DS    CL15                2C NUM  PROD                                 
P2CNAM   DS    CL36                2C NAME PROD                                 
E2CNUM   DS    CL15                2C NUM  EXP                                  
E2CNAM   DS    CL36                2C NAME EXP                                  
PROTROL  DS    CL15                27 NUM  PROD                                 
PROTROLN DS    CL36                27 NAME PROD                                 
EXPTROL  DS    CL15                27 NUM  EXP                                  
EXPTROLN DS    CL36                27 NAME EXP                                  
VENDLNQS EQU   *-VENDVALS          LENGTH OF VENDOR VALUES                      
*                                                                               
MAXREM   DS    H                   MAX ITEM REMAINING AT ENTRY                  
MAXBUF   DS    H                   MAX ON SCREEN TO FILL BUFFER                 
MAXBAT   DS    H                   MAX ON SCREEN TO FILL BATCH                  
*                                                                               
NARRLEN  DS    H                   LENGTH OF NARRATIVE                          
NARR     DS    CL(L'MISNAR+L'MISNAR2+2)                                         
*                                                                               
DNARR1   DS    CL(L'MISNAR)        DEFAULT NARRATIVE'S                          
DNARR2   DS    CL(L'MISNAR2)                                                    
*                                                                               
PRDVNDL  DS    XL1                 LENGTH & PRODUCTION VENDOR                   
PRDVND   DS    CL(L'MISVEN)                                                     
*                                                                               
EXPVNDL  DS    XL1                 LENGTH & EXPENSE VENDOR                      
EXPVND   DS    CL(L'MISXVN)                                                     
*                                                                               
TXGON    DS    CL1                 GST/PST GROSS OR NET                         
TXGGRSQ  EQU   C'G'                GST/PST GROSS                                
TXGNETQ  EQU   C'N'                GST/PST NET                                  
TXGNONQ  EQU   C'*'                NON                                          
*                                                                               
CTXSTAT  DS    XL1                 CANADIAN TAX STATUS                          
CTXSGSTI EQU   X'80'                GST AMOUNT INPUT                            
CTXSPSTI EQU   X'40'                PST AMOUNT INPUT                            
CTXRECAL EQU   X'20'                RECALCULATE ALL                             
CTXREEND EQU   X'10'                RECALCULATE DONE                            
CTXOKX   EQU   X'08'                EXISTING WITHOUT ERRORS                     
CTXNOPRV EQU   X'04'                NO PROVINCE CODE                            
CTXRECER EQU   X'02'                RECALCULATE ERROR                           
*                                                                               
DICO     DS    0C                                                               
         DSDDL PRINT=YES                                                        
         DS    XL1                                                              
*                                                                               
         EJECT                                                                  
* DETAIL VALUES AREA IS CLEARED BEFORE EDITING AND POSTING EACH ITEM            
*                                                                               
DETVALS  DS    0C                  DETAIL VALUES AREA                           
*                                                                               
SIMDAC   DS    CL15                                                             
SIMDACN  DS    CL36                                                             
*                                                                               
CLIACCT  DS    CL15                CLIENT ACCOUNT                               
CLINAME  DS    CL36                CLIENT NAME                                  
PROACCT  DS    CL15                PRODUCT ACCOUNT                              
PRONAME  DS    CL36                PRODUCT NAME                                 
JOBACCT  DS    CL15                JOB ACCOUNT                                  
JOBNAME  DS    CL36                JOB NAME                                     
WRKCODE  DS    CL2                 WORK CODE                                    
WRKNAME  DS    CL15                WORK CODE NAME                               
*                                                                               
CLIOFFIC DS    CL2                 SJ OFFICE CODE                               
CLICOST  DS    CL15                1C ACCOUNT POINTER                           
CLICODE  DS    CL6                 CLIENT CODE                                  
PRODCODE DS    CL6                 PRODUCT CODE                                 
JOBNUM   DS    CL6                 JOB NUMBER                                   
CLIPRO   DS    CL15                CLI,PRO ACCT KEY                             
CLIPRON  DS    CL36                CLI,PRD NAME FOR DISP                        
*                                                                               
FINOFF   DS    CL2                 FINANCIAL OFFICE                             
ANAOFF   DS    CL2                 ANALYSIS OFFICE                              
CRDOFF   DS    CL2                 CREDIT OFFICE                                
PRDOFF   DS    CL2                 PRODUCTION OFFICE                            
         ORG   PRDOFF                                                           
TAXOFF   DS    CL2                 OFFICE FOR TAX POSTINGS                      
TAXWKC   DS    CL2                 TAX WORKCODE                                 
*                                                                               
ANOELEM  DS    XL(ANOLNQ)          ANALYSED OFFICE ELEMENT                      
FFTELEM  DS    XL(FFTLN1Q+L'FFTDLEN+L'FFTCLPRA) CLIENT PRODUCT ELEMENT          
FFTWKELM DS    XL13                 FFTTWRKC FOR PROD                           
FFTTXELM DS    XL13                 FFTTWRKC FOR TAX                            
*                                                                               
DEPT     DS    CL4                                                              
STAFF    DS    CL12                                                             
*                                                                               
TYPE     DS    CL1                 E=EXPENSE,P=PRODUCTION                       
COSTSW   DS    CL1                 Y=COSTING REQUIRED                           
*                                                                               
ANLSTA   DS    XL1                 ANALYSIS STATUS                              
ANLOFF   EQU   X'80'                ANALYZE OFFICE                              
ANLDEP   EQU   X'40'                   "    DEPARTMENT                          
ANLSTF   EQU   X'20'                   "    STAFF                               
*                                                                               
ITMSTA   DS    XL1                 ITEM STATUS                                  
ITMNONC  EQU   X'80'                NON-COMMISSIONABLE                          
ITMTAXD  EQU   X'20'                TAXED                                       
ITMXJOB  EQU   X'10'                XJOB                                        
ITMEXPN  EQU   X'08'                EXPENSE ITEM                                
ITMGROS  EQU   X'04'                INPUT AS GROSS                              
*                                                                               
EXPOST   DS    PL6                 EXPENSE AMOUNT (LESS CD IF OK)               
CDAMNT   DS    PL6                                                              
*                                                                               
TAXCNT   DS    XL1                                                              
*                                                                               
INAMNT   DS    PL6                 INVOICE AMOUNT                               
OLDAMNT  DS    PL6                 OLD INVOICE AMOUNT                           
OLDBASE  DS    PL6                 OLD BASIS                                    
PASSCD   DS    CL1                 PASS CD TO CLIENT                            
ORDRNUM  DS    CL6                 ORDER NUMBER                                 
POSTAMT  DS    PL6                 AMOUNT POSTED                                
NORDR    DS    XL2                 NUMBER OF FIRST ORDER RECORD                 
*                                                                               
POSTACC  DS    CL15                AGY EXP KEY                                  
POSTACCN DS    CL36                AGY EXP NAME                                 
POSTCNTR DS    CL3                 COST CENTER CODE                             
POSTCPOS DS    XL1                 COST CENTER POSITION IN KEY                  
COSTANAL DS    CL5                                                              
*                                                                               
CRDSNUM  DS    CL15                2/8 ACCT KEY                                 
CRDSNAME DS    CL36                NAME                                         
COSTNUM  DS    CL15                1/C ACCT KEY                                 
COSTNAME DS    CL36                NAME                                         
CRCNUM   DS    CL15                1/P ACCT KEY                                 
CRCNAME  DS    CL36                NAME                                         
CR13NUM  DS    CL15                1/3 ACCT KEY                                 
CRPSNUM  DS    CL15                2/9 ACCT KEY                                 
CRPSNAME DS    CL36                NAME                                         
STAFFNUM DS    CL15                2/P STAFF ACCT KEY                           
STAFFNAM DS    CL36                STAFF NAME                                   
DEPNUM   DS    CL15                DEPT ACCT KEY                                
DEPNAME  DS    CL36                DEPT NAME                                    
DEPSTFN  DS    CL36                OFF,DEPT,STAFF NAME FOR DISP                 
*                                                                               
VATLNQ   EQU   (VTCPRVD-VTCEFFD)   LENGTH OF GST/PST  DATA                      
GSTDATA  DS    XL(VATLNQ)          GST  DATA                                    
PSTDATA  DS    XL(VATLNQ)          PST  DATA                                    
GSTPSTD  DS    XL(VATLNQ)          GST RATE + PST RATE                          
*                                                                               
ITMGTY   DS    CL1                 ITEM GST TYPE                                
ITMPTY   DS    CL1                      PST TYPE                                
ITMPRV   DS    CL2                      PROVINCE                                
ITMTXRA  DS    CL(L'CTXTXRA)                                                    
*                                                                               
DETVALNQ EQU   *-DETVALS                                                        
         EJECT                                                                  
*                                  TSAR EQUATES                                 
TSRADDQ  EQU   1                   ADD                                          
TSRGETQ  EQU   2                   GET                                          
TSRPUTQ  EQU   3                   PUT                                          
TSRSAVQ  EQU   4                   SAVE                                         
TSRDELQ  EQU   5                   DELETE                                       
TSRINTQ  EQU   6                   INITIALIZE                                   
*                                                                               
TSARBLK  DS    XL(TSARDL)          TSAR BLOCK                                   
         EJECT                                                                  
LDICT    DS    0X                                                               
LD@AMT   DS    CL6                 AMOUNT                                       
LD@CPJ   DS    CL22                CLIENT/PRODUCT JOB                           
LD@WKC   DS    CL10                WORKCODE                                     
LD@EXPA  DS    CL16                EXPENSE ACCOUNT                              
LD@DOF   DS    CL13                DEBIT OFFICE                                 
LD@COF   DS    CL13                CREDIT OFFICE                                
LD@AOF   DS    CL13                ANALYSIS OFFICE                              
LD@DPT   DS    CL12                DEPARTMENT                                   
LD@PER   DS    CL7                 PERSON                                       
LD@PFK   DS    CL2                 PF                                           
LD@DTL   DS    CL6                 DETAIL                                       
LD@MULT  DS    CL8                 MULTIPLE                                     
LD@UP    DS    CL2                 UP                                           
LD@DOWN  DS    CL4                 DOWN                                         
LD@DEL   DS    CL6                 DELETE                                       
LD@POST  DS    CL4                 POST                                         
LD@CLEAR DS    CL5                 CLEAR                                        
LD@FIS   DS    CL3                 FIS                                          
LD@TAX   DS    CL3                 TAX                                          
LD@INV   DS    CL7                 INVOICE                                      
LD@NAME  DS    CL4                 NAME                                         
LD@LEFT  DS    CL4                 LEFT                                         
LD@RIGHT DS    CL5                 RIGHT                                        
LD@RETRN DS    CL6                 RETURN                                       
         EJECT                                                                  
**********************************************************************          
* FOLLOWING DATA SAVED SPECIAL TSAR RECORD                           *          
**********************************************************************          
*                                  **UPPER SCREEN FIELDS LENGTH/DATA**          
SAVDATA  DS    0H                                                               
ORDNUML  DS    XL1                 LENGTH & ORDER NUMBER                        
ORDNUM   DS    CL(L'MISORD)                                                     
*                                                                               
DOCNUML  DS    XL1                 LENGTH & DOCUMENT NUMBER                     
DOCNUM   DS    CL(L'MISDOC)                                                     
*                                                                               
DOCDTEL  DS    XL1                 LENGTH & DOCUMENT DATE                       
DOCDTE   DS    CL(L'MISDAT)                                                     
*                                                                               
DUEDTEL  DS    XL1                 LENGTH & DUE DATE                            
DUEDTE   DS    CL(L'MISDUE)                                                     
*                                                                               
URGENTL  DS    XL1                 LENGTH & URGENT                              
URGENT   DS    CL(L'MISURG)                                                     
*                                                                               
CASHACL  DS    XL1                 LENGTH & CASH ACCOUNT                        
CASHAC   DS    CL(L'MISCSH)                                                     
*                                                                               
PRDVCDL  DS    XL1                 LENGTH & PROD. VENDOR CD                     
PRDVCD   DS    CL(L'MISVCD)                                                     
*                                                                               
EXPVCDL  DS    XL1                 LENGTH & EXPENSE VENDOR CD                   
EXPVCD   DS    CL(L'MISXCD)                                                     
*                                                                               
OVRIDEL  DS    XL1                 OVERRIDE                                     
OVRIDE   DS    CL(L'MISOVR)                                                     
*                                                                               
GORNL    DS    XL1                 GROSS OR NET                                 
GORN     DS    CL(L'CMIGON)                                                     
*                                                                               
GTYPL    DS    XL1                 GST TYPE                                     
GTYP     DS    CL(L'CMIGTY)                                                     
*                                                                               
GAML     DS    XL1                 GST AMOUNT                                   
GAM      DS    CL(L'CMIGAM)                                                     
*                                                                               
PRVL     DS    XL1                 PROVINCE                                     
PRV      DS    CL(L'CMIPRV)                                                     
*                                                                               
PTYPL    DS    XL1                 PST TYPE                                     
PTYP     DS    CL(L'CMIPTY)                                                     
*                                                                               
PAML     DS    XL1                 PST AMOUNT                                   
PAM      DS    CL(L'CMIPAM)                                                     
SAVDATAL EQU   *-SAVDATA                                                        
         EJECT                                                                  
**********************************************************************          
* FOLLOWING DATA SAVED IN OSSAVE AREA OF TWA                         *          
*  MAX IS 256 BYTES                                                  *          
**********************************************************************          
STS      DS    0X                  **SAVED TWA STORAGE**                        
NBUF     DS    XL2                 NUMBER IN BUFFER                             
NFST     DS    XL2                 NUMBER OF FIRST ON SCREEN                    
NLST     DS    XL2                 NUMBER OF LAST ON SCREEN                     
NGET     DS    XL2                 NUMBER OF THE ITEM TO GET                    
NREC     DS    XL2                 NUMBER OF ADDS                               
NITM     DS    XL1                 NUMBER OF ITEMS ON THE SCREEN                
NLINL    DS    XL1                 NUMBER OF LINES USED ON LIST SCREEN          
*                                                                               
RTRNSCR  DS    XL1                 RETURN SCREEN NUMBER                         
RTRNFST  DS    XL2                 RETURN NUMBER OF FIRST ON SCREEN             
*                                                                               
VPFSW    DS    XL1                 VALID PF KEYS                                
VPFDTL   EQU   X'80'                DETAIL                                      
VPFMUL   EQU   X'40'                MULTIPLE                                    
VPFSWAP  EQU   VPFDTL+VPFMUL        SWAP                                        
VPFUP    EQU   X'20'                UP                                          
VPFDWN   EQU   X'10'                DOWN                                        
VPFDEL   EQU   X'08'                DELETE                                      
VPFPOST  EQU   X'04'                POST                                        
VPFFIS   EQU   X'01'                FIS                                         
*                                                                               
VPFSW1   DS    XL1                 VALID PF KEYS -                              
VPFTAX   EQU   X'80'                TAX                                         
VPFINV   EQU   X'40'                INVOICE                                     
VPFLIS   EQU   X'20'                LIST                                        
VPFLFT   EQU   X'10'                LEFT                                        
VPFRHT   EQU   X'08'                RIGHT                                       
VPFRTN   EQU   X'04'                RERURN                                      
*                                                                               
TOTDLRS  DS    PL8                 TOTAL CASH                                   
TAXDLRS  DS    PL8                 TAX                                          
         ORG   TAXDLRS                                                          
GSTDLRS  DS    PL8                 GST                                          
PSTDLRS  DS    PL8                 PST                                          
GSTBASE  DS    PL8                 GST BASIS                                    
PSTBASE  DS    PL8                 PST BASIS                                    
*                                                                               
LASTORD  DS    CL6                 LAST ORDER #                                 
ORDRINV  DS    PL6                 AMOUNT INVOICED                              
ORDRAMT  DS    PL6                 AMOUNT OF ORDER                              
*                                                                               
NTAXL    DS    XL1                 NUMBER OF TAX LINES                          
*                                                                               
FLG      DS    XL1                 MISCELLANEOUS                                
AUTODUE  EQU   X'80'                AUTO DUE DATE LOOKUP                        
NOANLOFC EQU   X'40'                DON'T DISPLAY ANALYSIS OFFICE               
RTNLIST  EQU   X'20'                RETURN TO LIST ROUTINE                      
LASTDUP  EQU   X'10'                LAST INVOICE # WAS DUPLICATE                
*                                                                               
INVSCRN  DS    XL1                 SAVE INVOICE SCREEN                          
*                                                                               
SBATREF  DS    CL(L'LSTBBREF)      BATCH REFERENCE                              
*                                                                               
*                                                                               
STAFFL   DS    XL1                 LENGTH OF STAFF CODE                         
LEV2P    DS    XL1                 NUMBER OF LEVELS 1N 2P                       
LIST#    DS    XL1                                                              
*                                                                               
DUPINVK  DS    XL(L'INVPKEY)       KEY OF DUPLICATE INVOICE                     
*                                                                               
LSTGON   DS    CL1                 LAST GROSS/NET VALUE                         
LSTGTY   DS    CL1                 LAST GST TYPE                                
LSTPTY   DS    CL1                 LAST PST TYPE                                
LSTPRV   DS    CL2                 LAST PROVINCE                                
LSTGAM   DS    PL6                 LAST GST INPUT TAX                           
LSTPAM   DS    PL6                 LAST PST INPUT TAX                           
*                                                                               
GSTXA    DS    PL2                 GST ROUNDING ADJUSTMENT                      
GSTXN    DS    XL2                 GST ADJUSTMENT RECORD                        
PSTXA    DS    PL2                 PST ROUNDING ADJUSTMENT                      
PSTXN    DS    XL2                 PST ADJUSTMENT RECORD                        
*                                                                               
STSLNQ   EQU   *-STS                                                            
**** END SAVED TWA STUFF  *******************************************           
         EJECT                                                                  
**********************************************************************          
*  OTHER LOCAL STORAGE AREAS                                         *          
**********************************************************************          
         DS    0D                                                               
KEY      DS    CL49                IOAREA                                       
IOAREA   DS    XL4000                                                           
*                                                                               
         DS    0D                                                               
EXCBLK   DS    XL(EXCELNQ)         EXCEL BLOCK                                  
*                                                                               
         DS    0D                                                               
CATBLK   DS    XL(CATLNQ)          CATCALL BLOCK                                
*                                                                               
         DS    0D                                                               
RECIOA   DS    XL(RECLNQ2)         TSAR RECORD                                  
RECOLD   DS    XL(RECLNQ2)                                                      
*                                                                               
         DS    0D                                                               
TAXIT    DS    XL((TXLNQ*MXTXLQ)+4)                                             
*                                                                               
PROGDX   DS    0C                                                               
         EJECT                                                                  
**********************************************************************          
* DSECT TO TSAR RECOR                                                *          
**********************************************************************          
RECSD    DSECT                                                                  
RECLEN   DS    XL2                 LENGTH                                       
RECKEY   DS    0XL2                KEY                                          
RECNUM   DS    XL2                 RECORD NUMBER                                
         ORG   RECNUM                                                           
RECSR    DS    XL1                 SPECIAL RECORD FLAG                          
RECSRQ   EQU   X'40'                                                            
         DS    XL1                                                              
*                                                                               
RECSTA   DS    XL1                 STATUS(SEE ITMSTA)                           
RECCLI   DS    CL(L'MISCLI)        CLIENT                                       
RECPRO   DS    CL(L'MISPRO)        PRODUCT                                      
RECJOB   DS    CL(L'MISJOB)        JOB                                          
RECWKC   DS    CL(L'MISWKC)        WORKCODE                                     
RECAMT   DS    CL(L'MISAMT)        AMOUNT                                       
RECXAC   DS    CL(L'MISEXA)        EXPENSE ACCOUNT                              
RECDOF   DS    CL(L'MISDOF)        DEBIT OFFICE                                 
RECCOF   DS    CL(L'MISCOF)        CREDIT OFFICE                                
RECAOF   DS    CL(L'MISAOF)        ANALYSIS OFFICE                              
RECDPT   DS    CL(L'MISDPT)        DEPARTMENT                                   
RECPER   DS    CL(L'MISPER)        PERSON                                       
*                                                                               
RECCPJ   DS    CL(L'USECPJ)        CLI/PRO/JOB                                  
         ORG   RECCPJ                                                           
RECCPJC  DS    CL(L'MISCLI)        CLI                                          
RECCPJP  DS    CL(L'MISPRO)        PRO                                          
RECCPJJ  DS    CL(L'MISJOB)        JOB                                          
         DS    CL1                                                              
RECCPJW  DS    CL(L'MISWKC)        WORKCODE                                     
         ORG   RECCPJ+L'RECCPJ                                                  
RECBAS   DS    CL(L'USEBAS)        BASIS                                        
*                                                                               
RECUSTX  DS    0X                  US TAX INFO                                  
RECLOCH  DS    XL8                 TAX FIELD HEADER                             
RECLOC   DS    CL(L'USELOC)        TAX(LOCALITY FIELD)                          
RECLOCT  DS    XL8                 TAX TRAILER                                  
RECLOCN  DS    XL1                 NUMBER OF TAX ITEMS                          
*                                                                               
RECTAX   DS    CL(L'USETAX)        TAX                                          
RECTXWC  DS    CL(L'USETXWC)       TAX W/C                                      
*                                                                               
RECCTAX  DS    0X                  CANADIAN TAX INFO                            
RECGTO   DS    CL(L'CTXGTO)        GST TYPE OVERRIDE                            
RECPTO   DS    CL(L'CTXPTO)        PST TYPE OVERRIDE                            
RECPVO   DS    CL(L'CTXPVO)        PROVINCE OVERRIDE                            
*                                                                               
RECOFF   DS    CL2                 OFFICE                                       
RECGAM   DS    CL(L'CMIGAM)        GST AMOUNT                                   
RECPAM   DS    CL(L'CMIPAM)        PST AMOUNT                                   
RECTXRA  DS    CL(L'CTXTXRA)       TX RATE & AMOUNT                             
*                                                                               
RECGSTR  DS    XL(L'VTCRATE)       GST RATE                                     
RECPSTR  DS    XL(L'VTCRATE)       PST DATA                                     
*                                                                               
RECLNQ   EQU   *-RECSD                                                          
RECNAR1  DS    CL(L'MISNAR)        NARRATIVE                                    
RECLNQ1  EQU   *-RECSD                                                          
RECNAR2  DS    CL(L'MISNAR2)       LINE 2 OF NARRATIVE                          
RECLNQ2  EQU   *-RECSD                                                          
         ORG   RECLEN                                                           
*                                                                               
NARLN    DS    XL2                  NARRATIVE RECORD - LENGTH                   
NARNUM   DS    XL2                                   - RECORD NUMBER            
NARNUMQ  EQU   1                    NARRATIVE RECORD                            
NARNAR1  DS    CL(L'MISNAR)                                                     
NARNAR2  DS    CL(L'MISNAR2)                                                    
NARLNQ   EQU   *-NARLN                                                          
         ORG   RECLEN                                                           
*                                                                               
SAVLN    DS    XL2                 SAVDATA - LENGTH                             
SAVNUM   DS    XL2                         - RECORD NUMBER                      
SAVNUMQ  EQU   2                                                                
SAVREC   DS    CL(SAVDATAL)                                                     
         ORG   RECLEN+RECLNQ2                                                   
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER TAX ENTRY                                           *          
**********************************************************************          
TXD      DSECT                                                                  
TXACC    DS    CL15                CREDIT ACCOUNT                               
TXACCN   DS    CL36                CREDIT ACCOUNT NAME                          
TXLOCK   DS    CL(L'SUTLOC)        LOCALITY -KEY                                
         ORG   TXLOCK+3                                                         
TXLOC    DS    CL8                 LOCALITY CODE                                
         ORG   TXLOCK+L'TXLOCK                                                  
TXLOCN   DS    CL36                NAME                                         
TXEFF    DS    XL3                 EFFECTIVE DATE                               
TXRTE    DS    PL4                 PERCENT                                      
TXBAS    DS    PL6                 BASIS                                        
TXTAX    DS    PL6                 TAX                                          
TXWKC    DS    CL2                 WORKCODE                                     
TXLNQ    EQU   *-TXD                                                            
                                                                                
                                                                                
**********************************************************************          
* DSECT TO COVER PFKEY TABLE                                         *          
**********************************************************************          
PFKTD    DSECT                                                                  
PFKTVBY  DS    XL1                 VALIDATION BYTE                              
PFKTVBY0 EQU   0                    DEFAULT (VPFSW)                             
PFKTVBY1 EQU   1                    SECOND (VPFSW1)                             
PFKTVBI  DS    XL1                 VALIDATION BIT                               
PFKTNUM  DS    XL1                 PFKEY NUMBER                                 
PFKTROU  DS    AL2                 ROUTINE                                      
PFKTDLN  DS    XL1                 LENGTH OF DICTIONARY WORD                    
PFKTDIC  DS    XL2                 DICTIONARY                                   
PFKTLNQ  EQU   *-PFKTD                                                          
                                                                                
**********************************************************************          
* DSECT TO COVER PFKEY MASK                                          *          
**********************************************************************          
PFKMD    DSECT                                                                  
PFKMSCTY DS    XL1                 SCREEN TYPE                                  
PFKMCREM DS    AL2                 CONTROLLER PF KEYS TO REMOVE                 
PFKMCSET DS    AL2                 CONTROLLER PF KEYS TO SET                    
PFKMSET  DS    XL1                 APPLICATION PF KEYS TO SET                   
PFKMSET1 DS    XL1                 APPLICATION PF KEYS TO SET -1                
PFKMLNQ  EQU   *-PFKMD                                                          
         EJECT                                                                  
**********************************************************************          
* DSECT TO COVER LIST LINES                                          *          
**********************************************************************          
LISD     DSECT                                                                  
LISITM   DS    CL3                 ITEM NUMBER                                  
         DS    CL1                                                              
LISAMT   DS    CL(L'RECAMT)        AMOUNT                                       
         DS    CL1                                                              
LISCPJ   DS    CL34                CLI/PROD/JOB                                 
         DS    CL2                                                              
LISWRK   DS    CL20                WORKCODE                                     
         ORG   LISITM+L'LISLIN1                                                 
*                                                                               
         ORG   LISCPJ                                                           
LISEXP   DS    CL26                EXPENSE                                      
         DS    CL2                                                              
LISDOF   DS    CL13                DEBIT OFFICE                                 
         DS    CL2                                                              
LISCOF   DS    CL13                CREDIT OFFICE                                
         ORG   LISITM+L'LISLIN1                                                 
*                                                                               
         ORG   LISCPJ                                                           
LISAOF   DS    CL13                ANALYSIS OFFICE                              
         DS    CL2                                                              
LISDPT   DS    CL19                DEPARTMNET                                   
         DS    CL2                                                              
LISPER   DS    CL19                PERSON                                       
*                                                                               
         ORG   LISCPJ                                                           
LISTAX   DS    CL56                ANALYSIS OFFICE                              
         ORG   LISITM+L'LISLIN1                                                 
                                                                                
**********************************************************************          
* DSECT TO COVER LIST COLUMN TABLE                                   *          
**********************************************************************          
COLD     DSECT                                                                  
COLEQU   DS    AL1                 COLUMN EQUATE NUMBER                         
COLWDTH  DS    AL1                 COLUMN WIDTH                                 
COLROUT  DS    AL2                 DISPLACEMENT ROUTINE                         
COLDLEN  DS    XL1                 LENGTH OF DICTIONARY FIELD                   
COLDICT  DS    XL2                 DICTIONARY FIELD                             
COLNQ    EQU   *-COLD                                                           
                                                                                
**********************************************************************          
* DSECT TO CANADIAN TAX DATA                                         *          
**********************************************************************          
CTD      DSECT                                                                  
CTAGTY   DS    CL1                 ACCOUNT LEVEL GST TYPE                       
CTAPTY   DS    CL1                               PST TYPE                       
CTAPRV   DS    CL2                               PROVINCE                       
CTUGTY   DS    CL1                 UPPER SCREEN  GST TYPE                       
CTUPTY   DS    CL1                               PST TYPE                       
CTUPRV   DS    CL2                               PROVINCE                       
CTLNQ    EQU   *-CTD                                                            
         EJECT                                                                  
         PRINT ON                                                               
* ACBATDSECT                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACBATDSECT                                                     
         PRINT ON                                                               
         EJECT                                                                  
         ORG   TWAAIND1                                                         
PROCSW   DS    XL1                 PROCESS CONTROL                              
PROCCLR  EQU   X'80'                CLEAR SAVED STORAGE AFTER INIT              
PROCNAR  EQU   X'40'                TSAR NARRATIVE RECORD ADDED                 
PROCDTL  EQU   X'20'                PROCESS DETAIL SCREEN                       
PROCEDT  EQU   X'10'                EDIT IN PROGRESS                            
PROCLST  EQU   X'08'                LIST CONTROLLER                             
PROCTOP  EQU   X'04'                LIST TOP ALREADY SET                        
         ORG   TWAAIND2                                                         
FRSTSCRN DS    XL1                 SAVE FIRST SCREEN                            
         ORG   BASOLY2H                                                         
*                                                                               
SCRNLEN  EQU   (OSSAVE-1)-TWAD                                                  
* ACBATF8D  - DSECT TO COVER MULTIPLE SCREEN - US                               
         PRINT OFF                                                              
       ++INCLUDE ACBATF8D                                                       
         PRINT ON                                                               
         ORG   MISAMTH                                                          
MISDTL   DS    0X                                                               
         ORG   MISWORK                                                          
                                                                                
* ACBATF5D  - DSECT TO COVER SINGLE SCREEN  - US                                
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATF5D                                                       
         PRINT ON                                                               
         ORG   SISAMTH                                                          
SISDTL   DS    0X                                                               
         ORG   SISWORK                                                          
                                                                                
* ACBATB0D  - DSECT TO COVER TAX  SCREEN  - US                                  
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATB0D                                                       
         PRINT ON                                                               
         ORG   USEBASH                                                          
USEDTL   DS    0X                                                               
         ORG   USEWORK                                                          
                                                                                
* ACBATFBD  - DSECT TO COVER MULTIPLE SCREEN - CANADIAN                         
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATFBD                                                       
         PRINT ON                                                               
         ORG   CMIAMTH                                                          
CMIDTL   DS    0X                                                               
         ORG   CMIWORK                                                          
                                                                                
* ACBATF3D  - DSECT TO COVER SINGLE SCREEN - CANADIAN                           
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATF3D                                                       
         PRINT ON                                                               
         ORG   CSIAMTH                                                          
CSIDTL   DS    0X                                                               
         ORG   CSIWORK                                                          
                                                                                
* ACBATB1D  - DSECT TO COVER TAX SCREEN - CANADIAN                              
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATB1D                                                       
         PRINT ON                                                               
         ORG   CTXBASH                                                          
CTXDTL   DS    0X                                                               
         ORG   CTXWORK                                                          
                                                                                
* ACBATB2D  - DSECT TO COVER NAME - SCREEN                                      
         ORG   BASOLY2H                                                         
         PRINT OFF                                                              
       ++INCLUDE ACBATB2D                                                       
         PRINT ON                                                               
         ORG   LISSEL1H                                                         
LISDTL   DS    0X                                                               
                                                                                
* ACCATCALLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE ACCATCALLD                                                     
         PRINT ON                                                               
* ACDDEQUS                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACDDEQUS                                                       
         PRINT ON                                                               
* ACEXCELD                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACEXCELD                                                       
         PRINT ON                                                               
* ACGENDAY                                                                      
         PRINT OFF                                                              
       ++INCLUDE ACGENDAY                                                       
         PRINT ON                                                               
* DDCOMFACS                                                                     
         PRINT OFF                                                              
       ++INCLUDE DDCOMFACS                                                      
         PRINT ON                                                               
CONBLKD  DSECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE DDCONBLK                                                       
         PRINT ON                                                               
* DDCUREDITD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDCUREDITD                                                     
         PRINT ON                                                               
* DDDSCANBLKD                                                                   
         PRINT OFF                                                              
       ++INCLUDE DDSCANBLKD                                                     
         PRINT ON                                                               
* DDFLDHDR                                                                      
         PRINT OFF                                                              
       ++INCLUDE DDFLDHDR                                                       
         PRINT ON                                                               
*                                  DISPLACEMENTS TO FIELD HEADER BYTES          
FLDLEND  EQU   FLDLEN-FLDHDRD      FIELD LENGTH                                 
FLDATBD  EQU   FLDATB-FLDHDRD      STANDARD ATTRIBUTES                          
FLDIINDD EQU   FLDIIND-FLDHDRD     INPUT INDICATORS                             
FLDILEND EQU   FLDILEN-FLDHDRD     INPUT DATA LENGTH                            
FLDOINDD EQU   FLDOIND-FLDHDRD     OUTPUT INDICATORS                            
FLDOLEND EQU   FLDOLEN-FLDHDRD     OUTPUT DATA LENGTH                           
FLDHDRL  EQU   FLDDATA-FLDHDRD     FIELD DATA                                   
*                                                                               
* DDGLOBEQUS                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLOBEQUS                                                     
         PRINT ON                                                               
* DDGLVXCTLD                                                                    
         PRINT OFF                                                              
       ++INCLUDE DDGLVXCTLD                                                     
         PRINT ON                                                               
* FASECRETD                                                                     
         PRINT OFF                                                              
       ++INCLUDE FASECRETD                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'048ACBAT0A   03/19/20'                                      
         END                                                                    
