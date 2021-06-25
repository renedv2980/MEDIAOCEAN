*          DATA SET SPREPJV02  AT LEVEL 012 AS OF 07/16/07                      
*PHASE SPJV02A                                                                  
                                                                                
*==================================================================             
* QOPT1       = Y TO PRINT DBLOCK TRACE                                         
* Q2USER+2(1) = WEEKLY POSTING FLAG (GOT USED IN SPMEDGETBY)                    
* Q2USER+4(3) = REQUESTED LINE NUMBER (COL 25)                                  
*==================================================================             
                                                                                
SPJV02   TITLE 'SPOT DEMO LOOKUP TRACE'                                         
SPJV02   CSECT                                                                  
         DS    4000C                                                            
         ORG   SPJV02                                                           
         PRINT NOGEN                                                            
         NMOD1 0,**SPJV02                                                       
*                                                                               
         L     RC,=A(SPJVWORK)                                                  
         USING SPJVWORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
*                                                                               
         CLI   MODE,STAFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,STAF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,ESTFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,ESTF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,CLTFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,CLTF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,RUNF                                                          
         J     EXIT                                                             
*                                                                               
EXIT     XIT1  ,                                                                
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* RUNFRST FIRST PROCESSING                                            *         
*=====================================================================*         
RUNF     NTR1  BASE=*,LABEL=*                                                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* CLIENT  FIRST PROCESSING                                            *         
*=====================================================================*         
                                                                                
CLTF     NTR1  BASE=*,LABEL=*                                                   
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* ESTIMATE FIRST PROCESSING                                           *         
*=====================================================================*         
                                                                                
ESTF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LHI   R0,1                                                             
         STCM  R0,15,MEDNUMPE                                                   
         LHI   R0,256                                                           
         STCM  R0,15,MEDLCHNK                                                   
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         MVC   MEDNUMWK,=F'56'                                                  
         MVI   MEDEXTDM,4                                                       
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
*                                                                               
         L     RE,=A(MYHDHK)                                                    
         ST    RE,HEADHOOK                                                      
         L     RF,=A(HDHKR9)                                                    
         STM   R9,RC,0(RF)                                                      
*                                                                               
         L     RE,=A(MYMIDHK)                                                   
         ST    RE,MIDHOOK                                                       
         L     RF,=A(MIDHKR9)                                                   
         STM   R9,RC,0(RF)                                                      
*                                                                               
         L     RE,=A(SPOTHK)                                                    
         ST    RE,SPOTHOOK                                                      
         L     RF,=A(SPOTHKR9)                                                  
         STM   R9,RC,0(RF)                                                      
*                                                                               
         GOTOR MEDPRDRD,DMCB,SPWORKD                                            
*                                                                               
         SR    R2,R2                                                            
         IC    R2,BPRD             PRODUCT NUMBER                               
         CLI   BPRD,X'FF'                                                       
         BNE   *+8                                                              
         LHI   R2,220                                                           
         BCTR  R2,0                                                             
         MH    R2,PRDBUFLN                                                      
         A     R2,PRDBUFF          POINT TO PRDBUFF ENTRY                       
         USING PTBUFFD,R2                                                       
         ST    R2,SVPRDBUF                                                      
* COUNT NUMBER OF REAL DEMOS                                                    
         SR    R0,R0                                                            
         LA    R1,PTDEMLST                                                      
         LHI   RF,4                                                             
*                                                                               
ESTF20   OC    0(3,R1),0(R1)                                                    
         BZ    ESTF22                                                           
         LA    R1,3(R1)                                                         
         AHI   R0,1                                                             
         BCT   RF,ESTF20                                                        
*                                                                               
ESTF22   STH   R0,SVNUMDEM         SAVE ACTUAL NUMBER OF DEMOS                  
*                                                                               
* GET AND SAVE DEMO NAMES                                                       
*                                                                               
         MVC   SVDEMNMS,SPACES                                                  
         L     RE,ADBLOCK                                                       
         USING DBLOCKD,RE                                                       
         MVC   DBSELMED,QMED       SET MEDIA CODE IN DBLOCK                     
         DROP  RE                                                               
* GET 6 CHARACTER DEMO NAMES                                                    
         L     R4,ADEST                                                         
         USING ESTHDRD,R4                                                       
*                                                                               
         MVC   SVDEMNMS,SPACES                                                  
         LH    R0,SVNUMDEM         UP TO 4 DEMOS                                
*                                                                               
         GOTO1 DEMOCON,DMCB,((R0),PTDEMLST),(6,SVDEMNMS),              X        
               (C'S',ADBLOCK),(SPOTPROF+9,EUSRNMS)                              
*                                                                               
         DROP  R2,R4                                                            
         EJECT                                                                  
         CLC   QBOOK1,SPACES                                                    
         BNE   *+8                                                              
         MVI   QRERATE,C' '                                                     
*                                                                               
         LHI   R0,2                SET EST ADJ                                  
         CLI   QRERATE,C' '                                                     
         BE    ESTF30                                                           
*                                                                               
         LHI   R0,3                SET FOR PURCHASED RERATED                    
         CLC   =C'NO',QHUT1                                                     
         BE    *+8                                                              
         AHI   R0,1                SET FOR ADJUSTMENT                           
         CLI   QRERATE,C'I'        RERATE BASED ON INVOICE                      
         BNE   *+8                                                              
         AHI   R0,3                                                             
                                                                                
ESTF30   ST    R0,SVRERATE                                                      
*                                                                               
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* MARKET FIRST                                                        *         
*=====================================================================*         
                                                                                
MKTF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
MKTFX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* STAFRST                                                             *         
* READ THE BUY AND GET THE DEMOS                                                
*=====================================================================*         
                                                                                
STAF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVI   FORCEHED,C'Y'                                                    
         MVI   STASW,C'N'          SET TO PRINT BUY DESC                        
*                                                                               
         MVC   KEY,SVBUYKEY                                                     
         PACK  DUB,Q2USER+4(3)     GET REQUESTED LINE NUMBER                    
         CVB   R0,DUB                                                           
         LA    RE,KEY+11                                                        
         CLI   KEY+10,X'FF'                                                     
         LA    RE,KEY+12                                                        
         STC   R0,0(RE)                                                         
*                                                                               
         GOTO1 HIGH                READ REQUESTED BUY                           
         CLC   KEY(12),KEYSAVE                                                  
         BNE   NOBUYERR                                                         
*                                                                               
         GOTO1 GETBUY                                                           
*                                                                               
STAF20   BRAS  RE,PROCB            PROCESS THE BUY RECORD                       
         J     EXIT                                                             
*                                                                               
NOBUYERR MVC   P3(37),=C'** ERROR ** UNABLE TO FIND BUY RECORD'                 
         B     BUYERRX                                                          
*                                                                               
BUYERRX  MVC   P(80),QAREA                                                      
         MVC   P2(80),QAREA2                                                    
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ                                                          
         LTORG                                                                  
         EJECT                                                                  
*===================================================================            
* CALL MEDGETBY ONCE FOR EACH SPOT IN BUYREC                                    
* IN SPOTHOOK WILL REJECT ALL SPOTS BUT ONE EACH TIME                           
*===================================================================            
                                                                                
PROCB    NTR1  BASE=*,LABEL=*                                                   
         XC    BUYTOTS,BUYTOTS                                                  
*                                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         MVC   MEDBRAND,BPRD       SET REQUESTED PRODUCT                        
         MVI   MEDSPTLN,0          AND SUPPRESS SLN FILTER                      
*                                                                               
         L     R8,ADBUY                                                         
         USING BUYRECD,R8                                                       
*                                                                               
         LA    R6,BDELEM                                                        
         MVI   ELCDLO,X'0B'                                                     
         MVI   ELCDHI,X'0C'                                                     
         CLI   BUYKEY+3,X'FF'                                                   
         BE    PROCB10                                                          
         MVI   ELCDLO,6                                                         
         MVI   ELCDHI,7                                                         
*                                                                               
PROCB10  BRAS  RE,NEXTEL                                                        
         BNE   PROCB100                                                         
*                                                                               
         TM    6(R6),X'C0'         TEST MINUS OR MINUSSED                       
         BNZ   PROCB10                                                          
*                                                                               
         CLI   BUYKEY+3,X'FF'                                                   
         BNE   PROCB20                                                          
*                                                                               
         CLI   1(R6),10            TEST ALLOCATED                               
         BNH   PROCB10                                                          
*                                                                               
         CLI   BPRD,X'FF'          TEST POL REQUEST                             
         BE    PROCB20                                                          
*                                                                               
         CLC   10(1,R6),BPRD       TEST ALLOCATED TO REQ PRD                    
         BE    PROCB20                                                          
*                                                                               
         CLI   1(R6),18            TEST PIGGYBACK                               
         BL    PROCB10                                                          
         CLC   14(1,R6),BPRD       TEST OUR PRODUCT IS SECOND                   
         BNE   PROCB10                                                          
                                                                                
PROCB20  ST    R6,SVELADDR                                                      
*                                                                               
         GOTO1 MEDGETBY,DMCB,(RA),SVRERATE                                      
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)            POINT TO PERIOD TOTALS                       
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    PROCB40                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,LINE                                                          
         AHI   R0,2                                                             
         CLM   R0,1,MAXLINES       WILL IT FIT                                  
         BL    *+8                                                              
         MVI   STASW,C'N'          FORCE BUY DESC DATA TO PRINT                 
         BAS   RE,FMTBUY                                                        
         MVI   STASW,C'Y'          SET BUY DESC PRINTED                         
*                                                                               
PROCB22  LA    R5,MEDBY1                                                        
         LA    R6,PDEM1                                                         
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
PROCB24  ICM   R1,15,0(R5)         DEMO                                         
         BZ    PROCB26                                                          
         M     R0,=F'2'            X 2                                          
         D     R0,MEDBYSPT         DIVIDE TOTAL DEMO BY SPOTS                   
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,DEMO                                                          
         MVC   DEMOTYPE,1(R2)                                                   
*                                                                               
         MVI   BYTE,C' '           SET OVRD CHAR TO ' '                         
*                                                                               
         BRAS  RE,FMTDEM                                                        
         MVC   0(6,R6),PRTDEMO                                                  
*                                                                               
PROCB26  AHI   R5,8                NEXT DEMO                                    
         AHI   R6,PDEM2-PDEM1      NEXT PRINT POSN                              
         AHI   R2,3                NEXT 3-BYTE DEMO                             
         LA    R0,MEDBY5                                                        
         CR    R5,R0                                                            
         BL    PROCB24                                                          
*                                                                               
         L     RE,MEDADEMO         GET MEDGETBY DEMO ELEMENT                    
         L     R6,4(RE)                                                         
         USING NDELEM,R6                                                        
         CLI   MEDSPILL,C'Y'       OK IF SPILL                                  
         BNE   PROCB28                                                          
         MVC   PDEMPRG(10),NDPROG+5   SPILL PROGRAM NAME                        
         B     PROCB30                                                          
*                                                                               
PROCB28  MVC   PDEMPRG,NDPROG    MOVE IN REGULAR PROG NAME                      
         DROP  R6                                                               
*                                                                               
PROCB30  GOTO1 REPORT                                                           
*                                                                               
         CLI   QOPT1,C'Y'          TEST PRINT DBLOCK                            
         BNE   PROCB40                                                          
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         L     R0,VDBLOCK                                                       
         GOTO1 PRNTBL,DMCB,=C'DBLOCK',(R0),C'DUMP',256,=C'1D'                   
         DROP  RF                                                               
                                                                                
* ADD SPOTS AND DEMOS TO BUYLINE TOTALS                                         
                                                                                
PROCB40  L     R0,MEDBYSPT                                                      
         A     R0,BUYSPOTS                                                      
         ST    R0,BUYSPOTS                                                      
*                                                                               
         LA    R5,MEDBY1                                                        
         LA    RE,BUYDEM1                                                       
         LHI   RF,4                                                             
*                                                                               
PROCB42  L     R0,0(R5)                                                         
         A     R0,0(RE)                                                         
         ST    R0,0(RE)                                                         
         AHI   R5,8                                                             
         AHI   RE,4                                                             
         BCT   RF,PROCB42                                                       
*                                                                               
PROCB50  L     R6,SVELADDR                                                      
         B     PROCB10                                                          
         DROP  R4                                                               
         EJECT                                                                  
*============================================================                   
* PRINT BUY TOTALS                                                              
*============================================================                   
                                                                                
PROCB100 OC    BUYSPOTS,BUYSPOTS                                                
         BZ    PROCB110                                                         
*                                                                               
         MVI   TOTFLAG,C'T'                                                     
         MVC   PSPOTDT(17),=C'AVERAGE BUY DEMOS'                                
         LA    R5,BUYDEM1                                                       
         LA    R6,PDEM1                                                         
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
PROCB102 ICM   R1,15,0(R5)         DEMO                                         
         BZ    PROCB104                                                         
         M     R0,=F'2'            X 2                                          
         D     R0,BUYSPOTS         DIVIDE TOTAL DEMO BY SPOTS                   
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,DEMO                                                          
         MVC   DEMOTYPE,1(R2)                                                   
*                                                                               
         BRAS  RE,FMTDEM                                                        
         MVC   0(6,R6),PRTDEMO                                                  
*                                                                               
PROCB104 AHI   R5,4                NEXT DEMO VALUE                              
         AHI   R6,PDEM2-PDEM1      NEXT PRINT POSITION                          
         AHI   R2,3                NEXT 3-BYTE DEMO                             
         LA    R0,BUYDEMX                                                       
         CR    R5,R0                                                            
         BL    PROCB102                                                         
         GOTO1 REPORT                                                           
*                                                                               
PROCB110 J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* FORMAT BUY DESC DATA                                                          
*==============================================================                 
                                                                                
FMTBUY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   STASW,C'Y'          TEST PRINT BUY DESC                          
         BE    FMTB4               NO                                           
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BUYKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PBUYLIN(3),DUB                                                   
         MVI   PBUYLIN+3,C'-'                                                   
*                                                                               
         IC    R0,BUYKEY+10                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PBUYLIN+4(3),DUB                                                 
*                                                                               
         LA    R0,7                                                             
         LA    R1,PDAYS                                                         
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,BDDAY                                                         
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
FMTB2    LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,FMTB2                                                         
*                                                                               
         MVC   WORK(11),SPACES                                                  
         GOTO1 UNTIME,DMCB,BDTIMST,WORK,0                                       
         MVC   PTIME(11),WORK                                                   
*                                                                               
         MVC   PPROG,BDPROGRM                                                   
*                                                                               
         MVC   PDPT,BDDAYPT        1 CHAR DAYPART                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PSLN(2),DUB                                                      
         CHI   R0,99                                                            
         BNH   *+10                                                             
         UNPK  PSLN(3),DUB                                                      
*                                                                               
         L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         L     R4,VDBLOCK                                                       
         USING DBLOCKD,R4                                                       
         MVC   PBKTYPE,DBBTYPE                                                  
         DROP  RF                                                               
*                                                                               
FMTB4    L     R6,SVELADDR                                                      
         GOTO1 DATCON,DMCB,(2,2(R6)),(4,PSPOTDT)                                
*                                                                               
         CLI   QRERATE,C'I'        TEST AFFID RERATE                            
         BNE   FMTB6                                                            
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),X'10'         TEST AFFID FOLLOWS                           
         BNE   FMTB6                                                            
         GOTO1 DATCON,DMCB,(2,2(R6)),(4,PAFFDT)                                 
         MVI   PAFFDT+5,C'-'                                                    
*                                                                               
         XC    WORK,WORK                                                        
         MVC   WORK(2),4(R6)       MOVE TIME                                    
         NI    WORK,X'0F'          DROP STATUS BITS FROM TIME                   
         GOTO1 UNTIME,DMCB,WORK,WORK+8,0                                        
         MVC   PAFFTM(5),WORK+8                                                 
*                                                                               
FMTB6    L     RF,ADCONLST                                                      
         USING SPADCONS,RF                                                      
         L     R4,VDBLOCK                                                       
         USING DBLOCKD,R4                                                       
         DROP  RF                                                               
*                                                                               
         MVC   PBOOK+1(1),DBSELSRC                                              
         MVC   PBOOK(1),DBSELMED                                                
*                                                                               
         CLC   DBACTBK(2),=X'5002'  TEST NO BOOK                                
         BNE   FMTB8                                                            
         MVC   PBOOK+2(7),=C'*BUYER*'                                           
         B     FMTBX                                                            
*                                                                               
FMTB8    CLI   DBSELMED,C'W'       TEST WEEKLY                                  
         BE    FMTB10                                                           
         CLI   DBSELMED,C'O'       TEST OVERNITE                                
         BE    FMTB10                                                           
* BOOK MUST BE MONTH/YEAR                                                       
         SR    RF,RF                                                            
         IC    RF,DBACTBK+1        GET MONTH                                    
         BCTR  RF,0                                                             
         MHI   RF,3                                                             
         LA    RF,MONTHS(RF)                                                    
         MVC   PBOOK+3(3),0(RF)                                                 
         B     FMTB12                                                           
*                                                                               
FMTB10   SR    R0,R0                                                            
         IC    R0,DBACTBK+1                                                     
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PBOOK+4(2),DUB                                                   
*                                                                               
FMTB12   MVI   PBOOK+6,C'/'                                                     
         SR    R0,R0               FORMAT YEAR                                  
         IC    R0,DBACTBK                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  PBOOK+7(2),DUB                                                   
         B     FMTBX                                                            
*                                                                               
FMTBX    J     EXIT                                                             
         DROP  R4                                                               
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* SPOT HOOK - SET TO IGNORE ALL SPOTS BUT ONE                                   
*==============================================================                 
                                                                                
SPOTHK   NTR1  BASE=*,LABEL=*                                                   
         LM    R9,RA,SPOTHKR9                                                   
         L     RC,SPOTHKRC                                                      
         USING SPJVWORK,RC                                                      
*                                                                               
         MVI   SPOTYORN,C'N'                                                    
         CLC   SVELADDR,SPOTADDR   IS THIS THE RIGHT REGEL                      
         BNE   *+8                                                              
         MVI   SPOTYORN,C'Y'                                                    
         J     EXIT                                                             
*                                                                               
SPOTHKR9 DS    A                                                                
SPOTHKRA DS    A                                                                
SPOTHKRB DS    A                                                                
SPOTHKRC DS    A                                                                
         EJECT                                                                  
*===========================================================                    
* FORMAT DEMO VALUES FOR PRINTING                                               
* INPUT FIELDS ARE DEMO(4),DEMOTYPE(1)                                          
* OUTPUT FIELDS IS PRTDEMO(6)                                                   
*===========================================================                    
         SPACE 1                                                                
FMTDEM   NTR1  BASE=*,LABEL=*                                                   
         MVC   PRTDEMO,SPACES                                                   
*                                                                               
         ICM   R1,15,DEMO                                                       
         BZ    FMTDEMX                                                          
*                                                                               
         CLI   DEMOTYPE,C'R'       TEST RATING                                  
         BE    *+12                                                             
         CLI   DEMOTYPE,C'E'                                                    
         BNE   FMTDEM2                                                          
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BZ    FMTDEM2                                                          
         CLI   TOTFLAG,C'T'        TEST DOING TOTALS                            
         BNE   FMTDEM10                                                         
*                                                                               
         M     R0,=F'2'            DROP 1 DEC PLACE IF 2-DEC ACTIVE             
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
FMTDEM2  C     R1,=F'9999'         MAX WITH DEC IS 999.9                        
         BNH   FMTDEM4                                                          
* DEMO VALUE MORE THAN 4 DIGITS SO DROP 1 DECIMAL PLACE                         
         M     R0,=F'2'            X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PRTDEMO),0                                               
         B     FMTDEMX                                                          
*                                                                               
FMTDEM4  EDIT  (R1),(6,PRTDEMO),1                                               
         B     FMTDEMX                                                          
*                                                                               
FMTDEM10 EDIT  (R1),(6,PRTDEMO),2                                               
*                                                                               
FMTDEMX  J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* FOR DEMO AT 0(R2), TEST VALUE IN DEMO ELEM IS OVRD                            
* IF IT IS, MOVE '*' TO BYTE                                                    
* THIS CODE NEEDS FIXING IF REPORTING SPILL ! !!!!!!!!!!!!!!!!!!!!!             
*=====================================================================*         
         SPACE 1                                                                
FLGOVRD  NTR1                                                                   
         L     R6,ADBUY                                                         
         LA    R6,24(R6)                                                        
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         AR    R6,R0                                                            
         CLI   0(R6),2                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         IC    R0,1(R6)                                                         
         AHI   R0,-24                                                           
         SRL   R0,3                                                             
         LA    R1,24(R6)                                                        
*                                                                               
FLGOVRD2 CLC   0(3,R2),0(R1)       MATCH DEMO                                   
         BE    FLGOVRD4                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,FLGOVRD2                                                      
         DC    H'0'                                                             
*                                                                               
FLGOVRD4 TM    4(R1),X'80'         TEST OVRD                                    
         BZ    *+8                                                              
         MVI   BYTE,C'*'                                                        
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
NEXTEL   CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         SR    R0,R0                                                            
         IC    R0,1(R6)                                                         
         LTR   R0,R0                                                            
         JNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
NEXTEL2  CLI   0(R6),0                                                          
         JE    NEXTELX                                                          
         CLC   ELCDLO,0(R6)                                                     
         JH    NEXTEL                                                           
         CLC   ELCDHI,0(R6)                                                     
         JL    NEXTEL                                                           
         CR    RB,RB                                                            
         J     *+6                                                              
NEXTELX  LTR   RB,RB                                                            
         BR    RE                                                               
         EJECT                                                                  
         DROP  RB,RC                                                            
         DS    0D                                                               
MYHDHK   NTR1  BASE=*,LABEL=*                                                   
         LM    R9,RA,HDHKR9                                                     
         L     RC,HDHKRC                                                        
         USING SPJVWORK,RC                                                      
*                                                                               
         MVC   H2+12(12),QUESTOR                                                
*                                                                               
         MVC   H6+48(6),=C'MARKET'                                              
         MVC   H6+55(4),QMKT                                                    
         MVC   H6+60(24),MKTNM                                                  
         OC    H6+48(36),SPACES                                                 
         GOTO1 CENTER,DMCB,H6+48,36                                             
*                                                                               
         MVC   H7+48(7),=C'STATION'                                             
         MVC   H7+56(8),BIGSTA                                                  
         OC    H7+48(36),SPACES                                                 
         GOTO1 CENTER,DMCB,H7+48,36                                             
*                                                                               
         MVI   H8,0                FORCE A BLANK LINE AFTER H7                  
         J     EXIT                                                             
*                                                                               
HDHKR9   DS    A                                                                
HDHKRA   DS    A                                                                
         DS    A                   *** NOT USED ***                             
HDHKRC   DS    A                                                                
         LTORG                                                                  
         EJECT                                                                  
*==============================================================                 
* MIDHOOK ROUTINES                                                              
*==============================================================                 
         SPACE 1                                                                
MYMIDHK  NTR1  BASE=*,LABEL=*                                                   
         LM    R9,RA,MIDHKR9                                                    
         L     RC,MIDHKRC                                                       
         USING SPJVWORK,RC                                                      
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         MVC   MID1(L'MIDSKD1A),MIDSKD1A                                        
         MVC   MID1+L'MIDSKD1A(L'MIDSKD1B),MIDSKD1B                             
         MVC   MID2(L'MIDSKD2A),MIDSKD2A                                        
         MVC   MID2+L'MIDSKD2A(L'MIDSKD2B),MIDSKD2B                             
*                                                                               
         LHI   R0,4                MOVE DEMO NAMES                              
         LA    R1,MID1+L'MIDSKD1A+L'MIDSKD1B+1                                  
         LA    RE,SVDEMNMS                                                      
         L     RF,SVPRDBUF                                                      
         LA    RF,PTDEMLST-PTBUFFD(RF)                                          
*                                                                               
MYMID2   MVC   0(6,R1),0(RE)       MOVE NAME                                    
         CLC   0(6,R1),SPACES                                                   
         BNH   *+10                                                             
         MVC   132(6,R1),=C'------'                                             
*                                                                               
         AHI   R1,8                                                             
         AHI   RE,6                                                             
         AHI   RF,3                                                             
         BCT   R0,MYMID2                                                        
*                                                                               
MYMIDHKX J     EXIT                                                             
*                                                                               
MIDHKR9  DS    A                                                                
MIDHKRA  DS    A                                                                
         DS    A                   *** NOT USED ***                             
MIDHKRC  DS    A                                                                
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
SPJVWORK DS    0D                  THIS AREA ADDRESSED BY RC                    
*                                                                               
SVPRDBUF DS    A                   PRDBUFF ENTRY FOR THIS PRD                   
SVRERATE DS    A                                                                
SVELADDR DS    A                                                                
DEMO     DS    F                                                                
*                                                                               
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
DEMOTYPE DS    C                                                                
TOTFLAG  DS    C                                                                
PRTDEMO  DS    CL6                                                              
SVNUMDEM DS    H                                                                
SVDEMNMS DS    4CL6                DEMO NAMES                                   
         DS    0D                                                               
BUYTOTS  DS    0XL20                                                            
         ORG   BUYTOTS                                                          
BUYSPOTS DS    F                                                                
BUYDEM1  DS    F                                                                
BUYDEM2  DS    F                                                                
BUYDEM3  DS    F                                                                
BUYDEM4  DS    F                                                                
BUYDEMX  EQU   *                                                                
*                                                                               
MIDSKD1A DC    CL51'BUYLINE --DAYS- ---TIMES--- DLN  SCHED --ACTUAL---'         
******              001-103 MTWTFSS 1000A-1130P F30  MAR01 MAR03-1013P          
MIDSKD2A DC    CL51'------- -- PROGRAMMING --   ---  ----- -----------'         
*                                                                               
MIDSKD1B DC    CL10' ---BOOK--' DEMO01 DEMO02 DEMO03 DEMO04'                    
MIDSKD2B DC    CL10' ---------'                            '                    
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
PBUYLIN  DS    CL7                 EST-LIN                                      
         DS    CL1                                                              
PDAYS    DS    CL7                 MTWTFSS                                      
         DS    CL1                                                              
PTIME    DS    CL11                                                             
         DS    CL1                                                              
PDPT     DS    CL1                                                              
PSLN     DS    CL3                                                              
         DS    CL1                                                              
PSPOTDT  DS    CL5                                                              
         DS    CL1                                                              
PAFFDT   DS    CL5                                                              
         DS    CL1                                                              
PAFFTM   DS    CL5                                                              
         DS    CL2                                                              
PBOOK    DS    CL9                                                              
         DS    CL1                                                              
PDEM1    DS    CL7                                                              
         DS    CL1                                                              
PDEM2    DS    CL7                                                              
         DS    CL1                                                              
PDEM3    DS    CL7                                                              
         DS    CL1                                                              
PDEM4    DS    CL7                                                              
*                                                                               
         DS    CL1                                                              
PDEMPRG  DS    CL15                                                             
*                                                                               
         ORG   PBUYLIN+3+132                                                    
PBKTYPE  DS    CL1                                                              
         ORG   PDAYS+132                                                        
PPROG    DS    CL17                                                             
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPPTBUF                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPJV02 07/16/07'                                      
         END                                                                    
