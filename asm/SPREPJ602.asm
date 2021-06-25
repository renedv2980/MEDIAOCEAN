*          DATA SET SPREPJ602  AT LEVEL 023 AS OF 11/02/06                      
*PHASE SPJ602C                                                                  
*INCLUDE BUFFERIN                                                               
SPJ602   TITLE 'DAILY SORTED MEDIA SCHEDULE'                                    
SPJ602   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SPJ602                                                       
*                                                                               
         L     RC,=A(SPJ6WORK)                                                  
         USING SPJ6WORK,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         LR    R9,RA                                                            
         AHI   R9,4096                                                          
         USING SPWORKD,RA,R9                                                    
                                                                                
         CLI   MODE,PROCBUY                                                     
         BNE   *+12                                                             
         BRAS  RE,PROCB                                                         
         J     EXIT                                                             
*                                                                               
         CLI   MODE,PROCGOAL                                                    
         BNE   *+12                                                             
         BRAS  RE,PROCG                                                         
         J     EXIT                                                             
*                                                                               
         CLI   MODE,STAFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,STAF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,STALAST                                                     
         BNE   TESTMKTF                                                         
         BRAS  RE,STAL                                                          
         CLI   MODE,REREAD                                                      
         JNE   EXIT                                                             
         BRAS  RE,STAF                                                          
         J     EXIT                                                             
*                                                                               
TESTMKTF CLI   MODE,MKTFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,MKTF                                                          
         J     EXIT                                                             
*                                                                               
         CLI   MODE,MKTLAST                                                     
         BNE   *+12                                                             
         BRAS  RE,MKTL                                                          
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
         CLI   MODE,REQFRST                                                     
         BNE   *+12                                                             
         BRAS  RE,REQF                                                          
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
* REQUEST FIRST PROCESSING                                            *         
*=====================================================================*         
                                                                                
REQF     NTR1  BASE=*,LABEL=*                                                   
         L     RE,MEDBUFF                                                       
         USING MEDBLOCK,RE                                                      
         LHI   R0,1                                                             
         STCM  R0,15,MEDNUMPE                                                   
         LHI   R0,256                                                           
         STCM  R0,15,MEDLCHNK                                                   
         XC    MEDNUMMO,MEDNUMMO                                                
         XC    MEDNUMQT,MEDNUMQT                                                
         MVC   MEDNUMWK,=F'14'                                                  
         MVI   MEDEXTDM,4                                                       
         MVI   MEDDAILY,C'Y'       GET DAILY DATA                               
         MVC   MEDEXTAX,SPOTPROF+12  SET TAX EXCLUSION OPTION                   
         MVI   RQPRDEST,C'Y'                                                    
         MVI   RQDAYPT,C'Y'                                                     
         MVI   RQEQUIV,C'Y'                                                     
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
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
         J     EXIT                                                             
         DROP  RE                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* CLIENT  FIRST PROCESSING                                            *         
* READ SPOT D6 PROFILE                                                          
*=====================================================================*         
                                                                                
CLTF     NTR1  BASE=*,LABEL=*                                                   
         MVC   WORK(12),=CL12'S0D6'                                             
         MVC   WORK+4(3),SVAGY                                                  
         MVC   WORK+7(3),CLIENT                                                 
         L     R6,ADCLT                                                         
         USING CLTHDRD,R6                                                       
         MVI   WORK+10,C'*'                                                     
         MVC   WORK+11(1),COFFICE                                               
         MVC   WORK+12(12),WORK                                                 
*                                                                               
         GOTO1 GETPROF,DMCB,(X'40',WORK),PROGPROF,DATAMGR                       
*                                                                               
         MVC   WORK,WORK+12        RESTORE                                      
         MVC   WORK(4),=C'SD6A'                                                 
         NI    WORK,X'BF'          MAKE S LOWERCASE                             
         GOTO1 GETPROF,DMCB,(X'40',WORK),SD6APROF,DATAMGR                       
                                                                                
*=====================================================================*         
* SET REPORT DETAIL OPTIONS                                                     
*=====================================================================*         
                                                                                
         LA    RE,DETOP1           DEFAULT TO OPTION 1                          
         SR    R4,R4                                                            
         IC    R4,PROGPROF+0                                                    
         CLI   PROGPROF+0,C'0'                                                  
         BL    CLTF10                                                           
         CLI   PROGPROF+0,C'7'                                                  
         BH    CLTF10                                                           
         N     R4,=X'0000000F'                                                  
         LA    RE,DETOPTAB(R4)                                                  
*                                                                               
CLTF10   MVC   DETOPTS,0(RE)                                                    
*                                                                               
         J     EXIT                                                             
*                                                                               
DETOPTAB DS    0F                                                               
DETOP1   DC    C'YYYY'         DEMOS/FLAG OVRD/PRINT CPP/PRINT COST             
DETOP2   DC    C'YNYY'                                                          
DETOP3   DC    C'NNNY'                                                          
DETOP4   DC    C'YYNN'                                                          
DETOP5   DC    C'NNNN'                                                          
DETOP6   DC    C'YYNY'                                                          
DETOP7   DC    C'YNNY'                                                          
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* ESTIMATE FIRST PROCESSING                                           *         
*=====================================================================*         
                                                                                
ESTF     NTR1  BASE=*,LABEL=*                                                   
         MVC   SVQSTART(12),QSTART   SAVE ORIGINAL START/END DATES              
         XC    PASSTAB(96),PASSTAB   CLEAR TABLE OF 14 DAY PERIODS              
* BUILD NEW TABLE                                                               
         LA    R4,PASSTAB                                                       
         LA    R5,7                MAX IS 7 14-DAY PERIODS                      
         MVC   PASSTAB(6),QSTART                                                
*                                                                               
ESTF2    GOTO1 ADDAY,DMCB,0(R4),6(R4),F'13'  GET END DATE                       
         CLC   6(6,R4),QEND                  TEST PAST REQUEST END              
         BNL   ESTF10                                                           
         GOTO1 (RF),(R1),6(R4),12(R4),F'1'   GET NEXT START DATE                
         LA    R4,12(R4)                                                        
         BCT   R5,ESTF2                                                         
*                                                                               
ESTF10   MVC   6(6,R4),QEND        SET REQ END DATE IN LAST ENTRY               
         MVI   PASS,1              RESET PASS COUNTER                           
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
*                                                                               
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
         MVI   RQGETBF,C'N'                                                     
         CLI   Q2NET,C'Y'                                                       
         BNE   *+8                                                              
         MVI   RQGETBF,C'X'        REQUEST NET DOLLARS                          
         J     EXIT                                                             
         EJECT                                                                  
*=====================================================================*         
* MARKET FIRST                                                        *         
*=====================================================================*         
                                                                                
MKTF     NTR1  BASE=*,LABEL=*                                                   
         MVI   PASS,1              RESET PASS                                   
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         GOTOR BUFFERIN,DMCB,('BUFFAINI',BUFFET),0,ACOMFACS                     
         XC    BUFFCNT,BUFFCNT     CLEAR BUFFALO COUNTER                        
*                                                                               
         LA    R0,MKTTOTS                                                       
         LHI   R1,MKTTOTX-MKTTOTS                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
MKTFX    J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* STAFRST                                                             *         
*=====================================================================*         
                                                                                
STAF     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   PROGPROF+1,C'Y'     TEST NEW PAGE FOR STATION                    
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         MVI   ALLOWLIN,24         START WITH AT LEAST 24 LINES                 
*                                                                               
         MVI   RCSUBPRG,1                                                       
         XC    TOTSPOTS,TOTSPOTS   CLEAR STATION TOTALS                         
*                                                                               
         LA    R0,STATOTS                                                       
         LHI   R1,STATOTX-STATOTS                                               
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         L     R0,=A(SORTBUFF)                                                  
         ST    R0,ASORTNXT                                                      
         XC    SORTCNT,SORTCNT     CLEAR COUNT                                  
*                                                                               
         L     R1,=A(SORTBUFX-SORTBUFF)                                         
         SR    RE,RE                                                            
         SR    RF,RF                                                            
         MVCL  (R0),(RE)                                                        
                                                                                
         EJECT                                                                  
*====================================================================           
* FORMAT DATES FOR CURRENT REQUEST PERIOD                                       
*====================================================================           
                                                                                
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         BCTR  RE,0                                                             
         MHI   RE,12               POINT TO CURRENT DATES                       
         LA    RE,PASSTAB(RE)                                                   
         MVC   QSTART(12),0(RE)                                                 
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
                                                                                
*====================================================================           
* FORMAT DATES FOR MIDLINES                                                     
*====================================================================           
                                                                                
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         L     R5,MEDAFRST         GET A(DAY 1)                                 
         LA    R4,MYDATES          3 SETS OF CL56 DATA                          
*                                                                               
         LHI   R0,3                                                             
         LA    R1,MYDATES                                                       
         MVC   0(L'MYDATES,R1),SPACES                                           
         AHI   R1,L'MYDATES                                                     
         BCT   R0,*-10                                                          
*                                                                               
STAF10   GOTO1 DATCON,DMCB,(2,(R5)),WORK  GET YYMMDD                            
         PACK  DUB,WORK+2(2)              PACK MONTH                            
         CVB   RE,DUB                                                           
         BCTR  RE,0                                                             
         MHI   RE,3                                                             
         LA    RE,MONTHS(RE)                                                    
         MVC   L'MYDATES(3,R4),0(RE)       3 CHARACTER MONTH                    
         MVC   2*L'MYDATES+1(2,R4),WORK+4  DAY NUMBER                           
*                                                                               
         GOTO1 GETDAY,DMCB,WORK,WORK+16                                         
         MVC   0(3,R4),WORK+16             3 BYTE DAY                           
*                                                                               
         LA    R4,4(R4)                                                         
         LA    R5,12(R5)                                                        
         C     R5,MEDALAST                                                      
         BNH   STAF10                                                           
* SAVE DATES FOR MULTI-PASS REPORT                                              
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         BCTR  RE,0                                                             
         MHI   RE,MYDATEX-MYDATES                                               
         A     RE,=A(SVMYDTS)                                                   
         LHI   RF,MYDATEX-MYDATES                                               
         LA    R0,MYDATES                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* STALAST                                                             *         
*=====================================================================*         
                                                                                
STAL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         OC    SORTCNT,SORTCNT                                                  
         BZ    STAL40                                                           
* SORT THE BUFFER                                                               
         GOTO1 XSORT,DMCB,A(SORTBUFF),SORTCNT,12,8,0                            
                                                                                
*=======================================================                        
* READ THE BUYS AND PRINT THE SCHEDULE                                          
*=======================================================                        
                                                                                
         L     R4,=A(SORTBUFF)                                                  
         L     R5,SORTCNT                                                       
         MVI   RCSUBPRG,1          SET FOR SCHEDULE MIDLINES                    
*                                                                               
STAL2    CLC   KEY+6(3),0(R4)      TEST SAME STATION                            
         BE    *+8                                                              
         MVI   FORCEMID,C'Y'                                                    
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY+6(3),0(R4)      SET CURRENT STATION IN KEY                   
         MVC   KEY+14(4),8(R4)     SET DISK ADDRESS                             
*                                                                               
         GOTO1 GETBUY              SPREPDM UPDATES BIGSTA AS NEEDED!            
*                                                                               
         BAS   RE,PRTSKED                                                       
*                                                                               
         LA    R4,12(R4)                                                        
         BCT   R5,STAL2                                                         
*                                                                               
         SR    RE,RE               SAVE DAILY MKT TOTS FROM THIS PASS           
         IC    RE,PASS                                                          
         BCTR  RE,0                                                             
         MHI   RE,MKTTOTX-MKTTOTS                                               
         A     RE,=A(SVMKTOTS)     SAVE AREA ADDRESS                            
         LHI   RF,MKTTOTX-MKTTOTS  LENGTH TO SAVE                               
         LA    R0,MKTTOTS                                                       
         LR    R1,RF                                                            
         MVCL  RE,R0                                                            
         EJECT                                                                  
*=======================================================                        
* NOW PRINT SUMMARIES AND TOTALS                                                
*=======================================================                        
                                                                                
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK1Q                                                     
         MVC   BFK1STA,BSTA                                                     
         MVC   DUB(4),BFKEY                                                     
*                                                                               
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         CLC   DUB(4),BFKEY                                                     
         BNE   STAL40                                                           
*                                                                               
         ICM   R0,15,BFSPOTS                                                    
         BZ    STAL5                                                            
         EDIT  (R0),(3,SKSPOTS)                                                 
         MVI   SKSPOTS+3,C'*'                                                   
*                                                                               
STAL5    L     R1,BFBDOL                                                        
         M     R0,=F'2'            X 2                                          
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(8,SKCOST),FLOAT=$                                          
*                                                                               
* PRINT SPOT TOTALS BY DAY IN P1                                                
*                                                                               
         LA    RE,TOTSPOTS                                                      
         LA    RF,14                                                            
         LA    R2,SKGRID                                                        
*                                                                               
STAL8    SR    R0,R0                                                            
         ICM   R0,3,0(RE)                                                       
         BZ    STAL8X                                                           
         EDIT  (R0),(3,(R2))                                                    
         MVI   3(R2),C'*'                                                       
*                                                                               
STAL8X   LA    R2,4(R2)            NEXT GRID POSITION                           
         LA    RE,2(RE)            NEXT DAY TOTAL                               
         BCT   RF,STAL8                                                         
*                                                                               
         ICM   R0,15,BFSPOTS                                                    
         EDIT  (R0),(3,SKSPOTS)                                                 
         MVI   SKSPOTS+3,C'*'                                                   
*                                                                               
         LA    R4,BFBDEM1                                                       
         LHI   R5,8                                                             
         LA    R6,SKDEMS                                                        
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
STAL10   MVC   DEMO,0(R4)                                                       
         MVC   DEMOTYPE,1(R2)                                                   
         MVC   DOLS,BFBDOL                                                      
*                                                                               
         BRAS  RE,FMTDEM                                                        
         MVC   0(6,R6),PRTDEMO                                                  
         MVC   132(6,R6),PRTCPP                                                 
*                                                                               
STAL20   LA    R2,3(R2)            NEXT 3 BYTE DEMO                             
         LA    R4,8(R4)            NEXT DEMO IN MEDBUFF                         
         LA    R6,L'SKDEMS(R6)     NEXT PRINT LINE DEMO                         
         BCT   R5,STAL10                                                        
*                                                                               
         MVI   SPACING,1                                                        
         MVC   P1(23),=C'STATION WABCXXXX TOTALS'                               
         MVC   P1+8(8),BIGSTA                                                   
         GOTO1 SQUASHER,DMCB,P1,23                                              
         MVC   P2,P1               MOVE P2 TO P1                                
*                                                                               
         MVI   P1,C'='                                                          
         MVC   P1+1(131),P1                                                     
*                                                                               
STAL30   GOTO1 REPORT                                                           
         MVI   FORCEHED,C'Y'                                                    
         XC    TOTSPOTS,TOTSPOTS                                                
*                                                                               
* PRINT STATION DAILY TOTALS IF NECESSARY                                       
*                                                                               
         CLI   PROGPROF+7,C'S'     TEST STATION RECAPS                          
         BNE   STAL40                                                           
         MVI   RCSUBPRG,7          PRINT DAILY STATION TOTALS                   
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+52(28),=C'** STATION WABCXXX TOTALS **'                        
         MVC   P+63(8),BIGSTA                                                   
         GOTO1 SQUASHER,DMCB,P+52,28                                            
*                                                                               
         LA    R7,MYDATES                                                       
         LA    R8,STATOTS          POINT TO ACCUMS                              
         BRAS  RE,DODAYTOT                                                      
*                                                                               
STAL40   SR    RE,RE                                                            
         IC    RE,PASS                                                          
         MHI   RE,12                                                            
         LA    RE,PASSTAB(RE)      THIS POINTS TO NEXT ENTRY                    
         CLI   0(RE),0             TEST MORE PERIODS TO PROCESS                 
         BNE   STAL42              YES                                          
         MVI   PASS,1                                                           
* RESTORE MARKET TOTALS FOR PASS 1                                              
         L     RE,=A(SVMKTOTS)     SAVE AREA ADDRESS                            
         LHI   RF,MKTTOTX-MKTTOTS  LENGTH TO SAVE                               
         LA    R0,MKTTOTS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE               RESTORE PREVIOUS ACCUMS                      
         J     EXIT                                                             
*                                                                               
STAL42   MVI   MODE,REREAD                                                      
         MVI   FORCEHED,C'Y'       MUST START ON NEW PAGE                       
         SR    RE,RE                                                            
         IC    RE,PASS                                                          
         AHI   RE,1                                                             
         STC   RE,PASS             INCREMENT PASS                               
* RESTORE DAILY MARKET TOTALS FOR THIS PASS                                     
         BCTR  RE,0                                                             
         MHI   RE,MKTTOTX-MKTTOTS                                               
         A     RE,=A(SVMKTOTS)     SAVE AREA ADDRESS                            
         LHI   RF,MKTTOTX-MKTTOTS  LENGTH TO SAVE                               
         LA    R0,MKTTOTS                                                       
         LR    R1,RF                                                            
         MVCL  R0,RE                                                            
*                                                                               
         J     EXIT                WILL PROCESS STAF NEXT                       
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* THIS ROUTINE CALLED BY STALAST TO PRINT SORTED SCHEDULE                       
*===========================================================                    
                                                                                
PRTSKED  NTR1                                                                   
         XC    PSLIST,PSLIST                                                    
         GOTOR MEDPSL,DMCB,SPWORKD,PSLIST                                       
         CLI   BPRD,X'FF'                                                       
         BNE   *+10                                                             
         MVC   PSLIST,=X'FF000000'   SET FOR POL                                
*                                                                               
         LA    R2,PSLIST                                                        
PRTSK04  CLI   0(R2),0                                                          
         JE    EXIT                                                             
         CLC   0(1,R2),BPRD                                                     
         BNE   PRTSK08                                                          
                                                                                
PRTSK06  L     R3,MEDBUFF          SET PRODUCT AND SPOT LENGTH                  
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,0(R2)                                                   
         MVC   MEDSPTLN,1(R2)                                                   
         GOTOR MEDGETBY,DMCB,SPWORKD,SVRERATE                                   
                                                                                
         BAS   RE,BUYSKED          PRINT SCHEDULE GRID                          
*                                                                               
         BAS   RE,BUYPUT           POST BUY VALUES TO BUFFERIN                  
*                                                                               
         BAS   RE,BUYDLY           POST BUY VALUES TO DAILY TOTALS              
                                                                                
PRTSK08  LA    R2,2(R2)                                                         
         B     PRTSK04                                                          
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* BUFFERIN DATA POST ROUTINES                                         *         
* NOTE USES PERIOD TOTALS SO NO DAILY PROCESSING                      *         
*=====================================================================*         
                                                                                
BUYPUT   NTR1  BASE=*,LABEL=*                                                   
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)            POINT TO DOLLARS                             
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         JZ    EXIT                                                             
* SET COL DATA                                                                  
         XC    BFREC(BFRECL),BFREC                                              
         MVC   BFSPOTS,MEDBYSPT                                                 
         MVC   BFBDOL,MEDBYD                                                    
         MVC   BFBEQDOL,MEDBYDEQ                                                
*                                                                               
         LH    R0,SVNUMDEM         MOVE UP TO 4 DEMOS                           
         LA    R5,BFBDEM1                                                       
         LA    R6,MEDBY1                                                        
*                                                                               
BUYPUT2  MVC   0(8,R5),0(R6)       MOVE DEMO AND EQUIV DEMO                     
         LA    R5,8(R5)                                                         
         LA    R6,8(R6)                                                         
         BCT   R0,BUYPUT2                                                       
*                                                                               
         MVI   BFTYPE,BFK1Q                                                     
         MVC   BFK1STA,KEY+6                                                    
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK1STA(3),=3X'FF'                                               
         GOTOR PUTBUF                                                           
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK5Q                                                     
         MVC   BFK5TDPT,MEDDPGRP                                                
         MVC   BFK5SDPT,MEDDPART                                                
         MVC   BFK5SLN,1(R2)       SPOTLEN                                      
         CLI   BFK5SLN,0                                                        
         BNE   *+14                                                             
         L     RE,ADBUY                                                         
         MVC   BFK5SLN,BDSEC-BUYREC(RE)                                         
*                                                                               
         GOTOR PUTBUF                                                           
*                                                                               
         MVI   BFK5SLN,X'FF'                                                    
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK5SDPT,=3X'FF'                                                 
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK5TDPT,=3X'FF'                                                 
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK5SLN,1(R2)       FOR TDPT BY SLN                              
         GOTOR PUTBUF                                                           
*                                                                               
         J     EXIT                                                             
         DROP  R3                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* POST TO DAILY ACCUMULATORS                                          *         
*=====================================================================*         
                                                                                
BUYDLY   NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         L     R5,MEDAFRST                                                      
         SR    R6,R6               INDEX REG FOR DAY                            
*                                                                               
BUYDLY2  L     R4,4(R5)            POINT TO DOLLARS                             
         USING MEDDATA,R4                                                       
*                                                                               
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    BUYDLY10                                                         
*                                                                               
         LA    R1,STATOTS(R6)                                                   
         BAS   RE,POSTDLY                                                       
*                                                                               
         LA    R1,MKTTOTS(R6)                                                   
         BAS   RE,POSTDLY                                                       
*                                                                               
BUYDLY10 LA    R5,12(R5)                                                        
         AHI   R6,4                                                             
         C     R5,MEDALAST                                                      
         BNH   BUYDLY2                                                          
         J     EXIT                                                             
*                                                                               
         USING DLYTOTD,R1                                                       
POSTDLY  L     RF,MEDBYSPT                                                      
         A     RF,DLYSPTS                                                       
         ST    RF,DLYSPTS                                                       
*                                                                               
         L     RF,MEDBYD                                                        
         A     RF,DLYDOLS                                                       
         ST    RF,DLYDOLS                                                       
*                                                                               
         L     RF,MEDBY1                                                        
         A     RF,DLYDEM1                                                       
         ST    RF,DLYDEM1                                                       
*                                                                               
         L     RF,MEDBY2                                                        
         A     RF,DLYDEM2                                                       
         ST    RF,DLYDEM2                                                       
*                                                                               
         L     RF,MEDBY3                                                        
         A     RF,DLYDEM3                                                       
         ST    RF,DLYDEM3                                                       
*                                                                               
         L     RF,MEDBY4                                                        
         A     RF,DLYDEM4                                                       
         ST    RF,DLYDEM4                                                       
         BR    RE                                                               
         DROP  R1,R3,R4                                                         
         EJECT                                                                  
*=====================================================================*         
* PRINT THE BUY SCHEDULE                                              *         
*=====================================================================*         
                                                                                
BUYSKED  NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)            POINT TO PERIOD TOTALS                       
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         JZ    EXIT                                                             
*                                                                               
* HAVE SPOTS OR DOLLARS, SO PRINT GRID                                          
*                                                                               
         LA    R2,SKGRID                                                        
         L     R5,MEDAFRST         GET A(DAY1)                                  
         LA    R6,TOTSPOTS                                                      
*                                                                               
BUYSK2   ICM   R4,15,4(R5)         POINT TO MEDDATA                             
         BZ    BUYSK12X                                                         
         USING MEDDATA,R4                                                       
         OC    MEDBYD(12),MEDBYD   TEST SPOTS OR DOLLARS                        
         BZ    BUYSK12                                                          
*                                                                               
BUYSK10  ICM   R0,15,MEDBYSPT      TEST FOR SPOTS                               
         BZ    BUYSK12                                                          
         EDIT  (R0),(3,1(R2))                                                   
*                                                                               
         ICM   R0,15,MEDBYSPT      TEST FOR SPOTS                               
         AH    R0,0(R6)            BUMP SPOT TOTAL                              
         STH   R0,0(R6)                                                         
*                                                                               
BUYSK12  LA    R2,L'SKGRID(R2)     NEXT GRID POSITION                           
         LA    R6,2(R6)                                                         
*                                                                               
BUYSK12X LA    R5,12(R5)           NEXT DAY                                     
         C     R5,MEDALAST                                                      
         BNH   BUYSK2                                                           
*                                                                               
         LA    R5,MEDPERD                                                       
         L     R4,4(R5)                                                         
         ICM   R0,15,MEDBYSPT      TEST FOR SPOTS                               
         EDIT  (R0),(3,SKSPOTS)                                                 
         MVI   SKSPOTS+3,C'*'                                                   
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         LA    R0,7                                                             
         LA    R1,SKDAYS                                                        
         MVC   0(7,R1),=C'MTWTFSS'                                              
*                                                                               
         IC    RE,BDDAY                                                         
         SLL   RE,25               GET MONDAY BIT LEFT ALIGNED                  
*                                                                               
BUYSK14  LTR   RE,RE               REG IS NEG IF DAY BIT ON                     
         BM    *+8                                                              
         MVI   0(R1),C'.'                                                       
*                                                                               
         LA    R1,1(R1)                                                         
         SLL   RE,1                                                             
         BCT   R0,BUYSK14                                                       
*                                                                               
         GOTO1 UNTIME,DMCB,BDTIMST,SKTIME                                       
*                                                                               
         MVC   SKPROG,BDPROGRM                                                  
*                                                                               
         CLI   DETOPCOS,C'Y'       TEST TO PRINT COST                           
         BNE   BUYSK16                                                          
         SR    R0,R0                                                            
         ICM   R0,7,BDCOST                                                      
         CVD   R0,DUB                                                           
         C     R0,=F'9999999'      MAX COST IN 7 DIGITS                         
         BH    BUYSK15                                                          
         EDIT  (R0),(8,SKCOST),2                                                
         B     BUYSK16                                                          
*                                                                               
BUYSK15  AR    R0,R0               PRINT COST IN $                              
         SRDL  R0,32                                                            
         D     R0,=F'100'                                                       
         SRA   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(8,SKCOST),0,FLOAT=$                                        
*                                                                               
BUYSK16  DS    0H                                                               
**NOP**  MVC   SKDPT,MEDDPART      3 CHAR DAYPART                               
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BDSEC                                                         
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         CHI   R0,99                                                            
         BH    BUYSK18                                                          
         UNPK  SKSLN,DUB                                                        
         B     BUYSK20                                                          
*                                                                               
BUYSK18  UNPK  SKSLN-1(3),DUB                                                   
         MVI   SKPROG+L'SKPROG-1,C' ' MAKE LAST CHAR OF PROG A SPACE            
         DROP  R6                                                               
         EJECT                                                                  
*=============================================================                  
* DEMOS AND CPPS                                                                
*=============================================================                  
BUYSK20  LA    R5,MEDBY1                                                        
         LA    R6,SKDEMS                                                        
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
BUYSK22  ICM   R1,15,0(R5)         DEMO                                         
         BZ    BUYSK24                                                          
         M     R0,=F'2'            X 2                                          
         D     R0,MEDBYSPT         DIVIDE TOTAL DEMO BY SPOTS                   
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,DEMO                                                          
         MVC   DEMOTYPE,1(R2)                                                   
         XC    DOLS,DOLS           SET DOLS TO ZERO                             
* THIS CALL GETS DEMO VALUE FOR 1 SPOT                                          
         CLI   DETOPDEM,C'Y'       TEST TO PRINT DEMOS                          
         BNE   BUYSK24                                                          
         MVI   BYTE,C' '           SET OVRD CHAR TO ' '                         
         CLI   DETOPFLG,C'Y'       TEST TO FLAG OVRDS                           
         BNE   *+8                 NO                                           
         BAS   RE,FLGOVRD                                                       
*                                                                               
         BRAS  RE,FMTSKDEM                                                      
         MVC   0(5,R6),PRTDEMO                                                  
         MVC   5(1,R6),BYTE        MOVE OVRD CHAR                               
*                                                                               
* NOW GET CPP PASSING TOTAL FOR DEMO AND TOTAL DOLLARS                          
*                                                                               
         CLI   DETOPCPP,C'Y'       TEST TO PRINT CPP                            
         BNE   BUYSK24                                                          
         MVC   DEMO,0(R5)                                                       
         MVC   DOLS,MEDBYD                                                      
*                                                                               
         BRAS  RE,FMTSKDEM                                                      
         MVC   132(5,R6),PRTCPP                                                 
*                                                                               
BUYSK24  LA    R5,8(R5)            NEXT DEMO                                    
         LA    R6,L'SKDEMS(R6)                                                  
         LA    R2,3(R2)            NEXT 3-BYTE DEMO                             
         LA    R0,MEDBY5                                                        
         CR    R5,R0                                                            
         BL    BUYSK22                                                          
         B     BUYSK30                                                          
         DROP  R4                                                               
         SPACE 1                                                                
*=====================================================================*         
* FOR DEMO AT 0(R2), TEST VALUE IN DEMO ELEM IS OVRD                            
* IF IT IS, MOVE '*' TO BYTE                                                    
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
         EJECT                                                                  
*=====================================================================*         
* PRINT COMMENTS                                                                
* CAN SUPPRESS WITH QOPT1=N                                                     
*=====================================================================*         
         SPACE 1                                                                
BUYSK30  CLI   QOPT1,C'N'          TEST OPTION TO SUPPRESS                      
         BE    BUYSKX                                                           
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
         MVI   ELCDLO,X'66'                                                     
         MVI   ELCDHI,X'66'                                                     
         LA    R6,BDELEM                                                        
         BRAS  RE,NEXTEL                                                        
         BNE   BUYSKX                                                           
*                                                                               
         MVI   P3,0                FORCE P3 TO PRINT                            
         LA    R7,P4               COMMENTS START ON P4                         
*                                                                               
BUYSK32  CLC   3(2,R6),=C'X-'      THESE COMMENTS NEVER PRINT                   
         BE    BUYSK50                                                          
*                                                                               
         CLI   PROGPROF+4,C'0'                                                  
         BE    BUYSK40                                                          
         CLI   PROGPROF+4,C'1'     TEST PRINT ACCTG COMMENTS ONLY               
         BNE   BUYSK34                                                          
         CLI   3(R6),C'$'          TEST COMMENT BEGINS WITH $                   
         BE    BUYSK40                                                          
         CLI   2(R6),4             TEST COMMENT NUMBER 4                        
         BE    BUYSK40                                                          
         CLI   2(R6),5             OR 5                                         
         BE    BUYSK40                                                          
         B     BUYSK50                                                          
*                                                                               
BUYSK34  CLC   =C'COMMENT-',3(R6)  PRINT COMMENT- COMMENTS                      
         BNE   BUYSK50                                                          
*                                                                               
BUYSK40  SR    RE,RE                                                            
         IC    RE,1(R6)                                                         
         AHI   RE,-4               3 BYTES OVHD/1 FOR EX                        
         EX    RE,*+8                                                           
         B     *+10                                                             
         MVC   6(0,R7),3(R6)                                                    
*                                                                               
         LA    R7,132(R7)                                                       
BUYSK50  BRAS  RE,NEXTEL                                                        
         BE    BUYSK32                                                          
*                                                                               
BUYSKX   GOTO1 REPORT                                                           
         GOTO1 REPORT              PRINT A BLANK LINE                           
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* BUY RECORD PROCESSING                                                         
* EXTRACT SORT DATA INTO BUFFER                                                 
*        STATION(3)                                                             
*        BDSEDAY(1)                                                             
*        START/END TIME (4)                                                     
*        DISK ADDRESS (4)                                                       
*=====================================================================*         
                                                                                
PROCB    NTR1  BASE=*,LABEL=*                                                   
         L     R4,ASORTNXT         PUT BUY IN SORT BUFFER                       
*                                                                               
         L     R6,ADBUY                                                         
         USING BUYRECD,R6                                                       
*                                                                               
         MVC   0(3,R4),BUYKSTA     MOVE STATION                                 
         MVC   3(1,R4),BDSEDAY     DAY(S)                                       
         SR    R0,R0                                                            
         ICM   R0,3,BDTIMST        START TIME                                   
         CHI   R0,599                                                           
         BH    *+8                                                              
         AHI   R0,2400                                                          
         STH   R0,4(R4)                                                         
         SR    R0,R0                                                            
         ICM   R0,3,BDTIMEND       END TIME                                     
         CHI   R0,599                                                           
         BH    *+8                                                              
         AHI   R0,2400                                                          
         STH   R0,6(R4)                                                         
         MVC   8(4,R4),KEY+14      DISK ADDRESS                                 
*                                                                               
         L     R0,SORTCNT                                                       
         AHI   R0,1                                                             
         ST    R0,SORTCNT                                                       
*                                                                               
         LA    R4,12(R4)                                                        
         ST    R4,ASORTNXT                                                      
         C     R4,=A(SORTBUFX)                                                  
         BNH   *+6                                                              
         DC    H'0'                                                             
         J     EXIT                                                             
         DROP  R6                                                               
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* GOAL RECORD PROCESSING                                              *         
*=====================================================================*         
                                                                                
         USING GOALREC,KEY                                                      
PROCG    NTR1  BASE=*,LABEL=*                                                   
                                                                                
*====================================================================           
* FORMAT DATES FOR CURRENT REQUEST PERIOD                                       
*====================================================================           
                                                                                
         MVI   PASS,1                                                           
*                                                                               
PROCG2   SR    RE,RE                                                            
         IC    RE,PASS                                                          
         BCTR  RE,0                                                             
         MHI   RE,12               POINT TO CURRENT DATES                       
         LA    RE,PASSTAB(RE)                                                   
         CLI   0(RE),0                                                          
         JE    EXIT                                                             
*                                                                               
         MVC   QSTART(12),0(RE)                                                 
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
*                                                                               
         L     R3,MEDBUFF                                                       
         USING MEDBLOCK,R3                                                      
         MVC   MEDBRAND,GKEYPRD                                                 
         MVC   MEDSPTLN,GKEYSLN                                                 
         GOTOR MEDGETGL,DMCB,SPWORKD                                            
*                                                                               
         L     R5,MEDAFRST                                                      
*                                                                               
PROCG10  GOTOR GOLPUT              POST GOAL DATA                               
         LA    R5,12(R5)           NEXT DAY                                     
         C     R5,MEDALAST                                                      
         BNH   PROCG10                                                          
*                                                                               
         SR    R0,R0                                                            
         IC    R0,PASS                                                          
         AHI   R0,1                                                             
         STC   R0,PASS                                                          
         B     PROCG2                                                           
         EJECT                                                                  
*=====================================================================*         
* GOAL RECORD POSTING ROUTINES                                        *         
*=====================================================================*         
                                                                                
GOLPUT   NTR1  ,                                                                
         L     R4,4(R5)            POINT TO DOLLARS                             
         USING MEDDATA,R4                                                       
         OC    MEDGLD(12),MEDGLD                                                
         JZ    EXIT                                                             
                                                                                
*=====================================================================*         
* REPORT 1 - DEMO TOTALS                                              *         
*=====================================================================*         
                                                                                
         XC    BFREC(BFRECL),BFREC                                              
         MVI   BFTYPE,BFK1Q        TYPE 1 - STATION ANALYSIS                    
         MVC   BFK1STA,=X'FFFFFF'  SET FOR TOTALS ONLY                          
                                                                                
         MVC   BFGDOL,MEDGLD                                                    
         MVC   BFGEQDOL,MEDGLDEQ                                                
         MVC   BFGDEM,MEDGL1                                                    
         MVC   BFGEQDEM,MEDGL1EQ                                                
         GOTOR PUTBUF              PUT RECORD                                   
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK5Q        TYPE 5 -SM2 ANALYSIS                         
         MVC   BFK5TDPT,MEDDPGRP                                                
         MVC   BFK5SDPT,MEDDPART                                                
         MVC   BFK5SLN,MEDSPTLN    SPOTLEN                                      
         GOTOR PUTBUF                                                           
*                                                                               
         MVI   BFK5SLN,X'FF'                                                    
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK5SDPT,=3X'FF'                                                 
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK5TDPT,=3X'FF'                                                 
         GOTOR PUTBUF                                                           
*                                                                               
         MVC   BFK5SLN,MEDSPTLN    FOR M2 TOTALS BY SLN ACROSS DPT              
         GOTOR PUTBUF                                                           
*                                                                               
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* PUT RECORD TO BUFFERIN AND DO TRACE IF NECESSARY                    *         
*=====================================================================*         
                                                                                
PUTBUF   NTR1  BASE=*,LABEL=*                                                   
         OC    BFDATA(BFDATAL),BFDATA                                           
         BZ    PUTBUFX                                                          
         L     R0,BUFFCNT                                                       
         AHI   R0,1                                                             
         ST    R0,BUFFCNT                                                       
         CLI   QOPT5,C'Y'         TEST TRACE OPTION                             
         BNE   PUTBUF02                                                         
         GOTOR PRNTBL,DMCB,=C'PUTBUF',BFREC,C'DUMP',BFRECL,=C'1D00'             
                                                                                
PUTBUF02 GOTOR BUFFERIN,DMCB,('BUFFAPUT',BUFFET),BFREC,ACOMFACS                 
                                                                                
PUTBUFX  XIT1                                                                   
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* LAST FOR MARKET - PRINT STATION SUMMARY                             *         
*=====================================================================*         
                                                                                
MKTL     NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         MVC   QSTART(12),SVQSTART   RESTORE REQUEST PERIOD DATES               
         GOTO1 MEDDATE,DMCB,SPWORKD                                             
*                                                                               
         CLI   PROGPROF+7,C'M'     TEST MARKET RECAPS                           
         BNE   MKTL40                                                           
*                                                                               
         MVI   RCSUBPRG,7          PRINT DAILY MARKET TOTALS                    
         MVI   FORCEHED,C'Y'                                                    
         MVC   P+56(19),=C'** MARKET TOTALS **'                                 
*                                                                               
         LA    R1,PASSTAB                                                       
         L     R7,=A(SVMYDTS)                                                   
         L     R8,=A(SVMKTOTS)     POINT TO ACCUMS                              
         XC    PERTOTS,PERTOTS     CLEAR PERIOD TOTALS                          
*                                                                               
MKTL10   MVI   TOTSW,C'N'          SET DO NOT PRINT TOTALS                      
         CLI   12(R1),0            TEST ANY MORE PERIODS                        
         BNE   *+8                                                              
         MVI   TOTSW,C'Y'          IF NOT, SET TO PRINT TOTALS                  
*                                                                               
         BRAS  RE,DODAYTOT                                                      
*                                                                               
         LA    R1,12(R1)                                                        
         AHI   R7,MYDATEX-MYDATES                                               
         AHI   R8,MKTTOTX-MKTTOTS                                               
         CLI   0(R1),0             TEST ANY MORE DATES TO PROCESS               
         BNE   MKTL10                                                           
         GOTO1 REPORT              PRINT A BLANK LINE                           
*                                                                               
MKTL40   MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,2          SET STATION ANALYSIS FLAG                    
         OC    BUFFCNT,BUFFCNT                                                  
         JZ    EXIT                                                             
         XC    BFKEY,BFKEY                                                      
         MVI   BFTYPE,BFK1Q                                                     
         GOTOR BUFFERIN,DMCB,('BUFFARDH',BUFFET),BFREC,ACOMFACS                 
         CLI   BFTYPE,BFK1Q                                                     
         BNE   MKTL52                                                           
         B     MKTL44                                                           
                                                                                
MKTL42   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
MKTL44   CLC   BFK1STA,=X'FFFFFF'                                               
         BE    MKTL50                                                           
         MVC   PSTA,SPACES                                                      
         GOTO1 MSUNPK,DMCB,BFK1STA-2,DUB,PSTA                                   
*                                                                               
MKTL46   BAS   RE,FORMAT                                                        
         MVI   ALLOWLIN,0                                                       
         B     MKTL42                                                           
*                                                                               
MKTL50   MVC   PSTA(5),=C'*TOTS'                                                
         MVI   TOTSW,C'Y'                                                       
         BAS   RE,FORMAT                                                        
*                                                                               
         BAS   RE,FMTGLS                                                        
         EJECT                                                                  
*=====================================================================*         
* PRINT SM2 SUMMARY                                                   *         
*=====================================================================*         
                                                                                
MKTL52   MVI   FORCEMID,C'Y'                                                    
         MVI   RCSUBPRG,6          SM2 SUMMARY                                  
         BAS   RE,CHKLIN           START WITH AT LEAST 30 LINES                 
*                                                                               
         CLI   PASSTAB+12,0        TEST MULTI-PASS                              
         BE    *+8                 NO                                           
         MVI   FORCEHED,C'Y'       THEN PRINT ON NEW PAGE                       
*                                                                               
         XC    TDPTCNT,TDPTCNT     CLEAR TOTDPT COUNTER                         
         XC    SDPTCNT,SDPTCNT     CLEAR SUBDPT COUNTER                         
         XC    SLNCNT,SLNCNT       CLEAR SLN COUNTER                            
         MVI   ONESLN,0            RESET DAYPART COUNTER                        
         MVI   TOTCHAR,C' '                                                     
         XC    PREVTDPT,PREVTDPT                                                
         XC    PREVSDPT,PREVSDPT                                                
*                                                                               
MKTL60   GOTOR BUFFERIN,DMCB,('BUFFASEQ',BUFFET),BFREC,ACOMFACS                 
*                                                                               
         CLI   BFK5TDPT,X'FF'      TEST E-O-R                                   
         BE    MKTL90                                                           
         CLC   PREVTDPT,BFK5TDPT   TEST SAME TDPT AS PREVIOUS                   
         BE    MKTL61                                                           
         LH    R0,TDPTCNT                                                       
         AHI   R0,1                                                             
         STH   R0,TDPTCNT                                                       
         MVC   PREVTDPT,BFK5TDPT                                                
         XC    PREVSDPT,PREVSDPT                                                
         MVC   SM2TDPT,BFK5TDPT                                                 
         MVI   SM2TDPT+3,C'-'      INSERT SEPARATOR                             
*                                                                               
MKTL61   CLI   BFK5SDPT,X'FF'      TEST SUBDPT TOTAL RECORD                     
         BNE   MKTL70              NO                                           
         CLC   SDPTCNT,=H'1'       TEST ONLY ONE SUBDPT                         
         BE    MKTL64              YES                                          
         MVI   TOTCHAR,C'*'                                                     
         MVC   SM2SDPT(6),=C'TOTAL*'                                            
*                                                                               
         CLI   BFK5SLN,X'FF'       TEST SLN TOTAL                               
         BE    MKTL62                                                           
*                                                                               
         CLI   ONESLN,0                                                         
         BNE   *+10                                                             
         MVC   ONESLN,BFK5SLN                                                   
*                                                                               
         CLC   ONESLN,BFK5SLN      TEST SAME SLN                                
         BE    *+8                                                              
         MVI   ONESLN,X'FF'        SET MORE THAN ONE SLN                        
*                                                                               
         SR    R0,R0                                                            
         IC    R0,BFK5SLN                                                       
         EDIT  (R0),(3,SM2SLN),ALIGN=LEFT                                       
*                                                                               
MKTL62   BAS   RE,SM2FMT                                                        
*                                                                               
MKTL64   XC    SDPTCNT,SDPTCNT                                                  
         MVI   TOTCHAR,C' '                                                     
         B     MKTL60                                                           
*                                                                               
MKTL70   CLI   BFK5SLN,X'FF'       TEST SLN TOTAL                               
         BNE   MKTL80                                                           
         CLC   SLNCNT,=H'1'        TEST ONLY ONE SLN                            
         BE    MKTL72              YES - DO NOT PRINT TOTAL                     
*                                                                               
         MVC   SM2SLN,=C'TOT'                                                   
         BAS   RE,SM2FMT                                                        
*                                                                               
MKTL72   XC    SLNCNT,SLNCNT       RESET SLNCNT                                 
         LH    R0,SDPTCNT          BUMP SDPTCNT                                 
         AHI   R0,1                                                             
         STH   R0,SDPTCNT                                                       
         B     MKTL60                                                           
*                                                                               
MKTL80   LH    R0,SLNCNT           PRINT A DETAIL LINE                          
         AHI   R0,1                                                             
         STH   R0,SLNCNT                                                        
*                                                                               
MKTL82   MVC   SM2SDPT,BFK5SDPT    MOVE SUBDPT TO PRINT                         
         CLC   BFK5TDPT,BFK5SDPT   TEST TOTDPT=SUBDPT                           
         BNE   *+10                                                             
         MVC   SM2TDPT(4),SPACES   YES - SUPPRESS TOTDPT AND C'-'               
*                                                                               
         MVI   SM2SLN-1,C'-'       INSERT SEPARATOR                             
         SR    R0,R0                                                            
         IC    R0,BFK5SLN                                                       
         EDIT  (R0),(3,SM2SLN),ALIGN=LEFT                                       
*                                                                               
         BAS   RE,SM2FMT                                                        
         B     MKTL60                                                           
*                                                                               
MKTL90   CLI   BFK5SLN,X'FF'       TEST DAYPART PRESENT                         
         BE    MKTL92              NO - PRINT FINAL TOTAL                       
         CLC   TDPTCNT,=H'1'       TEST MORE THAN ONE TDPT                      
         BNH   MKTL60              NO SKIP                                      
* PRINT TOTAL-SLN LINE                                                          
         MVC   SM2TDPT+1(6),=C'TOTAL-'                                          
         SR    R0,R0                                                            
         IC    R0,BFK5SLN                                                       
         EDIT  (R0),(3,SM2SLN),ALIGN=LEFT                                       
         BAS   RE,SM2FMT                                                        
         B     MKTL60                                                           
*                                                                               
MKTL92   MVC   PSLN(9),=C'* TOTAL *'                                            
         MVI   TOTSW,C'Y'                                                       
         MVI   TOTCHAR,C'*'                                                     
         BAS   RE,SM2FMT                                                        
         J     EXIT                                                             
         EJECT                                                                  
CHKLIN   SR    R0,R0                                                            
         IC    R0,MAXLINES                                                      
         SR    RF,RF                                                            
         IC    RF,LINE             CURRENT LINE COUNT                           
         SR    R0,RF                                                            
         CHI   R0,30                                                            
         BH    *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
         BR    RE                                                               
         EJECT                                                                  
*=============================================================                  
* SUBROUTINE FORMATS DATA FOR THE SM2 SUMMARY                                   
*=============================================================                  
         SPACE 1                                                                
SM2FMT   NTR1  BASE=*,LABEL=*                                                   
         MVC   DEMOTYPE,SVDEMNMS   TYPE IS SAME AS DEM1                         
         MVC   DEMO,BFGDEM         GOAL POINTS                                  
         MVC   EQDEMO,BFGEQDEM                                                  
         MVC   DOLS,BFGDOL         GOAL DOLLARS                                 
         LA    R2,SM2GPTS          POINT TO OUTPUT AREA                         
         BAS   RE,FMTPDCPP         FORMAT POINTS/DOLLARS/CPP                    
*                                                                               
         MVC   DEMOTYPE,SVDEMNMS                                                
         MVC   DEMO,BFBDEM1                                                     
         MVC   EQDEMO,BFBEQDM1                                                  
         MVC   DOLS,BFBDOL                                                      
         LA    R2,SM2PPTS                                                       
         BAS   RE,FMTPDCPP         FORMAT POINTS/DOLLARS/CPP                    
*                                                                               
         L     R0,BFSPOTS                                                       
         EDIT  (R0),(5,SM2SPOTS)                                                
         MVC   SM2SPOTS+5(1),TOTCHAR                                            
*                                                                               
         CLI   SM2TDPT,C'*'        TEST GRAND TOTAL                             
         BNE   SM2F2                                                            
         CLI   ONESLN,X'FF'        TEST ONLY ONE SLN                            
         BE    SM2F16              NO                                           
*                                                                               
SM2F2    L     R1,BFBDEM1                                                       
         M     R0,=F'2'                                                         
         ICM   RE,15,BFSPOTS                                                    
         BZ    SM2F16                                                           
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    SM2F4                                                            
         CLI   DEMOTYPE,C'R'                                                    
         BE    *+12                                                             
         CLI   DEMOTYPE,C'E'                                                    
         BNE   SM2F4                                                            
         MHI   RE,10               SCALE SPOTS X 10 FOR EXTRA DEC               
SM2F4    DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(5,SM2AVPTS),1                                              
         MVC   SM2AVPTS+5(1),TOTCHAR                                            
*                                                                               
SM2F10   L     R1,BFBDOL                                                        
         M     R0,=F'2000'         X 1000 X 2                                   
         OC    BFGDOL,BFGDOL                                                    
         BZ    SM2F12                                                           
         D     R0,BFGDOL                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,SM2PCDOL),1                                              
         MVC   SM2PCDOL+6(1),TOTCHAR                                            
*                                                                               
SM2F12   L     R1,BFBDEM1                                                       
         M     R0,=F'2000'         X 1000 X 2                                   
         OC    BFGDEM,BFGDEM                                                    
         BZ    SM2F16                                                           
         L     RE,BFGDEM           GET GOAL DEMO                                
         DR    R0,RE                                                            
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,SM2PCPTS),1                                              
         MVC   SM2PCPTS+6(1),TOTCHAR                                            
*                                                                               
SM2F16   MVC   DEMO,BFBDEM2                                                     
         MVC   DOLS,BFBEQDOL                                                    
*                                                                               
         CLI   SVDEMNMS+6,C' '                                                  
         BNH   SM2F20                                                           
         MVC   DEMOTYPE,SVDEMNMS+6                                              
         BRAS  RE,FMTDEM                                                        
         MVC   SM2DM2PT,PRTDEMO                                                 
         MVC   SM2DM2PT+6(1),TOTCHAR                                            
         MVC   SM2DM2CP,PRTCPP                                                  
         MVC   SM2DM2CP+6(1),TOTCHAR                                            
*                                                                               
         CLI   SVDEMNMS+12,C' '                                                 
         BNH   SM2F20                                                           
         MVC   DEMO,BFBDEM3                                                     
         MVC   DEMOTYPE,SVDEMNMS+12                                             
         BRAS  RE,FMTDEM                                                        
         MVC   SM2DM3PT,PRTDEMO                                                 
         MVC   SM2DM3PT+6(1),TOTCHAR                                            
         MVC   SM2DM3CP,PRTCPP                                                  
         MVC   SM2DM3CP+6(1),TOTCHAR                                            
*                                                                               
         CLI   SVDEMNMS+18,C' '                                                 
         BNH   SM2F20                                                           
         MVC   DEMO,BFBDEM4                                                     
         MVC   DEMOTYPE,SVDEMNMS+18                                             
         BRAS  RE,FMTDEM                                                        
         MVC   SM2DM4PT,PRTDEMO                                                 
         MVC   SM2DM4PT+6(1),TOTCHAR                                            
         MVC   SM2DM4CP,PRTCPP                                                  
         MVC   SM2DM4CP+6(1),TOTCHAR                                            
*                                                                               
SM2F20   GOTO1 REPORT                                                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=============================================================                  
* FORMAT POINTS/DOLLARS/CPP FOR GOAL OR PURCHASED OUTPUT                        
* POINTS/DOLS ARE GIVEN, R2 POINTS TO OUTPUT AREA                               
* IF DEMOTYPE SET TO 'G', ONLY 1-DECIMAL PRECISION                              
*=============================================================                  
                                                                                
         USING SM2GPTS,R2                                                       
FMTPDCPP NTR1  BASE=*,LABEL=*                                                   
         ICM   R1,15,DEMO                                                       
         BZ    FMTPD4                                                           
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    FMTPD1                                                           
         CLI   DEMOTYPE,C'R'                                                    
         BE    *+12                                                             
         CLI   DEMOTYPE,C'E'                                                    
         BNE   FMTPD1                                                           
* ROUND TO 1-DECIMAL                                                            
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
FMTPD1   C     R1,=F'99999'        MAX WITH DEC IS 9999.9                       
         BNH   FMTPD2                                                           
         M     R0,=F'2'            X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,SM2GPTS),0                                               
         B     FMTPD2X                                                          
*                                                                               
FMTPD2   EDIT (R1),(6,SM2GPTS),1                                                
*                                                                               
FMTPD2X  MVC   SM2GPTS+6(1),TOTCHAR                                             
*                                                                               
FMTPD4   L     R1,DOLS                                                          
         M     R0,=F'2'            X 2                                          
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LTR   R0,R1                                                            
         BZ    FMTPD6                                                           
         C     R0,=F'999999'       IF MORE THAN 6 DIGITS, NO $                  
         BH    FMTPD5                                                           
         EDIT  (R0),(7,SM2GDOLS),FLOAT=$                                        
         MVC   SM2GDOLS+7(1),TOTCHAR                                            
         B     FMTPD6                                                           
*                                                                               
FMTPD5   EDIT  (R0),(7,SM2GDOLS)                                                
*                                                                               
FMTPD6   ICM   RF,15,EQDEMO                                                     
         BZ    FMTPDX                                                           
*                                                                               
         L     R1,DOLS             GET DOLLARS                                  
         LHI   R0,20               X 10 X 2                                     
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    FMTPD8                                                           
         CLI   DEMOTYPE,C'R'                                                    
         BE    *+12                                                             
         CLI   DEMOTYPE,C'E'                                                    
         BNE   FMTPD8                                                           
         LHI   R0,200                                                           
*                                                                               
FMTPD8   MR    R0,R0               SCALE DOLLARS                                
         DR    R0,RF               DIVIDE BY EQUIV POINTS                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,GOALCPP          SAVE GOALCPP                                 
*                                                                               
         C     R1,=F'99999'        MAX CPP WITH CENTS IS 999.99                 
         BNH   FMTPD10                                                          
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'10000'                                                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(7,SM2GCPP)                                                 
         B     FMTPD12                                                          
*                                                                               
FMTPD10  EDIT  (R1),(7,SM2GCPP),2                                               
*                                                                               
FMTPD12  MVC   SM2GCPP+7(1),TOTCHAR                                             
*                                                                               
FMTPDX   J     EXIT                                                             
         LTORG                                                                  
         DROP  R2                                                               
         EJECT                                                                  
FORMAT   NTR1  BASE=*,LABEL=*                                                   
         L     R0,BFSPOTS                                                       
         EDIT  (R0),(5,PSPOTS)                                                  
*                                                                               
         L     R1,BFBDOL                                                        
         M     R0,=F'2'            X 2                                          
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LR    R0,R1                                                            
         EDIT  (R0),(10,PDOLS),FLOAT=$                                          
*                                                                               
         LA    R4,BFBDEM1                                                       
         LHI   R5,4                                                             
         LA    R6,PDEM1                                                         
         L     R2,SVPRDBUF         GET PRDBUFF ADDRESS                          
         LA    R2,PTDEMLST-PTBUFFD(R2)                                          
*                                                                               
FORMAT10 MVC   DEMO,0(R4)                                                       
         MVC   DEMOTYPE,1(R2)                                                   
         MVC   DOLS,BFBDOL                                                      
*                                                                               
         BRAS  RE,FMTDEM                                                        
*                                                                               
         MVC   0(6,R6),PRTDEMO                                                  
         MVC   7(6,R6),PRTCPP                                                   
*                                                                               
FORMAT20 LA    R2,3(R2)            NEXT 3-BYTE DEMO                             
         LA    R4,8(R4)            NEXT DEMO                                    
         AHI   R6,PDEM2-PDEM1      NEXT PRINT LINE DEMO                         
         BCT   R5,FORMAT10                                                      
*                                                                               
         CLI   TOTSW,C'Y'          TEST DOING TOTALS                            
         BNE   FORMATX                                                          
         MVI   SPACING,1                                                        
         MVC   P3,P1               MOVE PRINT LINE DOWN                         
         MVI   P1,C'='                                                          
         MVC   P1+1(131),P1                                                     
         MVI   P2,0                FORCE P2 TO PRINT                            
*                                                                               
FORMATX  GOTO1 REPORT                                                           
         MVI   TOTSW,C'N'          RESET SWITCH                                 
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*=====================================================================*         
* FORMAT GOALS AND INDICES                                            *         
*=====================================================================*         
                                                                                
FMTGLS   NTR1  BASE=*,LABEL=*                                                   
         MVC   PSTA(5),=C'GOALS'                                                
         XC    GOALCPP,GOALCPP                                                  
*                                                                               
         L     R1,BFGDOL                                                        
         M     R0,=F'2'            X 2                                          
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(10,PDOLS)                                                  
*                                                                               
         ICM   R0,15,BFGDEM                                                     
         C     R0,=F'99999'        MAX WITH DEC IS 9999.9                       
         BNH   FMTGL2                                                           
         SRDL  R0,32                                                            
         AR    R1,R1               X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PDEM1),0                                                 
         B     FMTGL4                                                           
*                                                                               
FMTGL2   EDIT (R0),(6,PDEM1),1                                                  
*                                                                               
FMTGL4   L     R1,BFGDOL           GET DOLLARS                                  
         LHI   R0,20               SET FOR X 10 X 2                             
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    FMTGL6                                                           
         CLI   SVDEMNMS,C'R'                                                    
         BE    *+12                                                             
         CLI   SVDEMNMS,C'E'                                                    
         BNE   FMTGL6                                                           
         LHI   R0,200              SET FOR X 100 X 2                            
*                                                                               
FMTGL6   MR    R0,R0                                                            
         OC    BFGDEM,BFGDEM                                                    
         BZ    FMTGL10                                                          
         D     R0,BFGDEM           DIVIDE BY UNEQUIV POINTS                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         ST    R1,GOALCPP          SAVE GOALCPP                                 
         C     R1,=F'99999'        MAX CPP WITH CENTS IS 999.99                 
         BNH   FMTGL8                                                           
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'10000'                                                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PCPP1)                                                   
         B     FMTGL10                                                          
*                                                                               
FMTGL8   EDIT  (R1),(7,PCPP1),2                                                 
*                                                                               
* NOW WORK OUT INDICES                                                          
*                                                                               
FMTGL10  MVC   P2(5),=C'INDEX'                                                  
*                                                                               
         L     R1,BFBDOL                                                        
         M     R0,=F'200'          X 100 X 2                                    
         OC    BFGDOL,BFGDOL                                                    
         BZ    FMTGL12                                                          
         D     R0,BFGDOL                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LA    R6,(PDOLS-P)+P2+7   POINT TO PRINT POSITION                      
         EDIT  (R1),(3,(R6)),0                                                  
*                                                                               
FMTGL12  L     R1,BFBDEM1                                                       
         M     R0,=F'200'          X 100 X 2                                    
         OC    BFGDEM,BFGDEM                                                    
         BZ    FMTGL14                                                          
         D     R0,BFGDEM                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LA    R6,(PDEM1-P)+P2+3   POINT TO PRINT POSITION                      
         EDIT  (R1),(3,(R6)),0                                                  
*                                                                               
FMTGL14  L     R1,BFBDOL           GET DOLLARS                                  
         M     R0,=F'20'           X 10 X 2                                     
         OC    BFBDEM1,BFBDEM1                                                  
         BZ    FMTGLX                                                           
         D     R0,BFBDEM1          DIVIDE BY UNEQUIV POINTS                     
         AHI   R1,1                                                             
         SRL   R1,1                THIS IS ACTUAL CPP                           
*                                                                               
         M     R0,=F'200'          X 100 X 2                                    
         OC    GOALCPP,GOALCPP                                                  
         BZ    FMTGLX                                                           
         D     R0,GOALCPP                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         LA    R6,(PCPP1-P)+P2+4                                                
         EDIT  (R1),(3,(R6)),0                                                  
*                                                                               
FMTGLX   GOTO1 REPORT                                                           
         J     EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* PRINT A DAILY RECAP                                                           
* R8 POINTS TO TOTALS                                                           
* R7 POINTS TO 'MYDATES' OR SAVE AREA                                           
*===========================================================                    
                                                                                
         USING DLYTOTD,R8                                                       
DODAYTOT NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         LA    R1,DLYSPTS                                                       
         SR    R2,R2               SET DSPL INTO PERIOD TOTALS                  
         BAS   RE,GETTOT                                                        
*                                                                               
         LA    R1,DLYDOLS                                                       
         AHI   R2,4                                                             
         BAS   RE,GETTOT                                                        
*                                                                               
         LA    R1,DLYDEM1                                                       
         AHI   R2,4                                                             
         BAS   RE,GETTOT                                                        
*                                                                               
         LA    R1,DLYDEM2                                                       
         AHI   R2,4                                                             
         BAS   RE,GETTOT                                                        
*                                                                               
         LA    R1,DLYDEM3                                                       
         AHI   R2,4                                                             
         BAS   RE,GETTOT                                                        
*                                                                               
         LA    R1,DLYDEM4                                                       
         AHI   R2,4                                                             
         BAS   RE,GETTOT                                                        
         B     DODAY0                                                           
*                                                                               
GETTOT   LHI   R0,14                                                            
         SR    RF,RF                                                            
*                                                                               
         A     RF,0(R1)                                                         
         LA    R1,4(R1)                                                         
         BCT   R0,*-8                                                           
         ST    RF,0(R1)            STORE TOTAL IN 15TH COLUMN                   
         A     RF,PERTOTS(R2)                                                   
         ST    RF,PERTOTS(R2)                                                   
         BR    RE                                                               
*                                                                               
DODAY0   LA    R5,56(R7)           POINT TO 4 BYTE MONTH NAMES                  
         LA    R1,P2+12            START IN COL13                               
         LHI   R0,14                                                            
*                                                                               
DODAY2   MVC   0(3,R1),0(R5)       MOVE 3 MONTH CHARS                           
         MVC   3(2,R1),57(R5)      MOVE DAY NUMBER                              
         LA    R5,4(R5)                                                         
         LA    R1,8(R1)                                                         
         BCT   R0,DODAY2                                                        
*                                                                               
         LHI   R0,14                                                            
         LA    R1,DLYSPTS+52       POINT TO LAST DAILY SPOT ACCUM               
         LA    R2,P2+12+(13*8)     LAST DATA PRINT POSN                         
*                                                                               
DODAY4   OC    0(4,R1),0(R1)       TEST ANY SPOTS                               
         BNZ   DODAY6                                                           
         AHI   R1,-4               BACK UP                                      
         AHI   R2,-8                                                            
         BCT   R0,DODAY4                                                        
*                                                                               
DODAY6   CLI   TOTSW,C'Y'          TEST TO PRINT TOTALS NOW                     
         BNE   DODAY10                                                          
         MVC   8(132,R2),SPACES    BLANK REST OF LINE PLUS A LITTLE             
         MVC   8(5,R2),=C'TOTAL'   MOVE TO NEXT SLOT                            
*                                                                               
* MOVE TOTALS TO APPROPRIATE COLUMN SO ALL PRINT AT ONCE                        
*                                                                               
         LA    R0,DLYSPTS                                                       
         SR    R1,R0               GIVES DSPL TO LAST COL WITH DATA             
         CHI   R1,52                                                            
         BE    DODAY10                                                          
*                                                                               
         LA    R1,4(R1)            POINT TO FREE SLOT                           
         LA    RE,DLYSPTS(R1)                                                   
         MVC   0(4,RE),PERTOTS+0                                                
         LA    RE,DLYDOLS(R1)                                                   
         MVC   0(4,RE),PERTOTS+4                                                
         LA    RE,DLYDEM1(R1)                                                   
         MVC   0(4,RE),PERTOTS+8                                                
         LA    RE,DLYDEM2(R1)                                                   
         MVC   0(4,RE),PERTOTS+12                                               
         LA    RE,DLYDEM3(R1)                                                   
         MVC   0(4,RE),PERTOTS+16                                               
         LA    RE,DLYDEM4(R1)                                                   
         MVC   0(4,RE),PERTOTS+20                                               
* NOW CLEAR OLD TOTALS                                                          
         LHI   R0,6                SPOTS/DOLS/4 DEMOS                           
         LA    R1,DLYSPTS                                                       
*                                                                               
         XC    56(4,R1),56(R1)                                                  
         LA    R1,L'DLYTOTS(R1)                                                 
         BCT   R0,*-10                                                          
*                                                                               
DODAY10  MVI   P3,0                FORCE P3 TO PRINT A BLANK LINE               
*                                                                               
         MVC   P4(5),=C'SPOTS'                                                  
         LA    R2,P4+10                                                         
         LA    R3,DLYSPTS                                                       
         LHI   R4,15                                                            
         CLI   TOTSW,C'Y'                                                       
         BE    *+6                                                              
         BCTR  R4,0                IF NO TOTALS, ONLY 14 COLS                   
*                                                                               
DODAY12  L     R0,0(R3)            SPOTS                                        
         EDIT  (R0),(7,(R2))                                                    
         LA    R2,8(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,DODAY12                                                       
*                                                                               
         MVC   P5(7),=C'DOLLARS'                                                
         LA    R2,P5+10                                                         
         LA    R3,DLYDOLS                                                       
         LHI   R4,15                                                            
         CLI   TOTSW,C'Y'                                                       
         BE    *+6                                                              
         BCTR  R4,0                IF NO TOTALS, ONLY 14 COLS                   
*                                                                               
DODAY14  ICM   R1,15,0(R3)          DOLLARS                                     
         BZ    DODAY16                                                          
         M     R0,=F'2'                                                         
         D     R0,=F'100'                                                       
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(7,(R2)),FLOAT=$                                            
*                                                                               
DODAY16  LA    R2,8(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,DODAY14                                                       
*                                                                               
         MVC   P6(6),SVDEMNMS                                                   
         LA    R2,P6                                                            
         LA    R3,DLYDEM1                                                       
         BAS   RE,DAYDEM                                                        
*                                                                               
         MVC   P7(6),SVDEMNMS+6                                                 
         LA    R2,P7                                                            
         LA    R3,DLYDEM2                                                       
         BAS   RE,DAYDEM                                                        
*                                                                               
         MVC   P8(6),SVDEMNMS+12                                                
         LA    R2,P8                                                            
         LA    R3,DLYDEM3                                                       
         BAS   RE,DAYDEM                                                        
*                                                                               
         MVC   P9(6),SVDEMNMS+18                                                
         LA    R2,P9                                                            
         LA    R3,DLYDEM4                                                       
         BAS   RE,DAYDEM                                                        
*                                                                               
         GOTO1 REPORT                                                           
         J     EXIT                                                             
*                                                                               
DAYDEM   NTR1                                                                   
         LHI   R4,14                                                            
*                                                                               
         CLI   0(R2),C'R'          TEST RATING                                  
         BE    DAYDEM2                                                          
         CLI   0(R2),C'E'                                                       
         BE    DAYDEM2                                                          
         B     DAYDEM10                                                         
*                                                                               
DAYDEM2  ICM   R1,15,0(R3)                                                      
         BZ    DAYDEM6                                                          
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    DAYDEM4                                                          
         M     R0,=F'2'            ROUND TO 1 DECIMAL                           
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
DAYDEM4  EDIT  (R1),(7,10(R2)),1                                                
*                                                                               
DAYDEM6  LA    R2,8(R2)                                                         
         LA    R3,4(R3)                                                         
         BCT   R4,DAYDEM2                                                       
         J     EXIT                                                             
*                                                                               
* IMPS HAVE NO DECIMAL                                                          
*                                                                               
DAYDEM10 ICM   R0,15,0(R3)                                                      
         BZ    DAYDEM12                                                         
         EDIT  (R0),(7,10(R2))                                                  
*                                                                               
DAYDEM12 LA    R2,8(R2)            NEXT PRINT POSN                              
         LA    R3,4(R3)            NEXT VALUE                                   
         BCT   R4,DAYDEM6                                                       
         J     EXIT                                                             
*                                                                               
         DROP  R8                                                               
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* THIS CODE IS EXACTLY THE SAME AS THE FMTDEM ROUTINE BELOW                     
* BUT FORMATS EVERYTHING FOR 5 CHARACTER DEMO FIELDS IN SCHEDULE                
*===========================================================                    
                                                                                
FMTSKDEM NTR1  BASE=*,LABEL=*                                                   
         MVC   PRTDEMO,SPACES                                                   
         MVC   PRTCPP,SPACES                                                    
*                                                                               
         ICM   R1,15,DEMO                                                       
         BZ    FMTSKDX                                                          
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC                                               
         BZ    FMTSKD2                                                          
         CLI   DEMOTYPE,C'R'       TEST RATING                                  
         BE    FMTSKD10                                                         
         CLI   DEMOTYPE,C'E'                                                    
         BE    FMTSKD10                                                         
         B     FMTSKD6                                                          
*                                                                               
FMTSKD2  CHI   R1,9999             MAX 4 DIGITS                                 
         BNH   FMTSKD4                                                          
* DEMO VALUE MORE THAN 4 DIGITS SO DROP 1 DECIMAL PLACE                         
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         B     FMTSKD6                                                          
*                                                                               
FMTSKD4  EDIT  (R1),(5,PRTDEMO),1                                               
         B     FMTSKD20                                                         
*                                                                               
FMTSKD6  EDIT  (R1),(5,PRTDEMO),0  IMPS HAVE NO DECIMALS                        
         B     FMTSKD20                                                         
                                                                                
*=========================================================                      
* FORMAT 2-DECIMAL DEMO VALUE                                                   
*=========================================================                      
                                                                                
FMTSKD10 CHI   R1,9999             MAX IS 99.99 IN 5 PRINT POSNS                
         BNH   FMTSKD12                                                         
* DEMO VALUE MORE THAN 4 DIGITS SO DROP 1 DECIMAL PLACE                         
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         CHI   R1,9999             TEST FITS WITH 1 DEC NOW                     
         BNH   FMTSKD14                                                         
* DROP ANOTHER DECIMAL PLACE                                                    
         M     R0,=F'2'                                                         
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(5,PRTDEMO),0  ELSE EDIT WITH 0 DECIMALS                    
         B     FMTSKD20                                                         
*                                                                               
FMTSKD12 EDIT  (R1),(5,PRTDEMO),2                                               
         B     FMTSKD20                                                         
*                                                                               
FMTSKD14 EDIT  (R1),(5,PRTDEMO),1                                               
         B     FMTSKD20                                                         
                                                                                
*==========================================================                     
* FORMAT CPP                                                                    
*==========================================================                     
                                                                                
FMTSKD20 L     R1,DOLS             GET DOLLARS                                  
         LHI   R0,20               X 10 X 2                                     
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2_DEC ACTIVE                            
         BZ    FMTSKD22                                                         
         CLI   DEMOTYPE,C'R'                                                    
         BE    *+12                                                             
         CLI   DEMOTYPE,C'E'                                                    
         BNE   FMTSKD22                                                         
         LHI   R0,200                                                           
*                                                                               
FMTSKD22 MR    R0,R0                                                            
         OC    DEMO,DEMO                                                        
         BZ    FMTSKDX                                                          
         D     R0,DEMO             DIVIDE BY UNEQUIV POINTS                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
         C     R1,=F'9999'        MAX CPP WITH CENTS IS 99.99                   
         BNH   FMTSKD24                                                         
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'10000'        /100 * 100                                   
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(5,PRTCPP),FLOAT=$                                          
         B     FMTSKDX                                                          
*                                                                               
FMTSKD24 EDIT (R1),(5,PRTCPP),2                                                 
*                                                                               
FMTSKDX J      EXIT                                                             
         LTORG                                                                  
         EJECT                                                                  
*===========================================================                    
* FORMAT DEMO AND CPP VALUES FOR PRINTING                                       
* INPUT FIELDS ARE DEMO(4),DEMOTYPE(1),DOLS(4)                                  
* OUTPUT FIELDS ARE PRTDEMO(6) AND PRTCPP(6)                                    
*===========================================================                    
         SPACE 1                                                                
FMTDEM   NTR1  BASE=*,LABEL=*                                                   
         MVC   PRTDEMO,SPACES                                                   
         MVC   PRTCPP,SPACES                                                    
*                                                                               
         ICM   R1,15,DEMO                                                       
         BZ    FMTDEMX                                                          
*                                                                               
         CLI   DEMOTYPE,C'R'       TEST RATING                                  
         BE    *+12                                                             
         CLI   DEMOTYPE,C'E'                                                    
         BNE   FMTDEM6                                                          
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DECIMALS ACTIVE                       
         BZ    FMTDEM2                                                          
*                                                                               
         M     R0,=F'2'            DROP 1 DEC PLACE IF 2-DEC ACTIVE             
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
FMTDEM2  C     R1,=F'99999'       MAX WITH DEC IS 9999.9                        
         BNH   FMTDEM4                                                          
* DEMO VALUE MORE THAN 5 DIGITS SO DROP 1 DECIMAL PLACE                         
         M     R0,=F'2'            X 2                                          
         D     R0,=F'10'                                                        
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PRTDEMO),0                                               
         B     FMTDEM10                                                         
*                                                                               
FMTDEM4  EDIT  (R1),(6,PRTDEMO),1                                               
         B     FMTDEM10                                                         
*                                                                               
FMTDEM6  EDIT  (R1),(6,PRTDEMO)    IMPS HAVE NO DECIMALS                        
                                                                                
*==========================================================                     
* FORMAT CPP                                                                    
*==========================================================                     
                                                                                
FMTDEM10 L     R1,DOLS             GET DOLLARS                                  
         LHI   R0,20                                                            
*                                                                               
         TM    RQOPTS,RQOPTS_2DEC  TEST 2-DEC ACTIVE                            
         BZ    FMTDEM12                                                         
         CLI   DEMOTYPE,C'R'                                                    
         BE    *+12                                                             
         CLI   DEMOTYPE,C'E'                                                    
         BNE   FMTDEM12                                                         
         LHI   R0,200                                                           
*                                                                               
FMTDEM12 MR    R0,R0                                                            
*                                                                               
         OC    DEMO,DEMO                                                        
         BZ    FMTDEMX                                                          
         D     R0,DEMO             DIVIDE BY UNEQUIV POINTS                     
         AHI   R1,1                                                             
         SRL   R1,1                                                             
*                                                                               
         C     R1,=F'99999'        MAX CPP WITH CENTS IS 999.99                 
         BNH   FMTDEM14                                                         
         M     R0,=F'200'          X 100 X 2                                    
         D     R0,=F'10000'        /100 * 100                                   
         AHI   R1,1                                                             
         SRL   R1,1                                                             
         EDIT  (R1),(6,PRTCPP)                                                  
         B     FMTDEMX                                                          
*                                                                               
FMTDEM14 EDIT  (R1),(6,PRTCPP),2                                                
*                                                                               
FMTDEMX  J     EXIT                                                             
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
         USING SPJ6WORK,RC                                                      
*                                                                               
         MVC   H2+12(12),QUESTOR                                                
*                                                                               
         CLI   Q2NET,C'Y'          TEST REPORT NET DOLLARS                      
         BNE   *+10                                                             
         MVC   H4+55(21),=CL21'STATION NET DOLLARS'                             
*                                                                               
         MVI   H7,0                FORCE BLANK LINE BEFORE MIDLINES             
*                                                                               
         MVI   MIDNEXT,0           CLEAR MIDHOOK ROUTINE NUMBER                 
         XIT1                                                                   
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
         USING SPJ6WORK,RC                                                      
*                                                                               
         MVC   MID1,SPACES                                                      
         MVC   MID2,SPACES                                                      
*                                                                               
         CLI   RCSUBPRG,1          TEST PRINTING SCHEDULE                       
         BE    MYMIDSK                                                          
*                                                                               
         CLI   RCSUBPRG,6          TEST PRINTING SM2 SUMMARY                    
         BE    MYMIDM2                                                          
*                                                                               
         CLI   RCSUBPRG,7          TEST PRINTING DAILY TOTALS                   
         BE    MYMIDHKX                                                         
*                                                                               
         CLI   MIDNEXT,0           TEST FIRST MIDHOOK CALL                      
         BNE   MYMID10                                                          
*                                                                               
         MVC   MID1+58(16),=C'STATION ANALYSIS'                                 
         CLI   RCSUBPRG,2                                                       
         BE    MYMID2                                                           
*                                                                               
MYMID2   MVI   MID2,0              FORCE SECOND LINE TO PRINT                   
         MVI   MIDHOOK,C'R'        REQUEST RETURN CALL                          
         MVI   MIDNEXT,1           SET NEXT ROUTINE                             
         B     MYMIDHKX                                                         
*                                                                               
MYMID10  CLI   RCSUBPRG,6          TEST SM2 SUMMARY                             
         BE    MYMIDM2                                                          
*                                                                               
         CLI   MIDNEXT,1                                                        
         BNE   MYMIDHKX                                                         
*                                                                               
         MVC   MID1(24),MIDSTA     MOVE STATION ANALYSIS TITLES                 
         MVC   MID2(24),MIDSTA2                                                 
*                                                                               
MYMID20  L     RE,=A(MIDDEMS)                                                   
         MVC   MID1+25(MIDDEMX-MIDDEMS),0(RE)                                   
         L     RE,=A(MIDDEM2)                                                   
         MVC   MID2+25(MIDDEMX-MIDDEMS),0(RE)                                   
*                                                                               
* NOW FILL IN DEMO NAMES                                                        
*                                                                               
         LHI   R0,4                                                             
         LA    R1,MID1+25                                                       
         LA    RE,SVDEMNMS                                                      
         L     RF,SVPRDBUF                                                      
         LA    RF,PTDEMLST-PTBUFFD(RF)                                          
*                                                                               
MYMID22  MVC   3(6,R1),0(RE)       MOVE DEMO NAME (OR SPACES)                   
*                                                                               
         CLC   0(3,RE),SPACES      IF NO NAME, NO DEMO                          
         BH    MYMID24                                                          
         MVC   0(13,R1),SPACES                                                  
         MVC   132(13,R1),SPACES                                                
         B     MYMID26                                                          
*                                                                               
MYMID24  CLI   1(RF),C'R'          TEST RATING                                  
         BE    MYMID26                                                          
         MVC   132(12,R1),=C'(IMP)  (CPM)'                                      
*                                                                               
MYMID26  LA    R1,13(R1)                                                        
         LA    RE,6(RE)                                                         
         LA    RF,3(RF)                                                         
         BCT   R0,MYMID22                                                       
*                                                                               
         MVI   MIDNEXT,0           CLEAR 'NEXT' ROUTINE NUMBER                  
*                                                                               
MYMIDHKX XIT1                                                                   
         EJECT                                                                  
*=================================================================              
* SPECIAL MIDLINES FOR SM2 SUMMARY                                              
*=================================================================              
         SPACE 1                                                                
MYMIDM2  CLI   MIDNEXT,0           TEST FIRST TIME                              
         BNE   MYMIDM2A                                                         
* FIRST MIDLINE                                                                 
         MVC   MID1+53(26),=C'BRAND PERFORMANCE ANALYSIS'                       
         MVI   MID2,0              FORCE A BLANK LINE AFTER                     
         MVI   MIDHOOK,C'R'                                                     
         MVI   MIDNEXT,1                                                        
         B     MYMIDHKX                                                         
*                                                                               
MYMIDM2A CLI   MIDNEXT,1                                                        
         BNE   MYMIDM2B                                                         
         MVC   MID1(MIDSM2AX-MIDSM2A),MIDSM2A                                   
         MVC   MID2(MIDSM2BX-MIDSM2B),MIDSM2B                                   
*                                                                               
         MVC   MID2+27(6),SVDEMNMS   SET FIRST DEMO NAME IN GOALS               
*                                                                               
         MVC   MID2+93(6),SVDEMNMS+6                                            
         CLI   SVDEMNMS+6,C' '                                                  
         BH    *+10                                                             
         MVC   MID2+91(10),SPACES                                               
*                                                                               
         MVC   MID2+107(6),SVDEMNMS+12                                          
         CLI   SVDEMNMS+12,C' '                                                 
         BH    *+10                                                             
         MVC   MID2+105(10),SPACES                                              
*                                                                               
         MVC   MID2+121(6),SVDEMNMS+18                                          
         CLI   SVDEMNMS+18,C' '                                                 
         BH    *+10                                                             
         MVC   MID2+119(10),SPACES                                              
*                                                                               
         MVI   MIDHOOK,C'R'                                                     
         MVI   MIDNEXT,2                                                        
         B     MYMIDHKX                                                         
*                                                                               
MYMIDM2B MVC   MID1(MIDSM2CX-MIDSM2C),MIDSM2C                                   
*                                                                               
         LA    R1,SVDEMNMS+6       STARTS WITH SECOND DEMO                      
         LA    R0,3                                                             
         LA    RE,MID1+90                                                       
*                                                                               
MYMIDM2C MVC   0(11,RE),=C'(RTG)   CPP'                                         
         CLI   0(R1),C'R'          TEST RATING                                  
         BE    *+10                                                             
         MVC   0(11,RE),=C'(IMP)   CPM'                                         
         CLI   0(R1),C' '                                                       
         BH    *+10                                                             
         MVC   0(11,RE),SPACES                                                  
         LA    R1,6(R1)                                                         
         LA    RE,14(RE)                                                        
         BCT   R0,MYMIDM2C                                                      
*                                                                               
         MVC   MID2(MIDSM2AX-MIDSM2A),MIDSM2A  REPEAT UNDERLINES                
         B     MYMIDHKX                                                         
         EJECT                                                                  
*=================================================================              
* SPECIAL MIDLINES FOR SCHEDULE                                                 
*=================================================================              
         SPACE 1                                                                
MYMIDSK  CLI   MIDNEXT,0           TEST FIRST TIME                              
         BNE   MYSK10                                                           
* FIRST MIDLINE                                                                 
         LA    RE,MID1+SKGRID+1-P                                               
         MVC   0(56,RE),MYDATES    PRINT MO TU WE ...                           
         MVI   MIDHOOK,C'R'                                                     
         MVI   MIDNEXT,1                                                        
         B     MYMIDHKX                                                         
*                                                                               
MYSK10   CLI   MIDNEXT,1                                                        
         BNE   MYSK20                                                           
         MVC   MID1(37),MIDSKD     DAYS/TIMES ...                               
         LA    RE,MID1+(SKSPOTS-P)                                              
         MVC   0(3,RE),=C'TOT'                                                  
         LA    RE,MID1+(SKCOST-P)                                               
         MVC   0(8,RE),=C'--COST--'                                             
*                                                                               
         LA    RE,MID1+SKGRID+1-P                                               
         MVC   0(56,RE),MYDATES+L'MYDATES                                       
         MVC   132(56,RE),MYDATES+2*L'MYDATES                                   
* NOW FILL IN DEMO NAMES AND TYPES                                              
         LHI   R0,4                                                             
         LA    R1,MID1+SKDEMS-P                                                 
         LA    RE,SVDEMNMS                                                      
         L     RF,SVPRDBUF                                                      
         LA    RF,PTDEMLST-PTBUFFD(RF)                                          
*                                                                               
MYSK12   MVC   0(6,R1),0(RE)       MOVE DEMO NAME (OR SPACES)                   
         MVC   6+132(6,R1),6(RE)                                                
         MVC   12(6,R1),12(RE)                                                  
         MVC   18+132(6,R1),18(RE)                                              
*                                                                               
         MVI   MIDNEXT,2                                                        
         MVI   MIDHOOK,C'R'        REQUEST RETURN                               
         B     MYMIDHKX                                                         
*                                                                               
MYSK20   CLI   MIDNEXT,2                                                        
         BNE   MYSK30                                                           
         MVC   MID1(8),=C'STATION:'                                             
         MVC   MID1+9(9),BIGSTA                                                 
         L     RE,ADSTAT                                                        
         USING STARECD,RE                                                       
         MVC   MID1+22(3),SNETWRK                                               
         DROP  RE                                                               
*                                                                               
         MVI   MIDNEXT,0                                                        
         B     MYMIDHKX                                                         
*                                                                               
MYSK30   DC    H'0'                                                             
*                                                                               
MIDHKR9  DS    A                                                                
MIDHKRA  DS    A                                                                
         DS    A                   *** NOT USED ***                             
MIDHKRC  DS    A                                                                
MIDNEXT  DS    X                                                                
         LTORG                                                                  
         DROP  RB,RC                                                            
         EJECT                                                                  
SPJ6WORK DS    0D                  THIS AREA ADDRESSED BY RC                    
*                                                                               
BUFFERIN DC    V(BUFFERIN)                                                      
                                                                                
BUFFET   BUFFD TYPE=B,KEYLEN=L'BFKEY,COLUMNS=BFDATAN,FILE=BUFFWK,      *        
               BUFFERS=10                                                       
*                                                                               
SAVERE   DS    A                   SAVED RE VALUE                               
SVPRDBUF DS    A                   PRDBUFF ENTRY FOR THIS PRD                   
SVRERATE DS    A                                                                
ASORTNXT DS    A                                                                
SORTCNT  DS    A                   SO GOTO1 DOES MVC                            
BUFFCNT  DS    F                                                                
GOALCPP  DS    F                                                                
EQDEMO   DS    F                                                                
DEMO     DS    F                                                                
DOLS     DS    F                                                                
SDPTCNT  DS    H                                                                
TDPTCNT  DS    H                                                                
*                                                                               
DETOPTS  DS    0CL4                                                             
DETOPDEM DS    CL1                                                              
DETOPFLG DS    CL1                                                              
DETOPCPP DS    CL1                                                              
DETOPCOS DS    CL1                                                              
*                                                                               
SLNCNT   DS    H                                                                
ONESLN   DS    C                                                                
PREVTDPT DS    CL3                                                              
PREVSDPT DS    CL3                                                              
DEMOTYPE DS    C                                                                
TOTSW    DS    C                                                                
TOTCHAR  DS    C                                                                
PASS     DS    C                                                                
PRTTOT   DS    C                                                                
ELCDLO   DC    X'00'                                                            
ELCDHI   DC    X'00'                                                            
PRTDEMO  DS    CL6                                                              
PRTCPP   DS    CL6                                                              
SVNUMDEM DS    H                                                                
PSLIST   DS    XL64                                                             
SVDEMNMS DS    8CL6                DEMO NAMES                                   
SD6APROF DS    CL16                                                             
         DS    0D                                                               
TOTSPOTS DS    0XL28               14 2-BYTE ACCUMS                             
         ORG   TOTSPOTS                                                         
         DS    14XL2                                                            
*                                                                               
SVQSTART DS    CL12                                                             
PERTOTS  DS    6F                  SAVE AREA FOR PERIOD MARKET TOTALS           
PASSTAB  DS    8CL12               MAX 7 YYMMDD START/END DATES                 
*                                                                               
         DS    0D                                                               
STATOTS  DS    0XL64               STATION DAILY TOTALS                         
STASPTS  DS    16F                                                              
STADOLS  DS    16F                                                              
STADEM1  DS    16F                                                              
STADEM2  DS    16F                                                              
STADEM3  DS    16F                                                              
STADEM4  DS    16F                                                              
STATOTX  EQU   *                                                                
         DS    0D                                                               
MKTTOTS  DS    0XL64               MARKET DAILY TOTALS                          
MKTSPTS  DS    16F                                                              
MKTDOLS  DS    16F                                                              
MKTDEM1  DS    16F                                                              
MKTDEM2  DS    16F                                                              
MKTDEM3  DS    16F                                                              
MKTDEM4  DS    16F                                                              
MKTTOTX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
MYDATES  DS    0XL56                                                            
         DS    14CL4               3 BYTE DAY + SPACE                           
         DS    14CL4               3 BYTE MONTH + SPACE                         
         DS    14CL4               SPACE + 2 BYTE DATE + SPACE                  
MYDATEX  EQU   *                                                                
                                                                                
BFREC    DS    0D                  ** BUFFERIN RECORD **                        
BFKEY    DS    0XL8                ** BUFFERIN RECORD KEY **                    
                                                                                
BFTYPE   DS    XL1                 ** REPORT TYPE **                            
BFKEOFQ  EQU   X'FF'               END OF FILE FLAG                             
                                                                                
BFKDATA  DS    0X                  ** KEY DATA **                               
                                                                                
BFKSQ    EQU   0                   SCHEDULE SORT DATA                           
BFKSDAYS DS    XL1                 BDSEDAY                                      
BFKSTIME DS    XL4                 START/END TIME (+2400 IF <600)               
                                                                                
         ORG   BFKDATA                                                          
BFK1Q    EQU   1                   STATION ANALYSIS REPORT                      
BFK1STA  DS    XL3                 STATION (X'FFFFFFF'=TOT)                     
                                                                                
         ORG   BFKDATA                                                          
BFK2Q    EQU   2                   DAYPART ANALYSIS                             
BFK2DPT  DS    CL3                 DAYPART CODE (X'FFFFFF'=TOT)                 
                                                                                
         ORG   BFKDATA                                                          
BFK3Q    EQU   3                   SPOTLENGTH ANALYSIS                          
BFK3SLN  DS    XL1                 SPOTLEN (X'FF'=TOT)                          
                                                                                
         ORG   BFKDATA                                                          
BFK4Q    EQU   4                   DPT/SLN ANALYSIS                             
BFK4DPT  DS    CL3                 DAYPART (X'FFFFFF'=TOT)                      
BFK4SLN  DS    XL1                 SPOTLEN (X'FF'=TOT)                          
                                                                                
* FOLLOWING REPRODUCES SM2 REPORT                                               
         ORG   BFKDATA                                                          
BFK5Q    EQU   5                   SM2 ANALYSIS                                 
BFK5TDPT DS    CL3                 TOT DPT (X'FFFFFF'=TOT) MEDDPGRP             
BFK5SDPT DS    CL3                 SUB DPT (X'FFFFFF'=TOT) MEDDPART             
BFK5SLN  DS    XL1                 SPOTLEN (X'FF'=TOT)                          
                                                                                
         ORG   BFREC+L'BFKEY                                                    
BFDATA   DS    0XL4                ** BUFFERIN RECORD DATA **                   
BFSPOTS  DS    XL4                 SPOTS                                        
BFBDOL   DS    XL4                 BUY DOLLARS (PENNIES)                        
BFBEQDOL DS    XL4                 BUY EQ DOLLARS                               
BFGDOL   DS    XL4                 GOAL DOLLARS (PENNIES)                       
BFGEQDOL DS    XL4                 GOAL EQ DOLS                                 
BFBDEM1  DS    XL4                 BUY DEMO                                     
BFBEQDM1 DS    XL4                 BUY EQ DEMO                                  
BFBDEM2  DS    XL4                 BUY DEMO                                     
BFBEQDM2 DS    XL4                 BUY EQ DEMO                                  
BFBDEM3  DS    XL4                 BUY DEMO                                     
BFBEQDM3 DS    XL4                 BUY EQ DEMO                                  
BFBDEM4  DS    XL4                 BUY DEMO                                     
BFBEQDM4 DS    XL4                 BUY EQ DEMO                                  
BFGDEM   DS    XL4                 GOAL DEMO                                    
BFGEQDEM DS    XL4                 GOAL EQ DEMO                                 
BFDATAL  EQU   *-BFDATA            L'DATA                                       
BFDATAN  EQU   BFDATAL/4                                                        
                                                                                
BFRECL   EQU   *-BFREC             L'BUFFERIN RECORD                            
                                                                                
COMLINEN DS    XL1                 N'COMMENT LINES TO PRINT                     
COMLINEP DS    XL1                 N'COMMENT LINES PRINTED                      
COMLINEM EQU   14                  MAXIMUM N'COMMENT LINES                      
COMLINES DS    (COMLINEM)CL70      COMMENTS                                     
COMLINEL EQU   *-COMLINES          L'COMMENT BLOCK                              
*                                                                               
MIDSKD   DC    CL37'-DAYS-  ---TIME--- --PROGRAMMING--SLN'                      
MIDSTA   DC    C'STAT   SPOTS  -- COST --'                                      
MIDSTA2  DC    C'----   -----  ----------'                                      
*                                                                               
MIDDEMS  DC    C'---XXXXXX--- ---XXXXXX--- ---XXXXXX--- ---XXXXXX---'           
MIDDEMX  EQU   *                                                                
*                                                                               
MIDDEM2  DC    C'(RTG)  (CPP) (RTG)  (CPP) (RTG)  (CPP) '                       
         DC    C'(RTG)  (CPP) (RTG)  (CPP) (RTG)  (CPP) '                       
MIDDEM2X EQU   *                                                                
*                                                                               
MIDDEM3  DC    C'------------ ------------ ------------ ------------'           
MIDDEM3X EQU   *                                                                
*                                                                               
* MIDLINES BELOW FOR SM2 SUMMARY                                                
*                                                                               
MIDSM2A  EQU   *                                                                
MIDSM2A1 DC    C'-----------------------------------     '                      
MIDSM2A2 DC    C'-------------------------------------------------'             
MIDSM2A3 DC    C' ---------------------------------------'                      
MIDSM2AX EQU   *                                                                
*                                                                               
MIDSM2B  EQU   *                                                                
MIDSM2B1 DC    C'              --  GOAL -- (XXXXXX)    '                        
MIDSM2B2 DC    C'----- PURCHASED -----          AVG    PCT   PCT '              
MIDSM2B3 DC    C'     --XXXXXX--    --XXXXXX--    --XXXXXX--'                   
MIDSM2BX EQU   *                                                                
*                                                                               
MIDSM2C  EQU   *                                                                
MIDSM2C1 DC    C'DAYPART-LEN   POINTS DOLLARS    CPP   '                        
MIDSM2C2 DC    C'POINTS DOLLARS    CPP  SPOTS   PTS    PTS   DOLS'              
MIDSM2C3 DC    C'    PTS     CPP   PTS     CPP   PTS     CPP'                   
MIDSM2CX EQU   *                                                                
*                                                                               
SVMKTOTS DS    0D                                                               
SVMKTOT1 DS    112F                PASS 1 SAVE AREA                             
SVMKTOT2 DS    112F                PASS 2 SAVE AREA                             
SVMKTOT3 DS    112F                PASS 3 SAVE AREA                             
SVMKTOT4 DS    112F                PASS 4 SAVE AREA                             
SVMKTOT5 DS    112F                PASS 5 SAVE AREA                             
SVMKTOT6 DS    112F                PASS 6 SAVE AREA                             
SVMKTOT7 DS    112F                PASS 7 SAVE AREA                             
SVMKTOTX EQU   *                                                                
*                                                                               
SVMYDTS  DS    0D                                                               
SVMYDT1  DS    3XL56               PASS 1 SAVE AREA                             
SVMYDT2  DS    3XL56               PASS 2 SAVE AREA                             
SVMYDT3  DS    3XL56               PASS 3 SAVE AREA                             
SVMYDT4  DS    3XL56               PASS 4 SAVE AREA                             
SVMYDT5  DS    3XL56               PASS 5 SAVE AREA                             
SVMYDT6  DS    3XL56               PASS 6 SAVE AREA                             
SVMYDT7  DS    3XL56               PASS 7 SAVE AREA                             
SVMYDTX  EQU   *                                                                
*                                                                               
         DS    0D                                                               
         DC    CL8'SORTBUFF'                                                    
SORTBUFF DS    1000XL12                                                         
SORTBUFX EQU   *-1                                                              
                                                                                
         PRINT OFF                                                              
       ++INCLUDE DDBUFFD                                                        
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPMEDBLOCK                                                     
       ++INCLUDE SPREPPTBUF                                                     
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
ESTHDRD  DSECT                                                                  
       ++INCLUDE SPGENEST                                                       
GOALRECD DSECT                                                                  
       ++INCLUDE SPGENGOAL                                                      
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
DBLOCKD  DSECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
SPWORKD  DSECT                                                                  
         ORG   P                                                                
PSTA     DS    CL4                                                              
         ORG   PSTA                                                             
PDPT     DS    CL3                                                              
         ORG   PSTA                                                             
PSLN     DS    CL3                                                              
         ORG   PSTA+7                                                           
PSPOTS   DS    CL5                                                              
         DS    CL1                                                              
PDOLS    DS    CL10                                                             
         DS    CL1                                                              
PDEM1    DS    CL5                                                              
         DS    CL1                                                              
PCPP1    DS    CL6                                                              
         DS    CL1                                                              
PDEM2    DS    CL5                                                              
         DS    CL1                                                              
PCPP2    DS    CL6                                                              
         DS    CL1                                                              
PDEM3    DS    CL5                                                              
         DS    CL1                                                              
PCPP3    DS    CL6                                                              
         DS    CL1                                                              
PDEM4    DS    CL5                                                              
         DS    CL1                                                              
PCPP4    DS    CL6                                                              
         DS    CL1                                                              
PDEM5    DS    CL5                                                              
         DS    CL1                                                              
PCPP5    DS    CL6                                                              
         DS    CL1                                                              
PDEM6    DS    CL5                                                              
         DS    CL1                                                              
PCPP6    DS    CL6                                                              
         DS    CL1                                                              
PDEM7    DS    CL5                                                              
         DS    CL1                                                              
PCPP7    DS    CL6                                                              
         DS    CL1                                                              
PDEM8    DS    CL5                                                              
         DS    CL1                                                              
PCPP8    DS    CL6                                                              
         DS    CL1                                                              
*=====================                                                          
         ORG   P                                                                
SKDAYS   DS    CL7                                                              
         DS    CL1                                                              
SKTIME   DS    CL11                                                             
         DS    CL1                                                              
SKPROG   DS    CL14                                                             
         DS    CL1                                                              
SKSLN    DS    CL2                                                              
         DS    CL1                                                              
SKGRID   DS    14CL4                                                            
         DS    CL1                                                              
SKSPOTS  DS    CL3                                                              
         DS    CL2                                                              
SKCOST   DS    CL7                                                              
         DS    CL1                                                              
SKDEMS   DS    4CL6                                                             
*========                                                                       
         SPACE 1                                                                
* DSECT FOR SM2 PRINT LINES                                                     
         ORG   P                                                                
SM2TDPT  DS    CL3                                                              
         DS    CL1                                                              
SM2SDPT  DS    CL3                                                              
         DS    CL1                                                              
SM2SLN   DS    CL3                                                              
         DS    CL3                                                              
SM2GPTS  DS    CL6                                                              
         DS    CL1                                                              
SM2GDOLS DS    CL7                                                              
         DS    CL1                                                              
SM2GCPP  DS    CL6                                                              
         DS    CL3                                                              
SM2PPTS  DS    CL6                                                              
         DS    CL1                                                              
SM2PDOLS DS    CL7                                                              
         DS    CL1                                                              
SM2PCPP  DS    CL6                                                              
         DS    CL2                                                              
SM2SPOTS DS    CL5                                                              
         DS    CL1                                                              
SM2AVPTS DS    CL5                                                              
         DS    CL1                                                              
SM2PCPTS DS    CL6                                                              
         DS    CL1                                                              
SM2PCDOL DS    CL6                                                              
         DS    CL2                                                              
SM2DM2PT DS    CL6                 POINTS                                       
         DS    CL1                                                              
SM2DM2CP DS    CL6                 CPP                                          
         DS    CL1                                                              
SM2DM3PT DS    CL6                                                              
         DS    CL1                                                              
SM2DM3CP DS    CL6                                                              
         DS    CL1                                                              
SM2DM4PT DS    CL6                                                              
         DS    CL1                                                              
SM2DM4CP DS    CL6                                                              
*                                                                               
         ORG   QAREA2+36                                                        
Q2NET    DS    CL1                 REPORT NET DOLLARS                           
         DS    0D                                                               
         SPACE 1                                                                
*==============================================================                 
* DSECT FOR DAILY TOTALS AREA (FOR STATION AND MARKET)                          
*==============================================================                 
         SPACE                                                                  
DLYTOTD  DSECT                                                                  
DLYTOTS  DS    0XL64                                                            
DLYSPTS  DS    16F                                                              
DLYDOLS  DS    16F                                                              
DLYDEM1  DS    16F                                                              
DLYDEM2  DS    16F                                                              
DLYDEM3  DS    16F                                                              
DLYDEM4  DS    16F                                                              
DLYTOTX  EQU   *                                                                
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'023SPREPJ602 11/02/06'                                      
         END                                                                    
