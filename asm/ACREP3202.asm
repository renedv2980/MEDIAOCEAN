*          DATA SET ACREP3202  AT LEVEL 067 AS OF 05/01/02                      
*PHASE AC3202A,+0                                                               
         SPACE 3                                                                
*              PROFILES                                                         
         SPACE 2                                                                
* 1            CLIENT PAGE BREAK IN PRODUCT SUMMARY                             
* 2            CLIENT PAGE BREAK IN CLIENT SUMMARY                              
* 3            NEW PAGE PER CLIENT IN MAIN REPORT                               
* 4            PRINT SUMMARY BASED ON NUMBER IN PRODUCT RECORD                  
         TITLE 'SALES ANALYSIS REPORT'                                          
AC3202   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC32**,R9,RR=R5                                              
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING AC32D,RC                                                         
         ST    R5,PRELOC                                                        
         SPACE 1                                                                
*&&US                                                                           
DDS      EQU   X'FF'                                                            
*&&                                                                             
*&&UK                                                                           
DDS      EQU   X'FF'                                                            
*&&                                                                             
         EJECT                                                                  
         CLI   MODE,REQFRST                                                     
         BNE   INC20                                                            
         ZAP   TBLMAX,=P'0'                                                     
         MVC   CLINE,=CL20'**TOTAL FOR CLIENT**'                                
         MVC   PRLINE,=CL20'*TOTAL FOR PRODUCT*'                                
         CLI   QCOMPANY,DDS                                                     
         B     *+16                                                             
         MVC   CLINE,=CL20'**TOTAL FOR GROUP**'                                 
         MVC   PRLINE,=CL20'*TOTAL FOR AGENCY*'                                 
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   FCRDTRNS,C'Y'       SETUP TO READ TRANS ONLY                     
         MVI   FCRDHIST,C'N'       NOT HISTORIES                                
         CLC   QPROG,=C'33'                                                     
         BE    INC12                                                            
         MVI   RCSUBPRG,0                                                       
         CLI   QOPT1,C'Y'          DETAILS                                      
         BE    *+8                                                              
         MVI   RCSUBPRG,1          NO DETAILS                                   
         SPACE 1                                                                
         MVC   BTCHM(1),QSTART+1                                                
         MVC   BTCHM+1(1),QSTART+3                                              
         CLC   QSTART+2(2),=C'10'                                               
         BL    INC10                                                            
         MVI   BTCHM+1,C'A'                                                     
         CLC   QSTART+2(2),=C'11'                                               
         BL    INC10                                                            
         MVI   BTCHM+1,C'B'                                                     
         CLC   QSTART+2(2),=C'12'                                               
         BL    INC10                                                            
         MVI   BTCHM+1,C'C'                                                     
         SPACE 1                                                                
INC10    MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,HDATE)                                   
         B     INC14                                                            
         SPACE 1                                                                
INC12    MVI   RCSUBPRG,5          SET-UP FOR HISTORICAL REPORT                 
         MVI   FCRDHIST,C'Y'       NOW SETUP TO READ HISTORIES ONLY             
         MVI   FCRDTRNS,C'N'       NOT TRANSACTIONS                             
         MVC   WORK(4),QSTART                                                   
         MVC   WORK+4(2),=C'01'                                                 
         GOTO1 DATCON,DMCB,(0,WORK),(6,HFROM)                                   
         GOTO1 (RF),(R1),,(1,YMFROM)                                            
         SPACE 1                                                                
         MVC   WORK(4),QEND                                                     
         GOTO1 (RF),(R1),,(6,HTO)                                               
         GOTO1 (RF),(R1),,(1,YMTO)                                              
         SPACE 1                                                                
INC14    L     R7,=A(BUFFALOC)                                                  
         A     R7,PRELOC                                                        
         ST    R7,ABUFF                                                         
         GOTO1 BUFFALO,DMCB,=C'SET',ABUFF                                       
         MVC   BUFFOFF,SPACES                                                   
         SPACE 1                                                                
         L     R7,=A(MEDTBL)                                                    
         A     R7,PRELOC                                                        
         ST    R7,AMEDT                                                         
         XC    MEDCNT,MEDCNT                                                    
         XC    PGCNT,PGCNT                                                      
         ZAP   PGSGR,=P'0'                                                      
         ZAP   PGSCM,=P'0'                                                      
         SPACE 1                                                                
         LA    R3,8                                                             
         LA    R4,CLIGR                                                         
         ZAP   0(8,R4),=P'0'                                                    
         LA    R4,8(R4)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         LA    R3,4                                                             
         LA    R4,DETLS                                                         
         ZAP   0(3,R4),=P'0'                                                    
         LA    R4,3(R4)                                                         
         BCT   R3,*-10                                                          
         SPACE 1                                                                
         MVC   SAVEKEY,SPACES                                                   
         MVC   SAVEKEY(15),KEY                                                  
         L     R4,=A(ACCBUFF)                                                   
         A     R4,PRELOC                                                        
         MVC   0(42,R4),SPACES                                                  
         MVC   0(1,R4),QCOMPANY                                                 
         MVC   1(2,R4),=C'SJ'                                                   
         CLI   QCOMPANY,DDS                                                     
         BNE   *+10                                                             
         MVC   1(2,R4),=C'SR'                                                   
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         MVI   ELCODE,ACHRELQ                                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING ACHEIRD,R4                                                       
         MVC   CLILEN,ACHRLEVA                                                  
         MVC   PRODLEN,ACHRLEVB                                                 
         CLI   QCOMPANY,DDS                                                     
         BNE   *+16                                                             
         MVC   CLILEN,ACHRLEVB                                                  
         MVC   PRODLEN,ACHRLEVC                                                 
         L     R4,=A(ACCBUFF)                                                   
         A     R4,PRELOC                                                        
         L     R3,=A(CPOFFTAB)                                                  
         A     R3,PRELOC                                                        
         MVI   0(R3),X'FF'                                                      
         CLI   QOPT5,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,CPOFFBLD         BUILD CLIENT/PRODUCT OFFICE TABLE            
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',SAVEKEY,(R4)                 
         B     INCXIT                                                           
         EJECT                                                                  
INC20    CLI   MODE,PROCACC                                                     
         BNE   INC30                                                            
         MVI   FORCEHED,C'Y'                                                    
         MVC   ACCT,SPACES                                                      
         MVC   ACCTNM,SPACES                                                    
         SPACE 1                                                                
         L     R3,ADACC                                                         
         MVC   ACCT,3(R3)                                                       
         L     R3,ADACCNAM                                                      
         LA    R5,ACCTNM                                                        
         BAS   RE,NAMOUT                                                        
         SPACE 1                                                                
         MVC   BUFFMED,ACCT        MEDIA TO BUFFALO                             
         MVC   SBACCT,SPACES                                                    
         B     INCXIT                                                           
         EJECT                                                                  
INC30    CLI   MODE,SBACFRST                                                    
         BNE   INC40                                                            
         L     R3,ADSUBAC                                                       
         USING TRSUBHD,R3                                                       
         MVC   SUBULSV,TRSBACNT+1                                               
         MVC   PGSPROD,TRSBACNT+3                                               
         ZAP   PGSGR,=P'0'                                                      
         ZAP   PGSCM,=P'0'                                                      
         SPACE 1                                                                
         CLC   SBACCT,SPACES       FIRST FOR ACCOUNT                            
         BE    INC35                                                            
         ZIC   RF,CLILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         CLC   SBACCT(0),TRSBACNT+3                                             
         BE    INC35                                                            
         SPACE 1                                                                
         BAS   RE,TOTCLI           DO CLIENT TOTAL                              
         SPACE 1                                                                
INC35    DS    0H                                                               
         MVC   SBACCT,SPACES                                                    
         CLC   TRSBACNT+1(2),=C'SJ'                                             
         BE    INC36                                                            
         CLC   TRSBACNT+1(2),=C'SR'                                             
         BE    INC36                                                            
         CLC   TRSBACNT+1(2),=C'SM'                                             
         BNE   INCXIT                                                           
INC36    ZIC   RF,CLILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                SAVE CLIENT                                  
         MVC   SBACCT(0),TRSBACNT+3                                             
         IC    RF,PRODLEN                                                       
         ZIC   R1,CLILEN                                                        
         SR    RF,R1                                                            
         LA    RE,TRSBACNT+3(R1)                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                SAVE PRODUCT                                 
         MVC   SBACCT+6(0),0(RE)                                                
         MVC   BUFFCLI,SBACCT      CLIENT                                       
         MVC   BUFFPRD,SBACCT+6    AND PRODUCT TO BUFFALO                       
         ZAP   BUFFGR,=P'0'                                                     
         ZAP   BUFFCM,=P'0'                                                     
         ZIC   R1,TRSBLEN                                                       
         MVC   PRODNAME,SPACES                                                  
         SH    R1,=H'18'                                                        
         BM    INCXIT                                                           
         EX    R1,*+8                                                           
         B     INCXIT                                                           
         MVC   PRODNAME(0),TRSBNAME                                             
         EJECT                                                                  
INC40    CLI   MODE,PROCTRNS                                                    
         BE    INC400              WE HAVE A TRANSACTION, CARRY ON              
         CLI   MODE,PROCHIST       NO, DO WE HAVE A HISTORY ?                   
         BNE   INC58               NO, SEE IF SUB-ACCT LAST                     
INC400   L     R3,ADSUBAC          YES, DO FOR HISTORY OR TRAN                  
         USING TRSUBHD,R3                                                       
         CLC   TRSBACNT+1(2),=C'SJ'                                             
         BE    INC40A                                                           
         CLC   TRSBACNT+1(2),=C'SR'                                             
         BE    INC40A                                                           
         CLC   TRSBACNT+1(2),=C'SM'                                             
         BNE INCXIT                                                             
INC40A   DS    0H                                                               
         CLC   QPROG,=C'33'                                                     
         BE    INC50                                                            
         L     R3,ADTRANS                                                       
         USING TRANSD,R3                                                        
         CLI   TRNSEL,TRNSELQ                                                   
         BNE   INCXIT                                                           
         SPACE 1                                                                
         CLC   BTCHM,TRNSBTCH                                                   
         BNE   INCXIT                                                           
         ZAP   GROSS,TRNSAMNT                                                   
         ZAP   COMM,TRNSAMNT                                                    
         MVI   ELCODE,X'50'                                                     
         SPACE 1                                                                
         LR    R4,R3                                                            
INC40C   BAS   RE,NEXTEL                                                        
         BNE   INC41                                                            
         USING TRCASHD,R4                                                       
         CLI   TRCSTYPE,C'G'       G BUCKETS ONLY                               
         BNE   INC40C                                                           
         ZAP   GROSS,TRCSAMNT                                                   
         B     INC42                                                            
         SPACE 1                                                                
INC41    CLI   TRNSTYPE,8                                                       
         BE    INC41G                                                           
         CLI   TRNSTYPE,21                                                      
         BE    INC41G                                                           
*&&US                                                                           
         CLI   TRNSTYPE,0          BUG IN BILLING                               
         BE    INC41G                                                           
         CLI   TRNSTYPE,6                                                       
         BE    INC41G                                                           
         CLI   TRNSTYPE,7                                                       
         BE    INC41G                                                           
         CLI   TRNSTYPE,9                                                       
         BE    INC41G                                                           
*&&                                                                             
         B     INC42                                                            
INC41G   ZAP   GROSS,=P'0'                                                      
         SPACE 1                                                                
INC42    AP    BUFFGR,GROSS                                                     
         AP    BUFFCM,COMM                                                      
         AP    PGSGR,GROSS                                                      
         AP    PGSCM,COMM                                                       
         AP    DETLS,=P'1'         COUNT TRANSACTIONS                           
         CLI   RCSUBPRG,0          PRINT DETAILS                                
         BNE   INCXIT              NO                                           
         SPACE 1                                                                
         CP    DETLS,=P'1'         1ST TIME FOR PROD                            
         BH    INC45               NO                                           
         MVC   P+3(6),BUFFCLI                                                   
         MVC   P+13(6),BUFFPRD                                                  
         SPACE 1                                                                
INC45    MVC   P+21(6),TRNSREF                                                  
         GOTO1 DATCON,DMCB,(1,TRNSDATE),(5,P+30)                                
         SPACE 1                                                                
         LA    R5,GROSS                                                         
         BAS   RE,FORMAT                                                        
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         B     INCXIT                                                           
         EJECT                                                                  
*              PROCTRNS FOR HISTORICAL REPORT                                   
         SPACE 2                                                                
INC50    DS    0H                                                               
         L     R3,ADTRANS                                                       
         USING TRHISTD,R3                                                       
         CLI   TRHSEL,X'45'                                                     
         BNE   INCXIT                                                           
         CLC   TRHSYEAR(2),YMFROM  DATE MATCH                                   
         BL    INCXIT                                                           
         CLC   TRHSYEAR(2),YMTO                                                 
         BH    INCXIT                                                           
         LA    RF,BUFFGR           GROSS OR INCOME BUCKET                       
         LA    RE,PGSGR                                                         
         CLI   BUCKTYPE,C'G'                                                    
         BE    INC52                                                            
         CLI   BUCKTYPE,C' '                                                    
         BNE   INCXIT                                                           
         LA    RF,BUFFCM                                                        
         LA    RE,PGSCM                                                         
INC52    AP    0(8,RF),TRHSCR                                                   
         AP    0(6,RE),TRHSCR                                                   
         AP    DETLS,=P'1'                                                      
         B     INCXIT                                                           
         EJECT                                                                  
INC58    CLI   MODE,SBACLAST                                                    
         BNE   INC60                                                            
         CLC   SBACCT,SPACES       NO CONTRA A/C ACTIVITY                       
         BE    INCXIT                                                           
         BAS   RE,TOTPRD           PRODUCT TOTAL                                
         CLI   PROGPROF+3,C'Y'     PROFILE FOR PROD GROUP SUMMARY               
         BE    INC59                                                            
         ZAP   BUFFGR,=P'0'                                                     
         ZAP   BUFFCM,=P'0'                                                     
         B     INCXIT                                                           
         SPACE 1                                                                
INC59    DS    0H                                                               
         CLC   SUBULSV,=C'SJ'                                                   
         BNE   INCXIT                                                           
         CP    PGSGR,=P'0'                                                      
         BNE   INC59A                                                           
         CP    PGSCM,=P'0'                                                      
         BE    INCXIT                                                           
INC59A   L     R4,PGCNT            NUMBER IN TABLE                              
         L     R7,=A(PGTBL)                                                     
         A     R7,PRELOC           ADDRESS                                      
         L     R5,0(R7)            MAX                                          
         LA    R7,4(R7)            DATA                                         
         GOTO1 BINSRCH,DMCB,(1,PGSPROD),(R7),(R4),20,(0,8),(R5)                 
         MVC   PGCNT,DMCB+8                                                     
         OC    DMCB(4),DMCB                                                     
         BNE   *+6                                                              
         DC    H'0'                TABLE FULL                                   
         ZAP   PGSGR,=P'0'                                                      
         ZAP   PGSCM,=P'0'                                                      
         ZAP   BUFFGR,=P'0'                                                     
         ZAP   BUFFCM,=P'0'                                                     
         B     INCXIT                                                           
         EJECT                                                                  
INC60    CLI   MODE,ACCLAST                                                     
         BNE   INC70                                                            
         BAS   RE,TOTCLI                                                        
         AP    LEVGR,MEDGR                                                      
         AP    LEVCM,MEDCM                                                      
         CP    CLILS,=P'0'         CLIENT LINES                                 
         BE    INC65                                                            
         SPACE 1                                                                
         ZAP   CLILS,=P'0'                                                      
         MVC   MEDACCT,ACCT        PUT TOTALS IN MEDIA TABLE                    
         MVC   MEDNAME,ACCTNM                                                   
         ZAP   MEDTGR,MEDGR                                                     
         ZAP   MEDTCM,MEDCM                                                     
         SPACE 1                                                                
         BAS   RE,BINADD           ADD TO MEDIA TOTALS                          
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT           SKIP LINE                                    
         SPACE 1                                                                
         MVC   P+11(9),=C'TOTAL FOR'                                            
         MVC   P+21(36),ACCTNM                                                  
         LA    R5,MEDGR                                                         
         BAS   RE,FORMAT                                                        
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         AP    MEDLS,=P'1'                                                      
         SPACE 1                                                                
INC65    ZAP   MEDGR,=P'0'                                                      
         ZAP   MEDCM,=P'0'                                                      
         CLI   PROGPROF+3,C'Y'                                                  
         BNE   INCXIT                                                           
         L     R7,=A(PGTBL)        BUILD BUFFALO RECORD FROM TABLE              
         A     R7,PRELOC                                                        
         MVC   BUFKY,SPACES                                                     
         MVI   BUFKTYPE,X'02'                                                   
         MVC   SAVEKEY,SPACES                                                   
         MVC   SAVEKEY(L'KEY),KEY                                               
         L     R4,=A(ACCBUFF)                                                   
         A     R4,PRELOC                                                        
         MVC   0(42,R4),SPACES                                                  
         MVC   0(1,R4),QCOMPANY                                                 
         MVC   1(2,R4),=C'SJ'                                                   
         LA    R5,4(R7)            POINT TO DATA LINES                          
         L     R3,PGCNT            NUMBER                                       
         LTR   R3,R3                                                            
         BZ    INCXIT                                                           
         SPACE 1                                                                
INC66    MVC   PGSPROD(20),0(R5)                                                
         MVC   3(8,R4),PGSPROD                                                  
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         CLI   DMCB+8,0            NOT FOUND                                    
         BNE   INC67                                                            
         MVI   ELCODE,ACOTELQ      FIND NUMBER ELEMENT                          
         BAS   RE,GETEL                                                         
         BNE   INC67                                                            
         USING ACOTHERD,R4                                                      
         MVC   BUFFOFF(14),SPACES                                               
         ZIC   RF,CLILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUFFCLI(0),PGSPROD  CLI AND PROD TO BUFFALO                      
         IC    RF,PRODLEN                                                       
         ZIC   R1,CLILEN                                                        
         SR    RF,R1                                                            
         LA    RE,PGSPROD(R1)                                                   
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                                                             
         MVC   BUFFPRD(0),0(RE)                                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,CPOFFND          FIND CLIENT/PRODUCT OFFICE CODE              
         MVC   BUFFCLI(12),SPACES                                               
         MVC   BUFFCLI(3),PGSPROD    BUFFALO KEY IS CLIENT                      
         MVC   BUFFPRD(3),ACOTNUM  PRODUCT GROUP NUMBER                         
         MVC   BUFFMED,ACCT        AND MEDIA                                    
         CLI   ACOTNUM+3,C' '                                                   
         BE    *+16                                                             
         MVC   BUFFCLI(3),ACOTNUM  FOR LONGER NUMBER USE PART FOR CLI           
         MVC   BUFFPRD(3),ACOTNUM+3 AND PART FOR PRODUCT                        
         ZAP   BUFFGR,PGSGR                                                     
         ZAP   BUFFCM,PGSCM                                                     
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   INC67                                                            
         MVC   BUFFPRD,=6X'FF'                                                  
         BASR  RE,RF                                                            
         MVC   BUFFCLI,=6X'FF'                                                  
         MVC   BUFFMED,SPACES                                                   
         BASR  RE,RF                                                            
INC67    LA    R5,20(R5)                                                        
         L     R4,=A(ACCBUFF)                                                   
         A     R4,PRELOC                                                        
         BCT   R3,INC66                                                         
         SPACE 1                                                                
         L     R4,=A(ACCBUFF)                                                   
         A     R4,PRELOC                                                        
         MVC   0(42,R4),SPACES                                                  
         MVC   0(L'KEY,R4),SAVEKEY RESET MONACC                                 
         GOTO1 DATAMGR,DMCB,=C'DMREAD',=C'ACCOUNT',(R4),(R4)                    
         XC    PGCNT,PGCNT                                                      
         B     INCXIT                                                           
         EJECT                                                                  
INC70    CLI   MODE,LEVALAST                                                    
         BNE   INC80                                                            
         AP    REQGR,LEVGR                                                      
         AP    REQCM,LEVCM                                                      
         CP    MEDLS,=P'0'                                                      
         BE    INC75                                                            
         MVI   FORCEHED,C'N'                                                    
         ZAP   MEDLS,=P'0'                                                      
         SPACE 1                                                                
         L     R3,ADHEIRA                                                       
         MVC   MEDACCT,SPACES                                                   
         MVC   MEDNAME,SPACES                                                   
         MVC   MEDLEVA,3(R3)                                                    
         MVI   MEDLEVB,X'FF'                                                    
         SPACE 1                                                                
         L     R3,ADLVANAM                                                      
         LA    R5,MEDNAME                                                       
         BAS   RE,NAMOUT                                                        
         ZAP   MEDTGR,LEVGR                                                     
         ZAP   MEDTCM,LEVCM                                                     
         SPACE 1                                                                
         BAS   RE,BINADD                                                        
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         MVC   P+11(9),=C'TOTAL FOR'                                            
         MVC   P+21(36),MEDNAME                                                 
         LA    R5,LEVGR                                                         
         SPACE 1                                                                
         BAS   RE,FORMAT                                                        
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         SPACE 1                                                                
INC75    ZAP   LEVGR,=P'0'                                                      
         ZAP   LEVCM,=P'0'                                                      
         B     INCXIT                                                           
         EJECT                                                                  
INC80    CLI   MODE,REQLAST                                                     
         BNE   INCXIT                                                           
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         MVC   P+11(17),=C'TOTAL FOR REQUEST'                                   
         LA    R5,REQGR                                                         
         BAS   RE,FORMAT                                                        
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'                                                    
         MVC   P,SPACES                                                         
         CLI   RCSUBPRG,5                                                       
         BNE   *+12                                                             
         MVI   RCSUBPRG,8                                                       
         B     *+8                                                              
         MVI   RCSUBPRG,2                                                       
         MVI   BYTE,1              BUFFALO KEY VALUE                            
         CLI   QOPT2,C'Y'          CLI/PRD SUMMARY                              
         BNE   *+8                                                              
         BAS   RE,SUMCLP                                                        
         CLI   RCSUBPRG,8          RESET                                        
         BNE   *+8                                                              
         MVI   RCSUBPRG,5                                                       
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'                                                    
         ZIC   RF,RCSUBPRG         CLIENT SUMMARY                               
         LA    RF,1(RF)                                                         
         STC   RF,RCSUBPRG         2 TO 3,AND 5 TO 6                            
         BAS   RE,SUMCL                                                         
         SPACE 1                                                                
         MVI   FORCEHED,C'Y'                                                    
         ZIC   RF,RCSUBPRG         MEDIA SUMMARY                                
         LA    RF,1(RF)                                                         
         STC   RF,RCSUBPRG         3 TO 4,AND 6 TO 7                            
         L     R4,AMEDT                                                         
         LA    R4,4(R4)                                                         
         L     R3,MEDCNT                                                        
         LTR   R3,R3                                                            
         BZ    INCXIT                                                           
         SPACE 1                                                                
MEDSUM   MVC   MEDTD,0(R4)                                                      
         MVC   P+1(12),MEDACCT                                                  
         MVC   P+14(36),MEDNAME                                                 
         CLI   MEDACCT+1,X'FF'                                                  
         BNE   MEDS2                                                            
         SPACE 1                                                                
         AP    REQGR,MEDTGR                                                     
         AP    REQCM,MEDTCM                                                     
         MVC   P+1(12),=C'   TOTAL FOR'                                         
         SPACE 1                                                                
MEDS2    LA    R5,MEDTGR                                                        
         BAS   RE,FORMAT                                                        
         MVI   SPACING,2                                                        
         CLI   MEDACCT+1,X'FF'                                                  
         BNE   *+8                                                              
         MVI   SPACING,3                                                        
         BAS   RE,REPORT                                                        
         SPACE 1                                                                
         LA    R4,64(R4)                                                        
         BCT   R3,MEDSUM                                                        
         MVC   P+14(17),=C'TOTAL FOR REQUEST'                                   
         LA    R5,REQGR                                                         
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         SPACE 1                                                                
         CLI   PROGPROF+3,C'Y'     PROFILE FOR PROD GROUP SUMMARY               
         BNE   INCXIT                                                           
         MVC   BUFKY,SPACES                                                     
         MVI   BUFKTYPE,X'02'                                                   
         MVI   RCSUBPRG,9                                                       
         MVI   FORCEHED,C'Y'                                                    
         MVI   BYTE,2                                                           
         BAS   RE,SUMCLP                                                        
         B     INCXIT                                                           
         SPACE 1                                                                
INCXIT   XIT1                                                                   
         EJECT                                                                  
TOTPRD   NTR1                                                                   
         AP    CLIGR,BUFFGR        ADD PRODUCT TO CLIENT                        
         AP    CLICM,BUFFCM                                                     
         CP    DETLS,=P'0'         ANY TRANSACTIONS                             
         BE    INCXIT                                                           
         SPACE 1                                                                
         CLI   QOPT2,C'Y'          CLIENT/PRD  SUMMARY                          
         BNE   TOTP3                                                            
         CP    BUFFGR,=P'0'                                                     
         BNE   *+14                                                             
         CP    BUFFCM,=P'0'                                                     
         BE    TOTP3                                                            
         MVI   BUFKTYPE,X'01'                                                   
         MVC   BUFFOFF,SPACES                                                   
         CLI   QOPT5,C'Y'                                                       
         BNE   *+8                                                              
         BAS   RE,CPOFFND                                                       
         GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
         CLI   QOPT5,C'Y'                                                       
         BNE   TOTP3               IF WE ARE OFFICE BASED                       
         MVC   WORK(L'BUFKY),BUFKY SAVE FULL BUFFALO KEY AND PUT                
         MVC   BUFFPRD,=6X'FF'                                                  
         BASR  RE,RF                                                            
         MVC   BUFFCLI,=6X'FF'     CLI AND OFFICE TO BUFFALO                    
         MVC   BUFFMED,SPACES                                                   
         BASR  RE,RF                                                            
TOTP2    MVC   BUFKY,WORK                                                       
         SPACE 1                                                                
TOTP3    LA    R5,BUFFGR                                                        
         CLI   RCSUBPRG,0          DETAILS PRINTED                              
         BE    TOTP5               YES                                          
         MVC   P+13(6),BUFFPRD                                                  
         CP    PRDLS,=P'0'                                                      
         BNE   *+10                                                             
         MVC   P+3(6),BUFFCLI                                                   
         CLI   RCSUBPRG,5                                                       
         BNE   TOTP3A                                                           
         GOTO1 CHOPPER,DMCB,(36,PRODNAME),(30,P+21),(C'P',2)                    
         SPACE 1                                                                
TOTP3A   BAS   RE,FORMAT                                                        
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
TOTP4    ZAP   DETLS,=P'0'                                                      
         AP    PRDLS,=P'1'                                                      
         B     INCXIT                                                           
         SPACE 1                                                                
TOTP5    DS    0H                                                               
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         CP    DETLS,=P'1'                                                      
         BE    TOTP4                                                            
         MVC   P+20(20),PRLINE     **TOTAL FOR PRODUCT                          
         MVI   SPACING,2                                                        
         B     TOTP3A                                                           
         EJECT                                                                  
TOTCLI   NTR1                                                                   
         AP    MEDGR,CLIGR         ADD CLIENT TO MEDIA                          
         AP    MEDCM,CLICM                                                      
         MVI   BUFKTYPE,X'01'                                                   
         MVC   BUFFOFF,SPACES                                                   
         CP    PRDLS,=P'0'         NO PRODUCT LINES                             
         BE    TOTC4                                                            
         SPACE 1                                                                
         ZAP   BUFFGR,CLIGR                                                     
         ZAP   BUFFCM,CLICM                                                     
         MVC   BUFFPRD,=6X'FF'     CLIENT TOTALS TO BUFFALO                     
         SPACE 1                                                                
         CP    BUFFGR,=P'0'                                                     
         BNE   *+14                                                             
         CP    BUFFCM,=P'0'                                                     
         BE    TOTCL2                                                           
         CLI   QOPT2,C'Y'                                                       
         BNE   TOTCL1                                                           
         CLI   QOPT5,C'Y'          ALREADY DONE IT                              
         BE    TOTCL2                                                           
         B     TOTCL1A                                                          
TOTCL1   CLI   QOPT5,C'Y'                                                       
         BNE   TOTCL1A                                                          
         BAS   RE,CPOFFND                                                       
TOTCL1A  GOTO1 BUFFALO,DMCB,=C'PUT',ABUFF,BUFKY                                 
         SPACE 1                                                                
TOTCL2   LA    R5,CLIGR                                                         
         MVI   HEDBYTE,C'N'                                                     
         CLI   RCSUBPRG,0                                                       
         BNE   TOTCL2A                                                          
         CLC   QPROG(2),=C'32'                                                  
         BNE   TOTCL2C                                                          
         CLI   MODE,ACCLAST                                                     
         BE    TOTCL2C                                                          
         CLI   PROGPROF+2,C'Y'     PROFILE FOR 1-PAGE PER CLIENT                
         BNE   TOTCL2C                                                          
         MVI   HEDBYTE,C'Y'                                                     
         B     TOTCL2C                                                          
TOTCL2A  DS    0H                                                               
         CP    PRDLS,=P'1'                                                      
         BE    TOTCL3                                                           
TOTCL2C  BAS   RE,FORMAT                                                        
         MVC   P+19(20),CLINE      *TOTAL FOR CLIENT*                           
         MVI   SPACING,3                                                        
         SPACE 1                                                                
TOTCL3   DS    0H                                                               
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         BAS   RE,REPORT                                                        
         MVC   FORCEHED,HEDBYTE                                                 
         AP    CLILS,=P'1'                                                      
         SPACE 1                                                                
TOTC4    ZAP   PRDLS,=P'0'                                                      
         ZAP   CLIGR,=P'0'                                                      
         ZAP   CLICM,=P'0'                                                      
         B     INCXIT                                                           
         EJECT                                                                  
SUMCLP   NTR1                                                                   
         ZAP   DETLS,=P'0'                                                      
         ZAP   REQGR,=P'0'                                                      
         ZAP   REQCM,=P'0'                                                      
         MVC   BUFKY,SPACES                                                     
         MVC   BUFKTYPE,BYTE                                                    
         GOTO1 BUFFALO,DMCB,=C'HIGH',(BYTE,ABUFF),BUFKY,1                       
         TM    DMCB+8,X'80'                                                     
         BO    INCXIT                                                           
         MVC   SAVBUF,BUFFCLI                                                   
         SPACE 1                                                                
SUMCLP1  CLC   BUFFCLI(12),SAVBUF   SAME CLIENT/PRODUCT                         
         BE    SUMCLP8                                                          
         AP    CLIGR,LEVGR                                                      
         AP    CLICM,LEVCM                                                      
         CLC   SAVBUF(6),=6X'FF'   JUST DONE OFFICE TOTALS                      
         BE    SUMCLP8                                                          
         BAS   RE,REPORT                                                        
         SPACE 1                                                                
         CP    DETLS,=P'1'                                                      
         BE    SUMCLP4                                                          
         MVC   P+8(20),PRLINE      **TOTAL FOR PRODUCT                          
         MVI   SPACING,2                                                        
         LA    R5,LEVGR                                                         
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         SPACE 1                                                                
SUMCLP4  ZAP   DETLS,=P'0'                                                      
         ZAP   LEVGR,=P'0'                                                      
         ZAP   LEVCM,=P'0'                                                      
         CLC   BUFFCLI,SAVBUF                                                   
         BE    SUMCLP8                                                          
         BAS   RE,CLITOT                                                        
SUMCLP8  CLC   BUFFOFF(14),=14X'FF'                                             
         BE    SUMCLPA                                                          
         SPACE 1                                                                
         AP    LEVGR,BUFFGR                                                     
         AP    LEVCM,BUFFCM                                                     
         MVC   P+1(2),BUFFOFF                                                   
         MVC   P+4(6),BUFFCLI                                                   
         MVC   P+13(6),BUFFPRD                                                  
         SPACE 1                                                                
         MVC   MEDACCT,BUFFMED                                                  
         BAS   RE,BINADD           GET MEDIA NAME                               
         CLI   DMCB,1                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R2,DMCB                                                          
         MVC   P+19(36),12(R2)                                                  
         LA    R5,BUFFGR                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         MVC   SAVBUF,BUFFCLI                                                   
         AP    DETLS,=P'1'                                                      
         SPACE 1                                                                
SUMCLP9  GOTO1 BUFFALO,DMCB,=C'SEQ',(BYTE,ABUFF),BUFKY,1                        
         TM    DMCB+8,X'80'                                                     
         BO    SUMCLPD                                                          
         CLC   BUFFCLI(12),=12X'FF'                                             
         BE    SUMCLPF                                                          
         CLC   BUFFPRD,=6X'FF'     CLIENT TOTAL                                 
         BE    SUMCLP9             IGNORE                                       
         B     SUMCLP1                                                          
         SPACE 1                                                                
SUMCLPD  MVC   BUFFOFF(14),=14X'FF'                                             
         B     SUMCLP1                                                          
         SPACE 1                                                                
SUMCLPA  BAS   RE,REPORT                                                        
         MVC   P+11(17),=C'TOTAL FOR REQUEST'                                   
         LA    R5,REQGR                                                         
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         B     INCXIT                                                           
         SPACE 1                                                                
SUMCLPF  DS    0H                                                               
         AP    CLIGR,LEVGR                                                      
         AP    CLICM,LEVCM                                                      
         BAS   RE,CLITOT                                                        
         MVI   SPACING,2                                                        
         MVC   P+6(22),=C'***TOTAL FOR OFFICE***'                               
         LA    R5,BUFFGR                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         ZAP   DETLS,=P'0'                                                      
         ZAP   LEVGR,=P'0'                                                      
         ZAP   LEVCM,=P'0'                                                      
         MVC   SAVBUF,BUFFCLI                                                   
         MVI   FORCEHED,C'Y'                                                    
         B     SUMCLP9                                                          
         SPACE 2                                                                
CLITOT   NTR1                                                                   
         BAS   RE,REPORT                                                        
         AP    REQGR,CLIGR                                                      
         AP    REQCM,CLICM                                                      
         MVC   P+7(20),CLINE       *TOTAL FOR CLIENT                            
         MVI   SPACING,2                                                        
         CLI   PROGPROF,C'Y'                                                    
         BNE   CLIT2                                                            
         CLI   BUFKTYPE,2          ONLY FOR MAIN CLI/PROD SUMMARY               
         BE    CLIT2                                                            
         MVC   P+19(9),SPACES                                                   
         MVC   WORK(6),SAVBUF                                                   
         GOTO1 PUTNAME,DMCB,P+19,WORK                                           
CLIT2    DS    0H                                                               
         LA    R5,CLIGR                                                         
         BAS   RE,FORMAT                                                        
         CLC   P+52(55),SPACES                                                  
         BNE   *+14                                                             
         MVC   P,SPACES                                                         
         B     INCXIT                                                           
         BAS   RE,REPORT                                                        
         CLI   PROGPROF,C'Y'                                                    
         BNE   INCXIT                                                           
*        CLI   BUFKTYPE,2                                                       
*        BE    INCXIT                                                           
         MVI   FORCEHED,C'Y'                                                    
         B     INCXIT                                                           
         EJECT                                                                  
SUMCL    NTR1                                                                   
         ZAP   DETLS,=P'0'                                                      
         MVC   BUFKY,SPACES                                                     
         MVI   BUFKTYPE,X'01'                                                   
         GOTO1 BUFFALO,DMCB,=C'HIGH',(X'01',ABUFF),BUFKY,1                      
         TM    DMCB+8,X'80'                                                     
         BO    INCXIT                                                           
         MVC   SAVBUF,BUFFCLI                                                   
         B     SUMCL9A                                                          
         SPACE 1                                                                
SUMCL2   DS    0H                                                               
         CLC   BUFFOFF(14),=14X'FF'                                             
         BE    SUMCLA                                                           
         CLC   BUFFCLI,SAVBUF                                                   
         BE    SUMCL6                                                           
         CLC   SAVBUF(6),=6X'FF'   JUST DONE OFFICE TOTALS                      
         BE    SUMCL6                                                           
         BAS   RE,CCLITOT                                                       
         MVC   SAVBUF,BUFFCLI                                                   
         B     SUMCL2                                                           
         SPACE 1                                                                
SUMCL6   DS    0H                                                               
         AP    CLIGR,BUFFGR                                                     
         AP    CLICM,BUFFCM                                                     
         MVC   P+1(2),BUFFOFF                                                   
         MVC   P+4(6),BUFFCLI                                                   
         SPACE 1                                                                
         MVC   MEDACCT,BUFFMED                                                  
         BAS   RE,BINADD                                                        
         CLI   DMCB,1                                                           
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE 1                                                                
         L     R2,DMCB                                                          
         MVC   P+11(36),12(R2)                                                  
         LA    R5,BUFFGR                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         MVC   SAVBUF,BUFFCLI                                                   
         AP    DETLS,=P'1'                                                      
         B     SUMCL9                                                           
         SPACE 1                                                                
         SPACE 1                                                                
SUMCL9   GOTO1 BUFFALO,DMCB,=C'SEQ',(X'01',ABUFF),BUFKY,1                       
         TM    DMCB+8,X'80'                                                     
         BO    SUMCLD                                                           
         CLC   BUFFCLI(12),=12X'FF'                                             
         BE    SUMCLF                                                           
SUMCL9A  CLC   BUFFPRD,=6X'FF'                                                  
         BE    SUMCL2                                                           
         B     SUMCL9                                                           
         SPACE 1                                                                
SUMCLD   MVC   BUFFOFF(14),=14X'FF'                                             
         B     SUMCL2                                                           
         SPACE 1                                                                
SUMCLA   BAS   RE,CCLITOT                                                       
         MVC   P+11(17),=C'TOTAL FOR REQUEST'                                   
         LA    R5,REQGR                                                         
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         B     INCXIT                                                           
         SPACE 1                                                                
SUMCLF   BAS   RE,REPORT                                                        
         BAS   RE,CCLITOT                                                       
         MVC   P+6(22),=C'***TOTAL FOR OFFICE***'                               
         LA    R5,BUFFGR                                                        
         BAS   RE,FORMAT                                                        
         BAS   RE,REPORT                                                        
         BAS   RE,REPORT                                                        
         MVC   SAVBUF,BUFFCLI                                                   
         B     SUMCL9                                                           
         SPACE 1                                                                
CCLITOT  NTR1                                                                   
         BAS   RE,REPORT                                                        
         AP    REQGR,CLIGR                                                      
         AP    REQCM,CLICM                                                      
         CLI   PROGPROF,C'Y'                                                    
         BE    CCLIT2                                                           
         CP    DETLS,=P'1'                                                      
         BE    CCLIT6                                                           
CCLIT2   MVC   P+7(20),CLINE       *TOTAL FOR CLIENT                            
         CLI   PROGPROF,C'Y'                                                    
         BNE   CCLIT4                                                           
         MVC   P+19(9),SPACES                                                   
         MVC   WORK(6),SAVBUF                                                   
         GOTO1 PUTNAME,DMCB,P+19,WORK                                           
CCLIT4   LA    R5,CLIGR                                                         
         BAS   RE,FORMAT                                                        
         CLC   P+52(55),SPACES                                                  
         BNE   *+14                                                             
         MVC   P,SPACES                                                         
         B     CCLIT8                                                           
         MVI   SPACING,3                                                        
CCLIT6   BAS   RE,REPORT                                                        
         CLI   PROGPROF+1,C'Y'                                                  
         BNE   *+8                                                              
         MVI   FORCEHED,C'Y'                                                    
CCLIT8   ZAP   DETLS,=P'0'                                                      
         ZAP   CLIGR,=P'0'                                                      
         ZAP   CLICM,=P'0'                                                      
         B     INCXIT                                                           
         EJECT                                                                  
*              FIND CLIENT NAME                                                 
         SPACE 2                                                                
PUTNAME  NTR1                                                                   
         L     R5,0(R1)            FOR NAMOUT                                   
         L     R4,=A(ACCBUFF)                                                   
         A     R4,PRELOC                                                        
         MVC   0(42,R4),SPACES                                                  
         MVC   0(1,R4),QCOMPANY                                                 
         MVC   1(2,R4),=C'SJ'                                                   
         CLI   QCOMPANY,DDS                                                     
         BNE   *+10                                                             
         MVC   1(2,R4),=C'SR'                                                   
         L     RF,4(R1)                                                         
         MVC   3(6,R4),0(RF)                                                    
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'ACCOUNT',(R4),(R4)                    
         MVI   ELCODE,X'20'                                                     
         BAS   RE,GETEL                                                         
         BNE   INCXIT                                                           
         ZIC   R3,1(R4)                                                         
         SH    R3,=H'2'                                                         
         GOTO1 CHOPPER,DMCB,((R3),2(R4)),(28,0(R5)),1                           
         LA    R5,33(R5)                                                        
         CLI   0(R5),C' '                                                       
         BNE   *+8                                                              
         BCT   R5,*-8                                                           
         MVC   1(3,R5),=C' **'                                                  
         B     INCXIT                                                           
         EJECT                                                                  
CPOFFBLD NTR1                      BUILD A LIST OF CLIENTS,PRODUCTS             
         CLI   QCOMPANY,DDS        AND OFFICES                                  
         BE    INCXIT                                                           
         USING ACKEYD,R4                                                        
         L     R3,=A(CPOFFTAB)                                                  
         A     R3,PRELOC                                                        
         MVI   0(R3),X'FF'                                                      
         ZIC   RF,PRODLEN                                                       
         LA    RE,3(R4,RF)         POINT TO FIRST CHARACTER OF JOB              
         ST    RE,JOBAD                                                         
         SPACE 1                                                                
CB2      DS    0H                                                               
         GOTO1 DATAMGR,DMCB,DMRSEQ,=C'ACCOUNT',(R4),(R4)                        
         CLC   1(2,R4),=C'SJ'                                                   
         BNE   INCXIT                                                           
         L     RE,JOBAD                                                         
         CLC   0(3,RE),SPACES                                                   
         BE    CB10                                                             
CB4      ZIC   RF,0(RE)            BUMP JOB POSITION BY ONE                     
         LA    RF,1(RF)                                                         
         STC   RF,0(RE)                                                         
         B     CB2                                                              
         SPACE 2                                                                
CB10     LA    R5,ACRECORD                                                      
         SR    RF,RF                                                            
CB12     CLI   0(R5),0                                                          
         BE    CB4                                                              
         CLI   0(R5),ACPRELQ                                                    
         BE    CB14                                                             
         IC    RF,1(R5)                                                         
         AR    R5,RF                                                            
         B     CB12                                                             
         USING ACPROFD,R5                                                       
CB14     CLC   ACPROFFC,SPACES                                                  
         BNH   CB4                 NO OFFICE CODE                               
         MVC   0(12,R3),SPACES                                                  
         IC    RF,CLILEN                                                        
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                SAVE CLIENT                                  
         MVC   0(0,R3),ACKEYACC+3                                               
         IC    RF,PRODLEN                                                       
         ZIC   R1,CLILEN                                                        
         SR    RF,R1                                                            
         LA    R2,ACKEYACC+3(R1)                                                
         BCTR  RF,0                                                             
         EX    RF,*+8                                                           
         B     *+10                SAVE PRODUCT                                 
         MVC   6(0,R3),0(R2)                                                    
         MVC   12(2,R3),ACPROFFC                                                
         LA    R3,14(R3)                                                        
         MVI   0(R3),X'FF'                                                      
         CP    TBLMAX,=P'12200'                                                 
         BL    *+6                                                              
         DC    H'0'                TABLE IS FULL                                
         AP    TBLMAX,=P'1'                                                     
         B     CB4                                                              
         EJECT                                                                  
CPOFFND  NTR1                      FIND AN-OFFICE CODE                          
         L     R3,=A(CPOFFTAB)                                                  
         A     R3,PRELOC                                                        
CF2      CLI   0(R3),X'FF'                                                      
         BE    INCXIT                                                           
         CLC   BUFFCLI,0(R3)                                                    
         BE    CF4                                                              
         LA    R3,14(R3)                                                        
         B     CF2                                                              
         SPACE 1                                                                
CF4      MVC   BUFFOFF,12(R3)      SAVE CLIENT OFFICE CODE FIRST                
         LA    R3,14(R3)                                                        
CF6      CLC   BUFFCLI,0(R3)                                                    
         BNE   INCXIT                                                           
         CLC   BUFFPRD,6(R3)       NOW SEE IF WE CAN FIND PRODUCT               
         BE    CF8                                                              
         LA    R3,14(R3)                                                        
         B     CF6                                                              
         SPACE 1                                                                
CF8      MVC   BUFFOFF,12(R3)                                                   
         B     INCXIT                                                           
         EJECT                                                                  
NAMOUT   NTR1                                                                   
         USING ACNAMED,R3                                                       
         XR    R2,R2                                                            
         IC    R2,ACNMLEN                                                       
         SH    R2,=H'3'                                                         
         EX    R2,*+8                                                           
         B     INCXIT                                                           
         MVC   0(0,R5),ACNMNAME                                                 
         SPACE 2                                                                
BINADD   NTR1                                                                   
         L     R4,MEDCNT           NUMBER IN TABLE                              
         L     R7,AMEDT            ADDRESS                                      
         L     R5,0(R7)            MAX.                                         
         LA    R7,4(R7)            DATA                                         
         SPACE 1                                                                
         GOTO1 BINSRCH,DMCB,(1,MEDTD),(R7),(R4),64,(0,12),(R5)                  
         MVC   MEDCNT,DMCB+8                                                    
         OC    DMCB(4),DMCB                                                     
         BNZ   INCXIT                                                           
         DC    H'0'                TABLE FULL                                   
         EJECT                                                                  
         GETEL R4,DATADISP,ELCODE                                               
         SPACE 2                                                                
FORMAT   NTR1                                                                   
         CP    0(8,R5),=P'0'                                                    
         BE    FORMAT2                                                          
         EDIT  (P8,0(R5)),(15,P+52),2,COMMAS=YES,MINUS=YES                      
         SPACE 1                                                                
FORMAT2  CP    8(8,R5),=P'0'                                                    
         BE    FORMAT3                                                          
         EDIT  (P8,8(R5)),(15,P+68),2,COMMAS=YES,MINUS=YES                      
         SPACE 1                                                                
FORMAT3  ZAP   PWRK,0(8,R5)        GROSS                                        
         SP    PWRK,8(8,R5)        MINUS COMMISSION                             
         CP    PWRK,=P'0'          NET                                          
         BE    FORMAT4                                                          
         CLI   QCOMPANY,DDS                                                     
         BE    FORMAT4                                                          
         EDIT  PWRK,(15,P+92),2,COMMAS=YES,MINUS=YES                            
         SPACE 1                                                                
FORMAT4  CP    0(8,R5),=P'0'                                                    
         BE    FORMAT7                                                          
         CP    8(8,R5),=P'0'                                                    
         BE    FORMAT7                                                          
         SPACE 1                                                                
         ZAP   WORK(8),8(8,R5)     COMMISSION                                   
         OI    WORK+7,X'0F'                                                     
         ZAP   WORK+8(8),0(8,R5)   GROSS                                        
         OI    WORK+15,X'0F'                                                    
         CP    WORK(8),WORK+8(8)                                                
        BH     FORMAT7                                                          
         SPACE 1                                                                
         ZAP   PWRK,8(8,R5)        COMMISSION                                   
         MP    PWRK,=P'100000'                                                  
         DP    PWRK,0(8,R5)        COMMISSION/GROSS                             
         AP    PWRK(4),=P'5'       ROUND                                        
         EDIT  (P4,PWRK),(7,P+85),3                                             
         MVI   P+91,C' '           DROP LEAST SIGNIF.                           
         SPACE 1                                                                
FORMAT7  ZAP   0(8,R5),=P'0'                                                    
         ZAP   8(8,R5),=P'0'                                                    
         CLI   QOPT3,C'Y'          BLANK OUT ALL BUT GROSS                      
         BNE   INCXIT                                                           
         MVC   P+69(50),SPACES                                                  
         B     INCXIT                                                           
         EJECT                                                                  
REPORT   NTR1                                                                   
         CLC   QPROG,=C'33'                                                     
         BE    *+14                                                             
         MVC   HEAD1+43(21),=C'SALES ANALYSIS REPORT'                           
         B     *+16                                                             
         MVC   HEAD1+41(25),=C'HISTORICAL SALES ANALYSIS'                       
         MVC   HEAD2+41(25),=25C'-'                                             
         MVC   HEAD8,SPACES                                                     
         MVC   HEAD9,SPACES                                                     
         CLI   QOPT3,C'Y'          SUPPRESS COMM., NET, PCT.                    
         BE    REPRT0                                                           
         MVC   HEAD8+72(10),=C'COMMISSION'                                      
         MVC   HEAD9+72(10),=25C'-'                                             
         MVC   HEAD8+87(3),=C'PCT'                                              
         MVC   HEAD9+87(3),=25C'-'                                              
         MVC   HEAD8+100(3),=C'NET'                                             
         MVC   HEAD9+100(3),=25C'-'                                             
REPRT0   DS    0H                                                               
         CLI   RCSUBPRG,9                                                       
         BE    REPRT1                                                           
         CLI   RCSUBPRG,4          HISTORICAL REPORT SPROGS-5,6,7,8             
         BH    REPRT4                                                           
REPRT1   CLI   RCSUBPRG,1                                                       
         BH    REPRT2                                                           
         CLI   MODE,REQLAST                                                     
         BE    REPRT2                                                           
         MVC   HEAD4+1(7),=C'ACCOUNT'                                           
         MVC   HEAD4+9(12),ACCT                                                 
         MVC   HEAD5+1(5),=C'MEDIA'                                             
         MVC   HEAD5+9(36),ACCTNM                                               
         SPACE 1                                                                
REPRT2   CLI   RCSUBPRG,2                                                       
         BNE   *+10                                                             
         MVC   HEAD4+1(22),=C'CLIENT/PRODUCT SUMMARY'                           
         SPACE 1                                                                
         CLI   RCSUBPRG,3                                                       
         BNE   *+10                                                             
         MVC   HEAD4+1(14),=C'CLIENT SUMMARY'                                   
         SPACE 1                                                                
         CLI   RCSUBPRG,4                                                       
         BNE   *+10                                                             
         MVC   HEAD4+1(13),=C'MEDIA SUMMARY'                                    
         SPACE 1                                                                
         CLI   RCSUBPRG,9                                                       
         BNE   REPRT3                                                           
         MVC   HEAD4+1(21),=C'PRODUCT GROUP SUMMARY'                            
         CLC   QPROG,=C'33'                                                     
         BE    REPRT12                                                          
         SPACE 1                                                                
REPRT3   MVC   HEAD4+83(16),=C'FOR THE MONTH OF'                                
         MVC   HEAD4+100(6),HDATE                                               
         GOTO1 ACREPORT                                                         
         B     INCXIT                                                           
         SPACE 2                                                                
REPRT4   DS    0H                                                               
         CLI   RCSUBPRG,8                                                       
         BE    REPRT6                                                           
         CLI   RCSUBPRG,6                                                       
         BE    REPRT8                                                           
         CLI   RCSUBPRG,7                                                       
         BE    REPRT10                                                          
REPRT5   DS    0H                                                               
         MVC   HEAD4+1(7),=C'ACCOUNT'                                           
         MVC   HEAD4+9(12),ACCT                                                 
         MVC   HEAD5+1(5),=C'MEDIA'                                             
         MVC   HEAD5+9(36),ACCTNM                                               
         B     REPRT12                                                          
         SPACE 1                                                                
REPRT6   DS    0H                                                               
         MVC   HEAD4+1(22),=C'CLIENT/PRODUCT SUMMARY'                           
         B     REPRT12                                                          
         SPACE 1                                                                
REPRT8   CLI   RCSUBPRG,6                                                       
         BNE   REPRT10                                                          
         MVC   HEAD4+1(14),=C'CLIENT SUMMARY'                                   
         B     REPRT12                                                          
         SPACE 1                                                                
REPRT10  MVC   HEAD4+1(13),=C'MEDIA SUMMARY'                                    
         SPACE 1                                                                
REPRT12  DS    0H                                                               
         MVC   HEAD4+83(4),=C'FROM'                                             
         MVC   HEAD4+88(6),HFROM                                                
         MVC   HEAD4+95(2),=C'TO'                                               
         MVC   HEAD4+98(6),HTO                                                  
         GOTO1 ACREPORT                                                         
         B     INCXIT                                                           
         EJECT                                                                  
         BUFF  LINES=050,ROWS=1,COLUMNS=2,FLAVOR=PACKED,KEYLIST=(27,A)          
         EJECT                                                                  
MEDTBL   CSECT                                                                  
         DC    F'500'               MAX ITEMS                                   
         DS    500CL64                                                          
*                                  12 BYTE KEY (ACCOUNT NUMBER)                 
*                                  36 BYTE NAME                                 
*                                  2 PL8   GROSS/COMMISSION                     
         SPACE 2                                                                
PGTBL    CSECT                                                                  
*&&UK                                                                           
         DC    F'100'              MAX                                          
         DC    F'100'              MAX ITEMS                                    
         DS    100CL20                                                          
*&&                                                                             
*&&US                                                                           
         DC    F'1000'                                                          
         DS    1000CL20                                                         
*&&                                                                             
         EJECT                                                                  
AC32D    DSECT                                                                  
BTCHM    DS    CL2                 YEAR, MONTH                                  
HDATE    DS    CL6                 MMM/YY                                       
         SPACE 1                                                                
ABUFF    DS    A                   A(BUFFALOC)                                  
AMEDT    DS    A                   A(MEDTBL)                                    
MEDCNT   DS    F                   NUMBER IN MEDTBL                             
PGCNT    DS    F                                                                
PRELOC   DS    F                   PROGRAM RELOCATION                           
JOBAD    DS    F                   JOB POSITION IN KEY                          
         SPACE 1                                                                
CLIGR    DS    PL8                 CLIENT GROSS                                 
CLICM    DS    PL8                 CLIENT COMMISSION                            
MEDGR    DS    PL8                 MEDIA GROSS                                  
MEDCM    DS    PL8                 MEDIA COMMISSION                             
LEVGR    DS    PL8                 LEVA GROSS                                   
LEVCM    DS    PL8                 LEVA  COMMISSION                             
REQGR    DS    PL8                 REQUEST GROSS                                
REQCM    DS    PL8                 REQUEST COMMISSION                           
         SPACE 1                                                                
DETLS    DS    PL3                 DETAIL LINES                                 
PRDLS    DS    PL3                 PRODUCT LINES                                
CLILS    DS    PL3                 CLIENT LINES                                 
MEDLS    DS    PL3                 MEDIA (ACCOUNT) LINES                        
         SPACE 1                                                                
ACCT     DS    CL12                ACCOUNT                                      
ACCTNM   DS    CL36                NAME                                         
SBACCT   DS    CL12                CLI/PRD                                      
         SPACE 1                                                                
GROSS    DS    PL8                 TRANSACTION GROSS                            
COMM     DS    PL8                 TRANSACTION COMMISSION                       
         SPACE 1                                                                
ELCODE   DS    CL1                                                              
PGSPROD  DS    CL8                 CLI/PROD                                     
PGSGR    DS    PL6                 GROSS                                        
PGSCM    DS    PL6                 COMMISSION                                   
SUBULSV  DS    CL2                                                              
         SPACE 1                                                                
MEDTD    DS    0CL64               MEDIA TABLE DSECT                            
MEDACCT  DS    CL12                ACCOUNT                                      
         ORG   MEDACCT                                                          
MEDLEVA  DS    CL1                 TYPE MEDIA - LEVEL A                         
MEDLEVB  DS    CL11                MEDIA                                        
MEDNAME  DS    CL36                NAME                                         
MEDTGR   DS    PL8                 GROSS                                        
MEDTCM   DS    PL8                 COMMISSION                                   
         SPACE 1                                                                
BUFKY    DS    0CL27                                                            
BUFKTYPE DS    CL1                 X'01' - REGULAR RECORD                       
*                                  X'02' - PRODUCT GROUP                        
BUFFOFF  DS    CL2                 OFFICE                                       
BUFFCLI  DS    CL6                 CLIENT                                       
BUFFPRD  DS    CL6                 PRODUCT                                      
BUFFMED  DS    CL12                MEDIA                                        
BUFFGR   DS    PL8                 GROSS                                        
BUFFCM   DS    PL8                 COMMISSION                                   
BUFFRECL EQU   *-BUFKY                                                          
         SPACE 1                                                                
PWRK     DS    PL12                WORK                                         
SAVBUF   DS    CL(BUFFRECL)                                                     
PRODNAME DS    CL36                                                             
HFROM    DS    CL6                                                              
HTO      DS    CL6                                                              
YMFROM   DS    CL2                                                              
         DS    CL1                                                              
YMTO     DS    CL2                                                              
         DS    CL1                                                              
SAVEKEY  DS    CL42                                                             
CLILEN   DS    CL1                                                              
PRODLEN  DS    CL1                                                              
HEDBYTE  DS    CL1                                                              
         SPACE 1                                                                
CLINE    DS    CL20                                                             
PRLINE   DS    CL20                                                             
TBLMAX   DS    PL6                                                              
*        ACGENBOTH                                                              
*        ACREPWORKD                                                             
*        ACGENMODES                                                             
         PRINT OFF                                                              
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENMODES                                                     
         PRINT ON                                                               
ACCBUFF  CSECT                                                                  
         DS    1100C                                                            
CPOFFTAB CSECT                                                                  
         DS    20000CL14                                                        
         DC    X'FE'                                                            
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'067ACREP3202 05/01/02'                                      
         END                                                                    
