*          DATA SET PPREPPL02  AT LEVEL 030 AS OF 11/05/10                      
*PHASE PPPL02A                                                                  
*                                                                               
         TITLE 'PPPL02 - CHANGE LOG'                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
* KWAN 10/20/10 SCAN ALL PUBS DON'T HAVE ANY BUYS AND NOT LOCKED                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
*  QOPT4 - 'Y' TO MARK INACTIVE PUB RECORDS AS LOCKED                           
*                                                                               
*  QOPT5 - '1' SORT BY PUB CODE                                                 
*        - '2' SORT BY PUB NAME                                                 
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PPPL02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPPL02                                                         
*                                                                               
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
*                                                                               
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
         LA    R8,SPACEND                                                       
         USING PPPLWRKD,R8                                                      
*                                                                               
         CLI   MODE,PROCREQ                                                     
         BNE   EXIT                                                             
*                                                                               
         RELOC (R3)                                                             
         L     RF,=A(BSPUB#)       ADDRESS OF PUB TABLE BY #                    
         AR    RF,R3                                                            
         ST    RF,APUB#TAB                                                      
         L     RF,=A(BSPUBN)       ADDRESS OF PUB TABLE BY NAME AND #           
         AR    RF,R3                                                            
         ST    RF,APUBNTAB                                                      
*                                                                               
         XC    BS1PARMS,BS1PARMS   INIT BINARY SEARCH PARAMETERS (1)            
         L     RF,APUB#TAB                                                      
         ST    RF,BS1PARMS+04      ADDRESS OF PUB TABLE BY #                    
         LHI   RF,BSPUB#KL                                                      
         ST    RF,BS1PARMS+12      LENGTH OF RECORD                             
         LHI   RF,BSPUB#KL                                                      
         ST    RF,BS1PARMS+16      LENGTH OF KEY                                
         LHI   RF,MAXPUB#Q                                                      
         ST    RF,BS1PARMS+20      MAXIMUM # OF RECORDS IN TABLE                
*                                                                               
         XC    BS2PARMS,BS2PARMS   INIT BINARY SEARCH PARAMETERS (2)            
         L     RF,APUBNTAB                                                      
         ST    RF,BS2PARMS+04      ADDRESS OF PUB TABLE BY NAME AND #           
         LHI   RF,BSPUBNKL                                                      
         ST    RF,BS2PARMS+12      LENGTH OF RECORD                             
         LHI   RF,BSPUBNKL                                                      
         ST    RF,BS2PARMS+16      LENGTH OF KEY                                
         LHI   RF,MAXPUB#Q                                                      
         ST    RF,BS2PARMS+20      MAXIMUM # OF RECORDS IN TABLE                
*                                                                               
         OI    DMINBTS,X'08'       PASS DELETES                                 
         MVI   DMOUTBTS,X'FD'                                                   
*                                                                               
         ZAP   TOT#PUBR,=P'0'                                                   
         ZAP   TOT#PUBL,=P'0'                                                   
         ZAP   TOT#PUBF,=P'0'                                                   
         MVC   PAGE,=H'1'                                                       
         MVI   FORCEHED,C'Y'                                                    
*                                                                               
         MVI   RCWRITE,C'N'        DEFAULT TO TEST RUN                          
         CLI   QOPT4,C'Y'                                                       
         BE    *+8                                                              
         MVI   RCWRITE,C'Y'        SET TO MARK FILE                             
*                                                                               
         GOTO1 DATCON,DMCB,(0,QSTART),(3,LOCKDATE)                              
         LA    R0,PUBREC                                                        
         ST    R0,AREC             NOTE: ONLY WORKING WITH PUB RECORDS          
*                                                                               
         BRAS  RE,PROCBUYS         PROCESS BUY RECORDS                          
         BRAS  RE,PROCPUBS         PROCESS PUB RECORDS                          
         BRAS  RE,PRNTSUMM         PPRINT SUMMARY                               
*                                                                               
EXIT     DS    0H                                                               
         XIT1                                                                   
*                                                                               
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCBUYS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(3),PAGYKAGY     AGENCY AND MEDIA CODE                        
         MVI   KEY+3,X'20'         BUY RECORDS                                  
         MVC   KEY+4(3),QCLIENT    CLIENT CODE                                  
         CLC   KEY+4(3),SPACES     HAVE CLIENT CODE?                            
         JNE   *+10                                                             
         XC    KEY+4(3),KEY+4                                                   
*                                                                               
P_BUYS10 GOTO1 HIGH                                                             
         B     P_BUYS30                                                         
*                                                                               
P_BUYS20 GOTO1 SEQ                                                              
*                                                                               
P_BUYS30 CLC   KEY(4),KEYSAVE      SAME AGENCY/MEDIA/RECORD CODE?               
         BNE   P_BUYS_X                                                         
         CLC   KEY+(PBUYKDAT-PBUYKEY)(L'PBUYKDAT),LOCKDATE                      
         BL    P_BUYS20                                                         
*                                                                               
         MVC   BPUBZNED,KEY+(PBUYKPUB-PBUYKEY)                                  
         GOTO1 BINSRCH,BS1PARMS,(X'01',BPUBZNED)                                
*                                                                               
         OC    1(3,R1),1(R1)       TABLE FULL?                                  
         BNZ   P_BUYS40                                                         
         DC    H'0'                                                             
         DC    C'INCREASE MAXPUB#Q'                                             
*                                                                               
P_BUYS40 SR    RE,RE               BUMP TO NEXT PUB                             
         ICM   RE,3,KEY+(PBUYKZON-PBUYKEY)                                      
         AHI   RE,1                                                             
         STCM  RE,3,HALF                                                        
         CLC   HALF,KEY+(PBUYKZON-PBUYKEY)                                      
         BNE   P_BUYS44                                                         
         SR    RE,RE                                                            
         ICM   RE,15,KEY+(PBUYKPUB-PBUYKEY)                                     
         AHI   RE,1                                                             
         STCM  RE,15,FULL                                                       
         CLC   FULL,KEY+(PBUYKPUB-PBUYKEY)                                      
         BE    P_BUYS20                                                         
         MVC   KEY+(PBUYKPUB-PBUYKEY)(BSPUB#KL),FULL                            
         LA    RE,KEY+(PBUYKZON-PBUYKEY)                                        
         XC    0(L'PBUYKZON+L'PBUYKEDT,RE),0(RE)                                
         B     *+10                                                             
P_BUYS44 MVC   KEY+(PBUYKZON-PBUYKEY)(L'PBUYKZON+L'PBUYKEDT),HALF               
         LA    RE,KEY+(PBUYKDAT-PBUYKEY)                                        
         XC    0(L'PBUYKDAT+L'PBUYKACT+L'PBUYKLIN,RE),0(RE)                     
         B     P_BUYS10                                                         
*                                                                               
P_BUYS_X XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PROCPUBS NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         XC    KEY,KEY                                                          
         MVC   KEY(1),PAGYKMED                                                  
         MVC   KEY+(PUBKAGY-PUBKEY)(L'PUBKAGY),PAGYKAGY                         
         MVI   KEY+(PUBKCOD-PUBKEY),X'81'                                       
         GOTO1 HIGHPUB                                                          
         B     P_PUBS30                                                         
*                                                                               
P_PUBS20 GOTO1 SEQPUB                                                           
*                                                                               
P_PUBS30 CLC   KEY(L'PUBKMED),KEYSAVE                                           
         BNE   P_PUBS_X                                                         
         CLC   KEY+(PUBKAGY-PUBKEY)(L'PUBKAGY),PAGYKAGY                         
         BNE   P_PUBS20                                                         
         CLI   KEY+(PUBKCOD-PUBKEY),X'81'                                       
         BNE   P_PUBS20                                                         
*                                                                               
         AP    TOT#PUBR,=P'1'      TOTAL PUB RECORD COUNTER                     
         GOTO1 GETNAME                                                          
*                                                                               
         CLI   PUBNAMEL,X'10'                                                   
         BE    *+6                                                              
         DC    H'0'                                                             
         TM    PUBLOCSW,PUBLCKDQ   ALREADY LOCKED?                              
         BZ    P_PUBS50                                                         
         AP    TOT#PUBL,=P'1'      TOTAL PUB RECORD ALREADY LOCKED              
         B     P_PUBS20                                                         
*                                                                               
P_PUBS50 MVC   BPUBZNED,KEY+(PUBKPUB-PUBKEY)                                    
         GOTO1 BINSRCH,BS1PARMS,(X'00',BPUBZNED)                                
*                                                                               
         CLI   0(R1),X'01'         RECORD NOT FOUND?                            
         BNE   P_PUBS20                                                         
*                                                                               
         AP    TOT#PUBF,=P'1'      TOTAL PUB RECORD CAN BE LOCKED               
*                                                                               
         CLI   QOPT5,C'1'          SORT BY PUB CODE?                            
         BE    P_PUBS60                                                         
         MVC   PUBNBPUB(L'PUBNAME),PUBNAME                                      
         MVC   PUBNBPUB+L'PUBNAME(L'BPUBZNED),BPUBZNED                          
         GOTO1 BINSRCH,BS2PARMS,(X'01',PUBNBPUB)                                
         B     P_PUBS80                                                         
*                                                                               
P_PUBS60 MVC   P,SPACES                                                         
         SR    R0,R0                                                            
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),BPUBZNED),P+5                                 
*                                                                               
         MVC   P+25(L'PUBNAME),PUBNAME                                          
         GOTO1 REPORT                                                           
*                                                                               
P_PUBS80 CLI   RCWRITE,C'Y'        MARK PUB AS LOCKED?                          
         BNE   P_PUBS98                                                         
         LA    R5,PUBREC+33                                                     
         USING PUBNAMEL,R5                                                      
         CLI   PUBNAMEL,X'10'      MAIN PUB ELEMENT?                            
         BE    *+6                                                              
         DC    H'0'                BAD RECORD!                                  
         TM    PUBLOCSW,PUBLCKDQ   ALREADY LOCKED?                              
         BNZ   P_PUBS98                                                         
         OI    PUBLOCSW,PUBLCKDQ   MARK PUB AS LOCKED                           
         DROP  R5                                                               
*                                                                               
P_PUBS94 GOTO1 PUTPUB                                                           
*                                                                               
P_PUBS98 B     P_PUBS20                                                         
*                                                                               
P_PUBS_X XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
PRNTSUMM NTR1  BASE=*,LABEL=*                                                   
*                                                                               
         CLI   QOPT5,C'1'          SORT BY PUB CODE?                            
         BE    P_SUMM20                                                         
         ICM   R4,15,BS2PARMS+08   NUMBER OF ENTRIES IN TABLE                   
         CHI   R4,0                                                             
         BNH   P_SUMM20                                                         
         L     R5,APUBNTAB         POINT TO PUB NAME TABLE                      
*                                                                               
P_SUMM14 MVC   P,SPACES                                                         
         MVC   P+5(L'PUBNAME),0(R5)                                             
*                                                                               
         MVC   BPUBZNED,L'PUBNAME(R5)                                           
         SR    R0,R0                                                            
         IC    R0,PAGYPROF+12                                                   
         GOTO1 PUBEDIT,DMCB,((R0),BPUBZNED),P+35                                
         GOTO1 REPORT                                                           
*                                                                               
         LA    R5,BSPUBNKL(R5)     BUMP TO NEXT ENTRY IN TABLE                  
         BCT   R4,P_SUMM14                                                      
*                                                                               
P_SUMM20 MVI   LINE,3                                                           
         MVC   P,SPACES                                                         
         MVC   P(37),=C'* TEST RUN - PUB RECORDS NOT LOCKED *'                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   *+16                                                             
         MVC   P,SPACES                                                         
         MVC   P(37),=C'* LIVE RUN - PUB RECORDS ARE LOCKED *'                  
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(40),=C'# OF PUBLICATION RECORDS FOR AGENCY:    '               
         EDIT  (P8,TOT#PUBR),(10,P+40),COMMAS=YES                               
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(40),=C'# OF PUBLICATION RECORDS HAVE BUYS:     '               
         ICM   R5,15,BS1PARMS+08                                                
         EDIT  (R5),(10,P+40),COMMAS=YES                                        
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(40),=C'# OF PUBLICATION RECORDS CAN BE LOCKED: '               
         EDIT  (P8,TOT#PUBF),(10,P+40),COMMAS=YES                               
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
         MVC   P(40),=C'# OF PUBLICATION RECORDS ALREADY LOCKED:'               
         EDIT  (P8,TOT#PUBL),(10,P+40),COMMAS=YES                               
         MVI   LINE,1                                                           
         GOTO1 REPORT                                                           
*                                                                               
P_SUMM_X XIT1                                                                   
         LTORG                                                                  
         DROP  RB                                                               
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         EJECT                                                                  
PPPLWRKD DSECT                                                                  
         DS    0F                                                               
BS1PARMS DS    XL24                BINARY SEARCH PARAMETER LIST 1               
BS2PARMS DS    XL24                BINARY SEARCH PARAMETER LIST 2               
APUB#TAB DS    A                   ADDRESS OF PUB TABLE BY #                    
APUBNTAB DS    A                   ADDRESS OF PUB TABLE BY NAME AND #           
*                                                                               
LOCKDATE DS    XL3                 LOCK DATE TO COMPARE IER END DATE            
BPUBZNED DS    XL(BSPUB#KL)        BINARY PUB #, ZONE AND EDITION               
PUBNBPUB DS    XL(BSPUBNKL)        PUB NAME AND BINARY PUB #                    
*                                                                               
TOT#PUBR DS    PL8                 TOTAL # OF PUBS READ                         
TOT#PUBL DS    PL8                 TOTAL # OF PUBS LOCKED                       
TOT#PUBF DS    PL8                 TOTAL # OF PUBS CAN BE LOCKED                
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
BSPUB#   CSECT                                                                  
BSPUB#TB DS    (MAXPUB#Q)XL(BSPUB#KL)                                           
BSPUB#KL EQU   L'PUBKPUB+L'PUBKZON+L'PUBKED                                     
*                                                                               
BSPUBN   CSECT                                                                  
BSPUBNTB DS    (MAXPUB#Q)XL(BSPUBNKL)                                           
BSPUBNKL EQU   L'PUBNAME+L'PUBKPUB+L'PUBKZON+L'PUBKED                           
*                                                                               
MAXPUB#Q EQU   10000                                                            
*                                                                               
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *         
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE PPMODEQU                                                       
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPREPWORK2                                                     
       ++INCLUDE PPNEWFILE                                                      
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'030PPREPPL02 11/05/10'                                      
         END                                                                    
