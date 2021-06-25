*          DATA SET SPREPLR21  AT LEVEL 132 AS OF 05/01/02                      
*PHASE SPLR02G,+0,NOAUTO                                                        
*INCLUDE SORTER                                                                 
         TITLE 'SPREPLR12 RADIO DIRECTORY REPORT'                               
SPLR02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPLR02,RR=R5                                                   
         L     RA,0(R1)                                                         
         LA    RC,2048(RA)                                                      
         LA    RC,2048(RC)                                                      
         USING SPWORKD,RA,RC                                                    
         STM   RA,RC,SPLRRA                                                     
         ST    R5,RELO                                                          
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         L     R2,ADAGY                                                         
         GOTO1 DATAMGR,DMCB,=C'OPEN',=C'SPOT',MFILLIST,(R2)                     
         B     RADOPEN                                                          
MFILLIST DS    0C                                                               
         DC    CL8'NDEMDIRR'                                                    
         DC    C'X '                                                            
         DC    H'0'                                                             
RADOPEN  GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
         B     RADOPEN2                                                         
SORTCARD DC    CL80'SORT FIELDS=(5,23,A),FORMAT=BI,WORK=1'                      
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=27'                                    
RADOPEN2 LA    R6,SPLKEY                                                        
         USING SBKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   WANTSRCE,C'A'       ARBITRON                                     
         MVC   SBCODE(3),=C'ERA'   SET TO READ PRIMARY DIRECTORY                
EQU1     MVC   SVSPLKEY,SPLKEY                                                  
         XC    SPILCTR,SPILCTR                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIRR',SVSPLKEY,SPLKEY              
         MVI   PRINTIT,C'N'                                                     
         MVI   SHOWSTAT,C'N'                                                    
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIRR',SVSPLKEY,SPLKEY              
SPLR6    CLC   SBCODE(3),=C'ERA'   STATIONS ONLY                                
         BNE   SPLR8                                                            
         CLI   SBCODE+3,X'01'      LEAVE GOOD RECORDS ALONE, THIS               
         BNH   SPLR2                SHOULD FILTER OUT BAD RECORDS               
         MVC   P1(DSERECLQ),SPLKEY                                              
         SPACE                                                                  
         MVC   SPLKEY2(3),SPLKEY                                                
         MVC   SPLKEY2+4(DSERECLQ-3),SPLKEY+3                                   
         MVI   SPLKEY2+3,0                                                      
         MVC   BMONTH,(DSEEFFBK+1-DSEKEY)+SPLKEY2                               
         XI    BMONTH,X'FF'        UNDO THE MUCK-UP                             
         ZIC   R1,BMONTH                                                        
         CVD   R1,DUB                                                           
         UNPK  CMONTH,DUB                                                       
         OI    CMONTH+(L'CMONTH-1),X'F0'                                        
         LA    RE,BOOKEQ           GET MONTH THE CORRECT WAY                    
SPLR6A   CLC   CMONTH,0(RE)                                                     
         BE    SPLR6B                                                           
         LA    RE,L'BOOKEQ(RE)                                                  
         CLI   0(RE),X'FF'                                                      
         BNE   SPLR6A                                                           
         DC    H'0'                                                             
SPLR6B   MVC   (DSEEFFBK+1-DSEKEY+SPLKEY2)(1),2(RE)                             
         XI    (DSEEFFBK+1-DSEKEY+SPLKEY2),X'FF'                                
         MVC   SPLKEY2+20(2),=X'FFFF'  INDICATE PASSIVE POINTER                 
         SPACE                                                                  
         XC    TAPEBUFF,TAPEBUFF                                                
         MVC   TAPEBUFF(2),=Y(DSERECLQ+4)                                       
         MVC   TAPEBUFF+4(DSERECLQ),SPLKEY2                                     
         MVC   P1+DSERECLQ+5(DSERECLQ),SPLKEY2                                  
         GOTO1 =V(SORTER),DMCB,=C'PUT',TAPEBUFF                                 
*                                                                               
* BUILD PASSIVE W/ BOOK HIGH                                                    
*                                                                               
         XC    TAPEBUFF,TAPEBUFF                                                
         MVC   TAPEBUFF(2),=Y(DSERECLQ+4)                                       
         MVC   TAPEBUFF+4(DSERECLQ),SPLKEY2                                     
KEEY     USING DSEKEY,SPLKEY2                                                   
BUFF     USING DSEKEY,TAPEBUFF+4                                                
         MVI   BUFF.DSEIND,DSEIBKHI                                             
         MVC   BUFF.DSEBKEFF,KEEY.DSEEFFBK                                      
         MVC   BUFF.DSECLOLD,KEEY.DSEOLDCL                                      
         MVC   BUFF.DSECLNEW,KEEY.DSENEWCL                                      
         DROP  KEEY,BUFF                                                        
         MVC   P1+2*(DSERECLQ+5)(DSERECLQ),TAPEBUFF+4                           
         GOTO1 =V(SORTER),DMCB,=C'PUT',TAPEBUFF                                 
         GOTO1 REPORT                                                           
         B     SPLR2                                                            
                                                                                
SPLR8    OPEN  (OUT,OUTPUT)                                                     
         XC    TAPEBUFF,TAPEBUFF                                                
SPLR8A   GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         ICM   R2,15,4(R1)                                                      
         BZ    EXIT                                                             
                                                                                
         ZICM  RF,0(R2),(3)                                                     
         BCTR  RF,0                                                             
         EXCLC RF,TAPEBUFF,0(R2)                                                
         BE    SPLR8A                                                           
                                                                                
         EXMVC RF,TAPEBUFF,0(R2)                                                
         PUT   OUT,TAPEBUFF                                                     
         B     SPLR8A                                                           
*                                                                               
         MVC   H6,=CL132'ARBITRON SOURCE'                                       
         CLI   QOPT2,C'M'          PUT MONTHS TOGETHER?                         
         BE    PMNTHS              YES                                          
         MVC   H8,=CL132'STATION      NOV87   FEB88   MAY88   JUL88   N*        
               OV88   FEB89   MAY89   JUL89   NOV89   FEB90   MAY90   J*        
               UL90   NOV90'                                                    
         B     AFTHEAD                                                          
PMNTHS   MVC   H8,=CL132'STATION      FEB88   FEB89   FEB90   MAY88   M*        
               OV89   MAY90   JUL88   JUL89   JUL90   NOV87   NOV88   N*        
               OV89   NOV90'                                                    
AFTHEAD  MVC   H9,=CL132'-------      -----   -----   -----   -----   -*        
               ----   -----   -----   -----   -----   -----   -----   -*        
               ----   -----'                                                    
         MVI   FORCEHED,C'Y'                                                    
         MVI   CLEARHED,C'N'       DON'T CLEAR HEADLINES                        
         MVI   FRSTTIME,C'Y'                                                    
         LA    R6,SPLKEY                                                        
         USING SBKEY,R6                                                         
         XC    SPLKEY,SPLKEY                                                    
         MVI   WANTSRCE,C'A'       ARBITRON                                     
         MVC   SBCODE(3),=C'STA'   SET TO READ PRIMARY DIRECTORY                
NIEL_1   MVC   SVSPLKEY,SPLKEY                                                  
         XC    SPILCTR,SPILCTR                                                  
         GOTO1 DATAMGR,DMCB,=C'DMRDHI',=C'DEMDIR ',SVSPLKEY,SPLKEY              
         MVI   PRINTIT,C'N'                                                     
         MVI   SHOWSTAT,C'N'                                                    
         B     SPLR6                                                            
         SPACE 2                                                                
SPLR2    GOTO1 DATAMGR,DMCB,=C'DMRSEQ',=C'DEMDIR',SVSPLKEY,SPLKEY               
SPLR6    CLI   SBCODE,C'S'         STATIONS ONLY                                
         BNE   EXIT                                                             
         CLI   SBMEDIA,C'T'        TELEVISION ONLY                              
         BNE   EXIT                                                             
         CLC   SBSRC,WANTSRCE      EITHER (A)RBITRON OR (N)IELSON               
         BNE   EXIT                                                             
         OC    SBKMKT,SBKMKT       NOT A SPILL MARKET?                          
         BZ    SPLR2               YES, GET NEXT ONE                            
         CLI   SBBTYP,0            INCLUDE REGULAR BOOK TYPE                    
         BZ    GOODBOOK                                                         
         CLI   SBBTYP,X'E0'        AND EXTENDED BOOK TYPE                       
         BE    GOODBOOK                                                         
         B     SPLR2                                                            
*                                                                               
GOODBOOK CLC   SBBOOK,=X'570B'                                                  
         BL    SPLR2                                                            
         CLC   SBSTAT,PRVSTAT                                                   
         BE    MRKMEQL                                                          
         BAS   RE,SPLR8                                                         
         MVI   SHOWSTAT,C'Y'                                                    
MRKMEQL  CLC   SBKMKT,PRVMKT                                                    
         BE    MRKEQL                                                           
         BAS   RE,SPLR8                                                         
*                                                                               
MRKEQL   MVI   PRINTIT,C'Y'                                                     
         CLI   SHOWSTAT,C'Y'                                                    
         BNE   *+10                                                             
         MVC   P(5),SBSTAT                                                      
*                                                                               
         LA    R2,P                                                             
         CLI   QOPT2,C'M'                                                       
         BE    USEMTABL                                                         
         LA    R4,TABLE0                                                        
         B     DISPLOOP                                                         
USEMTABL LA    R4,TABLE1                                                        
DISPLOOP CLC   =X'FFFF',0(R4)                                                   
         BNE   CHECK1                                                           
         B     SPLR2               NOT IN TABLE, NEXT RECORD                    
*                                                                               
CHECK1   CLC   0(2,R4),SBBOOK                                                   
         BE    GETDISP                                                          
         LA    R4,3(R4)                                                         
         B     DISPLOOP                                                         
GETDISP  ZIC   R3,2(R4)                                                         
         AR    R2,R3               CALCULATE DISPLACEMENT                       
         SR    R3,R3                                                            
         ICM   R3,3,SBKMKT                                                      
         EDIT  (R3),(4,(R2))                                                    
         MVC   PRVSTAT,SBSTAT                                                   
         MVC   PRVMKT,SBKMKT                                                    
         B     SPLR2                                                            
         SPACE 2                                                                
SPLR8    NTR1                                                                   
         OC    PRVSTAT,PRVSTAT                                                  
         BZ    SKIPREP                                                          
         CLI   PRINTIT,C'Y'                                                     
         BNE   SKIPREP                                                          
NOPLUSP  GOTO1 REPORT                                                           
SKIPREP  MVC   PRVSTAT,SBSTAT                                                   
         MVC   PRVMKT,SBKMKT                                                    
         CLI   PRINTIT,C'Y'                                                     
         BNE   SKIPREP1                                                         
         MVI   SHOWSTAT,C'N'                                                    
SKIPREP1 XIT1                                                                   
* ---------------------------------------------------------------               
         SPACE 3                                                                
EXIT     CLOSE (OUT,)                                                           
         CLI   FRSTTIME,C'Y'                                                    
         BNE   EXIT_RL                                                          
         MVI   FRSTTIME,C'N'                                                    
         XC    SPLKEY,SPLKEY                                                    
         MVC   SBCODE(3),=C'STN'   NIELSON'S                                    
         MVC   H6,=CL132'NIELSON SOURCE'                                        
         MVI   WANTSRCE,C'N'                                                    
         MVI   FORCEFUT,C'Y'       PAGE BREAK BEFORE NIELSON                    
WRAPUP   CLI   PRINTIT,C'Y'        MAKE SURE LAST ONE IS PRINTED                
         BE    PRNTREPT                                                         
         XC    P,P                                                              
PRNTREPT GOTO1 REPORT                                                           
         MVI   FORCEFUT,C'N'                                                    
         B     NIEL_1                                                           
*                                                                               
EXIT_RL  CLI   PRINTIT,C'Y'                                                     
         BNE   EXIT_XIT                                                         
         GOTO1 REPORT                                                           
EXIT_XIT XMOD1 1                                                                
         LTORG                                                                  
         EJECT                                                                  
OUT      DCB   DDNAME=OUT,DSORG=PS,RECFM=VB,MACRF=PM,                  +        
               LRECL=L'TAPEBUFF,BLKSIZE=25000                                   
         SPACE 2                                                                
* P DISPLACEMENT FOR DIFFERENT BOOKS                                            
TABLE0   DC    AL1(87,11,14)       NOV '87                                      
         DC    AL1(88,02,22)       FEB '88                                      
         DC    AL1(88,05,30)       MAY '88                                      
         DC    AL1(88,07,38)       JUL '88                                      
         DC    AL1(88,11,46)       NOV '88                                      
         DC    AL1(89,02,54)       FEB '89                                      
         DC    AL1(89,05,62)       MAY '89                                      
         DC    AL1(89,07,70)       JUL '89                                      
         DC    AL1(89,11,78)       NOV '89                                      
         DC    AL1(90,02,86)       FEB '90                                      
         DC    AL1(90,05,94)       MAY '90                                      
         DC    AL1(90,07,102)      JUL '90                                      
         DC    AL1(90,11,110)      NOV '90                                      
         DC    X'FFFF'                                                          
         SPACE 2                                                                
TABLE1   DC    AL1(88,02,14)       FEB '88                                      
         DC    AL1(89,02,22)       FEB '89                                      
         DC    AL1(90,02,30)       FEB '90                                      
         DC    AL1(88,05,38)       MAY '88                                      
         DC    AL1(89,05,46)       MAY '89                                      
         DC    AL1(90,05,54)       MAY '90                                      
         DC    AL1(88,07,62)       JUL '88                                      
         DC    AL1(89,07,70)       JUL '89                                      
         DC    AL1(90,07,78)       JUL '90                                      
         DC    AL1(87,11,86)       NOV '87                                      
         DC    AL1(88,11,94)       NOV '88                                      
         DC    AL1(89,11,102)      NOV '89                                      
         DC    AL1(90,11,110)      NOV '90                                      
         DC    X'FFFF'                                                          
         EJECT                                                                  
*          DATA SET DEDERA8907 AT LEVEL 135 AS OF 04/06/93                      
BOOKEQ   DS    0CL3                BOOK EQUATES FOR SURVEY MONTH                
         DC    C'13',AL1(2)                                                     
         DC    C'14',AL1(5)                                                     
         DC    C'15',AL1(7)                                                     
         DC    C'16',AL1(11)                                                    
         DC    C'17',AL1(05)        WINTER/SPRING ETHNIC                        
         DC    C'18',AL1(05)        FALL/WINTER/SPRING ETHNIC                   
         DC    C'19',AL1(11)        SUMMER/FALL ETHNIC                          
         DC    X'FF'                                                            
         SPACE 2                                                                
SPLRRA DC      F'0'                                                             
SPLRRB DC      F'0'                                                             
SPLRRC DC      F'0'                                                             
         SPACE 2                                                                
         EJECT                                                                  
RELO     DC    F'0'                                                             
RANKCTR  DC    F'0'                                                             
SPILCTR  DC    F'0'                                                             
LOOPCTR  DC    F'0'                                                             
USTOTAL  DC    F'0'                                                             
INDXCTR  DC    F'0'                                                             
LASTRANK DC    F'0'                                                             
DUB      DS    D                                                                
PRVSTAT  DS    CL5                                                              
PRVMKT   DS    CL2                                                              
PRINTIT  DS    C                                                                
SHOWSTAT DS    C                                                                
FRSTTIME DS    C                                                                
WANTSRCE DS    C                                                                
BMONTH   DS    XL1                 BINARY MONTH                                 
CMONTH   DS    CL2                 CHAR MONTH                                   
PVSMS    DS    CL7                                                              
SPLKEY   DS    CL(DSERECLQ)                                                     
SVSPLKEY DS    CL(DSERECLQ)                                                     
SPLKEY2  DS    CL(DSERECLQ)        W/ CORRECTED BOOK MONTH                      
HRNKLEN  DC    AL2(RANKLEN)                                                     
RPTYPE   DS    C                                                                
TAPEBUFF DS    CL80                                                             
PREVBUFF DS    CL(L'TAPEBUFF)                                                   
         SPACE 2                                                                
RANKTAB  DS    10000C                                                           
SPILTAB  DS    100000C                                                          
*                                                                               
RANKTABD DSECT                                                                  
RMKT     DS    CL2                                                              
RNKUNV   DS    CL4                                                              
RNKPCT   DS    CL4                                                              
RANK     DS    CL2                                                              
SMARANK  DS    CL2                                                              
RMKTNAM  DS    CL30                                                             
RNKTABEN DS    0C                                                               
RANKLEN  EQU   RNKTABEN-RMKT                                                    
         SPACE 2                                                                
SPILTABD DSECT                                                                  
SMS      DS    0CL7                                                             
SMKT     DS    CL2                                                              
SSTA     DS    CL5                                                              
SBOOK    DS    CL2                                                              
SPILEND  DS    0C                                                               
SPILLEN  EQU   SPILEND-SMKT                                                     
         SPACE 2                                                                
HOMETABD DSECT                                                                  
HSTA     DS    CL5                                                              
HMKT     DS    CL2                                                              
HOMTABEN DS    0C                                                               
HOMELEN  EQU   HOMTABEN-HSTA                                                    
         SPACE 2                                                                
         EJECT                                                                  
PLINE    DSECT                                                                  
         DS    CL1                                                              
PLMNUM   DS    CL4                                                              
         DS    CL3                                                              
PLMNAME  DS    CL30                                                             
         DS    CL3                                                              
PLMRANK  DS    CL3                                                              
         DS    CL3                                                              
PLSRANK  DS    CL3                                                              
         DS    CL2                                                              
PLUSPCT  DS    CL6                                                              
         DS    CL1                                                              
PLHMSUNV DS    CL11                                                             
         SPACE 2                                                                
PSLINE   DSECT                                                                  
         DS    CL3                                                              
PSSTA    DS    CL4                                                              
         DS    CL2                                                              
PSMKTNUM DS    CL4                                                              
         DS    CL2                                                              
PSMKTNAM DS    CL30                                                             
         DS    CL4                                                              
PSBOOKS  DS    CL80                                                             
         EJECT                                                                  
       ++INCLUDE DEDBLOCK                                                       
       ++INCLUDE DEDEMFILE                                                      
       ++INCLUDE DDCOMFACS                                                      
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'132SPREPLR21 05/01/02'                                      
         END                                                                    
