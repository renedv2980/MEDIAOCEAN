*          DATA SET ACREP8002  AT LEVEL 017 AS OF 06/14/12                      
*PHASE AC8002A                                                                  
         TITLE 'LIST REPORT'                                                    
AC8002   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**AC80**,RR=R5                                                 
         L     RA,0(R1)                                                         
         USING ACWORKD,RA                                                       
         LA    RC,SPACEND                                                       
         USING PROGD,RC                                                         
         ST    R5,PRELOC                                                        
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   LST1                                                             
         MVI   FORCEHED,C'Y'                                                    
         MVC   PAGE,=H'1'                                                       
         CLI   QOPT1,C'Y'          REQUEST FOR ALL COMPANIES                    
         BNE   EXIT                NO                                           
         CLI   FIRST,0             FIRST REQFRST                                
         BNE   EXIT                NO                                           
         L     RE,CARDS            NOOP CALL TO CARDS UNTIL I'M                 
         MVC   SVCRDCAL,0(RE)      DONE READING LIST RECS                       
         MVC   0(2,RE),=X'07FE'                                                 
         XC    IO(42),IO                                                        
         MVI   IO,X'1D'            SET TO READ 1ST LIST COMPANY REC             
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',IO,IO                    
         MVC   QCOMPANY,IO+1                                                    
         MVC   SVQRQST,QRECORD     SAVE FOR RESTORE AT RUNLAST TIME             
         B     EXIT                                                             
         EJECT                                                                  
LST1     CLI   MODE,PROCACL                                                     
         BNE   LST60                                                            
         L     RF,ADACC                                                         
         MVC   P+1(5),2(RF)                                                     
*                                                                               
         LA    R0,LISTWRK             FILL LISTWRK WITH SPACES                  
         LHI   R1,L'LISTWRK                                                     
         SR    RF,RF                                                            
         ICM   RF,8,SPACES                                                      
         MVCL  R0,RE                                                            
*                                                                               
         L     R2,ADACC                                                         
         AH    R2,DATADISP                                                      
         LA    R3,LISTWRK                                                       
LST10    CLI   0(R2),0                                                          
         BE    LST50                                                            
         CLI   0(R2),X'1F'                                                      
         BE    LST30                                                            
         CLI   0(R2),X'1E'                                                      
         BE    LST12                                                            
         CLI   0(R2),X'20'                                                      
         BNE   LST18                                                            
         USING ACNAMED,R2                                                       
         ZIC   RE,ACNMLEN                                                       
         SH    RE,=H'3'                                                         
         EX    RE,*+8                                                           
         B     LST18                                                            
         MVC   P+7(0),ACNMNAME                                                  
*                                                                               
         USING ACLISTD,R2                                                       
LST12    MVC   P+47(15),=C'LEDGER         '                                     
         CLI   ACLITYPE,C'L'                                                    
         BE    LST14                                                            
         MVC   P+47(15),=C'WORK-CODE      '                                     
         CLI   ACLITYPE,C'W'                                                    
         BE    LST14                                                            
         MVC   P+47(15),=C'MEDIA          '                                     
         CLI   ACLITYPE,C'M'                                                    
         BE    LST14                                                            
         MVC   P+47(15),=C'ACCOUNT        '                                     
*                                                                               
LST14    MVC   LISTYPE,ACLITYPE                                                 
         CLC   ACLIDATE,=X'FFFFFF'                                              
         BNE   LST16                                                            
*                                                                               
LST15    MVC   P+66(9),=C'PERMANENT'                                            
         B     LST18                                                            
*                                                                               
LST16    DS    0H                                                               
         GOTO1 DATCON,DMCB,(1,ACLIDATE),(8,P+66)                                
         B     LST18                                                            
*                                                                               
LST18    ZIC   RE,1(R2)                                                         
         AR    R2,RE                                                            
         B     LST10                                                            
*                                                                               
LST30    DS    0H                                                               
         LR    R4,R2                                                            
         USING ACLDATAD,R4                                                      
         CLI   LISTYPE,C'A'        IF ACCOUNT TYPE                              
         BNE   *+8                                                              
         BAS   RE,GETNAME          GET LEVEL NAME                               
         ZIC   RE,ACLDLEN          CALCULATE NO OF ITEMS                        
         SH    RE,=H'6'                                                         
         CLI   LISTYPE,C'L'                                                     
         BE    *+8                                                              
         SH    RE,=H'4'                                                         
         ZIC   R1,ACLDITLN                                                      
         SRDA  RE,32                                                            
         DR    RE,R1                                                            
         LTR   RF,RF                                                            
         BNP   LST18                                                            
         LA    R5,ACLDACCS                                                      
         CLI   LISTYPE,C'L'                                                     
         BNE   LST32                                                            
         LA    R5,ACLDUL                                                        
LST32    CLI   LISTYPE,C'L'                                                     
         BNE   LST34               BRANCH IF ACCOUNT DATA                       
         MVC   0(2,R3),0(R5)       UNIT/LEDGER LIST                             
         LA    R3,2(R3)                                                         
         B     LST36                                                            
*                                                                               
LST34    ZIC   R6,ACLDITLN                                                      
         BCTR  R6,0                                                             
         LA    R1,0(R6,R5)         SET TO END                                   
         CLI   0(R1),C' '          SCAN BACKWARDS TO GET TRUE LENGTH            
         BNE   LST35                                                            
         BCTR  R1,0                                                             
         BCT   R6,*-10                                                          
LST35    EX    R6,*+8                                                           
         B     *+10                                                             
         MVC   0(0,R3),0(R5)                                                    
         LA    R3,1(R3,R6)                                                      
LST36    MVI   0(R3),C','          SEPARATE ITEMS WITH COMMAS                   
         LA    R3,1(R3)                                                         
         ZIC   RE,ACLDITLN                                                      
         AR    R5,RE                                                            
         BCT   RF,LST32                                                         
         B     LST18                                                            
                                                                                
LST50    BCTR  R3,0                                                             
         MVI   0(R3),C' '          DELETE TRAILING COMMA                        
         GOTO1 CHOPPER,DMCB,LISTWRK,(30,CHOPAR),40,C'LEN=',L'CHOPAR             
         L     R3,DMCB+8                                                        
         LA    R2,CHOPAR                                                        
LST52    CLC   0(30,R2),SPACES                                                  
         BE    LST54                                                            
         MVC   P+79(30),0(R2)                                                   
         BAS   RE,PRINTIT                                                       
         LA    R2,30(R2)                                                        
         BCT   R3,LST52                                                         
LST54    BAS   RE,PRINTIT                                                       
         B     EXIT                                                             
         SPACE 2                                                                
EXIT     XMOD1                                                                  
         EJECT                                                                  
LST60    CLI   MODE,RUNLAST                                                     
         BNE   EXIT                                                             
         MVC   QRECORD,SVQRQST     RESTORE INITIAL REQUEST                      
         CLI   QOPT1,C'Y'          IF REQUESTING MULTUPLE COMPANIES             
         BNE   EXIT                                                             
         XC    IO(42),IO           CLEAR KEY                                    
         ZIC   RF,QCOMPANY         BUILD AND INCREMENT KEY TO READ              
         LA    RF,1(RF)            NEXT COMPANY'S 1ST LIST REC                  
         STC   RF,IO+1                                                          
         MVI   IO,X'1D'                                                         
         GOTO1 DATAMGR,DMCB,(0,=C'DMRDHI'),=C'ACCOUNT',IO,IO                    
         CLI   IO,X'1D'            IF NE                                        
         BE    *+18                                                             
         L     RE,CARDS            RESTORE CARD CALL CODE                       
         MVC   0(2,RE),SVCRDCAL                                                 
         B     EXIT                DONE                                         
         MVC   QCOMPANY,IO+1       TRICK CONTROLLER INTO PROCESSING             
         MVC   SVQRQST,QRECORD     REQUEST FOR NEXT COMPANY                     
         L     RE,AMONACC                                                       
         USING ACMD,RE                                                          
         MVI   ACMMODE,REQFRST                                                  
         MVI   FIRST,X'FF'                                                      
         B     EXIT                                                             
         DROP  RE                                                               
         EJECT                                                                  
*              ROUTINE TO GET LEVEL NAMES                                       
         SPACE 2                                                                
GETNAME  NTR1                                                                   
         CLC   ACLDAUNT(2),=C'ME'  OLD MEDIA RECORDS (BEFORE TYPE M)            
         BE    EXIT                                                             
         L     RF,ADACC            ADDR. OF LIST RECORD                         
         MVC   KEY,SPACES                                                       
         MVC   KEY(1),1(RF)        COMPANY                                      
         MVC   KEY+1(2),ACLDAUNT   UNIT,LEDGER                                  
         MVC   IO(60),SPACES                                                    
         MVC   IO(L'KEY),KEY                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'DMREAD'),=C'ACCOUNT',IO,IO,    *        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                NO LEDGER RECORD                             
         L     RF,DMCB+8                                                        
         USING ACKEYD,RF                                                        
         LA    RF,ACRECORD         FIRST ELEMENT                                
         XR    R1,R1                                                            
GET2     CLI   0(RF),0                                                          
         BNE   *+6                                                              
         DC    H'0'                NO HIERARCHY ELEMENT                         
         CLI   0(RF),X'16'                                                      
         BE    GET4                                                             
         IC    R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     GET2                                                             
         SPACE 1                                                                
GET4     LA    RF,3(RF)            LEVEL 1 NAME                                 
         XR    RE,RE                                                            
         ICM   RE,1,ACLDALVL       LEVEL OF LIST                                
         BCTR  RE,0                                                             
         MH    RE,=H'16'                                                        
         AR    RF,RE                                                            
         MVC   P+47(15),0(RF)      MOVE IN LEVEL DESCRIPTION                    
         SPACE 1                                                                
         L     RF,ADACC            RESET DATAMGR FOR NEXT LIST RECORD           
         MVC   KEY(L'KEY),0(RF)                                                 
         MVC   IO(60),SPACES                                                    
         MVC   IO(L'KEY),KEY                                                    
         GOTO1 DATAMGR,DMCB,(DMINBTS,=C'DMREAD'),=C'ACCOUNT',IO,IO,    *        
               DMWORK                                                           
         CLI   DMCB+8,0                                                         
         BE    EXIT                                                             
         DC    H'0'                LIST RECORD VANISHED                         
         EJECT                                                                  
PRINTIT  NTR1                                                                   
         CLC   SVCOMP,QCOMPANY     SINCE I MAY BE REPORTING ON                  
         BNE   PT1A                ALL COMPS WITHIN AN ACC FILE                 
         LA    RE,SVNAME           I NEED TO FETCH THE COMPANY NAME             
         ZIC   R1,SVLEN                                                         
         B     PT1D                                                             
*                                                                               
PT1A     MVC   IO(42),SPACES       READ FOR CURRENT CO.-GET NAME                
         MVC   IO(1),QCOMPANY                                                   
         GOTO1 DATAMGR,DMCB,(0,=C'DMREAD'),=C'ACCOUNT',IO,IO                    
         TM    DMCB+8,X'FF'                                                     
         BNZ   PT1E                NO COMPANY REC ?                             
         L     RF,DMCB+8                                                        
         USING ACKEYD,RF                                                        
         LA    RF,ACRECORD                                                      
PT1B     CLI   0(RF),X'20'                                                      
         BE    PT1C                                                             
         CLI   0(RF),0                                                          
         BE    PT1E                NO NAME ?                                    
         ZIC   R1,1(RF)                                                         
         AR    RF,R1                                                            
         B     PT1B                                                             
         USING ACNAMED,RF                                                       
PT1C     ZIC   R1,ACNMLEN                                                       
         SH    R1,=H'3'                                                         
         LA    RE,ACNMNAME                                                      
PT1D     MVC   HEAD3+1(8),=C'COMPANY '                                          
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   HEAD3+9(0),0(RE)                                                 
         EX    R1,*+8                                                           
         B     *+10                                                             
         MVC   SVNAME(0),0(RE)                                                  
         STC   R1,SVLEN                                                         
         MVC   SVCOMP,QCOMPANY                                                  
         DROP  RF                                                               
*                                                                               
PT1E     GOTO1 ACREPORT                                                         
         B     EXIT                                                             
         EJECT                                                                  
*                                                                               
         LTORG                                                                  
*                                                                               
PROGD    DSECT                                                                  
PRELOC   DS    F                                                                
LISTYPE  DS    CL1                                                              
FIRST    DS    CL1                                                              
SVCRDCAL DS    CL2                                                              
SVNAME   DS    CL36                                                             
SVCOMP   DS    CL1                                                              
SVLEN    DS    CL1                                                              
SVQRQST  DS    CL80                                                             
LISTWRK  DS    CL1200                                                           
CHOPAR   DS    CL1200                                                           
IO       DS    CL1028                                                           
         EJECT                                                                  
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
*        ACMASTD                                                                
         PRINT OFF                                                              
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACMASTD                                                        
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'017ACREP8002 06/14/12'                                      
         END                                                                    
