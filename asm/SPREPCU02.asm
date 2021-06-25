*          DATA SET SPREPCU02  AT LEVEL 025 AS OF 05/01/02                      
*PHASE SPCU02A,*                                                                
         TITLE 'SPCU02 - ADD NEW CLIENT COMMERCIALS TAKEN FROM CC'              
*                                                                               
* LEV 15-17 MAR04/88 ONLY ADD CURRENT DATES                                     
* LEV 18    JUL22/88 BIGGER TABLES, CK SIZES                                    
* LEV 21    MAR10/89 CHANGE FRON SPREPFX02X TO SPREPCU                          
* LEV 22    MAR29/91 INCREASE EXISTING CMLS TABLE SIZE - ANCMLS                 
*                    DO MESSAGE FOR MISSING/UNMATCHED CLIST                     
* LEV 23    APR01/91 BYPASS ALL CMMLS BELOW SEQ 1000                            
* LEV 24    APR16/93 ADD PRINT READABLE COMML CODE                              
*                                                                               
SPCU02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPCU02,RC                                                      
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         STM   R9,RC,SPCUR9                                                     
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    SR100                                                            
         CLI   MODE,CLTFRST                                                     
         BE    IN100                                                            
EXIT     XIT1                                                                   
         EJECT                                                                  
* RUNFRST                                                                       
*                                                                               
SR100    L     RE,=A(SPCUHL)                                                    
         LA    RE,RELO(RE)                                                      
         S     RE,RELO                                                          
*NOP*    ST    RE,HEADHOOK                                                      
*                                                                               
         B     EXIT                                                             
*                                                                               
RELO     DC    A(*)                                                             
         EJECT                                                                  
* CLTFRST - INITIALIZATION                                                      
*                                                                               
IN100    DS    0H                                                               
         ZAP   ACMLS,=P'0'         COMMLS ADDED                                 
         ZAP   TOTCCMLS,=P'0'      TOTAL CC COMMLS                              
         ZAP   CURCCMLS,=P'0'      CURR CC COMMLS                               
         XC    NCMLSN,NCMLSN                                                    
         MVI   DATADISP+1,24                                                    
         XC    KEY,KEY              READ CLIENT 'CC'                            
         MVC   KEY+1(1),BAGYMD                                                  
         MVC   KEY+2(2),=X'885F'   'CC' CLIENT CODE                             
         GOTO1 READ                                                             
         CLI   12(R1),0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         LA    R6,ACLTCC                                                        
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         LA    R0,220             # OF POSSIBLE PRDS (AGY)                      
         LA    R6,CLIST-CLTHDR(R6)                                              
IN120    L     R5,ADCLT            NEW CLIENT                                   
         LA    R5,CLIST-CLTHDR(R5)                                              
         LA    RF,220             # OF POSSIBLE PRDS (AGY)                      
IN140    OC    0(4,R5),0(R5)                                                    
         BZ    INDUMP                                                           
         CLC   0(3,R6),0(R5)                                                    
         BE    IN200                                                            
         LA    R5,4(R5)                                                         
         BCT   RF,IN140                                                         
INDUMP   MVC   P+1(30),=C'CLIENT PRODUCTS DON''T MATCH CC'                      
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ             END THIS REORT                               
*                                                                               
IN200    LA    R6,4(R6)                                                         
         OC    0(4,R6),0(R6)                                                    
         BZ    *+8                                                              
         BCT   R0,IN120                                                         
*                                                                               
* READ ALL COMMERCIALS FOR NEW CLIENT - ADD TO TABLE - COUNT                    
         LA    R2,ANCMLS           STORE COMML HERE                             
         LR    RE,R2                                                            
         LH    RF,=Y(L'ANCMLS)     LEN OF TABLE                                 
         XCEFL                                                                  
         XC    KEY,KEY                                                          
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),BCLT       NEW CLIENT CODE                              
         GOTO1 HIGH                                                             
         B     IN320                                                            
*                                                                               
IN300    GOTO1 SEQ                                                              
IN320    CLC   KEY(5),KEYSAVE                                                   
         BNE   RD100                                                            
         MVC   0(8,R2),KEY+5                                                    
         ICM   RE,3,NCMLSN                                                      
         LA    RE,1(RE)                                                         
         STH   RE,NCMLSN                                                        
         LA    R2,8(R2)                                                         
         LA    R1,ANCMLS           START OF NEXT TABLE                          
         AH    R1,=Y(L'ANCMLS)     END OF TABLE                                 
         CR    R2,R1                                                            
         BL    IN300                                                            
         MVC   P+1(40),=CL40'EXCEEDED COMML TABLE SIZE (2500 CMLS)'             
         GOTO1 REPORT                                                           
         DC    H'0'                                                             
         EJECT                                                                  
*                                                                               
* READ ALL COMMERCIALS FOR 'CC' & ADD TO NEW CLIENT IF NOT THERE                
         SPACE                                                                  
RD100    XC    KEY,KEY              READ CLIENT 'CC'                            
         MVC   KEY(2),=X'0A21'                                                  
         MVC   KEY+2(1),BAGYMD                                                  
         MVC   KEY+3(2),=X'885F'   'CC' CLIENT CODE                             
         GOTO1 HIGH                                                             
         B     RD220                                                            
*                                                                               
RD200    GOTO1 SEQ                                                              
RD220    CLC   KEY(5),KEYSAVE                                                   
         BNE   ENDIN1                                                           
         CLI   KEY+5,0                                                          
         BE    RD200                                                            
*                                                                               
* CHECK TO SEE IF COMMERCIAL ALREADY ON FILE - COUNT CC COMMLS                  
*                                                                               
         AP    TOTCCMLS,=P'1'                                                   
         SPACE                                                                  
         LA    R2,ANCMLS                                                        
         ICM   R0,3,NCMLSN                                                      
         BZ    RD320                                                            
RD300    CLC   0(8,R2),KEY+5                                                    
         BE    RD200                                                            
         LA    R2,8(R2)                                                         
         BCT   R0,RD300                                                         
*                                                                               
RD320    L     R6,ADBUY                                                         
         ST    R6,AREC                                                          
         GOTO1 GET                                                              
         SPACE                                                                  
         MVI   ELCODE,X'10'        FIND SEQ #                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         USING CMLDTAEL,R6                                                      
         CLC   CMLSEQ,=X'0003E8'   ONLY DO SEQ #'S ABOVE 1000                   
         BL    RD200                                                            
         SPACE                                                                  
         CLC   TODAYB(1),CMLRCL    BYPASS OLD COMMLS (BEFORE THIS YR)           
         BH    RD200                                                            
         DROP  R6                                                               
         AP    CURCCMLS,=P'1'                                                   
         SPACE                                                                  
         L     R6,ADBUY                                                         
         SPACE                                                                  
* MOVE IN NEW CLIENT CODE - CHANGE PRODUCT CODES                                
         SPACE                                                                  
         USING CMLRECD,R6                                                       
         MVC   CMLKCLT,BCLT                                                     
         BAS   RE,MTCHPRD                                                       
         EJECT                                                                  
* ADD RECORD TO FILE                                                            
         SPACE                                                                  
WR100    MVC   SVKEY,KEY                                                        
         CLI   RCWRITE,C'Y'                                                     
         BNE   WR200                                                            
         GOTO1 DATAMGR,DMCB,=C'ADDREC',=C'SPTFILE',KEY+14,CMLKEY,DMWORK         
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WR200    MVC   KEY(5),0(R6)        BUILD PASSIVE KEY                            
         OI    KEY+1,X'80'         CHANGE 21 TO A1                              
*                                                                               
         MVI   ELCODE,X'10'        FIND SEQ #                                   
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
         MVC   KEY+5(3),2(R6)      FINISHES BUILDING PASSIVE KEY                
         XC    KEY+8(5),KEY+8                                                   
         CLI   RCWRITE,C'Y'                                                     
         BNE   WR300                                                            
         GOTO1 DATAMGR,DMCB,=C'DMADD',=C'SPTDIR',KEY,KEY                        
         CLI   DMCB+8,0                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
*                                                                               
WR300    GOTO1 HEXOUT,DMCB,KEY,P,13,=C'T0G'                                     
         GOTO1 REPORT                                                           
         L     R6,ADBUY                                                         
         GOTO1 HEXOUT,DMCB,CMLKID,P,60,=C'T0G'                                  
         GOTO1 REPORT                                                           
         MVC   P+10(8),CMLKCML                                                  
         MVI   SPACING,2                                                        
         GOTO1 REPORT                                                           
         AP    ACMLS,=P'1'                                                      
         MVC   KEY,SVKEY                                                        
         GOTO1 HIGH                FOR SEQ READ                                 
         B     RD200                                                            
         EJECT                                                                  
ENDIN1   DS    0H                                                               
         MVC   P(26),=CL26'** EXISTING COMMERCIALS **'                          
         EDIT  (2,NCMLSN),(5,P+30)                                              
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=CL26'** COMML RECORDS ADDED **'                           
         EDIT  (P3,ACMLS),(5,P+30)                                              
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=CL26'** TOTAL CC COMMERCIALS **'                          
         EDIT  (P3,TOTCCMLS),(5,P+30)                                           
         GOTO1 REPORT                                                           
         GOTO1 REPORT                                                           
         MVC   P(26),=CL26'** CURR CC COMMERCIALS **'                           
         EDIT  (P3,CURCCMLS),(5,P+30)                                           
         GOTO1 REPORT                                                           
         GOTO1 AENDREQ             END THIS REORT                               
         B     EXIT                                                             
*                                                                               
         EJECT                                                                  
* MATCH PRODUCTS FROM 1 CLIENT TO ANOTHER - NO MATCH DUMP                       
* ANCMLS  - CLIENT 'CC'                                                         
* ADCLT   - NEW CLIENT                                                          
         SPACE                                                                  
MTCHPRD  NTR1                                                                   
         L     R6,ADBUY                                                         
         MVI   ELCODE,X'20'        GET PRD LIST                                 
         BAS   RE,GETEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         XC    ELEM,ELEM                                                        
*                                                                               
         ZIC   R1,1(R6)                                                         
         BCTR  R1,0                                                             
         EX    R1,SAVEEL                                                        
         GOTO1 HEXOUT,DMCB,ELEM,P+60,25,=C'T0G'                                 
*                                                                               
         CLI   2(R6),X'FF'         ALL PRODUCTS NO CHANGE NECESSARY             
         BE    MC400                                                            
*                                                                               
         ZIC   R5,1(R6)            LENGTH                                       
         BCTR  R5,0                                                             
         BCTR  R5,0                NUMBER OF PRODUCTS                           
         LA    R2,ELEM+2           START OF CC PRODUCT LIST                     
         LR    R7,R6               SAVE ADDR OF ELEMENT                         
         LA    R7,2(R7)            START OF NEW PRODUCT LIST                    
MC120    LA    R4,ACLTCC           'CC' CLIENT LIST                             
         LA    R4,CLIST-CLTHDR(R4)                                              
         LA    R0,220              MAX # OF PRDS                                
MC140    OC    0(4,R4),0(R4)       END OF LIST                                  
         BE    MCDUMP              PUT CLI/PRD TO ERROR MESSAGE                 
         CLC   3(1,R4),0(R2)       A MATCH                                      
         BE    MC200                                                            
         LA    R4,4(R4)            NEXT PRD                                     
         BCT   R0,MC140                                                         
MCDUMP   DC    H'0'                R0 INDICATES WHICH DUMP                      
*                                                                               
MC200    LA    R0,220              MAX # OF PRDS                                
         L     R3,ADCLT                                                         
         LA    R3,CLIST-CLTHDR(R3) THIS CLIENT PRD LIST                         
MC220    OC    0(4,R3),0(R3)       END OF LIST                                  
         BE    MCDUMP                                                           
         CLC   0(3,R3),0(R4)       A MATCH                                      
         BE    MC300                                                            
         LA    R3,4(R3)            NEXT PRD                                     
         BCT   R0,MC220                                                         
         DC    H'0'                                                             
*                                                                               
MC300    MVC   0(1,R7),3(R3)       PRD NUMBER                                   
         LA    R2,1(R2)            NEXT ELEMENT ADDRESS                         
         LA    R7,1(R7)            NEXT PRD IN NEW ELEMENT                      
         BCT   R5,MC120                                                         
*                                                                               
         XC    ELEM,ELEM                                                        
         ZIC   R1,1(R6)            LENGTH                                       
         BCTR  R1,0                                                             
         EX    R1,SAVEEL                                                        
*                                                                               
MC400    GOTO1 HEXOUT,DMCB,ELEM,P,25,=C'T0G'                                    
         GOTO1 REPORT                                                           
         B     EXIT                                                             
*                                                                               
SAVEEL   MVC   ELEM(0),0(R6)                                                    
         EJECT                                                                  
* HEADLINE HOOK                                                                 
*                                                                               
         USING *,RF                                                             
SPCUHL   NTR1                                                                   
         LM    R9,RC,SPCUR9                                                     
         B     HDHK2                                                            
*                                                                               
SPCUR9   DC    4F'0'                                                            
         DROP  RF                                                               
*                                                                               
HDHK2    B     EXIT                                                             
         SPACE                                                                  
         PRINT GEN                                                              
GETEL    GETEL R6,DATADISP,ELCODE                                               
         PRINT NOGEN                                                            
*                                                                               
         LTORG                                                                  
         SPACE 3                                                                
ACMLS    DC    PL3'0'              COMMLS ADDED                                 
TOTCCMLS DC    PL3'0'              TOTAL CC COMMLS                              
CURCCMLS DC    PL3'0'              CURR CC COMMLS                               
SVKEY    DS    CL32                                                             
ELEM     DS    CL50                                                             
ELCODE   DS    XL1                                                              
NCMLSN   DS    H                                                                
         DS    0D                                                               
         DC    CL8'*CCLTHD*'                                                    
ACLTCC   DS    2000C               CLIENT CC CLIENT HDR REC                     
         DS    0D                                                               
         DC    CL8'*CMLTAB*'                                                    
ANCMLS   DS    CL20000                                                          
         DC    CL8'*ENDTAB*'                                                    
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPTRCMML                                                       
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPGENCLT                                                       
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'025SPREPCU02 05/01/02'                                      
         END                                                                    
