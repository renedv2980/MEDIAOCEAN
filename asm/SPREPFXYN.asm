*          DATA SET SPREPFXYN  AT LEVEL 012 AS OF 05/01/02                      
*          DATA SET SPREPFXBS  AT LEVEL 043 AS OF 02/04/93                      
*PHASE SPFX02K,+0                                                               
         TITLE 'SPFX02 - FIND ANY BAD BUY ACTIVITY RECS'                        
SPFX02   CSECT                                                                  
         DS    4096C                                                            
         ORG   *-4096                                                           
         PRINT NOGEN                                                            
         NMOD1 0,SPFX02,RR=R2                                                   
         LA    RC,2048(RB)                                                      
         LA    RC,2048(RC)                                                      
         USING SPFX02+4096,RC                                                   
         ST    R2,RELO                                                          
*                                                                               
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
*                                                                               
         CLI   MODE,RUNFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
*                                                                               
* RUNFRST - RUN THROUGH ALL TRAFFIC BUY ACTIVITY RECS                           
*                                                                               
FX10     XC    KEY,KEY             RESET                                        
         MVC   KEY(2),=X'0A2E'                                                  
*                                                                               
FX20     OI    DMINBTS,X'08'       PASS DELETED RECS                            
         MVI   DMOUTBTS,X'FD'                                                   
         GOTO1 HIGH                                                             
         SPACE                                                                  
         CLC   KEY(2),KEYSAVE                                                   
         BNE   FX90                                                             
         TM    KEY+13,X'80'        THIS A DELETED REC                           
         BZ    FX22                 NO                                          
         L     RE,INACTD                                                        
         LA    RE,1(RE)                                                         
         ST    RE,INACTD                                                        
         B     FX80                                                             
FX22     L     RE,INACTS                                                        
         LA    RE,1(RE)                                                         
         ST    RE,INACTS                                                        
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'GETREC',SPTFILE,KEY+14,TBAREC,DMWORK             
         SPACE                                                                  
* CHECK FOR BAD ENDING CHARACTER                                                
         SPACE                                                                  
         MVC   FULL,SPACES                                                      
         SPACE                                                                  
         LA    R5,TBAREC+24                                                     
         SPACE                                                                  
FX25     CLI   0(R5),X'05'         AN ACTIVITY ELEM?                            
         BNE   FX40                                                             
         SPACE                                                                  
         CLC   =X'B99C',2(R5)                                                   
         BH    FX30                                                             
         MVI   FULL+1,C'*'                                                      
         SPACE                                                                  
FX30     LA    R5,7(,R5)                                                        
         B     FX25                                                             
FX40     CLI   0(R5),X'F1'                                                      
         BNE   FX60                                                             
         SPACE                                                                  
         LA    R1,TBAREC                                                        
         LA    R0,20(,R5)                                                       
         SR    R0,R1                                                            
         CLM   R0,3,13(R1)                                                      
         BH    FX80                                                             
         BE    FX45                                                             
         SPACE                                                                  
         AH    R0,=H'1'                                                         
         CLM   R0,3,13(R1)         MUST BE SAME LENGTH                          
         BL    FX60                                                             
         BE    FX45                                                             
         MVC   P+60(17),=C'RECORD TOO SHORT?'                                   
         B     FX60                                                             
         SPACE                                                                  
FX45     CLI   20(R5),0                                                         
         BE    FX80                                                             
         MVI   FULL,C'*'                                                        
         L     RE,BADBA                                                         
         LA    RE,1(RE)                                                         
         ST    RE,BADBA                                                         
         CLI   FULL+1,C'*'                                                      
         BNE   FX50                                                             
         L     RE,BADBACUR                                                      
         LA    RE,1(RE)                                                         
         ST    RE,BADBACUR                                                      
FX50     CLI   FULL,C' '                                                        
         BE    FX80                                                             
         SPACE                                                                  
         B     FX66                                                             
*                                                                               
FX60     MVI   FULL,C'#'                                                        
         L     RE,EXTRAEL                                                       
         LA    RE,1(RE)                                                         
         ST    RE,EXTRAEL                                                       
FX66     LA    R6,KEY                                                           
         USING TBARECD,R6                                                       
         MVI   BYTE,0                                                           
         MVZ   BYTE,KEY+2                                                       
         MVI   PMED,C'T'                                                        
         MVI   BYTE,0                                                           
         MVN   BYTE,KEY+2                                                       
         CLI   BYTE,X'01'          TV                                           
         BE    PRTACT10                                                         
         MVI   PMED,C'R'                                                        
         CLI   BYTE,X'02'          RADIO                                        
         BE    PRTACT10                                                         
         MVI   PMED,C'N'                                                        
         CLI   BYTE,X'03'          NETWORK TV                                   
         BE    PRTACT10                                                         
         MVI   PMED,C'X'                                                        
         CLI   BYTE,X'04'          NETWORK RADIO                                
         BE    PRTACT10                                                         
         DC    H'0'                                                             
PRTACT10 GOTO1 CLUNPK,DMCB,TBAKCLT,PCLT                                         
         GOTO1 MSUNPK,DMCB,TBAKMKT,PMKT,WORK                                    
         MVC   PSTA(4),WORK                                                     
         MVI   PSTA+4,C'-'                                                      
         MVC   PSTA+5(1),WORK+4                                                 
         CLI   WORK+4,C' '                                                      
         BNE   *+8                                                              
         MVI   PSTA+5,C'T'                                                      
         ZIC   R0,TBAKEST                                                       
         CVD   R0,DUB                                                           
         OI    DUB+7,X'0F'                                                      
         UNPK  WORK(10),DUB                                                     
         MVC   PEST,WORK+7                                                      
         MVI   ELCDLO,X'F1'                                                     
         MVI   ELCDHI,X'F1'                                                     
         LA    R6,TBAREC+24                                                     
         SPACE                                                                  
         CLI   0(R6),X'F1'                                                      
         BE    PRTACT30                                                         
         SPACE                                                                  
         BAS   RE,BUYEL                                                         
         BE    *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
PRTACT30 DS    0H                                                               
         OC    2(3,R6),2(R6)                                                    
         BNZ   *+6                                                              
         DC    H'0'                                                             
         GOTO1 DATCON,DMCB,(3,2(R6)),(5,PADDT)                                  
         SPACE                                                                  
         OC    8(3,R6),8(R6)                                                    
         BNZ   *+14                                                             
         MVC   PCHGDT+2(4),=C'NONE'                                             
         B     PRTACT40                                                         
         SPACE                                                                  
         GOTO1 DATCON,DMCB,(3,8(R6)),(5,PCHGDT)                                 
         SPACE                                                                  
PRTACT40 DS    0H                                                               
         GOTO1 HEXOUT,DMCB,KEY+14,PDSKAD,4,0,0                                  
         SPACE                                                                  
         MVC   PACTION(2),FULL                                                  
         SPACE                                                                  
         GOTO1 REPORT                                                           
         SPACE                                                                  
         CLI   FULL,C'#'                                                        
         BE    FX80                                                             
         SPACE                                                                  
         CLI   0(R5),X'F1'                                                      
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   20(R5),0                                                         
         BNE   *+6                                                              
         DC    H'0'                                                             
         SPACE                                                                  
         CLI   RCWRITE,C'Y'                                                     
         BNE   FX80                                                             
         SPACE                                                                  
         MVI   20(R5),0                                                         
         SPACE                                                                  
         GOTO1 DATAMGR,DMCB,=C'PUTREC',SPTFILE,KEY+14,TBAREC,DMWORK             
         SPACE                                                                  
FX80     DS    0H                                                               
         ZIC   R1,KEY+12           FORCE TO NEXT BUY ACT REC                    
         LA    R1,1(,R1)                                                        
         STC   R1,KEY+12                                                        
         SPACE                                                                  
         B     FX20                                                             
*                                                                               
*          EOJ TOTALS                                                           
*                                                                               
FX90     LA    R2,BUCKTABL                                                      
         LA    R3,BUCKTAB                                                       
FX94     L     R4,0(,R3)                                                        
         EDIT  (R4),(8,P+2)                                                     
         MVC   P+12(24),4(R3)                                                   
         LA    R3,28(,R3)                                                       
         GOTO1 REPORT                                                           
         BCT   R2,FX94                                                          
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 2                                                                
BUYEL    CLI   0(R6),0                                                          
         BE    BUYELX                                                           
         SR    R0,R0                                                            
         ICM   R0,1,1(R6)                                                       
         BNZ   *+6                                                              
         DC    H'0'                                                             
         AR    R6,R0                                                            
         CLC   0(1,R6),ELCDLO                                                   
         BL    BUYEL                                                            
         CLC   0(1,R6),ELCDHI                                                   
         BH    BUYEL                                                            
         CR    RE,RE               SET CC EQ                                    
         BR    RE                                                               
BUYELX   LTR   RE,RE                                                            
         BR    RE                                                               
         SPACE                                                                  
         GETEL R6,24,ELCODE                                                     
         EJECT                                                                  
* CONSTANTS                                                                     
*                                                                               
         DS    0D                                                               
ELEM     DS    XL256                                                            
ELCODE   DS    X                                                                
*                                                                               
* TABLE OF RECORD COUNT BUCKETS                                                 
*                                                                               
         DS    0F                                                               
BUCKTAB  DS    0CL28                                                            
INACTS   DC    F'0',CL24'BUY ACT RECS READ  '                                   
INACTD   DC    F'0',CL24'DEL BUY ACT RECS READ'                                 
BADBA    DC    F'0',CL24'BAD BUY ACT RECS'                                      
BADBACUR DC    F'0',CL24'BAD BUY ACT - CURR'                                    
EXTRAEL  DC    F'0',CL24'EXTRA ELEM     '                                       
BUCKTABL EQU   (*-BUCKTAB)/28                                                   
         SPACE 2                                                                
SVBUYKY  DS    CL18                                                             
         SPACE                                                                  
SVTBAKEY DC    CL18' '                                                          
         SPACE                                                                  
CLTMT    DC    XL2'B27B'           MT2                                          
         DC    XL2'B27C'           MT3                                          
ELCDLO   DC    XL1'00'                                                          
ELCDHI   DC    XL1'00'                                                          
ACTFLAG  DC    XL1'00'                                                          
WKTAB    DC    XL200'00'           TABLE OF WEEKS WITH BUYS                     
WKTABLEN EQU   (*-WKTAB)/2                                                      
         SPACE                                                                  
ACTAB    DC    XL240'00'           TABLE OF WEEKS FROM ACTIVITY REC             
ACTABLEN EQU   (*-ACTAB)/3         ENTRY IS DATE AND FLAG                       
ACTSW    DC    X'00'               1 = NO BUYS FOR ACTIVITY RECORD              
         SPACE                                                                  
* LITERAL POOL                                                                  
*                                                                               
         LTORG                                                                  
         DC    C'* BUY ACTIVITY RECORD **'                                      
TBAREC   DS    CL2000                                                           
         EJECT                                                                  
CLTHDRD  DSECT                                                                  
       ++INCLUDE SPGENCLT                                                       
BUYRECD  DSECT                                                                  
       ++INCLUDE SPGENBUY                                                       
       ++INCLUDE SPTRTBAE                                                       
         EJECT                                                                  
*                                                                               
       ++INCLUDE SPGENSDEF                                                      
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
         EJECT                                                                  
         PRINT ON                                                               
* DSECT FOR PRINT LINE                                                          
SPWORKD  DSECT                                                                  
         ORG   P                                                                
         DS    CL2                                                              
PAGY     DS    CL2                                                              
         DS    CL1                                                              
PMED     DS    CL1                                                              
         DS    CL1                                                              
PCLT     DS    CL3                                                              
         DS    CL1                                                              
PMKT     DS    CL4                                                              
         DS    CL1                                                              
PSTA     DS    CL6                                                              
         DS    CL1                                                              
PPRD     DS    CL7                                                              
         DS    CL1                                                              
PPTR     DS    CL7                                                              
         DS    CL1                                                              
PEST     DS    CL3                                                              
         DS    CL1                                                              
PADDT    DS    CL8                                                              
         DS    CL1                                                              
PCHGDT   DS    CL8                                                              
         DS    CL1                                                              
PDSKAD   DS    CL8                                                              
         DS    CL1                                                              
PACTION  DS    CL1                                                              
         DS    CL1                                                              
PREASON  DS    CL7                                                              
         DS    CL1                                                              
PACTIV   DS    CL52                                                             
         DS    CL1 SHOULD BE POSITION 133                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012SPREPFXYN 05/01/02'                                      
         END                                                                    
