*          DATA SET SPREP0203  AT LEVEL 019 AS OF 12/13/00                      
*PHASE SP0203A                                                                  
         TITLE 'STATION FILE *PURGE PROGRAM - SUBCONTROLLER'                    
*                                                                               
*        THIS PROGRAM SHOULD ONLY BE USED ON A COPIED STATION FILE              
*        BEFORE ANY ACTIVITY HAS BEEN ADDED                                     
*        IT DOES NOT CHECK FOR ACTIVITY                                         
***                                                                             
*        QOPT1  RECORD TYPE                                                     
*               S=STATIONS                                                      
*               A=STATION ADDRESSES                                             
*               R=REPS                                                          
*               M=MARKETS                                                       
*                                                                               
*        QOPT2  *=ONLY PURGE THOSE RECORDS WITH "*PURGE" IN NAME                
*               P=ONLY PURGE THOSE RECORDS WITH "PURGE" IN NAME                 
*               B=ONLY PURGE THOSE RECORDS WITH "PURGE" OR "*PURGE"             
*               (STWIX FOR STATION RECORDS)                                     
*                                                                               
*        QOPT5  N=DON'T MARK FILE                                               
*               Y=LIVE RUN                                                      
*                                                                               
SP0203   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SP0203                                                         
         SPACE 1                                                                
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 2                                                                
         CLI   QOPT1,C'S'         STATIONS                                      
         BNE   SPREPS                                                           
         L     R6,ADSTAT                                                        
         XC    KEY,KEY                                                          
         MVI   KEY,C'S'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 HIGHSTA                                                          
         B     SPCTL10                                                          
         SPACE 1                                                                
SPCTL00  DC    0H'0'                                                            
         GOTO1 SEQSTA                                                           
         SPACE 1                                                                
SPCTL10  CLC   KEY(2),0(R6)                                                     
         BNE   EXIT                NO MORE STATIONS                             
         CLC   7(2,R6),QAGY                                                     
         BNE   SPCTL00             WRONG AGENCY                                 
         CLI   QOPT2,C'*'                                                       
         BNE   SPCTL10C                                                         
         USING STARECD,R6                                                       
         CLC   STWIX(6),=C'*PURGE'                                              
         BNE   SPCTL00                                                          
         B     SPCTL15                                                          
*                                                                               
SPCTL10C CLI   QOPT2,C'P'                                                       
         BNE   SPCTL10D                                                         
         CLC   STWIX(5),=C'PURGE'                                               
         BNE   SPCTL00                                                          
         B     SPCTL15                                                          
*                                                                               
SPCTL10D CLI   QOPT2,C'B'              EITHER PURGE OR *PURGE                   
         BNE   SPCTL15                 IF NOT *,P,B PURGE ALL STATIONS          
         CLC   STWIX(6),=C'*PURGE'                                              
         BE    SPCTL15                                                          
         CLC   STWIX(5),=C'PURGE'                                               
         BNE   SPCTL00                                                          
         B     SPCTL15                                                          
*                                                                               
SPCTL15  MVI   MODE,PROCSTA                                                     
         GOTO1 GO                                                               
         B     SPCTL00                                                          
         SPACE 1                                                                
SPREPS   DC    0H'0'               REPS                                         
         CLI   QOPT1,C'R'                                                       
         BNE   SPMAR               DONE                                         
         L     R6,ADREP                                                         
         XC    KEY,KEY                                                          
         MVI   KEY,C'R'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 HIGHREP                                                          
         B     SPREPS10                                                         
         SPACE 1                                                                
SPREPS00 DC    0H'0'                                                            
         GOTO1 SEQREP                                                           
         SPACE 1                                                                
SPREPS10 CLC   KEY(2),0(R6)                                                     
         BNE   SPREPSX             NO MORE STATIONS                             
         CLC   5(2,R6),QAGY                                                     
         BNE   SPREPS00            WRONG AGENCY                                 
         CLI   QOPT2,C'*'                                                       
         BNE   SPREP10C                                                         
         USING REPRECD,R6                                                       
         CLC   RNAME(6),=C'*PURGE'                                              
         BNE   SPREPS00                                                         
         B     SPREPS20                                                         
*                                                                               
SPREP10C CLI   QOPT2,C'P'                                                       
         BNE   SPREP10D                                                         
         CLC   RNAME(5),=C'PURGE'                                               
         BNE   SPREPS00                                                         
         B     SPREPS20                                                         
*                                                                               
SPREP10D CLI   QOPT2,C'B'              EITHER PURGE OR *PURGE                   
         BNE   SPREPS20                IF NOT *,P,B PURGE ALL STATIONS          
         CLC   RNAME(6),=C'*PURGE'                                              
         BE    SPREPS20                                                         
         CLC   RNAME(5),=C'PURGE'                                               
         BNE   SPREPS00                                                         
         B     SPREPS20                                                         
*                                                                               
SPREPS20 MVI   MODE,PROCREP                                                     
         GOTO1 GO                                                               
         B     SPREPS00                                                         
*                                                                               
SPREPSX  DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
         SPACE 1                                                                
SPMAR    DC    0H'0'               MARKETS                                      
         CLI   QOPT1,C'M'                                                       
         BNE   SPADDR                                                           
         L     R6,ADMARKET                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,C'M'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 HIGHMKT                                                          
         B     SPMAR10                                                          
         SPACE 1                                                                
SPMAR00  DC    0H'0'                                                            
         GOTO1 SEQMKT                                                           
         SPACE 1                                                                
SPMAR10  CLC   KEY(2),0(R6)                                                     
         BNE   SPMARX              NO MORE STATIONS                             
         CLC   6(2,R6),QAGY                                                     
         BNE   SPMAR00             WRONG AGENCY                                 
         USING MKTRECD,R6                                                       
         CLI   QOPT2,C'*'                                                       
         BNE   SPMAR10C                                                         
         CLC   MKTNAME(6),=C'*PURGE'                                            
         BNE   SPMAR00                                                          
         B     SPMAR20                                                          
*                                                                               
SPMAR10C CLI   QOPT2,C'P'                                                       
         BNE   SPMAR10D                                                         
         CLC   MKTNAME(5),=C'PURGE'                                             
         BNE   SPMAR00                                                          
         B     SPMAR20                                                          
*                                                                               
SPMAR10D CLI   QOPT2,C'B'              EITHER PURGE OR *PURGE                   
         BNE   SPMAR20                IF NOT *,P,B PURGE ALL STATIONS           
         CLC   MKTNAME(6),=C'*PURGE'                                            
         BE    SPMAR20                                                          
         CLC   MKTNAME(5),=C'PURGE'                                             
         BNE   SPMAR00                                                          
         B     SPMAR20                                                          
*                                                                               
SPMAR20  MVI   MODE,PROCMKT                                                     
         GOTO1 GO                                                               
         B     SPMAR00                                                          
*                                                                               
SPMARX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
         SPACE 1                                                                
SPADDR   DC    0H'0'               STATION ADDRESSES                            
         CLI   QOPT1,C'A'                                                       
         BNE   SPERR                                                            
         L     R6,ADSTATAD                                                      
         XC    KEY,KEY                                                          
         MVI   KEY,C'A'                                                         
         MVC   KEY+1(1),QMED                                                    
         GOTO1 HIGHSTAD                                                         
         B     SPADD10                                                          
         SPACE 1                                                                
SPADD00  DC    0H'0'                                                            
         GOTO1 SEQSTAD                                                          
         SPACE 1                                                                
SPADD10  CLC   KEY(2),0(R6)                                                     
         BNE   SPADDX              NO MORE STATION ADDRESSES                    
         CLC   7(2,R6),QAGY                                                     
         BNE   SPADD00             WRONG AGENCY                                 
         USING ADDRECD,R6                                                       
         CLI   QOPT2,C'*'                                                       
         BNE   SPADD10C                                                         
         CLC   ANAME(6),=C'*PURGE'                                              
         BNE   SPADD00                                                          
         B     SPADD20                                                          
*                                                                               
SPADD10C CLI   QOPT2,C'P'                                                       
         BNE   SPADD10D                                                         
         CLC   ANAME(5),=C'PURGE'                                               
         BNE   SPADD00                                                          
         B     SPADD20                                                          
*                                                                               
SPADD10D CLI   QOPT2,C'B'              EITHER PURGE OR *PURGE                   
         BNE   SPADD20                IF NOT *,P,B PURGE ALL STATIONS           
         CLC   ANAME(6),=C'*PURGE'                                              
         BE    SPADD20                                                          
         CLC   ANAME(5),=C'PURGE'                                               
         BNE   SPADD00                                                          
         B     SPADD20                                                          
*                                                                               
SPADD20  MVI   MODE,PROCADD                                                     
         GOTO1 GO                                                               
         B     SPADD00                                                          
*                                                                               
SPADDX   DS    0H                                                               
         B     EXIT                                                             
         DROP  R6                                                               
*                                                                               
SPERR    DC    H'0'                                                             
*                                                                               
PROCSTA  EQU   40                                                               
PROCREP  EQU   50                                                               
PROCMKT  EQU   60                                                               
PROCADD  EQU   70                                                               
*                                                                               
***********************************************OLD CODE                         
*                                                                               
SPCTL20  DS    0H                                                               
         GOTO1 FCNXTCLT            GET NEXT CLIENT                              
         BNE   EXIT                                                             
         XC    KEY,KEY                                                          
         MVC   KEY(3),BAGYMD                                                    
SPCTL40  MVC   KEY+9(7),=7X'FF'    FORCE READING OF NEXT STATION                
         GOTO1 HIGH                                                             
         CLC   KEY(3),KEYSAVE                                                   
         BNE   SPCTL60                                                          
         SPACE 2                                                                
SPCTL50  MVI   MODE,PROCBUY                                                     
         GOTO1 GO                  CHECK IF ANY ACTIVITY FOR THIS               
         B     SPCTL40              STATION                                     
         SPACE 1                                                                
SPCTL60  MVI   KEY,2               CHECK FOR GOALS IN OUR INACTIVE MKT          
         MVC   KEY+1(1),SVAGYMD                                                 
         MVC   KEY+2(2),SVCLT                                                   
         MVI   KEY+4,0                                                          
SPCTL70  DC    0H'0'                                                            
         GOTO1 HIGH                                                             
         CLC   KEY(4),KEYSAVE                                                   
         BNE   SPCTL80                                                          
         MVI   MODE,PROCGOAL       FLAG THIS MARKET ACTIVE                      
         GOTO1 GO                                                               
         MVC   KEY+7(6),=6X'FF'    FORCE THE READING OF THE NEXT MARKET         
         B     SPCTL70                                                          
         SPACE 1                                                                
SPCTL80  MVI   MODE,CLTLAST                                                     
         GOTO1 GO                                                               
         B     SPCTL20                                                          
         SPACE 1                                                                
EXIT     DC    0H'0'                                                            
         XIT1                                                                   
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
*                                                                               
STARECD  DSECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
*                                                                               
REPRECD  DSECT                                                                  
       ++INCLUDE SPGENREP                                                       
         EJECT                                                                  
*                                                                               
MKTRECD  DSECT                                                                  
       ++INCLUDE SPGENMKT                                                       
         EJECT                                                                  
ADDRECD  DSECT                                                                  
       ++INCLUDE SPGENADD                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'019SPREP0203 12/13/00'                                      
         END                                                                    
