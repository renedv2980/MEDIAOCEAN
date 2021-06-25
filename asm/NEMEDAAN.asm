*          DATA SET NEMEDAAN   AT LEVEL 005 AS OF 05/01/02                      
*          DATA SET NEMEDAA    AT LEVEL 044 AS OF 12/12/01                      
*PHASE T31EAAA,+0                                                               
*INCLUDE PRNTBL                                                                 
*INCLUDE PRINT                                                                  
*INCLUDE CLUNPK                                                                 
         TITLE 'T31EAA - FILE FIX MODULE'                                       
         PRINT NOGEN                                                            
************************************************************                    
* NETWORK FILEFIX                                                               
*   THIS PROGRAM UPDATES UNITS WITH STATION TYPE AND POSTING TYPE               
*   FROM THE STATION MASTER RECORD.                                             
*                                                                               
**************************************************************                  
T31EAA   CSECT                                                                  
         NMOD1 0,**UNFX**,RR=R2                                                 
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         L     R7,ANETWS1                                                       
         USING WORKD,R7                                                         
         ST    R2,RELO                                                          
         MVI   UVLPRT,C'N'         FORCE THIS TO NO                             
*                                  ELSE IT DIES WHEN CLOSING APP                
*                                  WHEN RUN OVERNIGHT - SAVE FOR                
*                                  TSO TESTING                                  
                                                                                
         CLI   UVLTYP,C'X'         NEW BILLING REC XTRACT                       
         BNE   *+12                                                             
         BAS   RE,CHKBILLS                                                      
         B     XIT                                                              
         CLI   UVLTYP,C'H'         HISTORY REC MAINTENANCE                      
         BNE   XIT                                                              
         BAS   RE,CHKHIST                                                       
         B     XIT                                                              
*                                                                               
XIT      XIT1                                                                   
                                                                                
                                                                                
                                                                                
*                                                                               
COUNTER  DS    F                                                                
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
*********************************************************                       
* READ THROUGH NEW BILLING RECS AND MATCH WITH UNIT                             
* PRINT OUT UNIT KEY IF NO MATCH                                                
*                                                                               
CHKBILLS NTR1                                                                   
         LA    R2,KEY2                                                          
         USING NUBKEY,R2                                                        
         MVC   0(2,R2),=X'0E06'                                                 
         MVC   NUBKAM,NBACTAM                                                   
         MVC   KEY2SV,KEY2                                                      
         GOTO1 NBDM,DMCB,(0,=CL8'DMRDHI'),=C'XSPDIR',KEY2,KEY2,0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
CK10     CLC   KEY2(3),KEY2SV    FOR AGENCY                                     
         BNE   CKX                                                              
         LA    R3,NUBDA                                                         
         L     R4,NBAIO                                                         
         GOTO1 NBDM,DMCB,=CL8'GETREC',=C'XSPFIL  ',(R3),(R4),DMWORK             
         LA    R4,NUBELDQ(R4)                                                   
         USING NBILD,R4                                                         
         TM    NBILST,NBILUBQ      UNBILLED?                                    
         BO    CK40                                                             
         XC    KEY,KEY             DO WE HAVE 84 KEY MATCH?                     
         MVI   KEY,X'84'                                                        
         MVC   KEY+1(18),KEY2+2    SET UP 84 KEY                                
         MVC   KEYSV,KEY                                                        
         GOTO1 NBDM,DMCB,=C'DMRDHI',=C'UNTDIR',KEY,KEY,0                        
         L     R1,NOSTAT           USE AS COUNTER FOR RECS READ                 
         LA    R1,1(R1)                                                         
         ST    R1,NOSTAT                                                        
         CLC   KEY(20),KEYSV       DID WE FIND IT?                              
         BE    CK40                                                             
         L     R1,NO02             USE AS COUNTER FOR RECS NOT FOUND            
         LA    R1,1(R1)                                                         
         ST    R1,NO02                                                          
***      GOTO1 =V(PRNTBL),DMCB,=C'PRINT',KEYSV,C'DUMP',20,=C'1D'                
         GOTO1 =V(PRNTBL),DMCB,=C'PRINT',NBAIO,C'DUMP',40,=C'1D'                
         GOTO1 =V(CLUNPK),DMCB,KEY+2,P+1                                        
         MVC   P+7(4),KEYSV+4                                                   
         MVC   P+12(6),KEYSV+8                                                  
         EDIT  (B1,KEYSV+16),(4,P+20)                                           
         MVI   P+24,C'-'                                                        
         EDIT  (B1,KEYSV+17),(3,P+25)                                           
         GOTO1 DATCON,DMCB,(2,KEYSV+14),(5,P+30)                                
         GOTO1 SPOOL,DMCB,(R8)                                                  
         LA    R5,P+7                                                           
CK37     EDIT  (B4,NBILGRS),(10,0(R5)),2                                        
         GOTO1 =V(PRNTBL),DMCB,=C'PRINT',NBILPRD,C'DUMP',1,=C'1D'               
         LA    R5,12(R5)                                                        
         ZIC   R1,1(R4)                                                         
         LTR   R1,R1                                                            
         BZ    CK39                                                             
         AR    R4,R1                                                            
         CLI   0(R4),X'10'                                                      
         BE    CK37                                                             
CK39     GOTO1 SPOOL,DMCB,(R8)                                                  
*                                                                               
CK40     GOTO1 NBDM,DMCB,(0,=CL8'DMRSEQ'),=C'XSPDIR  ',KEY2,KEY2,0              
         B     CK10                                                             
                                                                                
CKX      MVC   P+1(20),=C'TOTAL BILL RECS READ'                                 
         EDIT  (B4,NOSTAT),(8,P+22)                                             
         MVC   P2+1(22),=C'TOTAL RECS NOT MATCHED'                              
         EDIT  (B4,NO02),(8,P2+22)                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
         EJECT                                                                  
*********************************************************                       
* READ THROUGH HISTORY RECORDS AND MATCH WITH UNIT                              
* PRINT OUT UNIT KEY IF NO MATCH                                                
*                                                                               
CHKHIST  NTR1                                                                   
         LA    R2,KEY2                                                          
         XC    KEY2,KEY2                                                        
         MVI   KEY2,X'40'                                                       
         MVC   KEY2SV,KEY2                                                      
         GOTO1 NBDM,DMCB,(0,=CL8'DMRDHI'),=C'UNTDIR',KEY2,KEY2,0                
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
CHKH10   CLI   KEY2,X'40'          ..IF IT'S A HISTORY RECORD                   
         BNE   CHKHX                                                            
         MVC   KEY(20),KEY2        ..READ CORRESPONDING UNIT                    
         MVI   KEY,X'84'                                                        
         GOTO1 NBDM,DMCB,(0,=CL8'DMRDHI'),=C'UNTDIR',KEY,KEY,0                  
         CLC   KEY(20),KEYSAVE      FOUND MATCHING UNIT?                        
         BE    CHKH50              GET NEXT HISTORY RECORD                      
*                                                                               
         LA    R3,KEY2+21          NO MATCH - READ HISTORY REC                  
         L     R4,NBAIO                                                         
         GOTO1 NBDM,DMCB,=CL8'GETREC',=C'UNTFIL  ',(R3),(R4),DMWORK             
         CLI   8(R1),0                                                          
         BE    *+6                                                              
         DC    H'0'                                                             
         CLI   UVLTST,C'Y'         TEST RUN?                                    
         BE    CHKH20                                                           
         OI    KEY2+20,X'C0'        CLOSE OUT                                   
         GOTO1 NBDM,DMCB,=C'DMWRT',=C'UNTDIR  ',KEY2,KEY2,0                     
         L     R2,NBAIO                                                         
         OI    22(R2),X'C0'        CLOSE OUT                                    
         GOTO1 NBDM,DMCB,=C'PUTREC',=C'UNTFIL  ',(R3),(R4),DMWORK               
CHKH20   L     R1,NO02             USE AS COUNTER FOR RECS DELETED              
         LA    R1,1(R1)                                                         
         ST    R1,NO02                                                          
         LA    R2,KEY2                                                          
         USING NHRECD,R2                                                        
         GOTO1 =V(CLUNPK),DMCB,NHKPCLT,P+1                                      
         GOTO1 DATCON,DMCB,NHKDATE,(5,P+5)                                      
         GOTO1 SPOOL,DMCB,(R8)                                                  
         GOTO1 =V(PRNTBL),DMCB,=C'PRINT',NBAIO,C'DUMP',40,=C'1D'                
CHKH50   GOTO1 NBDM,DMCB,(0,=CL8'DMRHI'),=C'UNTDIR  ',KEY2,KEY2,0               
         GOTO1 NBDM,DMCB,(0,=CL8'DMRSEQ'),=C'UNTDIR  ',KEY2,KEY2,0              
         L     R1,NOSTAT           USE AS COUNTER FOR RECS READ                 
         LA    R1,1(R1)                                                         
         ST    R1,NOSTAT                                                        
         B     CHKH10                                                           
                                                                                
CHKHX    MVC   P+1(20),=C'TOTAL HIST RECS READ'                                 
         EDIT  (B4,NOSTAT),(8,P+22)                                             
         MVC   P2+1(22),=C'TOTAL RECS NOT MATCHED'                              
         EDIT  (B4,NO02),(8,P2+22)                                              
         GOTO1 SPOOL,DMCB,(R8)                                                  
         XIT1                                                                   
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
*                                                                               
*** PRINT LINE DESECT ***                                                       
         SPACE                                                                  
PLINED   DSECT                                                                  
PLNET    DS    CL4                 NETWORK                                      
         DS    CL2                                                              
PLPKCDE  DS    CL3                 PACKAGE CODE                                 
         DS    CL2                                                              
PLPKNM   DS    CL16                PACKAGE NAME                                 
         DS    CL2                                                              
PLPRGCDE DS    CL6                 PROGRAM CODE                                 
         DS    CL2                                                              
PLPRGNM  DS    CL16                PROGRAM NAME                                 
         DS    CL2                                                              
PLUNTOT  DS    CL5                 TOTAL UNITS                                  
         DS    CL2                                                              
ENDP     EQU   *-PLNET                                                          
*                                                                               
**** PASDATA STORAGE (IN W/S AREA1 FROM EDIT OVERLAY) ***                       
WORKD    DSECT                                                                  
         DS    0F                                                               
RELO     DS    F                                                                
NOSTAT   DS    F                                                                
NO02     DS    F                                                                
MYELEM   DS    CL20                                                             
*                                                                               
RSTAT    DS    CL1                 FLAG IF NURSTAT UPDATED                      
BINSTR   DS    CL2                                                              
BINEND   DS    CL2                                                              
KEYSV    DS    CL30                                                             
KEY2     DS    CL40                                                             
KEY2SV   DS    CL40                                                             
*                                                                               
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         EJECT                                                                  
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDDCD                                                       
*                                                                               
         PRINT ON                                                               
         EJECT                                                                  
       ++INCLUDE SPGENSTA                                                       
         EJECT                                                                  
       ++INCLUDE NEGENUNIT                                                      
       ++INCLUDE NEGENUBILL                                                     
       ++INCLUDE NEGENHIST                                                      
         EJECT                                                                  
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'005NEMEDAAN  05/01/02'                                      
         END                                                                    
