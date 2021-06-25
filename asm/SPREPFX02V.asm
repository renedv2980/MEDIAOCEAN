*          DATA SET SPREPFX02V AT LEVEL 064 AS OF 07/02/97                      
*PHASE SPFX02V                                                                  
*INCLUDE SORTER                                                                 
         TITLE 'SPFX02V- SORT RECORDS FORM VARIOUS SP FILES + PRINT'            
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
         CLI   MODE,REQFRST                                                     
         BE    FX10                                                             
*                                                                               
EXIT     XIT1                                                                   
*                                                                               
RELO     DC    A(0)                                                             
         EJECT                                                                  
* REQFRST                                                                       
*                                                                               
FX10     DS    0H                                                               
         USING DCLIENT,R3                                                       
*                                                                               
         GOTO1 =V(SORTER),DMCB,SORTCARD,RECCARD,0                               
*                                                                               
         OPEN  (FILEIN,INPUT)     OPEN DATASET                                  
         LTR   RF,RF                                                            
         BZ    *+6                                                              
         DC    H'0'                                                             
*                                                                               
FX14     DS    0H                                                               
         GET   FILEIN,CLTREC                                                    
         GOTO1 =V(SORTER),DMCB,=C'PUT',CLTREC                                   
         B     FX14                                                             
*                                                                               
FX18     DS    0H                                                               
         CLOSE FILEIN                                                           
         USING DPRCLT,R6                                                        
         LA    R6,P                                                             
*                                                                               
         GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SR    R3,R3                                                            
         ICM   R3,15,4(R1)                                                      
         BZ    FXX                                                              
*                                                                               
         MVC   LASTREC,0(R3)                                                    
*                                                                               
FXSRT    GOTO1 =V(SORTER),DMCB,=C'GET'                                          
         SR    R3,R3                                                            
         ICM   R3,15,4(R1)                                                      
         BZ    FXX                                                              
*                                                                               
         MVC   CLTREC,0(R3)                                                     
         LA    R3,CLTREC                                                        
*                                                                               
         CLC   DCLTC,LASTREC                                                    
         BNE   NEXT                                                             
         CLC   DCLTNM,LASTREC+DCLNMDSP                                          
         BE    NEXT                                                             
*                                                                               
         MVC   DPCLTC,DCLTC                                                     
         MVC   DPMEDC,DMEDC                                                     
         MVC   DPAGYC,DAGYC                                                     
         MVC   DPSYS,DSYS                                                       
         MVC   DPCLTNM,DCLTNM                                                   
         GOTO1 REPORT                                                           
*                                                                               
         LA    R3,LASTREC                                                       
         MVC   DPCLTC,DCLTC                                                     
         MVC   DPMEDC,DMEDC                                                     
         MVC   DPAGYC,DAGYC                                                     
         MVC   DPSYS,DSYS                                                       
         MVC   DPCLTNM,DCLTNM                                                   
         GOTO1 REPORT                                                           
*                                                                               
NEXT     MVC   LASTREC,CLTREC                                                   
         B     FXSRT                                                            
*                                                                               
FXX      DS    0H                                                               
         GOTO1 AENDREQ                                                          
         EJECT                                                                  
*                                                                               
         EJECT                                                                  
* LITERAL POOL                                                                  
         LTORG                                                                  
         EJECT                                                                  
* STORAGE                                                                       
*                                                                               
         DS    0D                                                               
RECTOT   DS    PL8                                                              
CLTREC   DS    CL27           CLIENT RECORD TO WRT TO DATASET                   
LASTREC  DS    CL27           PREVIOUS CLIENT RECORD                            
*                                                                               
FILEIN   DCB   DDNAME=FILEIN,DSORG=PS,RECFM=FB,MACRF=GM,EODAD=FX18              
*                                                                               
SORTCARD DC    CL80'SORT FIELDS=(1,3,A),FORMAT=BI,WORK=1'                       
RECCARD  DC    CL80'RECORD TYPE=F,LENGTH=27'                                    
*                                                                               
DCLIENT  DSECT                DSECT FOR RECORD TO READ FROM DATASET             
DCLTC    DS    CL3                                                              
DCLNMDSP EQU   *-DCLTC                                                          
DCLTNM   DS    CL20                                                             
DAGYC    DS    CL2                                                              
DSYS     DS    C                                                                
DMEDC    DS    C                                                                
*                                                                               
DPRCLT   DSECT                DSECT FOR PRINTING RECORDS                        
DPCLTC   DS    CL3                                                              
         DS    CL2                                                              
DPCLTNM  DS    CL20                                                             
         DS    CL2                                                              
DPAGYC   DS    CL2                                                              
         DS    CL2                                                              
DPSYS    DS    C                                                                
         DS    CL2                                                              
DPMEDC   DS    C                                                                
*                                                                               
         EJECT                                                                  
       ++INCLUDE SPNWSDTL                                                       
         EJECT                                                                  
       ++INCLUDE SPGENCLT                                                       
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
       ++INCLUDE SPREPWORKD                                                     
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'064SPREPFX02V07/02/97'                                      
         END                                                                    
