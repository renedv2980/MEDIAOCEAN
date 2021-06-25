*          DATA SET APGHIVBSD  AT LEVEL 021 AS OF 07/01/94                      
*PHASE ACHIVBSD,+0                                                              
         TITLE 'APG HOOK TEMPORYARY FIX FOR BLANK MOA'                          
ACHIVBSD CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**ACHK**,R9,R8,RR=R5                                           
         L     RA,0(R1)                                                         
         USING MAND,RA                                                          
         L     RC,HOOKAWRK                                                      
         USING ACWORKD,RC                                                       
         ST    R5,HKRELO                                                        
         USING SRECD,R5                                                         
         L     R5,HOOKAREC                                                      
         L     R6,VBIGPRNT                                                      
         USING BIGPRNTD,R6                                                      
         LA    R3,SRROW7+2                                                      
         CLI   HOOKNUM,1           REPORT 1                                     
         BE    FIXDATA                                                          
         LA    R3,SRROW1+2                                                      
         CLI   HOOKNUM,2           REPORT 2                                     
         BE    FIXDATA                                                          
         DC    H'00'                                                            
         EJECT                                                                  
FIXDATA  CLC   0(2,R3),SPACES                                                   
         BNE   XIT                                                              
         MVC   0(2,R3),=X'4141'                                                 
XIT      SR    RE,RE                                                            
XITNO    LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
HKRELO   DS    A                                                                
         EJECT                                                                  
*--------------------------*                                                    
*        SORT RECORD       *                                                    
*--------------------------*                                                    
SRECD    DSECT                                                                  
SRREC    DS    0C                                                               
SRROW1   DS    CL16                ROW 1 OFFICE CODE                            
SRROW2   DS    CL16                ACCOUNT LEVEL 1 (SUPERLEDGER)                
SRROW3   DS    CL16                ACCOUNT LEVEL 2 (SUPERLEDGER)                
SRROW4   DS    CL16                ACCOUNT LEVEL 3 (SUPERLEDGER)                
SRROW5   DS    CL16                ACCOUNT LEVEL 3 (SUPERLEDGER)                
SRROW6   DS    CL16                ACCOUNT LEVEL 3 (SUPERLEDGER)                
SRROW7   DS    CL16                ACCOUNT LEVEL 3 (SUPERLEDGER)                
SRREP#   DS    CL1                 REPORT NUMBER/COPY                           
         DS    CL1                 REPORT NUMBER/COPY                           
SRBINZ   DS    XL2                 BINARY ZERO                                  
SRNAM1   DS    CL36                OFFICE NAME                                  
SRNAM2   DS    CL36                ROW 2 NAME                                   
SRNAM3   DS    CL36                ROW 3 NAME                                   
SRNAM4   DS    CL36                ROW 4 NAME                                   
SRNAM5   DS    CL36                ROW 4 NAME                                   
SRNAM6   DS    CL36                ROW 4 NAME                                   
SRNAM7   DS    CL36                ROW 4 NAME                                   
*                                                                               
SRAMT1   DS    PL8                 BUCKETS                                      
SRAMT2   DS    PL8                 BUCKETS                                      
SRBUDM   DS    PL8                 MONTH BUDGET                                 
SRAMT4   DS    PL8                 BUCKETS                                      
SRAMT5   DS    PL8                 BUCKETS                                      
SRAMT6   DS    0PL8                                                             
SRBUDM1  DS    PL8                 BUCKETS                                      
SRBUDM2  DS    PL8                 BUCKETS                                      
SRAMT8   DS    PL8                 BUCKETS                                      
SRAMT9   DS    PL8                 BUCKETS                                      
SRAMT10  DS    PL8                 BUCKETS                                      
SRAMT11  DS    PL8                 BUCKETS                                      
SRBUDY   DS    PL8                 YTD BUDGET                                   
SRAMT12  DS    PL8                 BUCKETS                                      
SRAMT13  DS    PL8                 BUCKETS                                      
SRBUDY1  DS    PL8                 BUCKETS                                      
SRBUDY2  DS    PL8                 BUCKETS                                      
SRAMT16  DS    PL8                 BUCKETS                                      
SRAMT17  DS    PL8                 BUCKETS                                      
SRLNQ    EQU   *-SRREC                                                          
         EJECT                                                                  
*        ACAPGWORKD                                                             
*        ACREPWORKD                                                             
*        ACGENBOTH                                                              
*        ACGENMODES                                                             
*        ACBIGPRNTD                                                             
*        DDBIGBOX                                                               
         PRINT OFF                                                              
       ++INCLUDE ACAPGWORKD                                                     
       ++INCLUDE ACREPWORKD                                                     
       ++INCLUDE ACGENBOTH                                                      
       ++INCLUDE ACGENMODES                                                     
       ++INCLUDE ACBIGPRNTD                                                     
       ++INCLUDE DDBIGBOX                                                       
         PRINT   ON                                                             
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'021APGHIVBSD 07/01/94'                                      
         END                                                                    
