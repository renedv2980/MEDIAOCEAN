*          DATA SET APGHFIRU1  AT LEVEL 021 AS OF 11/18/94                      
*PHASE ACHFIRU1,+0                                                              
         TITLE 'APG HOOK RUMN BUDGET OVERHEAD DATA'                             
ACHFIRU1 CSECT                                                                  
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
         CLI   HOOKNUM,1           HOOKS AFTER SORT                             
         BE    GETDATA                                                          
         CLI   HOOKNUM,2           GET TOTAL DIRECT LABOR OR PROCESS OH         
         BE    PROCESS                                                          
         DC    H'00'                                                            
         EJECT                                                                  
GETDATA  LA    R0,2                                                             
         LA    RE,SRAMT1                                                        
         LA    RF,SVPCT                                                         
GETDT10  ZAP   PACKAMT,0(8,RE)                                                  
         SRP   PACKAMT,4,0                                                      
         CP    SRAMT5,=P'0'                                                     
         BNE   GETDT11                                                          
         ZAP   0(8,RF),=P'0'                                                    
         B     GETDT12                                                          
*                                                                               
GETDT11  DP    PACKAMT,SRAMT5      DIRECT LABOR FOR 1C MONTHLY                  
         ZAP   0(8,RF),ANSWER                                                   
GETDT12  LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
*                                                                               
         ZAP   PACKAMT,0(8,RE)                                                  
         SRP   PACKAMT,4,0                                                      
         CP    SRAMT6,=P'0'                                                     
         BNE   GETDT13                                                          
         ZAP   0(8,RF),=P'0'                                                    
         B     GETDT14                                                          
GETDT13  DP    PACKAMT,SRAMT6      DIRECT LABOR FOR 1C YTD                      
         ZAP   0(8,RF),ANSWER                                                   
GETDT14  LA    RE,8(RE)                                                         
         LA    RF,8(RF)                                                         
         BCT   R0,GETDT10                                                       
*                                                                               
         ZAP   PACKAMT,=P'8333333'       TEST FOR NOW                           
         SRP   PACKAMT,4,0                                                      
         ZAP   OTHERMON,=P'0'                                                   
         CP    SRAMT5,=P'0'                                                     
         BE    GETDT20                                                          
         DP    PACKAMT,SRAMT5      DIRECT LABOR FOR 1C MONTHLY                  
         ZAP   OTHERMON,ANSWER                                                  
*                                                                               
GETDT20  ZAP   PACKAMT,=P'100000000'      TEST FOR NOW                          
         SRP   PACKAMT,4,0                                                      
         ZAP   OTHERPER,=P'0'                                                   
         CP    SRAMT6,=P'0'                                                     
         BE    GETDT25                                                          
         DP    PACKAMT,SRAMT6     DIRECT LABOR FOR 1C MONTHLY                   
         ZAP   OTHERPER,ANSWER                                                  
GETDT25  CLI   QOPT5,C'S'                                                       
         BE    XIT                 SHOW FIRST REPORT WITH O/H DEBUG             
         B     XITNO               ELIMINATE RECORDS                            
         EJECT                                                                  
PROCESS  EQU   *                                                                
         LA    R3,SRROW3                                                        
         CLI   SRREP#,2            REPORTS 2                                    
         BE    GETDIR3                                                          
         CLI   SRREP#,10           REPORTS 10                                   
         BE    GETDIR3                                                          
         LA    R3,SRROW4           REPORTS 4                                    
         CLI   SRREP#,4                                                         
         BE    GETDIR3                                                          
         LA    R3,SRROW5           REPORTS 6                                    
         CLI   SRREP#,6                                                         
         BE    GETDIR3                                                          
         LA    R3,SRROW6           REPORTS 8                                    
         CLI   SRREP#,8                                                         
         BE    GETDIR3                                                          
         B     XIT                                                              
*                                                                               
GETDIR3  EQU   *                                                                
         CLI   2(R3),C'2'          IS IT GROUP 2?                               
         BNE   XIT                                                              
         LA    R3,L'SRROW1(R3)                                                  
         CLI   2(R3),C'F'          IS IT DIRECT LABOR                           
         BNE   PROC10                                                           
         ZAP   DIRLBMON,SRBUDM                                                  
         ZAP   DIRLBPER,SRBUDY                                                  
         B     XIT                                                              
         EJECT                                                                  
PROC10   CLI   2(R3),C'L'          OVERHEAD?                                    
         BNE   XIT                                                              
*        CLI   QOPT5,C'D'                                                       
*        BNE   *+6                                                              
*        DC    H'00'                                                            
         LA    R3,L'SRROW1(R3)                                                  
         LA    RE,NEWBSMON                                                      
         CLI   2(R3),C'N'          NEW BUSINESS?                                
         BE    PROC20                                                           
         LA    RE,INTPJMON                                                      
         CLI   2(R3),C'P'          INTERNAL PROJECTS?                           
         BE    PROC20                                                           
         LA    RE,OTHERMON                                                      
         CLI   2(R3),C'Z'          OTHER?                                       
         BE    PROC20                                                           
         B     XIT                                                              
PROC20   EQU   *                                                                
         ZAP   PACKAMT,0(8,RE)     MONTHLY AMOUNT                               
         MP    PACKAMT,DIRLBMON                                                 
         SRP   PACKAMT,60,5                                                     
         ZAP   SRBUDM,PACKAMT                                                   
         ZAP   PACKAMT,8(8,RE)     PERIOD AMOUNT                                
         MP    PACKAMT,DIRLBPER                                                 
         SRP   PACKAMT,60,5                                                     
         ZAP   SRBUDY,PACKAMT                                                   
         B     XIT                                                              
XIT      SR    RE,RE                                                            
XITNO    LTR   RE,RE                                                            
         XIT1                                                                   
         EJECT                                                                  
HKRELO   DS    A                                                                
PACKAMT  DS    0PL16                                                            
ANSWER   DS    PL8                                                              
REMAIN   DS    PL8                                                              
*                                                                               
SVPCT    DS    0PL8                                                             
NEWBSMON DS    PL8                 NB OH / NON-NB DIRECT LABOR                  
NEWBSPER DS    PL8                                                              
INTPJMON DS    PL8                 INTERAL PROJECT OH /NON-NB DIR LABOR         
INTPJPER DS    PL8                                                              
OTHERMON DS    PL8                 TOTAL OH / NON-NB DIRECT LABOR               
OTHERPER DS    PL8                 TOTAL OH / NON-NB DIRECT LABOR               
DIRLBMON DS    PL8                 DIRECT LABOR                                 
DIRLBPER DS    PL8                 DIRECT LABOR                                 
         EJECT                                                                  
*--------------------------*                                                    
*        SORT RECORD       *                                                    
*--------------------------*                                                    
SRECD    DSECT                                                                  
SRREC    DS    0C                                                               
SRROW1   DS    CL16                ROW F2 (FILTER)                              
SRROW2   DS    CL16                ROW 2 OFFICE CODE                            
SRROW3   DS    CL16                ACCOUNT LEVEL 1 (SUPERLEDGER)                
SRROW4   DS    CL16                ACCOUNT LEVEL 2 (SUPERLEDGER)                
SRROW5   DS    CL16                ACCOUNT LEVEL 3 (SUPERLEDGER)                
SRROW6   DS    CL16                ACCOUNT LEVEL 4 (SUPERLEDGER)                
SRROW7   DS    CL16                ACCOUNT LEVEL 5 (SUPERLEDGER)                
SRROW8   DS    CL16                ACCOUNT LEVEL 6 (SUPERLEDGER)                
SRREP#   DS    CL1                 REPORT NUMBER/COPY                           
         DS    CL1                 REPORT NUMBER/COPY                           
SRBINZ   DS    XL2                 BINARY ZERO                                  
SRNAM1   DS    CL36                OFFICE NAME                                  
SRNAM2   DS    CL36                ROW 2 NAME                                   
SRNAM3   DS    CL36                ROW 3 NAME                                   
SRNAM4   DS    CL36                ROW 4 NAME                                   
SRNAM5   DS    CL36                ROW 5 NAME                                   
SRNAM6   DS    CL36                ROW 6 NAME                                   
SRNAM7   DS    CL36                ROW 7 NAME                                   
SRNAM8   DS    CL36                ROW 8 NAME                                   
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
**PAN#1  DC    CL21'021APGHFIRU1 11/18/94'                                      
         END                                                                    
