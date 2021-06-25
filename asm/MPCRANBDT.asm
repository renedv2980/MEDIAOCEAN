*          DATA SET MPCRANBDT  AT LEVEL 012 AS OF 05/01/02                      
*PHASE MPCTEST                                                                  
*INCLUDE CARDS                                                                  
*INCLUDE PRINT                                                                  
*INCLUDE PRINTER                                                                
         TITLE 'TEST NBD MODULE'                                                
         PRINT  GEN                                                             
TEST     CSECT                                                                  
         NBASE 0,**TEST**,WORK                                                  
         L     RA,=V(CPRINT)                                                    
         USING DPRINT,RA                                                        
         MVC   TITLE(30),=C'NEGATIVE BINOMIAL DISTRIBUTION'                     
         LOAD  EPLOC=NBDMOD,ERRET=INIERR1                                       
         ST    R0,VNBDMOD                                                       
         LH    R1,=Y(WKTAB-TEST)                                                
         AR    R1,RB                                                            
         ST    R1,AWKTAB                                                        
         LH    R1,=Y(FREQ-TEST)                                                 
         AR    R1,RB                                                            
         ST    R1,AFREQ                                                         
         LH    R1,=Y(INPFREQ-TEST)                                              
         AR    R1,RB                                                            
         ST    R1,AINPFREQ                                                      
*                                                                               
         SPACE 1                                                                
GO       L     R0,AFREQ                                                         
         L     RE,AINPFREQ                                                      
         LH    R1,=Y(INPFREQ-FREQ)                                              
         LR    RF,R1                                                            
         MVCL  R0,RE                                                            
         BAS   RE,CALLNBD                                                       
         MVC   P+2(6),=C'ERRFLG'                                                
         EDIT  (B4,ERRFLG),(2,P+10),ZERO=NOBLANK                                
         GOTO1 =V(PRINTER)                                                      
         ICM   RF,15,ADJTVR                                                     
         EDIT  (RF),(10,P+2),2,ZERO=NOBLANK,MINUS=YES                           
         SH    RF,=Y(10)                                                        
         BNP   DONE                                                             
         STCM  RF,15,ADJTVR                                                     
         GOTO1 =V(PRINTER)                                                      
         MVC   P+2(12),=C'RETURN ARRAY'                                         
         GOTO1 =V(PRINTER)                                                      
         L     R2,AFREQ                                                         
         ICM   R3,15,SPOTS                                                      
         LA    R3,1(R3)                                                         
GO1      ICM   RF,15,0(R2)                                                      
         EDIT  (RF),(10,P+2),1,ZERO=NOBLANK,MINUS=YES                           
*                                                                               
         GOTO1 =V(PRINTER)         CALL PRINT ROUTINE                           
         LA    R2,4(R2)                                                         
         BCT   R3,GO1                                                           
         B     GO                                                               
         SPACE 2                                                                
DONE     MVC   P+1(4),=C'DONE'                                                  
         GOTO1 =V(PRINTER)                                                      
         DELETE EPLOC=NBDMOD                                                    
         B     EOJ                                                              
         EJECT                                                                  
***********************************************************************         
* CALL NBD MODULE                                                     *         
***********************************************************************         
         SPACE 1                                                                
CALLNBD  NTR1  ,                                                                
         ST    RD,SAVERD                                                        
         L     RD,AWKTAB           USE WORK TABLE FOR REGSAVE AREA              
         GOTO1 VNBDMOD,DMCB,SPOTS,UNIV,RAWTVR,ADJTVR,AFREQ,ERRFLG               
         L     RD,SAVERD                                                        
CALLNBDX B     EXIT                                                             
         SPACE 2                                                                
INIERR1  DC    H'0'                                                             
EXIT     XMOD1 ,                                                                
*                                                                               
EOJ      XBASE                                                                  
         EJECT                                                                  
         LTORG                                                                  
*                                                                               
NBDMOD   DC    CL10'T512NBD'                                                    
*                                                                               
DUB      DS    D                                                                
VNBDMOD  DS    V                                                                
AWKTAB   DS    A                                                                
AFREQ    DS    A                                                                
AINPFREQ DS    A                                                                
SAVERD   DS    A                                                                
DMCB     DS    6F                                                               
ERRFLG   DC    XL4'00000000'                                                    
*                                                                               
UNIV     DC    F'26595'            1DP (INDIVIDUALS)                            
RAWTVR   DC    F'26667'            TVR 2DP (%)                                  
ADJTVR   DC    F'26667'            TVR 2DP (%)                                  
*DJTVR   DC    F'13130'            TVR 2DP (%)                                  
SPOTS    DC    F'53'               INTEGER                                      
WORK     DS    1000D                                                            
FREQ     DC    1000F'00'                                                        
INPFREQ  DC    1000F'00'                                                        
         ORG   INPFREQ                                                          
         DC    F'00000'            1DP (INDIVIDUALS)                            
         DC    F'00000'                                                         
         DC    F'17730'                                                         
         DC    F'0000'                                                          
         DC    F'08865'                                                         
         ORG                                                                    
*                                                                               
WKTAB    DS    1000D                                                            
         EJECT                                                                  
       ++INCLUDE DDDPRINT                                                       
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'012MPCRANBDT 05/01/02'                                      
         END                                                                    
