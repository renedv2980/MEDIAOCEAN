*          DATA SET SPREPSP03  AT LEVEL 009 AS OF 08/29/00                      
*PHASE SPSP03A                                                                  
         TITLE 'STATION FILE PURGE PROGRAM - SUBCONTROLLER'                     
SPSP03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPSP03                                                         
         SPACE 1                                                                
         L     RA,0(R1)                                                         
         LA    R9,2048(RA)                                                      
         LA    R9,2048(R9)                                                      
         USING SPWORKD,RA,R9                                                    
         SPACE 2                                                                
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
         BNE   SPCTL20             NO MORE STATIONS                             
         CLC   7(2,R6),QAGY                                                     
         BNE   SPCTL00             WRONG AGENCY                                 
         CLI   2(R6),C'0'                                                       
         BNL   SPCTL00             BYPASS CABLE STATIONS                        
         MVI   MODE,STAFRST                                                     
         GOTO1 GO                                                               
         B     SPCTL00                                                          
         SPACE 1                                                                
SPCTL20  DC    0H'0'                                                            
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
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'009SPREPSP03 08/29/00'                                      
         END                                                                    
