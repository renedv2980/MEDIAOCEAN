*          DATA SET SPREPEM02  AT LEVEL 011 AS OF 07/30/03                      
*PHASE SPEM02A                                                                  
         TITLE 'SPEM02 - MOVE FILE TO PRINT QUEUE'                              
SPEM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,SPEM02,R8,RC                                                   
         LHI   RC,8192                                                          
         AR    RC,RB                                                            
         L     RA,0(R1)                                                         
         USING SPWORKD,RA,R9                                                    
         LHI   R9,4096                                                          
         AR    R9,RA                                                            
*                                                                               
         CLI   MODE,REQFRST                                                     
         BNE   EXIT                                                             
         OPEN  (EMLFILE,INPUT)                                                  
*                                                                               
MAIN02   GET   EMLFILE,EMLLINE                                                  
         MVC   P,EMLLINE+4                                                      
*                                                                               
         PACK  DUB,EMLLINE+2(2)                                                 
         CVB   R0,DUB                                                           
*                                                                               
MAIN04   MVI   LINE,0                                                           
         CLI   EMLLINE+1,C'C'      PAGE THROW?                                  
         BNE   MAIN06              NO                                           
         CLC   P+1(100),SPACES     MUST BE FIRST HEADLINE ON A8 REPORT          
         BNE   MAIN06                                                           
         MVI   LINE,99        FOR NEW PAGE                                      
*                                                                               
MAIN06   GOTO1 REPORT                                                           
         BCT   R0,MAIN04                                                        
         B     MAIN02                                                           
*                                                                               
EODAD    CLOSE EMLFILE                                                          
         GOTO1 AENDREQ                                                          
*                                                                               
EXIT     XIT1  ,                                                                
*                                                                               
         LTORG                                                                  
FSTLINE  DC    C'N'                                                             
EMLLINE  DC    136C' '                                                          
*                                                                               
EMLFILE  DCB   DSORG=PS,MACRF=GM,DDNAME=EMLFILE,RECFM=FB,LRECL=(136),  +        
               EODAD=EODAD                                                      
         EJECT                                                                  
***********************************************************************         
* OTHER INCLUDED DSECTS                                               *         
***********************************************************************         
         SPACE 1                                                                
* SPREPWORKD                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPWORKD                                                     
         PRINT ON                                                               
* SPREPMODES                                                                    
         PRINT OFF                                                              
       ++INCLUDE SPREPMODES                                                     
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'011SPREPEM02 07/30/03'                                      
         END                                                                    
