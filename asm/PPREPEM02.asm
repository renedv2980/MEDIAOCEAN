*          DATA SET PPREPEM02  AT LEVEL 006 AS OF 07/30/03                      
*PHASE PPEM02A                                                                  
         TITLE 'PPEM02 - MOVE FILE TO PRINT QUEUE'                              
PPEM02   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,PPEM02                                                         
         L     RA,0(R1)                                                         
         USING PPWORKD,RA                                                       
         L     RC,PPFILEC                                                       
         LA    R9,1(RC)                                                         
         LA    R9,4095(R9)                                                      
         USING PPFILED,RC,R9                                                    
*                                                                               
         CLI   MODE,RUNFRST                                                     
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
         CLC   P+1(100),SPACES        FIRST HEADLINE OF A8 REPORT?              
         BNE   MAIN06                                                           
         MVI   LINE,99        FORCE NEW PAGE                                    
*                                                                               
MAIN06   GOTO1 REPORT                                                           
         BCT   R0,MAIN04                                                        
         B     MAIN02                                                           
*                                                                               
EODAD    CLOSE EMLFILE                                                          
         MVI   MODE,LBUYREQ     TELLS PPG WE'RE DONE                            
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
         PRINT OFF                                                              
       ++INCLUDE PPNEWFILE                                                      
       ++INCLUDE PPREPWORK                                                      
       ++INCLUDE PPMODEQU                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006PPREPEM02 07/30/03'                                      
         END                                                                    
