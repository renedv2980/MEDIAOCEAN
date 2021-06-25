*          DATA SET NEMED4B    AT LEVEL 022 AS OF 12/12/11                      
*PHASE T31E4BA,+0                                                               
         TITLE 'T31E4B - EDIT FOR THREE IN ONE'                                 
T31E4B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**SCJE**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*   INITIALIZE DBLOCK                                                           
                                                                                
                                                                                
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         MVI   FTERMFLG,0          FIELDS ARE REQUIRED                          
         MVI   ERROR,INVALID                                                    
*                                                                               
         LA    R2,SPLCLIH                                                       
         NETGO NVCLIALL,DMCB,SPLCLIN  CLIENT. REQUIRED.                         
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         LA    R2,SPLPROH                                                       
         NETGO NVPRDALL,DMCB,SPLPRON  PRODUCT. REQUIRED.                        
*                                                                               
         LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN,0         ALLOW RANGE OF ESTS              
         OI    SPLESTNH+6,X'80'      TRANSMIT ESTIMATE NAME                     
*                                                                               
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE OPTIONAL.                         
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE. OPTIONAL. CKS END>=START           
*                                                                               
         B     XMOD                                                             
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEBD                                                       
*                                                                               
*                                                                               
*                                                                               
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'022NEMED4B   12/12/11'                                      
         END                                                                    
