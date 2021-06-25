*          DATA SET NEMED03    AT LEVEL 006 AS OF 08/10/00                      
*PHASE T31E03A                                                                  
         TITLE 'T31E03 - EDIT FOR PACKAGE LIST'                                 
T31E03   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**LIED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD            NETWORK SYSTEM DSECT                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*********** INITIALIZE NETBLOCK ******************                              
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         LA    R2,SPLCLIH          CLIENT. REQUIRED.                            
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIN                                
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON   FILL IN PRODUCT NAME                        
         OI    SPLPRONH+6,X'80'    TRANSMIT PRON                                
*                                                                               
         MVI   FTERMFLG,1          OPTIONAL. NO RESPONSE =ALL.                  
         LA    R2,SPLESTH          ESTIMATE. REQUIRED.                          
         NETGO NVESTRNG,DMCB       FILL IN EST NAME,                            
         OI    SPLESTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH          DAYPART                                      
         NETGO NVDPT,SPLDPTN,SPLDPTN                                            
         OI    SPLDPTNH+6,X'80'                                                 
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE. OPTIONAL.                        
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE. OPTIONAL.                          
*                                   CKS THAT END DATE IS NOT B4 STRT            
         LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
*                                                                               
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF3D                                                       
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEMED03   08/10/00'                                      
         END                                                                    
