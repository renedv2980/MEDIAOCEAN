*          DATA SET NEMED4AN   AT LEVEL 007 AS OF 06/28/06                      
*PHASE T31E4AA,+0                                                               
         TITLE 'T31E4A - EDIT FOR BRAND ALLOCATION'                             
T31E4A   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**BRED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*    INITIALIZE NETBLOCK                                                        
*                                                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
         L     R7,ANETWS2                                                       
         ST    R7,NBADEM           USE W/S AREA 2 FOR NET DEMO BLOCK            
         USING NDDEMBLK,R7                                                      
         L     R2,ANETWS1          PASS CLI REC TO PRINT IN W/S AREA 1          
         ST    R2,NBACLI                                                        
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         MVI   FTERMFLG,0          THESE FIELDS REQUIRED                        
*                                                                               
         LA    R2,SPLCLIH          CLIENT. REQUIRED.                            
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         LA    R2,SPLPROH          PRODUCT. REQUIRED.                           
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'    TRANSMIT PRODUCT NAME                        
*                                                                               
         LA    R2,SPLESTH          ESTIMATE. REQUIRED.                          
         NETGO NVEST,DMCB,SPLESTN,NDDEMBLK   FILL IN EST NAME,                  
         OI    SPLESTNH+6,X'80'                     DEMOS                       
*                                                                               
         LA    R2,SPLNETH          NETWORK                                      
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLPAKH          PACKAGE. REQUIRED.                           
         NETGO NVPAK,DMCB,SPLPAKN                                               
         OI    SPLPAKNH+6,X'80'    TRANSMIT PACKAGE NAME                        
*                                                                               
         MVI   FTERMFLG,1          FOLLOWING FIELDS OPTIONAL                    
*                                                                               
         LA    R2,SPLSEQH          PROGRAM SEQUENCE OPTION                      
         NETGO NVPSEQ,DMCB                                                      
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE. OPTIONAL.                        
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE. OPTIONAL.                          
*                                   CKS THAT END DATE IS NOT B4 STRT            
*                                                                               
*                                                                               
         LA    R2,SPLCLIH                                                       
         B     XMOD                                                             
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
XIT      XIT1                                                                   
*                                                                               
         EJECT                                                                  
***   COMMON WITH PRINT *****                                                   
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLN                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEAD                                                       
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'007NEMED4AN  06/28/06'                                      
         END                                                                    
