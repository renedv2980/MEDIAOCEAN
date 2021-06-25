*          DATA SET NEMED01    AT LEVEL 006 AS OF 05/02/02                      
*PHASE T31E01A                                                                  
         TITLE 'T31E01 - EDIT FOR WEEKLY STEWARDSHIP'                           
T31E01   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**STED**                                                       
         L     RC,0(R1)                                                         
         USING GEND,RC                                                          
         L     RA,ATWA                                                          
         USING T31EFFD,RA                                                       
         L     R8,ASPOOLD                                                       
         USING SPOOLD,R8                                                        
         L     R9,ASYSD                                                         
         USING NETSYSD,R9                                                       
         EJECT                                                                  
*      ** INITIALIZE NETBLOCK ***                                               
         MVI   NBQINIT,0           DO ALL VALIDATIONS EACH TIME                 
         L     R7,ANETWS2          USE W/S AREA 2 FOR NET DEMO BLOCK            
         ST    R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
*                                                                               
*              EDIT FIELDS                                                      
*                                                                               
         MVI   FTERMFLG,0          FOLLOWING FIELDS ARE REQUIRED                
         LA    R2,SPLCLIH          CLIENT. REQUIRED                             
         NETGO NVCLI,DMCB,SPLCLIN                                               
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         LA    R2,SPLPROH          PRODUCT REQUIRED                             
         NETGO NVPRD,DMCB,SPLPRON                                               
         OI    SPLPRONH+6,X'80'    XMIT PRODUCT NAME                            
*                                                                               
         MVI   FTERMFLG,1                                                       
         LA    R2,SPLESTH          ESTIMATE. OPTIONAL                           
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK   ALLOWS RANGE OF ESTS            
         OI    SPLESTNH+6,X'80'                  AND ALL,NO                     
*                                                                               
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB        NETWORK. OPTIONAL.                          
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN  DAYPART. OPTIONAL.                           
         OI    SPLDPTNH+6,X'80'     XMIT DAYPART NAME                           
*                                                                               
         LA    R2,SPLPAKH                                                       
         NETGO NVPAKLOK,DMCB,SPLPAKN   PACKAGE. OPTIONAL. BOTH AND LOCK         
         OI    SPLPAKNH+6,X'80'          ALLOWED.                               
*                                                                               
         LA    R2,SPLFLAVH                                                      
         MVI   FLAVOR,C'E'                                                      
         CLI   5(R2),0                                                          
         BE    EDD                                                              
         MVC   FLAVOR,8(R2)                                                     
         LA    R3,FLAVLIST                                                      
         BAS   RE,LISTVAL                                                       
*                                                                               
EDD      LA    R2,SPLDOPTH                                                      
         MVI   DATOPT,C'W'                                                      
         CLI   5(R2),0                                                          
         BE    EDE                                                              
         MVC   DATOPT,8(R2)                                                     
         LA    R3,DOPTLIST                                                      
         BAS   RE,LISTVAL                                                       
*                                                                               
EDE      LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE. OPTIONAL.                        
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE. OPTIONAL.                          
*                                   ALSO CKS END NOT BEFORE START.              
         LA    R2,SPLDEMH                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK   DEMOS. OPTIONAL.                    
         B     ED8                                                              
         SPACE 2                                                                
ED6      MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
ED8      LA    R2,SPLCLIH                                                       
*                                                                               
         B     XMOD                                                             
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
XIT      XIT1                                                                   
*                                                                               
*              ROUTINE TO VALIDATE AGAINST A LIST                               
         SPACE 2                                                                
LISTVAL  NTR1                                                                   
         SPACE 2                                                                
LISTVAL2 CLC   0(1,R3),8(R2)                                                    
         BE    XIT                                                              
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   LISTVAL2                                                         
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
*                                                                               
FLAVLIST DC    C'EA',X'FF'                                                      
DOPTLIST DC    C'MW',X'FF'                                                      
         EJECT                                                                  
*                                                                               
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDF1D                                                       
*                                                                               
**** PASSED TO EDIT *****                                                       
*                                                                               
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
****  ARGS TO EDIT ****                                                         
DPFILT   DS    CL1                                                              
FLAVOR   DS    CL1                                                              
DATOPT   DS    CL1                                                              
*                                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'006NEMED01   05/02/02'                                      
         END                                                                    
