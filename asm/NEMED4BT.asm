*          DATA SET NEMED4BT   AT LEVEL 002 AS OF 05/01/02                      
*          DATA SET NEMED4B    AT LEVEL 020 AS OF 02/23/99                      
*PHASE T31E4BA,+0                                                               
         TITLE 'T31E4B - EDIT FOR THREE IN ONE'                                 
T31E4B   CSECT                                                                  
         PRINT NOGEN                                                            
         NMOD1 0,**THED**                                                       
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
**       MVC   NBANBUFF,ANETWS1                                                 
**       MVI   NBQINIT,0           DO ALL VALIDATIONS EVERY TIME                
**       L     R7,ANETWS2          USE W/S AREA 2 FOR NET DEMO BLOCK            
         L     R7,ANETWS1          USE W1 FOR DEMOS/WRKAREA                     
         ST    R7,NBADEM                                                        
         USING NDDEMBLK,R7                                                      
* USE W2 ETC TEMPORARILY FOR NBANBUFF                                           
         MVC   NBANBUFF,ANETWS2    USE W2,W3,W4 ETC FOR 4K NBANBUFF             
                                                                                
                                                                                
*              EDIT FIELDS                                                      
         SPACE 3                                                                
         MVI   FTERMFLG,0          FIELDS ARE REQUIRED                          
         LA    R2,SPLCLIH                                                       
         NETGO NVCLI,DMCB,SPLCLIN  CLIENT. REQUIRED.                            
         OI    SPLCLINH+6,X'80'    TRANSMIT CLIENT NAME                         
*                                                                               
         LA    R2,SPLPROH                                                       
         NETGO NVPRD,DMCB,SPLPRON  PRODUCT. REQUIRED.                           
*                                                                               
         LA    R2,SPLESTH                                                       
         NETGO NVESTRNG,DMCB,SPLESTN,NDDEMBLK  ALLOW RANGE OF ESTS              
         OI    SPLESTNH+6,X'80'      TRANSMIT ESTIMATE NAME                     
         NETGO NVDELHOM,DMCB,NDDEMOS    REMOVE HOMES REQUESTS                   
*                                                                               
         MVI   FTERMFLG,1          OPTIONAL FIELDS                              
         LA    R2,SPLNETH                                                       
         NETGO NVNETALL,DMCB                                                    
*                                                                               
         LA    R2,SPLDPTH                                                       
         NETGO NVDPT,DMCB,SPLDPTN  DAYPART. OPTIONAL                            
         OI    SPLDPTNH+6,X'80'      TRANSMIT DAYPART NAME                      
*                                                                               
         LA    R2,SPLPAKH                                                       
         CLI   5(R2),0                                                          
         BE    EDB                                                              
         CLC   8(3,R2),=C'ALL'                                                  
         BE    EDB                                                              
         NETGO NVPAK,DMCB,SPLPAKN  PACKAGE. OPTIONAL                            
         OI    SPLPAKNH+6,X'80'    TRANSMIT PACKAGE.                            
*                                                                               
EDB      LA    R2,SPLSEQH          PROG SEQ OPTION                              
         NETGO NVPSEQ,DMCB                                                      
*                                                                               
         LA    R2,SPLFLAVH                                                      
         LA    R3,FLAVLIST                                                      
         BAS   RE,LISTVAL                                                       
         MVC   FLAVOR,0(R3)                                                     
*                                                                               
         LA    R2,SPLDOPTH                                                      
         LA    R3,DOPTLIST                                                      
         BAS   RE,LISTVAL                                                       
         MVC   DOPT,0(R3)                                                       
*                                                                               
         LA    R2,SPLDETH                                                       
         LA    R3,DETLIST                                                       
         BAS   RE,LISTVAL                                                       
         MVC   DETAIL,0(R3)                                                     
*                                                                               
         LA    R2,SPLSTRTH                                                      
         NETGO NVSTRDAT,DMCB       START DATE OPTIONAL.                         
*                                                                               
         LA    R2,SPLENDH                                                       
         NETGO NVENDDAT,DMCB       END DATE. OPTIONAL. CKS END>=START           
*                                                                               
ED3B     LA    R2,SPLDEMH                                                       
         NETGO NVDEM,DMCB,DBLOCK,NDDEMBLK                                       
*                                                                               
         LA    R2,SPLFILTH                                                      
         NETGO NVFILT,DMCB                                                      
         MVC   FILTER,SPLFILT                                                   
*                                                                               
         LA    R2,SPLCLIH                                                       
*                                                                               
         B     XMOD                                                             
*                                                                               
EDERR    GOTO1 ERREX,DMCB                                                       
*                                                                               
XMOD     XIT1  REGS=(R2)                                                        
*                                                                               
XIT      XIT1                                                                   
         EJECT                                                                  
*              ROUTINE TO VALIDATE AGAINST A LIST                               
         SPACE 2                                                                
LISTVAL  NTR1                                                                   
*                                                                               
LISTVAL2 CLC   0(1,R3),8(R2)                                                    
         BE    XITLV                                                            
         LA    R3,1(R3)                                                         
         CLI   0(R3),X'FF'                                                      
         BNE   LISTVAL2                                                         
         MVI   ERROR,INVALID                                                    
         B     EDERR                                                            
XITLV    XIT1  REGS=(R3)           RETURN VALUE                                 
*                                                                               
FLAVLIST DC    C'EVP',X'FF'                                                     
DOPTLIST DC    C'MQW',X'FF'                                                     
DETLIST  DC    C'PNE',X'FF'                                                     
         EJECT                                                                  
         PRINT OFF                                                              
       ++INCLUDE NETINCLS                                                       
         PRINT ON                                                               
*                                                                               
       ++INCLUDE NEMEDFFD                                                       
         ORG   CONTAGH                                                          
       ++INCLUDE NEMEDEBD                                                       
*                                                                               
         DSECT                                                                  
       ++INCLUDE NETDEMOT                                                       
       ++INCLUDE DEDBLOCK                                                       
*                                                                               
*  ARG PASSED TO PRINT MODULE                                                   
FLAVOR   DS    CL1                                                              
DOPT     DS    CL1                                                              
DETAIL   DS    CL1                                                              
FILTER   DS    CL1                                                              
*                                                                               
OI1T     EQU   X'80'                                                            
         PRINT ON                                                               
**PAN#1  CSECT                                                                  
**PAN#1  DC    CL21'002NEMED4BT  05/01/02'                                      
         END                                                                    
